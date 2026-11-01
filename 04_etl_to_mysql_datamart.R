# Practicum 2 - ETL into MySQL Analytical Datamart (Aiven)
# Author: Akshay Ashok Bannatti


# For Installation of the Package  
if (!requireNamespace("RSQLite",   quietly = TRUE)) install.packages("RSQLite")
if (!requireNamespace("DBI",       quietly = TRUE)) install.packages("DBI")
if (!requireNamespace("RMySQL",    quietly = TRUE)) install.packages("RMySQL")
if (!requireNamespace("mongolite", quietly = TRUE)) install.packages("mongolite")

library(RSQLite)
library(DBI)
library(RMySQL)
library(mongolite)

# Configuration 
sqlite_path <- "data/expenses.sqlite"
mongo_uri   <- "mongodb://localhost:27017"
mongo_db    <- "heracletes"
mongo_coll  <- "expense_reports"

db_host     <- Sys.getenv("MYSQL_HOST",     "mysql-28668c4b-dmspracticum.a.aivencloud.com")
db_port     <- as.integer(Sys.getenv("MYSQL_PORT", "15136"))
db_name     <- Sys.getenv("MYSQL_DBNAME",   "defaultdb")
db_user     <- Sys.getenv("MYSQL_USER",     "avnadmin")
db_password <- Sys.getenv("MYSQL_PASSWORD", "AVNS_IViZllANiN9ObvmKOnU")
db_ssl_ca   <- Sys.getenv("MYSQL_SSL_CA",
                          "C:/Users/admin/OneDrive/Documents/Practicum 1/CS5200.Practicum-I.BannattiA/CA Certificate.pem")


sqlite_con <- dbConnect(RSQLite::SQLite(), sqlite_path)
cat("Connected to SQLite.\n")

mongo_col <- mongo(collection = mongo_coll, db = mongo_db, url = mongo_uri)
cat("Connected to MongoDB.\n")

# Connect to Aiven MySQL
mysql_con <- dbConnect(
  RMySQL::MySQL(),
  host     = db_host,
  port     = db_port,
  user     = db_user,
  password = db_password,
  dbname   = db_name,
  ssl.ca   = db_ssl_ca
)
cat("Connected to Aiven MySQL.\n")

dbExecute(mysql_con, "SET NAMES utf8mb4")
dbExecute(mysql_con, "SET sql_mode = 'STRICT_TRANS_TABLES'")

# Helper used for the Safe string escaping

esc <- function(x) gsub("'", "''", as.character(x), fixed = TRUE)

# Helper used for the Execute MySQL DDL safely
safe_ddl <- function(con, sql) {
  tryCatch(dbExecute(con, sql), error = function(e) {
    cat("  DDL note:", conditionMessage(e), "\n")
  })
}

# Helper for the MySQL SELECT returning a single scalar
mysql_scalar <- function(con, sql) {
  res <- dbGetQuery(con, sql)
  if (nrow(res) == 0) return(NULL)
  res[[1, 1]]
}

# DDL to create Star Schema
cat("\nCreating star schema tables...\n")

safe_ddl(mysql_con, "DROP TABLE IF EXISTS fact_expenses")
safe_ddl(mysql_con, "DROP TABLE IF EXISTS dim_date")
safe_ddl(mysql_con, "DROP TABLE IF EXISTS dim_employee")
safe_ddl(mysql_con, "DROP TABLE IF EXISTS dim_client")
safe_ddl(mysql_con, "DROP TABLE IF EXISTS dim_project")
safe_ddl(mysql_con, "DROP TABLE IF EXISTS dim_category")
cat("All old tables dropped.\n")

safe_ddl(mysql_con, "
CREATE TABLE IF NOT EXISTS dim_date (
  date_key    INT         NOT NULL,
  full_date   DATE        NOT NULL,
  day         TINYINT     NOT NULL,
  month       TINYINT     NOT NULL,
  month_name  VARCHAR(10) NOT NULL,
  quarter     TINYINT     NOT NULL,
  year        SMALLINT    NOT NULL,
  PRIMARY KEY (date_key)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;")

safe_ddl(mysql_con, "
CREATE TABLE IF NOT EXISTS dim_employee (
  employee_key INT          NOT NULL AUTO_INCREMENT,
  employee_id  INT          NOT NULL,
  last_name    VARCHAR(100) NOT NULL,
  first_name   VARCHAR(100) NOT NULL,
  full_name    VARCHAR(200) NOT NULL,
  PRIMARY KEY (employee_key),
  UNIQUE KEY uq_employee_id (employee_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;")

safe_ddl(mysql_con, "
CREATE TABLE IF NOT EXISTS dim_client (
  client_key  INT          NOT NULL AUTO_INCREMENT,
  client_id   INT          NOT NULL,
  client_name VARCHAR(200) NOT NULL,
  PRIMARY KEY (client_key),
  UNIQUE KEY uq_client_id (client_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;")

safe_ddl(mysql_con, "
CREATE TABLE IF NOT EXISTS dim_project (
  project_key  INT           NOT NULL AUTO_INCREMENT,
  project_id   VARCHAR(20)   NOT NULL,
  project_name VARCHAR(200)  NOT NULL,
  client_key   INT           NOT NULL,
  budget_usd   DECIMAL(15,2) NOT NULL,
  PRIMARY KEY (project_key),
  UNIQUE KEY uq_project_id (project_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;")

safe_ddl(mysql_con, "
CREATE TABLE IF NOT EXISTS dim_category (
  category_key     INT          NOT NULL AUTO_INCREMENT,
  category_name    VARCHAR(100) NOT NULL,
  subcategory_name VARCHAR(100) NOT NULL,
  PRIMARY KEY (category_key),
  UNIQUE KEY uq_cat_subcat (category_name, subcategory_name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;")

safe_ddl(mysql_con, "
CREATE TABLE IF NOT EXISTS fact_expenses (
  fact_id        BIGINT        NOT NULL AUTO_INCREMENT,
  date_key       INT           NOT NULL,
  employee_key   INT           NOT NULL,
  project_key    INT           NOT NULL,
  client_key     INT           NOT NULL,
  category_key   INT           NOT NULL,
  txn_year       SMALLINT      NOT NULL,
  txn_month      TINYINT       NOT NULL,
  txn_quarter    TINYINT       NOT NULL,
  amount_local   DECIMAL(12,2) NOT NULL,
  currency_code  VARCHAR(5)    NOT NULL,
  billable       TINYINT(1)    NOT NULL,
  transaction_id INT           NOT NULL,
  PRIMARY KEY (fact_id, txn_year),
  INDEX idx_date_key     (date_key),
  INDEX idx_employee_key (employee_key),
  INDEX idx_project_key  (project_key),
  INDEX idx_client_key   (client_key),
  INDEX idx_category_key (category_key),
  INDEX idx_year_month   (txn_year, txn_month),
  INDEX idx_year_quarter (txn_year, txn_quarter)
)
ENGINE=InnoDB DEFAULT CHARSET=utf8mb4
PARTITION BY RANGE (txn_year) (
  PARTITION p2024    VALUES LESS THAN (2025),
  PARTITION p2025    VALUES LESS THAN (2026),
  PARTITION p2026    VALUES LESS THAN (2027),
  PARTITION p_future VALUES LESS THAN MAXVALUE
);")

cat("Schema created.\n")

# Used for the dimension for the dim_date 
cat("Loading dim_date...\n")

month_names <- c("January","February","March","April","May","June",
                 "July","August","September","October","November","December")

for (yr in 2024:2027) {
  for (mo in 1:12) {
    next_first <- as.Date(sprintf("%d-%02d-01", yr, mo)) + 32
    last_day   <- as.integer(format(as.Date(format(next_first, "%Y-%m-01")) - 1, "%d"))
    for (dy in 1:last_day) {
      date_obj <- as.Date(sprintf("%d-%02d-%02d", yr, mo, dy))
      date_key <- as.integer(format(date_obj, "%Y%m%d"))
      quarter  <- ceiling(mo / 3L)
      sql <- sprintf(
        "INSERT IGNORE INTO dim_date
           (date_key, full_date, day, month, month_name, quarter, year)
         VALUES (%d, '%s', %d, %d, '%s', %d, %d)",
        date_key, format(date_obj, "%Y-%m-%d"),
        dy, mo, esc(month_names[mo]), quarter, yr)
      dbExecute(mysql_con, sql)
    }
  }
}
cat("dim_date loaded.\n")


cat("Loading dim_employee...\n")
emps <- dbGetQuery(sqlite_con,
                   "SELECT employee_id, last_name, first_name FROM employees")
for (i in seq_len(nrow(emps))) {
  e <- emps[i, ]
  sql <- sprintf(
    "INSERT IGNORE INTO dim_employee
       (employee_id, last_name, first_name, full_name)
     VALUES (%d, '%s', '%s', '%s')",
    e$employee_id, esc(e$last_name), esc(e$first_name),
    esc(paste(e$first_name, e$last_name)))
  dbExecute(mysql_con, sql)
}
cat(sprintf("  %d employees loaded.\n", nrow(emps)))

# For loading the dimension of dim_client 
cat("Loading dim_client...\n")
clients <- dbGetQuery(sqlite_con, "SELECT client_id, client_name FROM clients")
for (i in seq_len(nrow(clients))) {
  cl <- clients[i, ]
  sql <- sprintf(
    "INSERT IGNORE INTO dim_client (client_id, client_name) VALUES (%d, '%s')",
    cl$client_id, esc(cl$client_name))
  dbExecute(mysql_con, sql)
}
cat(sprintf("  %d clients loaded.\n", nrow(clients)))


cat("Loading dim_project...\n")
projects <- dbGetQuery(sqlite_con,
                       "SELECT project_id, project_name, client_id, budget_usd FROM projects")
for (i in seq_len(nrow(projects))) {
  proj <- projects[i, ]
  ck   <- mysql_scalar(mysql_con,
                       sprintf("SELECT client_key FROM dim_client WHERE client_id = %d",
                               proj$client_id))
  if (is.null(ck)) next
  sql <- sprintf(
    "INSERT IGNORE INTO dim_project
       (project_id, project_name, client_key, budget_usd)
     VALUES ('%s', '%s', %d, %.2f)",
    esc(proj$project_id), esc(proj$project_name), ck, proj$budget_usd)
  dbExecute(mysql_con, sql)
}
cat(sprintf("  %d projects loaded.\n", nrow(projects)))

# Dimension dim_category 
cat("Loading dim_category...\n")
cats <- dbGetQuery(sqlite_con,
                   "SELECT cat.category_name, sub.subcategory_name
   FROM subcategories sub JOIN categories cat USING(category_id)")
for (i in seq_len(nrow(cats))) {
  sql <- sprintf(
    "INSERT IGNORE INTO dim_category (category_name, subcategory_name)
     VALUES ('%s', '%s')",
    esc(cats$category_name[i]), esc(cats$subcategory_name[i]))
  dbExecute(mysql_con, sql)
}
cat(sprintf("  %d category/subcategory pairs loaded.\n", nrow(cats)))

# This is for the fact table expenses 
cat("Loading fact_expenses...\n")

all_txn <- dbGetQuery(sqlite_con, "
  SELECT
    t.transaction_id,
    t.employee_id,
    t.txn_date,
    t.amount,
    t.billable,
    cur.currency_code,
    cat.category_name,
    sub.subcategory_name
  FROM transactions t
  JOIN categories    cat USING(category_id)
  JOIN subcategories sub USING(subcategory_id)
  JOIN currencies    cur USING(currency_id)
")

# Build hashmap
all_reports <- mongo_col$find('{}')
txn_splits  <- list()
if (nrow(all_reports) > 0) {
  for (ri in seq_len(nrow(all_reports))) {
    txns_in_report <- all_reports$transactions[[ri]]
    if (is.null(txns_in_report) || length(txns_in_report) == 0) next
    for (ti in seq_along(txns_in_report)) {
      txn_item <- txns_in_report[[ti]]
      tid_key  <- as.character(txn_item$transaction_id)
      splits   <- txn_item$project_splits
      if (length(splits) > 0) txn_splits[[tid_key]] <- splits
    }
  }
}
cat(sprintf("  Loaded project splits for %d transactions from MongoDB.\n",
            length(txn_splits)))


emp_keys  <- dbGetQuery(mysql_con, "SELECT employee_id, employee_key FROM dim_employee")
proj_keys <- dbGetQuery(mysql_con, "SELECT project_id, project_key, client_key FROM dim_project")
cat_keys  <- dbGetQuery(mysql_con, "SELECT category_name, subcategory_name, category_key FROM dim_category")

emp_map  <- setNames(emp_keys$employee_key, as.character(emp_keys$employee_id))
proj_map <- setNames(proj_keys$project_key, proj_keys$project_id)
cli_map  <- setNames(proj_keys$client_key,  proj_keys$project_id)
cat_map  <- setNames(cat_keys$category_key,
                     paste(cat_keys$category_name, cat_keys$subcategory_name, sep = "|"))


emp_first_proj <- dbGetQuery(sqlite_con,
                             "SELECT employee_id, project_id FROM emp_project_assignments GROUP BY employee_id")
emp_proj_map <- setNames(emp_first_proj$project_id,
                         as.character(emp_first_proj$employee_id))

fact_rows_inserted <- 0L

for (i in seq_len(nrow(all_txn))) {
  txn <- all_txn[i, ]
  
  date_obj <- as.Date(txn$txn_date)
  date_key <- as.integer(format(date_obj, "%Y%m%d"))
  yr       <- as.integer(format(date_obj, "%Y"))
  mo       <- as.integer(format(date_obj, "%m"))
  qtr      <- ceiling(mo / 3L)
  
  e_key   <- emp_map[[as.character(txn$employee_id)]]
  cat_key <- cat_map[[paste(txn$category_name, txn$subcategory_name, sep = "|")]]
  if (is.null(e_key) || is.null(cat_key)) next
  
  tid_key <- as.character(txn$transaction_id)
  splits  <- txn_splits[[tid_key]]
  
  if (!is.null(splits) && length(splits) > 0) {
    for (sp in splits) {
      p_key <- proj_map[[sp$project_id]]
      c_key <- cli_map[[sp$project_id]]
      if (is.null(p_key) || is.null(c_key)) next
      amt <- round(txn$amount * sp$pct / 100, 2)
      sql <- sprintf(
        "INSERT IGNORE INTO fact_expenses
           (date_key, employee_key, project_key, client_key, category_key,
            txn_year, txn_month, txn_quarter,
            amount_local, currency_code, billable, transaction_id)
         VALUES (%d, %d, %d, %d, %d, %d, %d, %d, %.2f, '%s', 1, %d)",
        date_key, e_key, p_key, c_key, cat_key,
        yr, mo, qtr, amt, esc(txn$currency_code), txn$transaction_id)
      dbExecute(mysql_con, sql)
      fact_rows_inserted <- fact_rows_inserted + 1L
    }
  } else {
    pid   <- emp_proj_map[[as.character(txn$employee_id)]]
    if (is.null(pid)) next
    p_key <- proj_map[[pid]]
    c_key <- cli_map[[pid]]
    if (is.null(p_key) || is.null(c_key)) next
    sql <- sprintf(
      "INSERT IGNORE INTO fact_expenses
         (date_key, employee_key, project_key, client_key, category_key,
          txn_year, txn_month, txn_quarter,
          amount_local, currency_code, billable, transaction_id)
       VALUES (%d, %d, %d, %d, %d, %d, %d, %d, %.2f, '%s', 0, %d)",
      date_key, e_key, p_key, c_key, cat_key,
      yr, mo, qtr, txn$amount, esc(txn$currency_code), txn$transaction_id)
    dbExecute(mysql_con, sql)
    fact_rows_inserted <- fact_rows_inserted + 1L
  }
}

cat(sprintf("  %d fact rows inserted.\n", fact_rows_inserted))

# Verify 
cat("\n--- Verification ---\n")
cat("Row counts per table:\n")
for (tbl in c("dim_date","dim_employee","dim_client","dim_project",
              "dim_category","fact_expenses")) {
  n <- mysql_scalar(mysql_con, sprintf("SELECT COUNT(*) FROM `%s`", tbl))
  cat(sprintf("  %-20s %d rows\n", tbl, n))
}

cat("\nSample fact query (total expense per category per year):\n")
sample_q <- dbGetQuery(mysql_con, "
  SELECT
    d.year                        AS year,
    c.category_name               AS category,
    COUNT(*)                      AS n_transactions,
    ROUND(SUM(f.amount_local), 2) AS total_amount
  FROM fact_expenses f
  JOIN dim_date     d ON f.date_key     = d.date_key
  JOIN dim_category c ON f.category_key = c.category_key
  GROUP BY d.year, c.category_name
  ORDER BY d.year, total_amount
")
print(sample_q)


dbDisconnect(sqlite_con)
dbDisconnect(mysql_con)
cat("\nAll connections closed. ETL complete.\n")
