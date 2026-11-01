# Practicum 2 - CSV Ingestion into SQLite 
# Author: Akshay Ashok Bannatti


# Packages used 
if (!requireNamespace("RSQLite",  quietly = TRUE)) install.packages("RSQLite")
if (!requireNamespace("DBI",      quietly = TRUE)) install.packages("DBI")

library(RSQLite)
library(DBI)

intake_dir <- "data/intake"       
ref_dir    <- "data/reference"   
db_path    <- "data/expenses.sqlite"

con <- dbConnect(RSQLite::SQLite(), db_path)
cat("Connected to SQLite:", db_path, "\n")

# Create Tables

dbExecute(con, "
CREATE TABLE IF NOT EXISTS employees (
  employee_id   INTEGER PRIMARY KEY AUTOINCREMENT,
  last_name     TEXT    NOT NULL,
  first_name    TEXT    NOT NULL,
  UNIQUE (last_name, first_name)
);")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS banks (
  bank_id   INTEGER PRIMARY KEY AUTOINCREMENT,
  bank_name TEXT    NOT NULL UNIQUE
);")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS categories (
  category_id   INTEGER PRIMARY KEY AUTOINCREMENT,
  category_name TEXT NOT NULL UNIQUE
);")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS subcategories (
  subcategory_id   INTEGER PRIMARY KEY AUTOINCREMENT,
  subcategory_name TEXT    NOT NULL,
  category_id      INTEGER NOT NULL REFERENCES categories(category_id),
  UNIQUE (subcategory_name, category_id)
);")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS currencies (
  currency_id   INTEGER PRIMARY KEY AUTOINCREMENT,
  currency_code TEXT NOT NULL UNIQUE
);")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS clients (
  client_id   INTEGER PRIMARY KEY AUTOINCREMENT,
  client_name TEXT NOT NULL UNIQUE
);")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS projects (
  project_id   TEXT    PRIMARY KEY,
  project_name TEXT    NOT NULL,
  client_id    INTEGER NOT NULL REFERENCES clients(client_id),
  budget_usd   REAL    NOT NULL
);")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS emp_project_assignments (
  assignment_id INTEGER PRIMARY KEY AUTOINCREMENT,
  employee_id   INTEGER NOT NULL REFERENCES employees(employee_id),
  project_id    TEXT    NOT NULL REFERENCES projects(project_id),
  UNIQUE (employee_id, project_id)
);")

# Core transactions
dbExecute(con, "
CREATE TABLE IF NOT EXISTS transactions (
  transaction_id  INTEGER PRIMARY KEY AUTOINCREMENT,
  employee_id     INTEGER NOT NULL REFERENCES employees(employee_id),
  bank_id         INTEGER NOT NULL REFERENCES banks(bank_id),
  txn_date        TEXT    NOT NULL,
  description     TEXT    NOT NULL,
  amount          REAL    NOT NULL,
  category_id     INTEGER NOT NULL REFERENCES categories(category_id),
  subcategory_id  INTEGER NOT NULL REFERENCES subcategories(subcategory_id),
  currency_id     INTEGER NOT NULL REFERENCES currencies(currency_id),
  billable        INTEGER NOT NULL DEFAULT 1,
  source_month    TEXT    NOT NULL,
  source_year     INTEGER NOT NULL
);")

cat("Schema created/verified.\n")

# Helper functions used 

get_or_insert <- function(con, table, col_name, col_value) {

  sql_sel <- sprintf("SELECT rowid FROM %s WHERE %s = ?", table, col_name)
  res <- dbGetQuery(con, sql_sel, params = list(col_value))
  if (nrow(res) > 0) return(res[[1, 1]])
  
  sql_ins <- sprintf("INSERT OR IGNORE INTO %s (%s) VALUES (?)", table, col_name)
  dbExecute(con, sql_ins, params = list(col_value))
  res2 <- dbGetQuery(con, sql_sel, params = list(col_value))
  return(res2[[1, 1]])
}

get_or_insert_employee <- function(con, last_name, first_name) {
  res <- dbGetQuery(con,
                    "SELECT employee_id FROM employees WHERE last_name = ? AND first_name = ?",
                    params = list(last_name, first_name))
  if (nrow(res) > 0) return(res[[1, 1]])
  dbExecute(con,
            "INSERT OR IGNORE INTO employees (last_name, first_name) VALUES (?, ?)",
            params = list(last_name, first_name))
  res2 <- dbGetQuery(con,
                     "SELECT employee_id FROM employees WHERE last_name = ? AND first_name = ?",
                     params = list(last_name, first_name))
  return(res2[[1, 1]])
}

get_or_insert_subcat <- function(con, subcat_name, category_id) {
  res <- dbGetQuery(con,
                    "SELECT subcategory_id FROM subcategories WHERE subcategory_name = ? AND category_id = ?",
                    params = list(subcat_name, category_id))
  if (nrow(res) > 0) return(res[[1, 1]])
  dbExecute(con,
            "INSERT OR IGNORE INTO subcategories (subcategory_name, category_id) VALUES (?, ?)",
            params = list(subcat_name, category_id))
  res2 <- dbGetQuery(con,
                     "SELECT subcategory_id FROM subcategories WHERE subcategory_name = ? AND category_id = ?",
                     params = list(subcat_name, category_id))
  return(res2[[1, 1]])
}

parse_filename <- function(fname) {

  base <- tools::file_path_sans_ext(basename(fname))

  pattern <- "^([^,]+),\\s+([^(]+)\\(([^)]+)\\)\\s+(\\w+)\\s+(\\d{4})$"
  m <- regmatches(base, regexec(pattern, base))[[1]]
  if (length(m) < 6) {
    warning(sprintf("Could not parse filename: %s", fname))
    return(NULL)
  }
  list(
    last  = trimws(m[2]),
    first = trimws(m[3]),
    bank  = trimws(m[4]),
    month = trimws(m[5]),
    year  = as.integer(m[6])
  )
}


load_reference_data <- function(con, ref_dir) {
  projects_path    <- file.path(ref_dir, "projects.csv")
  assignments_path <- file.path(ref_dir, "assignments.csv")
  
  if (!file.exists(projects_path)) {
    cat("  No projects.csv found in reference dir; skipping.\n")
    return(invisible(NULL))
  }
  
  proj_df <- read.csv(projects_path, stringsAsFactors = FALSE)
  
  for (i in seq_len(nrow(proj_df))) {
    row <- proj_df[i, ]
   
    client_id <- get_or_insert(con, "clients", "client_name", row$client)
 
    exists <- dbGetQuery(con,
                         "SELECT COUNT(*) AS n FROM projects WHERE project_id = ?",
                         params = list(row$project_id))[[1, 1]]
    if (exists == 0) {
      dbExecute(con,
                "INSERT INTO projects (project_id, project_name, client_id, budget_usd)
         VALUES (?, ?, ?, ?)",
                params = list(row$project_id, row$project_name, client_id, row$budget_usd))
    }
  }
  cat(sprintf("  Loaded %d projects.\n", nrow(proj_df)))
  
  if (!file.exists(assignments_path)) return(invisible(NULL))
  
  assign_df <- read.csv(assignments_path, stringsAsFactors = FALSE)
  for (i in seq_len(nrow(assign_df))) {
    row <- assign_df[i, ]
    emp_id <- get_or_insert_employee(con, row$emp_last, row$emp_first)
    exists <- dbGetQuery(con,
                         "SELECT COUNT(*) AS n FROM emp_project_assignments
       WHERE employee_id = ? AND project_id = ?",
                         params = list(emp_id, row$project_id))[[1, 1]]
    if (exists == 0) {
      dbExecute(con,
                "INSERT INTO emp_project_assignments (employee_id, project_id) VALUES (?, ?)",
                params = list(emp_id, row$project_id))
    }
  }
  cat(sprintf("  Loaded %d assignments.\n", nrow(assign_df)))
}

# Ingest Single CSV File 

ingest_csv <- function(con, fpath) {
  meta <- parse_filename(fpath)
  if (is.null(meta)) return(0L)
  
  df <- read.csv(fpath, stringsAsFactors = FALSE)
  if (nrow(df) == 0) return(0L)
  
  emp_id  <- get_or_insert_employee(con, meta$last, meta$first)
  bank_id <- get_or_insert(con, "banks", "bank_name", meta$bank)
  
  inserted <- 0L
  
  for (i in seq_len(nrow(df))) {
    row <- df[i, ]
    
    cat_id    <- get_or_insert(con, "categories", "category_name", row$category)
    subcat_id <- get_or_insert_subcat(con, row$subcategory, cat_id)
    cur_id    <- get_or_insert(con, "currencies", "currency_code", row$currency)
    
    billable_int <- if (toupper(trimws(row$billable)) == "Y") 1L else 0L
    
    dbExecute(con,
              "INSERT INTO transactions
         (employee_id, bank_id, txn_date, description, amount,
          category_id, subcategory_id, currency_id, billable,
          source_month, source_year)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
              params = list(emp_id, bank_id, row$date, row$description,
                            row$amount, cat_id, subcat_id, cur_id,
                            billable_int, meta$month, meta$year))
    inserted <- inserted + 1L
  }
  
  return(inserted)
}

# Main Ingestion Loop

cat("Loading reference data...\n")
load_reference_data(con, ref_dir)

csv_files <- list.files(intake_dir, pattern = "\\.csv$", full.names = TRUE)
cat(sprintf("\nFound %d CSV file(s) in '%s'\n", length(csv_files), intake_dir))

total_rows <- 0L
for (f in csv_files) {
  n <- ingest_csv(con, f)
  cat(sprintf("  Ingested: %-50s -> %d rows\n", basename(f), n))
  total_rows <- total_rows + n
}

cat(sprintf("\nTotal transactions inserted: %d\n", total_rows))


cat("\n--- Verification Queries ---\n")
cat("Employees:\n")
print(dbGetQuery(con, "SELECT * FROM employees"))

cat("Projects:\n")
print(dbGetQuery(con, "SELECT p.project_id, p.project_name, c.client_name, p.budget_usd
                        FROM projects p JOIN clients c USING(client_id)"))

cat("Transaction count by employee:\n")
print(dbGetQuery(con,
                 "SELECT e.last_name, e.first_name, COUNT(*) AS n_transactions
   FROM transactions t JOIN employees e USING(employee_id)
   GROUP BY e.employee_id"))

 
dbDisconnect(con)
cat("\nSQLite connection closed.\n")
