# Practicum 2 - Load Expense Reports into MongoDB
# Author: Akshay Ashok Bannatti


# For package Installation
if (!requireNamespace("RSQLite",   quietly = TRUE)) install.packages("RSQLite")
if (!requireNamespace("DBI",       quietly = TRUE)) install.packages("DBI")
if (!requireNamespace("mongolite", quietly = TRUE)) install.packages("mongolite")

library(RSQLite)
library(DBI)
library(mongolite)


# Configuration
db_path <- "data/expenses.sqlite"
mongo_uri <- "mongodb://localhost:27017"   
mongo_db <- "heracletes"
mongo_coll <- "expense_reports"


# SQLite connection 
sqlite_con <- dbConnect(RSQLite::SQLite(), db_path)
cat("Connected to SQLite.\n")


# Connect to MongoDB 
mongo_col <- mongo(collection = mongo_coll, db = mongo_db, url = mongo_uri)
cat("Connected to MongoDB collection:", mongo_coll, "\n")



# Dropping the existing collection we have for the clean re-run
mongo_col$drop()
cat("Dropped existing expense_reports collection.\n")



# To get the Data from SQLite 

transactions <- dbGetQuery(sqlite_con, "
  SELECT
    t.transaction_id,
    e.employee_id,
    e.last_name,
    e.first_name,
    b.bank_name,
    t.txn_date,
    t.description,
    t.amount,
    cat.category_name   AS category,
    sub.subcategory_name AS subcategory,
    cur.currency_code   AS currency,
    t.billable,
    t.source_month,
    t.source_year
  FROM transactions t
  JOIN employees  e   USING(employee_id)
  JOIN banks      b   USING(bank_id)
  JOIN categories cat USING(category_id)
  JOIN subcategories sub USING(subcategory_id)
  JOIN currencies cur USING(currency_id)
  ORDER BY e.employee_id, t.source_year, t.source_month, t.txn_date
")

emp_assignments <- dbGetQuery(sqlite_con, "
  SELECT
    a.employee_id,
    e.last_name,
    e.first_name,
    a.project_id,
    p.project_name,
    c.client_name
  FROM emp_project_assignments a
  JOIN employees e USING(employee_id)
  JOIN projects  p USING(project_id)
  JOIN clients   c USING(client_id)
")

cat(sprintf("Loaded %d transactions and %d assignments from SQLite.\n",
            nrow(transactions), nrow(emp_assignments)))

dbDisconnect(sqlite_con)
cat("SQLite connection closed.\n\n")


# Helper for making the Build Expense Allocations for a Report

build_allocations <- function(txn_rows, emp_projects) {
 
  n_proj <- nrow(emp_projects)
  if (n_proj == 0) return(list())
  
  pct_each <- round(100 / n_proj, 2)

  pcts <- rep(pct_each, n_proj)
  pcts[n_proj] <- 100 - sum(pcts[-n_proj])
  
  lapply(seq_along(txn_rows[["transaction_id"]]), function(i) {
    txn <- lapply(txn_rows, `[[`, i)   
    
    
    if (txn$billable == 0) return(NULL)
    
    project_allocs <- lapply(seq_len(n_proj), function(j) {
      list(
        project_id   = emp_projects$project_id[j],
        project_name = emp_projects$project_name[j],
        client_name  = emp_projects$client_name[j],
        pct          = pcts[j],
        allocated_amount = round(txn$amount * pcts[j] / 100, 2)
      )
    })
    
    list(
      transaction_id  = txn$transaction_id,
      txn_date        = txn$txn_date,
      description     = txn$description,
      amount          = txn$amount,
      category        = txn$category,
      subcategory     = txn$subcategory,
      currency        = txn$currency,
      bank            = txn$bank_name,
      billable        = TRUE,
      project_splits  = project_allocs
    )
  })
}

# Build and put the Expense Reports of it

groups <- unique(transactions[, c("employee_id","last_name","first_name",
                                  "source_month","source_year")])

cat(sprintf("Building %d expense report(s)...\n", nrow(groups)))

reports_inserted <- 0L

for (gi in seq_len(nrow(groups))) {
  grp <- groups[gi, ]
  
  mask <- (transactions$employee_id == grp$employee_id &
             transactions$source_month == grp$source_month &
             transactions$source_year  == grp$source_year)
  txn_subset <- transactions[mask, ]
  
  emp_proj <- emp_assignments[emp_assignments$employee_id == grp$employee_id, ]
  

  line_items_raw <- build_allocations(txn_subset, emp_proj)
  line_items <- Filter(Negate(is.null), line_items_raw)
  
  
  nb_mask <- txn_subset$billable == 0
  non_billable <- lapply(which(nb_mask), function(i) {
    row <- txn_subset[i, ]
    list(
      transaction_id  = row$transaction_id,
      txn_date        = row$txn_date,
      description     = row$description,
      amount          = row$amount,
      category        = row$category,
      subcategory     = row$subcategory,
      currency        = row$currency,
      bank            = row$bank_name,
      billable        = FALSE,
      project_splits  = list()
    )
  })
  
  all_items <- c(line_items, non_billable)
  
  # Calculating the subtotals for every project
  proj_subtotals <- list()
  for (li in line_items) {
    for (sp in li$project_splits) {
      pid <- sp$project_id
      if (is.null(proj_subtotals[[pid]])) {
        proj_subtotals[[pid]] <- list(
          project_id   = sp$project_id,
          project_name = sp$project_name,
          client_name  = sp$client_name,
          subtotal     = 0
        )
      }
      proj_subtotals[[pid]]$subtotal <-
        proj_subtotals[[pid]]$subtotal + sp$allocated_amount
    }
  }
  proj_subtotals_list <- unname(proj_subtotals)
  
 
  cat_totals <- tapply(txn_subset$amount, txn_subset$category, sum)
  cat_subtotals <- lapply(names(cat_totals), function(cn) {
    list(category = cn, total = round(cat_totals[[cn]], 2))
  })
  
  approver <- list(
    name  = "Director Finance",
    email = "finance.approver@heracletes.com"
  )
  
  # To Make the full expense document in the report
  report_doc <- list(
    report_id        = sprintf("ER-%d-%s-%d",
                               grp$employee_id, grp$source_month, grp$source_year),
    status           = "submitted",
    created_at       = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    period_month     = grp$source_month,
    period_year      = grp$source_year,
    employee         = list(
      employee_id  = grp$employee_id,
      last_name    = grp$last_name,
      first_name   = grp$first_name,
      full_name    = paste(grp$first_name, grp$last_name)
    ),
    approver         = approver,
    transactions     = all_items,
    project_subtotals = proj_subtotals_list,
    category_subtotals = cat_subtotals,
    total_amount     = round(sum(txn_subset$amount), 2),
    n_transactions   = nrow(txn_subset)
  )
  
  mongo_col$insert(report_doc)
  reports_inserted <- reports_inserted + 1L
  cat(sprintf("  Inserted report %s (%d transactions)\n",
              report_doc$report_id, nrow(txn_subset)))
}

cat(sprintf("\nTotal expense reports inserted into MongoDB: %d\n", reports_inserted))


# Verification 
cat("\n--- Verification ---\n")
cat(sprintf("Documents in collection: %d\n", mongo_col$count()))


# Show summary of one report

sample_doc <- mongo_col$find('{}', limit = 1)
if (nrow(sample_doc) > 0) {
  cat("Sample report ID:  ", as.character(unlist(sample_doc$report_id[1])),    "\n")
  cat("Employee:          ", as.character(sample_doc$employee[[1]]$full_name), "\n")
  cat("Period:            ", as.character(unlist(sample_doc$period_month[1])),
      as.integer(unlist(sample_doc$period_year[1])),    "\n")
  cat("Total amount:      ", as.numeric(unlist(sample_doc$total_amount[1])),   "\n")
  cat("N transactions:    ", as.integer(unlist(sample_doc$n_transactions[1])), "\n")
}

cat("\nMongoDB stage complete.\n")
