# ── PASTE THIS ENTIRE BLOCK INTO RSTUDIO CONSOLE ──
library(mongolite)
mongo_col <- mongo(collection="expense_reports", db="heracletes_expenses", url="mongodb://localhost:27017")
cat("Reports in MongoDB:", mongo_col$count(), "\n")

# Pull first report only
r1 <- mongo_col$find('{}', limit=1)
cat("Columns in r1:", paste(names(r1), collapse=", "), "\n")

# Check employee field
emp <- r1$employee[[1]]
cat("emp_id:", emp$emp_id, " | class:", class(emp$emp_id), "\n")

# Check line_items
raw_li <- r1$line_items
cat("raw_li class:", class(raw_li), " | length:", length(raw_li), "\n")
cat("raw_li[[1]] class:", class(raw_li[[1]]), "\n")

li <- if (is.list(raw_li[[1]])) raw_li[[1]] else raw_li
cat("line_items length:", length(li), "\n")

# Check first item
item <- li[[1]]
cat("item fields:", paste(names(item), collapse=", "), "\n")
cat("txn_date:", as.character(item$txn_date), "\n")
cat("category:", as.character(item$category), "\n")
cat("currency:", as.character(item$currency), "\n")
cat("amount:",   as.character(item$amount),   "\n")

# Check allocations
raw_a <- item$allocations
cat("raw_allocs class:", class(raw_a), " | length:", length(raw_a), "\n")
if (length(raw_a) > 0) {
  cat("raw_a[[1]] class:", class(raw_a[[1]]), "\n")
  cat("raw_a[[1]] names:", paste(names(raw_a[[1]]), collapse=", "), "\n")
  cat("project_code:", as.character(raw_a[[1]]$project_code), "\n")
  cat("client_code:",  as.character(raw_a[[1]]$client_code),  "\n")
}