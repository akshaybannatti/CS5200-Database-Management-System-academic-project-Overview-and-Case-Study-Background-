# Practicum 2 - Synthetic Data Generator
# Author: Akshay Ashok Bannatti



# To Setting it up

set.seed(42)

output_dir <- "data/intake"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Data

employees <- data.frame(
  last_name  = c("Smith",   "Patel",   "Bannatti"),
  first_name = c("Jordan",  "Riya",    "Akshay"),
  stringsAsFactors = FALSE
)

banks <- c("Amex", "Citi")
months <- c("MAR", "APR")
years <- c(2026, 2026)

categories <- c("Travel", "Lodging", "Meals", "Office", "Telecom")

subcategories <- list(
  Travel = c("Airfare", "Taxi", "Fuel", "Parking"),
  Lodging = c("Hotel", "Airbnb"),
  Meals = c("Restaurants", "Catering"),
  Office = c("Supplies", "Printing"),
  Telecom = c("Phone", "Internet")
)

currencies <- c("USD", "BRL", "EUR")

vendors <- c(
  "DELTA AIRLINES", "UBER", "MARRIOTT HOTEL", "HILTON",
  "MCDONALDS", "STARBUCKS", "AMAZON", "STAPLES",
  "AT&T", "VERIZON", "LUFTHANSA", "SHELL GAS",
  "HUDSON NEWS", "SUBWAY", "HERTZ RENT-A-CAR",
  "HOLIDAY INN", "UNITED AIRLINES", "LYFT"
)

# Helper

generate_transactions <- function(n_trans, month_str, year) {
  
  month_num <- match(tools::toTitleCase(tolower(month_str)), month.abb)
  start_date <- as.Date(sprintf("%d-%02d-01", year, month_num))
  
  end_date <- as.Date(sprintf("%d-%02d-01", year, month_num)) + 32
  end_date <- as.Date(format(end_date, "%Y-%m-01")) - 1
  dates <- sample(seq(start_date, end_date, by = "day"), n_trans, replace = TRUE)
  dates <- sort(dates)
  
  cat_vec <- sample(categories, n_trans, replace = TRUE)
  subcat_vec <- mapply(function(cat) sample(subcategories[[cat]], 1), cat_vec)
  amount_vec <- round(-abs(rnorm(n_trans, mean = 150, sd = 120)), 2)  
 
  refund_idx <- sample(seq_len(n_trans), max(1, floor(n_trans * 0.05)))
  amount_vec[refund_idx] <- abs(amount_vec[refund_idx])
  
  data.frame(
    date        = format(dates, "%Y-%m-%d"),
    description = sample(vendors, n_trans, replace = TRUE),
    amount      = amount_vec,
    category    = cat_vec,
    subcategory = subcat_vec,
    currency    = sample(currencies, n_trans, replace = TRUE, prob = c(0.70, 0.20, 0.10)),
    billable    = sample(c("Y", "N"), n_trans, replace = TRUE, prob = c(0.75, 0.25)),
    stringsAsFactors = FALSE
  )
}

# Main Loop used to generate CSVs
file_count <- 0

for (mi in seq_along(months)) {
  month_str <- months[mi]
  year      <- years[mi]
  
  for (ei in seq_len(nrow(employees))) {
    last  <- employees$last_name[ei]
    first <- employees$first_name[ei]
    
    for (bank in banks) {
      n <- sample(15:25, 1)
      df <- generate_transactions(n, month_str, year)
      
      fname <- sprintf("%s, %s (%s) %s %d.csv", last, first, bank, month_str, year)
      fpath <- file.path(output_dir, fname)
      
      write.csv(df, fpath, row.names = FALSE)
      file_count <- file_count + 1
      cat(sprintf("  Created: %s  (%d rows)\n", fname, n))
    }
  }
}

cat(sprintf("\nDone. %d CSV files written to '%s/'\n", file_count, output_dir))

projects <- data.frame(
  project_id   = c("CC100", "CC200", "CC300", "CC654"),
  project_name = c("Usiminas Iron Survey", "Rio Tinto Feasibility",
                   "Cerro Verde Assessment", "Codelco Expansion"),
  client       = c("Usiminas", "Rio Tinto", "Cerro Verde", "Codelco"),
  budget_usd   = c(500000, 1200000, 750000, 980000),
  stringsAsFactors = FALSE
)

assignments <- data.frame(
  emp_last    = c("Smith",    "Smith",    "Patel",    "Patel",    "Bannatti", "Bannatti"),
  emp_first   = c("Jordan",   "Jordan",   "Riya",     "Riya",     "Akshay",   "Akshay"),
  project_id  = c("CC100",    "CC654",    "CC200",    "CC300",    "CC300",    "CC654"),
  stringsAsFactors = FALSE
)

ref_dir <- "data/reference"
if (!dir.exists(ref_dir)) dir.create(ref_dir, recursive = TRUE)

write.csv(projects,    file.path(ref_dir, "projects.csv"),    row.names = FALSE)
write.csv(assignments, file.path(ref_dir, "assignments.csv"), row.names = FALSE)

cat(sprintf("Reference files written to '%s/'\n", ref_dir))
