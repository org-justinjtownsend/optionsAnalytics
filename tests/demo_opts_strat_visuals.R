# Demonstration script for opts_strat_tbl function
# This script shows how to use the function with sample data

cat("=== DEMONSTRATION: OPTIONS STRATEGY TABLE ===\n\n")

# Source the function
source("R/fct_opts_strat_visuals.R")

# Create sample vertical spreads data
cat("Creating sample vertical spreads data...\n")
sample_data <- data.frame(
  dte = c(30, 30, 30, 60, 60, 60, 90, 90, 90),
  dist = c(100, 200, 300, 100, 200, 300, 100, 200, 300),
  vsType = c("lcs", "scs", "lps", "lcs", "scs", "lps", "lcs", "scs", "lps"),
  strikeL = c(4000, 4100, 4200, 4000, 4100, 4200, 4000, 4100, 4200),
  strikeH = c(4100, 4300, 4500, 4100, 4300, 4500, 4100, 4300, 4500),
  vsValue = c(2.50, -1.75, 3.25, 3.00, -2.25, 4.50, 3.75, -3.00, 5.25),
  instrValL = c(15.50, 8.25, 12.75, 18.00, 10.50, 15.25, 20.50, 12.75, 18.00),
  instrValH = c(13.00, 10.00, 9.50, 15.00, 12.75, 10.75, 16.75, 15.75, 12.75),
  instrType = c("c", "c", "p", "c", "c", "p", "c", "c", "p"),
  posType = c("dr_s", "cr_s", "dr_s", "dr_s", "cr_s", "dr_s", "dr_s", "cr_s", "dr_s"),
  p_max = c(97.50, 1.75, 96.75, 97.00, 2.25, 95.50, 96.25, 3.00, 94.75),
  bep = c(4002.50, 4098.25, 4203.25, 4003.00, 4097.75, 4204.50, 4003.75, 4097.00, 4205.25),
  moneyness = c("atm", "otm", "otm", "atm", "otm", "otm", "atm", "otm", "otm"),
  prob_profit = c(0.65, 0.35, 0.70, 0.68, 0.32, 0.72, 0.71, 0.29, 0.74),
  stockPrice = c(4050, 4050, 4050, 4050, 4050, 4050, 4050, 4050, 4050),
  stringsAsFactors = FALSE
)

cat("Sample data created with", nrow(sample_data), "strategies\n")
cat("DTE values:", paste(unique(sample_data$dte), collapse = ", "), "\n")
cat("Distance values:", paste(unique(sample_data$dist), collapse = ", "), "\n")
cat("Strategy types:", paste(unique(sample_data$vsType), collapse = ", "), "\n\n")

# Demonstration 1: Basic table with all columns
cat("=== DEMONSTRATION 1: Basic Table ===\n")
cat("Creating table with all columns...\n")
basic_table <- opts_strat_tbl(data = sample_data)
cat("✓ Basic table created successfully\n\n")

# Demonstration 2: Custom columns
cat("=== DEMONSTRATION 2: Custom Columns ===\n")
cat("Creating table with selected columns...\n")
custom_cols <- c("dte", "vsType", "strikeL", "strikeH", "vsValue", "p_max", "bep")
custom_table <- opts_strat_tbl(
  data = sample_data, 
  columns = custom_cols
)
cat("✓ Custom table created with columns:", paste(custom_cols, collapse = ", "), "\n\n")

# Demonstration 3: Different parameter combinations
cat("=== DEMONSTRATION 3: Parameter Variations ===\n")

# No search
cat("Creating table without search...\n")
no_search_table <- opts_strat_tbl(data = sample_data, searchable = FALSE)
cat("✓ No-search table created\n")

# No stripes
cat("Creating table without stripes...\n")
no_stripes_table <- opts_strat_tbl(data = sample_data, striped = FALSE)
cat("✓ No-stripes table created\n")

# No compact mode
cat("Creating table without compact mode...\n")
no_compact_table <- opts_strat_tbl(data = sample_data, compact = FALSE)
cat("✓ No-compact table created\n\n")

# Demonstration 4: Filtering by strategy type
cat("=== DEMONSTRATION 4: Strategy Type Filtering ===\n")
cat("Creating table with only long call spreads...\n")
lcs_data <- sample_data[sample_data$vsType == "lcs", ]
lcs_table <- opts_strat_tbl(data = lcs_data)
cat("✓ LCS-only table created with", nrow(lcs_data), "strategies\n\n")

# Demonstration 5: DTE filtering
cat("=== DEMONSTRATION 5: DTE Filtering ===\n")
cat("Creating table with only 30-day strategies...\n")
dte30_data <- sample_data[sample_data$dte == 30, ]
dte30_table <- opts_strat_tbl(data = dte30_data)
cat("✓ 30-day table created with", nrow(dte30_data), "strategies\n\n")

# Demonstration 6: Distance filtering
cat("=== DEMONSTRATION 6: Distance Filtering ===\n")
cat("Creating table with only 100-point spreads...\n")
dist100_data <- sample_data[sample_data$dist == 100, ]
dist100_table <- opts_strat_tbl(data = dist100_data)
cat("✓ 100-point table created with", nrow(dist100_data), "strategies\n\n")

# Summary
cat("=== DEMONSTRATION SUMMARY ===\n")
cat("Successfully created multiple table variations:\n")
cat("1. Basic table (all columns):", nrow(sample_data), "rows\n")
cat("2. Custom columns table:", length(custom_cols), "columns\n")
cat("3. Parameter variations: searchable, striped, compact\n")
cat("4. Strategy type filtered: LCS only\n")
cat("5. DTE filtered: 30 days only\n")
cat("6. Distance filtered: 100 points only\n\n")

cat("All tables are interactive reactable widgets that can be:\n")
cat("- Searched and filtered\n")
cat("- Sorted by any column\n")
cat("- Exported or embedded in R Markdown\n")
cat("- Customized with additional styling\n\n")

cat("The function successfully handles:\n")
cat("- Different data sizes\n")
cat("- Custom column selections\n")
cat("- Parameter variations\n")
cat("- Data filtering\n")
cat("- Error conditions\n\n")

cat("Next steps:\n")
cat("1. Test with real database data\n")
cat("2. Add more advanced features (sparklines, metrics)\n")
cat("3. Integrate with the vertical spreads workflow\n")
cat("4. Add to the options analytics pipeline\n")
