setwd("C:/Users/abdir/OneDrive - UC San Diego/Documents/Topics in International Trade/Final Project Analysis")
getwd()
list.files()
gravity <- readRDS("Gravity_V202211.rds")
# Load data and apply filters
library(haven)
library(purrr)
library(here)
library(tidyverse)
library(sqldf)
library(modelsummary)
library(fixest)
library(janitor)
library(huxtable)
library(openxlsx)
library(sjmisc)
#understand my data
names(gravity)
dim(gravity)
table(gravity$year)

#check which trade to use

gravity_work <-gravity
cat("=== COMPREHENSIVE TRADE VARIABLE CHECK ===\n\n")

# Check all trade variables
trade_vars <- c("tradeflow_comtrade_o", "tradeflow_comtrade_d", 
                "tradeflow_baci", "tradeflow_imf_o", "tradeflow_imf_d")

for(var in trade_vars) {
  cat("------", var, "------\n")
  cat("Total obs:", nrow(gravity_work), "\n")
  cat("Missing:", sum(is.na(gravity_work[[var]])), 
      sprintf("(%.1f%%)", 100*sum(is.na(gravity_work[[var]]))/nrow(gravity)), "\n")
  cat("Zero:", sum(gravity_work[[var]] == 0, na.rm = TRUE), 
      sprintf("(%.1f%%)", 100*sum(gravity_work[[var]] == 0, na.rm = TRUE)/nrow(gravity_work)), "\n")
  cat("Positive:", sum(gravity_work[[var]] > 0, na.rm = TRUE), 
      sprintf("(%.1f%%)", 100*sum(gravity_work[[var]] > 0, na.rm = TRUE)/nrow(gravity_work)), "\n")
  cat("\n")
}

# Summary stats
cat("\n=== SUMMARY STATISTICS ===\n")
summary(gravity_work %>% select(starts_with("tradeflow")))

# Check coverage by year for each variable
cat("\n=== COVERAGE BY YEAR ===\n")
coverage_by_year <- gravity_work %>%
  group_by(year) %>%
  summarize(
    comtrade_o_nonmiss = sum(!is.na(tradeflow_comtrade_o)),
    comtrade_d_nonmiss = sum(!is.na(tradeflow_comtrade_d)),
    baci_nonmiss = sum(!is.na(tradeflow_baci)),
    imf_o_nonmiss = sum(!is.na(tradeflow_imf_o)),
    imf_d_nonmiss = sum(!is.na(tradeflow_imf_d)),
    total_obs = n()
  )

# Show first few years and last few years
cat("\nFirst 5 years:\n")
print(head(coverage_by_year, 5))

cat("\nLast 5 years:\n")
print(tail(coverage_by_year, 5))

cat("\nMiddle years (around 1990):\n")
print(coverage_by_year %>% filter(year >= 1988, year <= 1992))

#data prep
cat("\n=== PTA VARIABLE (fta_wto) ===\n")
table(gravity_work$fta_wto, useNA = "ifany")
cat("% with PTA:", round(mean(gravity_work$fta_wto, na.rm=TRUE)*100, 2), "%\n")

cat("\n=== LANGUAGE VARIABLES ===\n")
cat("Common Official Language:\n")
table(gravity_work$comlang_off, useNA = "ifany")
cat("% same language:", round(mean(gravity_work$comlang_off, na.rm=TRUE)*100, 2), "%\n")

cat("\nEthnolinguistic Language:\n")
table(gravity_work$comlang_ethno, useNA = "ifany")
cat("% same ethno language:", round(mean(gravity_work$comlang_ethno, na.rm=TRUE)*100, 2), "%\n")

cat("\n=== RELIGION VARIABLE ===\n")
summary(gravity_work$comrelig)
# Just show quantiles instead of histogram
cat("Distribution breakdown:\n")
cat("How many > 0.5 (same religion):", sum(gravity_work$comrelig > 0.5, na.rm=TRUE), "\n")
cat("How many <= 0.5 (different religion):", sum(gravity_work$comrelig <= 0.5, na.rm=TRUE), "\n")

cat("\n=== GATT VARIABLES ===\n")
table(gravity_work$gatt_o, useNA = "ifany")
table(gravity_work$gatt_d, useNA = "ifany")

cat("\n=== CONTIGUITY ===\n")
table(gravity_work$contig, useNA = "ifany")
cat("% sharing border:", round(mean(gravity_work$contig, na.rm=TRUE)*100, 2), "%\n")


#start data analysis now 
# Filter to 1960-2018
gravity_clean <- gravity_work %>%
  filter(year >= 1960, year <= 2018)

# Check it worked
cat("Original data:\n")
cat("Rows:", nrow(gravity_work), "\n")
cat("Years:", min(gravity_work$year), "to", max(gravity_work$year), "\n\n")

cat("After filtering to 1960-2018:\n")
cat("Rows:", nrow(gravity_clean), "\n")
cat("Years:", min(gravity_clean$year), "to", max(gravity_clean$year), "\n")

# Check year distribution
cat("\nObservations per year (first 5 and last 5):\n")
year_counts <- table(gravity_clean$year)
print(head(year_counts, 5))
print(tail(year_counts, 5))

#check country existence problems and drop observations where countries dont exist

# Check how many have non-existent countries
gravity_clean1 <-gravity_clean
cat("=== COUNTRY EXISTENCE CHECK ===\n")
cat("Non-existent exporters:", sum(gravity_clean1$country_exists_o == 0), "\n")
cat("Non-existent importers:", sum(gravity_clean1$country_exists_d == 0), "\n")

# Drop observations where either country doesn't exist
gravity_clean1 <- gravity_clean1 %>%
  filter(country_exists_o == 1, country_exists_d == 1)

# Check results
cat("\nAfter dropping non-existent countries:\n")
cat("Rows:", nrow(gravity_clean1), "\n")
cat("Dropped:", 3746736 - nrow(gravity_clean1), "observations\n")

# Quick sanity check
cat("\nCountry existence now:\n")
table(gravity_clean1$country_exists_o, gravity_clean1$country_exists_d)

#Rename variables for clarity
# Rename for clarity
gravity_clean1 <- gravity_clean1 %>%
  rename(
    exporter_iso3 = iso3_o,
    importer_iso3 = iso3_d
  )

# Check it worked
cat("=== VARIABLE NAMES ===\n")
cat("Sample of renamed variables:\n")
head(gravity_clean1 %>% select(year, exporter_iso3, importer_iso3, fta_wto))

#CREATE COMBINED IMF TRADE FLOW
cat("=== STEP 4A-1: CREATING COMBINED TRADE FLOW ===\n")

# 1) Check missing patterns
gravity_clean1 <- gravity_clean1 %>%
  mutate(
    imf_o_missing = ifelse(is.na(tradeflow_imf_o), 1, 0),
    imf_d_missing = ifelse(is.na(tradeflow_imf_d), 1, 0)
  )

cat("Missing data patterns:\n")
ftable(imf_d_missing ~ imf_o_missing, data = gravity_clean1)

# 2) Create combined IMF trade flow
gravity_clean1 <- gravity_clean1 %>%
  mutate(tradeflow_imf = ifelse(is.na(tradeflow_imf_d), 
                                tradeflow_imf_o, 
                                tradeflow_imf_d))

# 3) Check the improvement
cat("\n=== COVERAGE CHECK ===\n")
cat("IMF Origin missing:", sum(is.na(gravity_clean1$tradeflow_imf_o)), "\n")
cat("IMF Destination missing:", sum(is.na(gravity_clean1$tradeflow_imf_d)), "\n")
cat("Combined IMF missing:", sum(is.na(gravity_clean1$tradeflow_imf)), "\n")

# Summary stats
cat("\nSummary of combined trade flow:\n")
summary(gravity_clean1$tradeflow_imf)

#DROP SELF-TRADE

cat("=== STEP 4A-2: DROPPING SELF-TRADE ===\n")

# Check how many self-trade observations
cat("Before dropping self-trade:\n")
cat("Total observations:", nrow(gravity_clean1), "\n")
cat("Self-trade observations:", 
    sum(gravity_clean1$exporter_iso3 == gravity_clean1$importer_iso3), "\n")

# Drop self-trade
gravity_clean1 <- gravity_clean1 %>%
  filter(exporter_iso3 != importer_iso3)

# Check results
cat("\nAfter dropping self-trade:\n")
cat("Total observations:", nrow(gravity_clean1), "\n")
cat("Observations dropped:", 2930862 - nrow(gravity_clean1), "\n")

# Verify no self-trade remains
cat("\nVerification - self-trade remaining:", 
    sum(gravity_clean1$exporter_iso3 == gravity_clean1$importer_iso3), "\n")

cat("\n✅ Self-trade removed!\n")


# CREATE GATT/WTO BOTH VARIABLE

cat("=== STEP 4A-3: CREATE JOINT GATT (BOTH COUNTRIES) ===\n")

# Check the individual GATT variables first
cat("GATT membership - Exporter (gatt_o):\n")
print(table(gravity_clean1$gatt_o, useNA = "ifany"))

cat("\nGATT membership - Importer (gatt_d):\n")
print(table(gravity_clean1$gatt_d, useNA = "ifany"))

# Create joint membership dummy: 1 if BOTH are in GATT, else 0
# (NAs will effectively become 0 because condition requires == 1 on both sides)
gravity_clean1 <- gravity_clean1 %>%
  mutate(gatt_wto = as.integer(gatt_o == 1 & gatt_d == 1))

# Check the new variable
cat("\n=== NEW VARIABLE: gatt_joint ===\n")
print(table(gravity_clean1$gatt_wto, useNA = "ifany"))

cat("\n% where BOTH in GATT:", 
    round(mean(gravity_clean1$gatt_wto, na.rm = TRUE) * 100, 2), "%\n")

gravity_clean1 <- gravity_clean1 %>%
  rename(gatt_joint = gatt_wto)

#CHANGE 0 COMMON COLONIZER TO ZERO BEFORE REGRESSION
cat("=== REPLACE comcol NA with 0 ===\n")

# Before
cat("Before replacement:\n")
print(table(gravity_clean1$comcol, useNA = "ifany"))

# Replace NA with 0
gravity_clean1 <- gravity_clean1 %>%
  mutate(comcol = as.integer(tidyr::replace_na(comcol, 0)))

# After
cat("\nAfter replacement:\n")
print(table(gravity_clean1$comcol, useNA = "ifany"))

cat("\n✅ NAs replaced with 0!\n")

#CREATE LOG TRADE VARIABLE
cat("=== CREATE LOG TRADE VARIABLE ===\n")

# Create log of trade
gravity_clean1 <- gravity_clean1 %>%
  mutate(log_tradeflow_imf = log(tradeflow_imf))

# Check it
cat("Summary of log trade:\n")
summary(gravity_clean1$log_tradeflow_imf)

cat("\nObservations breakdown:\n")
cat("Positive trade (can take log):", sum(!is.na(gravity_clean1$log_tradeflow_imf)), "\n")
cat("Zero/missing trade (log = NA):", sum(is.na(gravity_clean1$log_tradeflow_imf)), "\n")

# Show distribution
cat("\nDistribution of log trade:\n")
hist(gravity_clean1$log_tradeflow_imf, 
     breaks = 50, 
     main = "Distribution of Log Trade",
     xlab = "log(Trade)")

cat("\n✅ Log trade variable created!\n")



#CREATE ID VARIABLES for regression (clustering & fixed effects)
cat("=== CREATE ID VARIABLES (FINAL STEP!) ===\n")

# Create pair ID and fixed effects variables
gravity_clean1 <- gravity_clean1 %>%
  mutate(
    # Country-pair ID (for clustering standard errors)
    pair_id = paste(exporter_iso3, importer_iso3, sep = "_"),
    
    # Exporter-year fixed effects
    exporter_year = paste(exporter_iso3, year, sep = "_"),
    
    # Importer-year fixed effects
    importer_year = paste(importer_iso3, year, sep = "_")
  )

# Check they were created
cat("\nNew ID variables:\n")
cat("Unique country pairs:", length(unique(gravity_clean1$pair_id)), "\n")
cat("Unique exporter-years:", length(unique(gravity_clean1$exporter_year)), "\n")
cat("Unique importer-years:", length(unique(gravity_clean1$importer_year)), "\n")

# Show examples
cat("\nExample rows:\n")
head(gravity_clean1 %>% select(exporter_iso3, importer_iso3, year, 
                               pair_id, exporter_year, importer_year))

cat("\n✅ ALL ID VARIABLES CREATED!\n")
cat("\n🎉 DATA PREP COMPLETE! Ready for regression! 🎉\n")

# Prep data for PPML Robustness

cat("=== PREP FOR PPML ROBUSTNESS ===\n")

# Replace NA with 0 for PPML
gravity_clean1 <- gravity_clean1 %>%
  mutate(tradeflow_imf_ppml = ifelse(is.na(tradeflow_imf), 0, tradeflow_imf))

# Check
cat("Original tradeflow_imf:\n")
cat("  Missing:", sum(is.na(gravity_clean1$tradeflow_imf)), "\n")
cat("  Zero:", sum(gravity_clean1$tradeflow_imf == 0, na.rm = TRUE), "\n")
cat("  Positive:", sum(gravity_clean1$tradeflow_imf > 0, na.rm = TRUE), "\n")

cat("\nPPML version (tradeflow_imf_ppml):\n")
cat("  Missing:", sum(is.na(gravity_clean1$tradeflow_imf_ppml)), "\n")
cat("  Zero:", sum(gravity_clean1$tradeflow_imf_ppml == 0), "\n")
cat("  Positive:", sum(gravity_clean1$tradeflow_imf_ppml > 0), "\n")

cat("\n✅ PPML variable ready!\n")

#SUMMARY STATISTICS
cat("=== TABLE 1: SUMMARY STATISTICS ===\n")

library(modelsummary)
library(dplyr)

# Create clean table data with nice labels
summary_data <- gravity_clean1 %>%
  transmute(
    `Log(Trade Flow)` = log_tradeflow_imf,
    `PTA(fta_wto)` = fta_wto,
    `Common Official Language` = comlang_off,
    `Common Colonizer` = comcol,
    `Contiguity` = contig,
    `Joint GATT/WTO` = gatt_joint
  )

total_obs <- nrow(summary_data)
n_log_obs <- sum(!is.na(summary_data$`Log(Trade Flow)`))

datasummary(
  `Log(Trade Flow)` + `PTA(fta_wto)` + `Common Official Language` + `Common Colonizer` +
    `Contiguity` + `Joint GATT/WTO` ~ N + Mean + SD + Min + Max,
  data = summary_data,
  title = "Summary Statistics",
  notes = c(
    paste0("Sample: Bilateral country-pair observations, 1960–2018. Total observations: ", total_obs, "."),
    paste0("Log(Trade Flow) is the natural log of bilateral trade (IMF). N = ", n_log_obs,
           " with positive trade; zero/missing trade are excluded in OLS but included in PPML robustness."),
    "Binary variables (0/1): PTA = preferential trade agreement; Common Official Language = shared official language; Common Colonizer = common colonizer post-1945; Contiguity = shared border; Joint GATT/WTO = both members.",
    "Source: CEPII Gravity Database."
  )
)

cat("\n✅ Summary statistics table created!\n")


#REGRESSSIONS
#Baseline regression
cat("=== BASELINE REGRESSION (OLS) ===\n")
library(fixest)
baseline_ols <- feols(
  log_tradeflow_imf ~ 
    fta_wto * comlang_off +   # main hypothesis interaction
    fta_wto * comcol +        # colonial-network interaction control
    contig +
    gatt_joint |
    exporter_year + importer_year,
  data = gravity_clean1,
  cluster = ~pair_id
)
summary(baseline_ols)
cat("\n✅ Baseline regression complete!\n")

#Robustness checks
#ppml checks
cat("=== ROBUSTNESS CHECK: PPML ===\n")

# PPML with same specification
baseline_ppml <- fepois(
  tradeflow_imf_ppml ~ 
    fta_wto * comlang_off +
    fta_wto * comcol +
    contig +
    gatt_joint |
    exporter_year + importer_year,
  data = gravity_clean1,
  cluster = ~pair_id
)

# Show results
summary(baseline_ppml)

cat("\n✅ PPML robustness check complete!\n")


#roughwork to be deleted (to check what caused ppml coefficients to be different from ols)
cat("========================================\n")
cat("  PPML vs OLS INVESTIGATION\n")
cat("========================================\n")

# Investigation 1
cat("\n=== INVESTIGATION 1: Zero Trade by Language ===\n")
zero_patterns <- gravity_clean1 %>%
  group_by(comlang_off, fta_wto) %>%
  summarize(
    n = n(),
    n_zero = sum(tradeflow_imf_ppml == 0),
    pct_zero = round(mean(tradeflow_imf_ppml == 0) * 100, 2),
    .groups = "drop"
  )
print(zero_patterns)

# Investigation 2
cat("\n=== INVESTIGATION 2: Singleton Pairs ===\n")
singleton_check <- gravity_clean1 %>%
  group_by(pair_id) %>%
  summarize(
    all_zero = all(tradeflow_imf_ppml == 0),
    has_pta = any(fta_wto == 1),
    same_lang = first(comlang_off),
    .groups = "drop"
  ) %>%
  filter(all_zero == TRUE)

cat("Pairs with ALL ZEROS:\n")
cat("Total:", nrow(singleton_check), "\n")
cat("Same language:", sum(singleton_check$same_lang == 1, na.rm=TRUE), "\n")
cat("Different language:", sum(singleton_check$same_lang == 0, na.rm=TRUE), "\n")

# Investigation 3
cat("\n=== INVESTIGATION 3: Sample Composition ===\n")
ols_sample <- gravity_clean1 %>%
  filter(!is.na(log_tradeflow_imf)) %>%
  summarize(
    n = n(),
    pct_pta = mean(fta_wto) * 100,
    pct_same_lang = mean(comlang_off, na.rm=TRUE) * 100
  )
cat("OLS sample:\n")
print(ols_sample)

#2nd robustness checks comlang_ethno
cat("=== ROBUSTNESS: Ethnolinguistic Language ===\n")

# OLS with comlang_ethno instead of comlang_off
ethno_ols <- feols(
  log_tradeflow_imf ~ 
    fta_wto * comlang_ethno +     # SWAP: ethno instead of official
    fta_wto * comcol +
    contig +
    gatt_joint |
    exporter_year + importer_year,
  data = gravity_clean1,
  cluster = ~pair_id
)

# Show results
summary(ethno_ols)

cat("\n✅ Ethnolinguistic language robustness complete!\n")

#Create one regression table
cat("=== TABLE 2: MAIN REGRESSION RESULTS ===\n")

library(modelsummary)
library(fixest)

models <- list(
  "Baseline OLS" = baseline_ols,
  "PPML" = baseline_ppml,
  "Ethno Language" = ethno_ols
)

modelsummary(
  models,
  # Rename coefficients nicely
  coef_rename = c(
    "fta_wto" = "PTA(fta_wto)",
    "comlang_off" = "Common Official Language",
    "comlang_ethno" = "Common Ethnolinguistic Language",
    "comcol" = "Common Colonizer",
    "contig" = "Contiguity",
    "gatt_joint" = "Joint GATT/WTO",
    "fta_wto:comlang_off" = "PTA × Official Language",
    "fta_wto:comlang_ethno" = "PTA × Ethnolinguistic Language",
    "fta_wto:comcol" = "PTA × Common Colonizer"
  ),
  # Journal-style stars
  stars = c('*' = .10, '**' = .05, '***' = .01),
  # Keep only these goodness-of-fit stats
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  # Add fixed effects note
  add_rows = tibble::tribble(
    ~term, ~"Baseline OLS", ~"PPML", ~"Ethno Language",
    "Exporter-Year FE", "Yes", "Yes", "Yes",
    "Importer-Year FE", "Yes", "Yes", "Yes",
    "Clustered SE (Pair)", "Yes", "Yes", "Yes"
  ),
  fmt = 3,
  title = "Preferential Trade Agreements Effects on Bilateral Trade",
  notes = c(
    "Dependent variable: log(trade) in columns (1) and (3); trade levels in column (2).",
    "Sample: Bilateral trade flows, 1960-2018. All specifications include exporter-year and importer-year",
    "fixed effects. Standard errors clustered at country-pair level in parentheses.",
    "*** p<0.01, ** p<0.05, * p<0.1"
  ),
  output = "table2_results.html"  
)


cat("\n✅ Regression table created!\n")

# SOME SIMPLE CORRECTIONS AFTER FEEDBACK FROM PROFESSOR
cat("=== BASELINE WITH 5-YEAR INTERVALS ===\n")

# Filter to 5-year intervals
gravity_5yr <- gravity_clean1 %>%
  filter(year %in% seq(1960, 2015, by = 5))

# Check what years we have
cat("Years in 5-year sample:\n")
table(gravity_5yr$year)

# Run baseline regression
baseline_5yr <- feols(
  log_tradeflow_imf ~ 
    fta_wto * comlang_off +
    fta_wto * comcol +
    contig +
    gatt_joint |
    exporter_year + importer_year,
  data = gravity_5yr,
  cluster = ~pair_id
)

# Show results
summary(baseline_5yr)

cat("\n✅ 5-year interval regression complete!\n")

#check with pair fixed effects
cat("=== PAIR FIXED EFFECTS SPECIFICATION ===\n")

# Pair FE with 5-year intervals - including contig to see it drop!
pair_fe_5yr <- feols(
  log_tradeflow_imf ~ 
    fta_wto * comlang_off +
    fta_wto * comcol +
    contig +                # ← ADD THIS to see it get dropped!
    gatt_joint |
    exporter_year + importer_year + pair_id,
  data = gravity_5yr,
  cluster = ~pair_id
)

# Show results
summary(pair_fe_5yr)

#ppml with no pair FE
cat("=== TABLE 2, COLUMN 2: PPML 5-YEAR ===\n")

# PPML with 5-year intervals
ppml_5yr <- fepois(
  tradeflow_imf_ppml ~ 
    fta_wto * comlang_off +
    fta_wto * comcol +
    contig +
    gatt_joint |
    exporter_year + importer_year,
  data = gravity_5yr,
  cluster = ~pair_id
)

summary(ppml_5yr)

cat("\n✅ PPML 5-year complete!\n")

#comlang ethno with no pair fixed effects
cat("=== TABLE 2, COLUMN 3: ETHNOLINGUISTIC LANGUAGE 5-YEAR ===\n")

# Ethno language with 5-year intervals
ethno_5yr <- feols(
  log_tradeflow_imf ~ 
    fta_wto * comlang_ethno +  # ← ETHNO instead of official
    fta_wto * comcol +
    contig +
    gatt_joint |
    exporter_year + importer_year,
  data = gravity_5yr,
  cluster = ~pair_id
)

summary(ethno_5yr)

cat("\n✅ Ethno language 5-year complete!\n")

#UPDATED REGRESSION TABLE WITHOUT PAIR FIXED EFFECTS THAT HAS 5 YEAR INTERVALS

cat("=== TABLE 2: MAIN RESULTS (5-YEAR INTERVALS) ===\n")

library(modelsummary)
library(fixest)

# Table 2 models (5-year, NO pair FE)
models_table2 <- list(
  "Baseline OLS" = baseline_5yr,
  "PPML" = ppml_5yr,
  "Ethno Language" = ethno_5yr
)

modelsummary(
  models_table2,
  # Rename coefficients nicely
  coef_rename = c(
    "fta_wto" = "PTA(fta_wto)",
    "comlang_off" = "Common Official Language",
    "comlang_ethno" = "Common Ethnolinguistic Language",
    "comcol" = "Common Colonizer",
    "contig" = "Contiguity",
    "gatt_joint" = "Joint GATT/WTO",
    "fta_wto:comlang_off" = "PTA × Official Language",
    "fta_wto:comlang_ethno" = "PTA × Ethnolinguistic Language",
    "fta_wto:comcol" = "PTA × Common Colonizer"
  ),
  # Journal-style stars
  stars = c('*' = .10, '**' = .05, '***' = .01),
  # Keep only these goodness-of-fit stats
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  # Add fixed effects note
  add_rows = tibble::tribble(
    ~term, ~"Baseline OLS", ~"PPML", ~"Ethno Language",
    "Sample Period", "1960-2015", "1960-2015", "1960-2015",
    "Frequency", "5-year", "5-year", "5-year",
    "Exporter-Year FE", "Yes", "Yes", "Yes",
    "Importer-Year FE", "Yes", "Yes", "Yes",
    "Pair FE", "No", "No", "No",
    "Clustered SE (Pair)", "Yes", "Yes", "Yes"
  ),
  fmt = 3,
  title = "Preferential Trade Agreements Effects on Bilateral Trade",
  notes = c(
    "Dependent variable: log(trade) in columns (1) and (3); trade levels in column (2).",
    "Sample: Bilateral trade flows at 5-year intervals (1960, 1965, ..., 2015).",
    "All specifications include exporter-year and importer-year fixed effects.",
    "Standard errors clustered at country-pair level in parentheses.",
    "*** p<0.01, ** p<0.05, * p<0.1"
  ),
  output = "table2_main_results.tex"
)

cat("\n✅ Table 2 created and saved as table2_main_results.html!\n")

# ppml with pair fixed effects
cat("=== TABLE 3, COLUMN 2: PPML WITH PAIR FE ===\n")

# PPML with pair FE
ppml_pair_fe <- fepois(
  tradeflow_imf_ppml ~ 
    fta_wto * comlang_off +
    fta_wto * comcol +
    contig +
    gatt_joint |
    exporter_year + importer_year + pair_id,  # ← ADD PAIR FE!
  data = gravity_5yr,
  cluster = ~pair_id
)

summary(ppml_pair_fe)

cat("\n✅ PPML with Pair FE complete!\n")

#Ethno language with pair fixed effects
cat("=== TABLE 3, COLUMN 3: ETHNO WITH PAIR FE ===\n")

# Ethno language with pair FE
ethno_pair_fe <- feols(
  log_tradeflow_imf ~ 
    fta_wto * comlang_ethno +  # ← ETHNO instead of official
    fta_wto * comcol +
    contig +
    gatt_joint |
    exporter_year + importer_year + pair_id,  # ← ADD PAIR FE!
  data = gravity_5yr,
  cluster = ~pair_id
)

summary(ethno_pair_fe)

cat("\n✅ Ethno with Pair FE complete!\n")

# Final model summary table with pair fixed effects 

cat("=== TABLE 3: PAIR FIXED EFFECTS ROBUSTNESS ===\n")

library(modelsummary)
library(fixest)

# Table 3 models (5-year, WITH pair FE)
models_table3 <- list(
  "Baseline OLS" = pair_fe_5yr,
  "PPML" = ppml_pair_fe,
  "Ethno Language" = ethno_pair_fe
)

modelsummary(
  models_table3,
  # Rename coefficients nicely
  coef_rename = c(
    "fta_wto" = "PTA",
    "contig" = "Contiguity",
    "gatt_joint" = "Joint GATT/WTO",
    "fta_wto:comlang_off" = "PTA × Official Language",
    "fta_wto:comlang_ethno" = "PTA × Ethnolinguistic Language",
    "fta_wto:comcol" = "PTA × Common Colonizer"
  ),
  # Journal-style stars
  stars = c('*' = .10, '**' = .05, '***' = .01),
  # Keep only these goodness-of-fit stats
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  # Add fixed effects note
  add_rows = tibble::tribble(
    ~term, ~"Baseline OLS", ~"PPML", ~"Ethno Language",
    "Sample Period", "1960-2015", "1960-2015", "1960-2015",
    "Frequency", "5-year", "5-year", "5-year",
    "Exporter-Year FE", "Yes", "Yes", "Yes",
    "Importer-Year FE", "Yes", "Yes", "Yes",
    "Pair FE", "Yes", "Yes", "Yes",
    "Clustered SE (Pair)", "Yes", "Yes", "Yes"
  ),
  fmt = 3,
  title = "Table 3: Pair Fixed Effects Robustness",
  notes = c(
    "Dependent variable: log(trade) in columns (1) and (3); trade levels in column (2).",
    "Sample: Bilateral trade flows at 5-year intervals (1960, 1965, ..., 2015).",
    "All specifications include exporter-year, importer-year, and country-pair fixed effects.",
    "Common language and common colonizer main effects absorbed by pair FE (collinear).",
    "Standard errors clustered at country-pair level in parentheses.",
    "*** p<0.01, ** p<0.05, * p<0.1"
  ),
  output = "table3_pair_fe.tex"  # ← LATEX OUTPUT!
)

cat("\n✅ Table 3 saved as table3_pair_fe.tex!\n")

#new summary statistics table
cat("=== SUMMARY STATISTICS FOR 5-YEAR DATA ===\n")

library(modelsummary)

# Select variables from 5-year sample
summary_5yr <- gravity_5yr %>%
  select(
    log_tradeflow_imf,
    fta_wto,
    comlang_off,
    comcol,
    contig,
    gatt_joint
  )

# Create table
datasummary(
  (`Log(Trade Flow)` = log_tradeflow_imf) + 
    (`PTA` = fta_wto) + 
    (`Common Official Language` = comlang_off) +
    (`Common Colonizer` = comcol) +
    (`Contiguity` = contig) +
    (`Joint GATT/WTO` = gatt_joint) ~
    N + Mean + SD + Min + Max,
  data = summary_5yr,
  title = "Summary Statistics",
  notes = c("Sample: Bilateral country-pair observations at 5-year intervals, 1960-2015. Total observations: 586,894.",
            "Log(Trade Flow) = natural log of bilateral trade (thousands USD, IMF data). N = 198,931 with positive",
            "trade; 387,963 obs (66%) with zero/missing trade are excluded in OLS but included in PPML robustness.",
            "Binary variables (0/1): PTA = preferential trade agreement; Common Official Language = shared official",
            "language; Common Colonizer = both colonized by same power; Contiguity = shared border; Joint GATT/WTO",
            "= both GATT/WTO members. Source: CEPII Gravity Database."),
  output = "table1_summary_5yr.html"
)

cat("\n✅ Summary statistics for 5-year data created!\n")

#visual representation
# FIGURE 1 DATA PREP
cat("=== FIGURE 1 DATA CHECK ===\n")

figure1_data <- gravity_5yr %>%
  filter(fta_wto == 1) %>%
  mutate(
    language_group = ifelse(comlang_off == 1, 
                            "PTA + Common Language", 
                            "PTA + No Common Language")
  ) %>%
  filter(!is.na(log_tradeflow_imf)) %>%
  group_by(year, language_group) %>%
  summarize(
    mean_log_trade = mean(log_tradeflow_imf, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  )

print(figure1_data)
cat("\nUnique years:", length(unique(figure1_data$year)), "\n")
cat("Groups:", unique(figure1_data$language_group), "\n")
print(figure1_data, n = 24)

#plot the graph
figure1_data_plot1 <- figure1_data %>%
  filter(year >= 1965)

ggplot(figure1_data_plot1, aes(x = year, y = mean_log_trade, 
                              color = language_group, 
                              group = language_group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c("PTA + Common Language" = "#2166ac",
               "PTA + No Common Language" = "#d6604d"),
    name = ""
  ) +
  scale_x_continuous(breaks = seq(1965, 2015, by = 5)) +
  labs(
    title = "Figure 1: Log Trade Flows Among PTA Partners",
    subtitle = "Common Language vs. No Common Language Pairs, 1965–2015",
    x = "Year",
    y = "Mean Log Trade Flow",
    caption = "Note: Sample restricted to country pairs with an active PTA. Trade flows from IMF via CEPII Gravity Database.\n1960 excluded from graph due to only 2 common language PTA observations; regression sample is unchanged."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave("figure1_trade_trends.png", width = 9, height = 6, dpi = 300)
cat("✅ Figure 1 updated!\n")

#Figure 2 : Trade distriibution across 4 groups
# FIGURE 2 DATA PREP
# FIGURE 2 LINE GRAPH DATA PREP
figure2_line_data <- gravity_5yr %>%
  filter(!is.na(log_tradeflow_imf)) %>%
  mutate(
    trade_group = case_when(
      fta_wto == 1 & comlang_off == 1 ~ "PTA + Common Language",
      fta_wto == 1 & comlang_off == 0 ~ "PTA + No Common Language",
      fta_wto == 0 & comlang_off == 1 ~ "No PTA + Common Language",
      fta_wto == 0 & comlang_off == 0 ~ "No PTA + No Common Language"
    )
  ) %>%
  filter(!is.na(trade_group)) %>%
  group_by(year, trade_group) %>%
  summarize(
    mean_log_trade = mean(log_tradeflow_imf, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  )

print(figure2_line_data, n = 48)

#plot the graph
figure2_line_data_clean <- figure2_line_data %>%
  filter(year >= 1965) %>%
  mutate(trade_group = factor(trade_group, levels = c(
    "PTA + Common Language",
    "PTA + No Common Language",
    "No PTA + Common Language",
    "No PTA + No Common Language"
  )))

ggplot(figure2_line_data_clean, aes(x = year, 
                                    y = mean_log_trade,
                                    color = trade_group,
                                    group = trade_group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c(
    "PTA + Common Language"         = "#2166ac",
    "PTA + No Common Language"      = "#92c5de",
    "No PTA + Common Language"      = "#d6604d",
    "No PTA + No Common Language"   = "#f4a582"
  )) +
  scale_x_continuous(breaks = seq(1965, 2015, by = 5)) +
  labs(
    title = "Figure 2: Log Trade Flows by Treatment and Control Groups",
    subtitle = "All Four Groups, 1965–2015",
    x = "Year",
    y = "Mean Log Trade Flow",
    color = "",
    caption = "Note: Sample includes all country pairs with positive trade flows at 5-year intervals, 1965–2015.\n1960 excluded due to limited PTA observations. Source: CEPII Gravity Database."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    legend.direction = "vertical",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.text = element_text(size = 10)
  )

ggsave("figure2_linegraph.png", width = 10, height = 6, dpi = 300)
cat("✅ Figure 2 updated!\n")