setwd("C:/Users/abdir/OneDrive - UC San Diego/Documents/Designing Field Experiments/HW1 Materials")
getwd()

library(haven)
# basic data management
library(tidyverse)
# load libraries
library(modelsummary)
library(stargazer) #Write nice tables
# these are a nice set of packages for power calculations!
library(pwr)        # really nice for power calculations
library(WebPower)   # adds clustered designs
library(ICC)        # easy ICC calculations
library(fishmethods)
library(dplyr)
# these are optional for using gt to make tables
# you can always use stargazer / etable / others
library(parameters)
library(clubSandwich)
org_data_r <- read_dta("HW1.dta")

#check the data
# How many rows and columns?
dim(org_data_r)

# What do the first few rows look like?
head(org_data_r)

#Since the power analysis uses pre-treatment data only, let's create our working dataset:
#r# Filter to round 1 only
round1 <- org_data_r %>% filter(round == 1)

# Confirm we have 804 rows (one per person)
dim(round1)

# Basic summary statistics of earnings
summary(round1$earnings)

# Standard deviation
sd(round1$earnings)

# How many villages (clusters) do we have?
J <- length(unique(round1$vill_id))
print(J)

# Average number of individuals per village
n_avg <- nrow(round1) / J
print(n_avg)

# Also look at the distribution of village sizes
table(round1$vill_id)

# Calculate ICC for earnings clustered at village level
icc_result <- clus.rho(popchar = round1$earnings, 
                       cluster = round1$vill_id, 
                       type = 1)
print(icc_result)
names(round1)

# Regress earnings on baseline covariates
cov_model <- lm(earnings ~ age + highest_grade + urban + trusting + risk_amount, 
                data = round1)

# Get the R squared
summary(cov_model)
summary(cov_model)$r.squared

#power curve

# Effect sizes
effect_sizes <- seq(0.1, 1.0, by = 0.01)

# Adjusted ICC using R²
icc_adj <- 0.054 * (1 - 0.088)

# Scenario 1: No covariates
power_no_cov <- sapply(effect_sizes, function(f) {
  wp.crt2arm(n = 5, f = f, J = 168, icc = 0.054, 
             power = NULL, alpha = 0.05)$power
})

# Scenario 2: With covariates
power_with_cov <- sapply(effect_sizes, function(f) {
  wp.crt2arm(n = 5, f = f, J = 168, icc = icc_adj, 
             power = NULL, alpha = 0.05)$power
})

# Plot
plot(effect_sizes, power_no_cov, type = "l", lwd = 2, col = "blue",
     xlab = "Effect Size", ylab = "Power",
     main = "Power Curve: Cluster RCT",
     ylim = c(0, 1), las = 1)

lines(effect_sizes, power_with_cov, lwd = 2, col = "red", lty = 2)

abline(h = 0.8, lty = 3, col = "gray40")

legend("bottomright", 
       legend = c("No Covariates (R²=0)", "With Covariates (R²=0.088)"),
       col = c("blue", "red"), lwd = 2, lty = c(1, 2))

#Question 2
# MDE Scenario 1: No covariates
mde1 <- wp.crt2arm(n = 5, f = NULL, J = 168, icc = 0.054,
                   power = 0.8, alpha = 0.05)
print(mde1)

# MDE Scenario 2: With covariates
mde2 <- wp.crt2arm(n = 5, f = NULL, J = 168, icc = icc_adj,
                   power = 0.8, alpha = 0.05)
print(mde2)

# Standard deviation of earnings
sd_earnings <- sd(round1$earnings)

# MDE in Kenyan Shillings
mde1_ksh <- mde1$f * sd_earnings
mde2_ksh <- mde2$f * sd_earnings

cat("MDE without covariates:", round(mde1_ksh), "Ksh\n")
cat("MDE with covariates:", round(mde2_ksh), "Ksh\n")
cat("SD of earnings used:", round(sd_earnings), "Ksh\n")

#question3a

# For each village, check how many unique values of 'offered' exist
# If every village has only 1 unique value = village level assignment
village_check <- aggregate(offered ~ vill_id, data = round1, 
                           FUN = function(x) length(unique(x)))

# Show the distribution - we expect all villages to have exactly 1 unique value
table(village_check$offered)

# Double check - any village with both offered=0 and offered=1?
mixed_villages <- village_check[village_check$offered > 1, ]
print(nrow(mixed_villages))

#question 3b 
#Step 1: Baseline (Round 1) covariates at village level
baseline_data <- round1 %>%
  group_by(vill_id) %>%
  summarise(
    age = mean(age),
    highest_grade = mean(highest_grade),
    urban = mean(urban),
    trusting = mean(trusting),
    risk_amount = mean(risk_amount),
    earnings = mean(earnings)
  )
#Step 2: Treatment assignment from Round 2
treatment_data <- org_data_r %>%
  filter(round == 2) %>%
  group_by(vill_id) %>%
  summarise(offered = first(offered))
#step3 : merge them
village_data_f <- merge(baseline_data, treatment_data, by = "vill_id")
table(village_data_f$offered)
#run regression and stargazer
bal1f <- lm(age ~ offered, data = village_data_f)
bal2f <- lm(highest_grade ~ offered, data = village_data_f)
bal3f <- lm(urban ~ offered, data = village_data_f)
bal4f <- lm(trusting ~ offered, data = village_data_f)
bal5f <- lm(risk_amount ~ offered, data = village_data_f)
bal6f <- lm(earnings ~ offered, data = village_data_f)

stargazer(bal1f, bal2f, bal3f, bal4f, bal5f, bal6f,
          type = "text",
          covariate.labels = "Offered",
          keep.stat = c("n", "rsq"),
          title = "Regression Table: Village-Level")
#Question 3c - one-sided or two sided non compliance
# Crosstab of offered vs treated using full data
table(org_data_r$offered, org_data_r$treated)

#question3d - Uptake rate
# Uptake rate
offered_total <- 153 + 243
took_it <- 153

uptake <- took_it / offered_total
cat("Total offered:", offered_total, "\n")
cat("Actually took it:", took_it, "\n")
cat("Uptake rate:", round(uptake * 100, 1), "%\n")

#question4
# Filter to offered individuals in round 2 only
offered_sample <- org_data_r[org_data_r$round == 2 & org_data_r$offered == 1, ]

# Check how many we have
nrow(offered_sample)
table(offered_sample$treated)

# Run probit regression
probit_model <- glm(treated ~ age + highest_grade + urban + 
                      trusting + risk_amount + earnings,
                    family = binomial(link = "probit"),
                    data = offered_sample)

# Display results
summary(probit_model)

stargazer(probit_model,
          type = "text",
          title = "Probit Model: Program Uptake",
          dep.var.labels = "Treated (Program Uptake)",
          covariate.labels = c("Age", "Education", "Urban",
                               "Trusting", "Risk Amount", "Earnings"),
          digits = 3)
#question5
# Get round 2 data
round2 <- org_data_r[org_data_r$round == 2, ]

# Get baseline earnings from round 1
baseline_earnings <- round1[, c("indiv_id", "earnings")]
names(baseline_earnings)[2] <- "baseline_earnings"

# Merge baseline earnings into round 2
round2 <- merge(round2, baseline_earnings, by = "indiv_id")

# Check it looks right
head(round2[, c("indiv_id", "vill_id", "offered", "earnings", "baseline_earnings")])
dim(round2)
anyDuplicated(round2$indiv_id)
summary(round2$baseline_earnings)

#run regressions (ANCOVA and no covariates)

# Step 2: ITT regression (no covariates)
itt1 <- lm(earnings ~ offered, data = round2)

# Step 3: ANCOVA regression (with baseline + covariates)
itt2 <- lm(earnings ~ offered + baseline_earnings + age + highest_grade + 
             urban + trusting + risk_amount, data = round2)
library(dplyr)
library(clubSandwich)
library(lmtest)
# Step 4: Clustered standard errors at village level (CR2)
itt1_results <- coeftest(itt1, vcov = vcovCR(itt1, cluster = round2$vill_id, type = "CR2"))
itt2_results <- coeftest(itt2, vcov = vcovCR(itt2, cluster = round2$vill_id, type = "CR2"))

# Step 5: Display results
print("ITT Regression (No Covariates):")
print(itt1_results)
print("ANCOVA Regression (With Covariates):")
print(itt2_results)

#stargazer

# Extract clustered SEs
se_itt1 <- sqrt(diag(vcovCR(itt1, cluster = round2$vill_id, type = "CR2")))
se_itt2 <- sqrt(diag(vcovCR(itt2, cluster = round2$vill_id, type = "CR2")))

stargazer(itt1, itt2,
          type = "text",
          title = "Intention to Treat (ITT) Estimates",
          dep.var.labels = "Endline Earnings (Ksh)",
          column.labels = c("No Covariates", "ANCOVA"),
          covariate.labels = c("Offered",
                               "Baseline Earnings",
                               "Age",
                               "Highest Grade",
                               "Urban",
                               "Trusting",
                               "Risk Amount"),
          se = list(se_itt1, se_itt2),   # 🔥 clustered SEs
          digits = 3,
          star.cutoffs = c(0.1, 0.05, 0.01),
          omit.stat = c("f", "ser"),
          no.space = TRUE)

#Question 6
set.seed(123)

# Village-level info
villages <- unique(round2$vill_id)

village_treatment <- aggregate(offered ~ vill_id, data = round2, FUN = mean)
n_treated <- sum(village_treatment$offered == 1)

# Store results
ri_estimates <- numeric(500)

# RI loop
for(i in 1:500) {
  
  treat_villages <- sample(villages, n_treated)
  
  ri_offer <- as.integer(round2$vill_id %in% treat_villages)
  
  ri_reg <- lm(earnings ~ ri_offer, data = round2)
  
  ri_estimates[i] <- coef(ri_reg)["ri_offer"]
}

# RI standard error
ri_se <- sd(ri_estimates)

# Regression SE
reg_se <- itt1_results["offered", "Std. Error"]

cat("Randomization Inference SE:", round(ri_se), "Ksh\n")
cat("Regression clustered SE:", round(reg_se), "Ksh\n")
cat("Difference:", round(ri_se - reg_se), "Ksh\n")

# Plot
hist(ri_estimates,
     main = "Randomization Inference Distribution",
     xlab = "ITT Estimate (Ksh)",
     col = "lightblue",
     border = "white",
     breaks = 30)

abline(v = coef(itt1)["offered"], col = "red", lwd = 2, lty = 2)
abline(v = 0, col = "black", lwd = 1)

legend("topright",
       legend = c("Observed ITT", "Zero effect"),
       col = c("red", "black"),
       lwd = 2, lty = c(2, 1))

#Question7
#install.packages("AER")
library(AER)

# IV ToT Regression 1: No covariates (mirrors Q5 itt1)
tot1 <- ivreg(earnings ~ treated | offered, 
              data = round2)

# IV ToT Regression 2: ANCOVA (mirrors Q5 itt2)
tot2 <- ivreg(earnings ~ treated + baseline_earnings + age + 
                highest_grade + urban + trusting + risk_amount | 
                offered + baseline_earnings + age + 
                highest_grade + urban + trusting + risk_amount,
              data = round2)

# Clustered SEs for both
tot1_results <- coeftest(tot1, vcov = vcovCR(tot1, 
                                             cluster = round2$vill_id, type = "CR2"))
tot2_results <- coeftest(tot2, vcov = vcovCR(tot2, 
                                             cluster = round2$vill_id, type = "CR2"))

# Display both
print("IV ToT - No Covariates:")
print(tot1_results)
print("IV ToT - ANCOVA:")
print(tot2_results)

#stargazer
library(stargazer)

# Extract clustered SEs
se_tot1 <- sqrt(diag(vcovCR(tot1, cluster = round2$vill_id, type = "CR2")))
se_tot2 <- sqrt(diag(vcovCR(tot2, cluster = round2$vill_id, type = "CR2")))

stargazer(tot1, tot2,
          type = "text",
          title = "IV Estimates: Treatment on the Treated (ToT)",
          dep.var.labels = "Endline Earnings (Ksh)",
          column.labels = c("No Covariates", "ANCOVA"),
          covariate.labels = c("Treated",
                               "Baseline Earnings",
                               "Age",
                               "Highest Grade",
                               "Urban",
                               "Trusting",
                               "Risk Amount"),
          se = list(se_tot1, se_tot2),   # 🔥 clustered SEs
          digits = 3,
          star.cutoffs = c(0.1, 0.05, 0.01),
          omit.stat = c("f", "ser"),
          no.space = TRUE)

#manual verification
itt_estimate <- coef(itt1)["offered"]
uptake <- 153/396
manual_tot <- itt_estimate / uptake

cat("ITT estimate:", round(itt_estimate), "Ksh\n")
cat("Uptake rate:", round(uptake, 3), "\n")
cat("Manual ToT (ITT/uptake):", round(manual_tot), "Ksh\n")
cat("IV ToT estimate:", round(coef(tot1)["treated"]), "Ksh\n")

#Question8
#Question 8
library(MatchIt)

# Step 1: Use full sample
match_data_F <- round2

# Step 2: Estimate propensity score and match
match_model <- matchit(
  treated ~ age + highest_grade + urban + trusting + 
    risk_amount + baseline_earnings,
  data = match_data_F,
  method = "nearest",
  ratio = 1
)

# Step 3: Check balance
summary(match_model)

# Step 4: Extract matched dataset
matched_data_F <- match.data(match_model)

# Step 5: Estimate ToT using matched sample
tot_match <- lm(earnings ~ treated, data = matched_data_F)
summary(tot_match)

#Question 10
#step up the bonferroni threshold
# Number of tests we're running
n_tests <- 6

# Bonferroni corrected significance level
bonferroni_alpha <- 0.05 / n_tests
cat("Bonferroni corrected significance level:", round(bonferroni_alpha, 4), "\n")

library(lmtest)
library(clubSandwich)

# Heterogeneity with ANCOVA controls
# Run 6 interaction regressions with clustered SEs
het1 <- lm(earnings ~ offered * age + baseline_earnings + highest_grade + 
             urban + trusting + risk_amount, data = round2)

het2 <- lm(earnings ~ offered * highest_grade + baseline_earnings + age + 
             urban + trusting + risk_amount, data = round2)

het3 <- lm(earnings ~ offered * urban + baseline_earnings + age + 
             highest_grade + trusting + risk_amount, data = round2)

het4 <- lm(earnings ~ offered * trusting + baseline_earnings + age + 
             highest_grade + urban + risk_amount, data = round2)

het5 <- lm(earnings ~ offered * risk_amount + baseline_earnings + age + 
             highest_grade + urban + trusting, data = round2)

het6 <- lm(earnings ~ offered * baseline_earnings + age + highest_grade + 
             urban + trusting + risk_amount, data = round2)

# Clustered SEs
het1_r <- coeftest(het1, vcov = vcovCR(het1, cluster = round2$vill_id, type = "CR2"))
het2_r <- coeftest(het2, vcov = vcovCR(het2, cluster = round2$vill_id, type = "CR2"))
het3_r <- coeftest(het3, vcov = vcovCR(het3, cluster = round2$vill_id, type = "CR2"))
het4_r <- coeftest(het4, vcov = vcovCR(het4, cluster = round2$vill_id, type = "CR2"))
het5_r <- coeftest(het5, vcov = vcovCR(het5, cluster = round2$vill_id, type = "CR2"))
het6_r <- coeftest(het6, vcov = vcovCR(het6, cluster = round2$vill_id, type = "CR2"))

# Extract interaction terms and p-values from each regression
cat("=== HETEROGENEOUS TREATMENT EFFECTS ===\n")
cat("Bonferroni threshold: p <", bonferroni_alpha, "\n\n")

cat("1. offered x age interaction:\n")
print(het1_r["offered:age",])

cat("\n2. offered x highest_grade interaction:\n")
print(het2_r["offered:highest_grade",])

cat("\n3. offered x urban interaction:\n")
print(het3_r["offered:urban",])

cat("\n4. offered x trusting interaction:\n")
print(het4_r["offered:trusting",])

cat("\n5. offered x risk_amount interaction:\n")
print(het5_r["offered:risk_amount",])

cat("\n6. offered x baseline_earnings interaction:\n")
print(het6_r["offered:baseline_earnings",])

#table
library(stargazer)

# Extract clustered SEs
se_het1 <- sqrt(diag(vcovCR(het1, cluster = round2$vill_id, type = "CR2")))
se_het2 <- sqrt(diag(vcovCR(het2, cluster = round2$vill_id, type = "CR2")))
se_het3 <- sqrt(diag(vcovCR(het3, cluster = round2$vill_id, type = "CR2")))
se_het4 <- sqrt(diag(vcovCR(het4, cluster = round2$vill_id, type = "CR2")))
se_het5 <- sqrt(diag(vcovCR(het5, cluster = round2$vill_id, type = "CR2")))
se_het6 <- sqrt(diag(vcovCR(het6, cluster = round2$vill_id, type = "CR2")))

stargazer(het1, het2, het3, het4, het5, het6,
          type = "text",
          title = "Heterogeneous ITT Effects",
          keep = c("offered:"),   # 🔥 only interactions
          se = list(se_het1, se_het2, se_het3, se_het4, se_het5, se_het6),
          column.labels = c("Age", "Education", "Urban", "Trust", "Risk", "Baseline Earnings"),
          dep.var.labels = "Earnings",
          digits = 3,
          no.space = TRUE)

#Question 11b
set.seed(456)

# Separate treatment and control in round 2
treat_group <- round2[round2$offered == 1, ]
control_group <- round2[round2$offered == 0, ]

# Check original sizes
cat("Original treatment group:", nrow(treat_group), "\n")
cat("Original control group:", nrow(control_group), "\n")

# Randomly KEEP 80% of treatment (drop 20%)
keep_treat <- sample(1:nrow(treat_group), size = round(0.80 * nrow(treat_group)))
treat_attrited <- treat_group[keep_treat, ]

# Randomly KEEP 60% of control (drop 40%)
keep_control <- sample(1:nrow(control_group), size = round(0.60 * nrow(control_group)))
control_attrited <- control_group[keep_control, ]

# Combine back together
attrited_data <- rbind(treat_attrited, control_attrited)

# Check new sizes
cat("Attrited treatment group:", nrow(treat_attrited), "\n")
cat("Attrited control group:", nrow(control_attrited), "\n")
cat("Total attrited sample:", nrow(attrited_data), "\n")

#step 2
# Naive ITT on attrited sample
naive_itt <- lm(earnings ~ offered, data = attrited_data)
naive_itt_r <- coeftest(naive_itt, vcov = vcovCR(naive_itt, 
                                                 cluster = attrited_data$vill_id, 
                                                 type = "CR2"))
print("Naive ITT on Attrited Sample:")
print(naive_itt_r)
coef(naive_itt)["offered"]
#step 3  - Compute and calculate lee bounds
# Sort treatment earnings
treat_earnings <- sort(treat_attrited$earnings)
n_treat <- length(treat_earnings)

# How much to trim = gap in attrition rates = 40% - 20% = 20%
trim_pct <- 0.20
n_trim <- round(trim_pct * n_treat)

cat("Treatment observations:", n_treat, "\n")
cat("Number to trim:", n_trim, "\n")

# Upper bound: trim BOTTOM 20% of treatment
upper_treat_mean <- mean(treat_earnings[(n_trim + 1):n_treat])

# Lower bound: trim TOP 20% of treatment  
lower_treat_mean <- mean(treat_earnings[1:(n_treat - n_trim)])

# Control mean (unchanged)
control_mean <- mean(control_attrited$earnings)

# Calculate bounds
upper_bound <- upper_treat_mean - control_mean
lower_bound <- lower_treat_mean - control_mean

cat("\nControl mean earnings:", round(control_mean), "Ksh\n")
cat("Upper treatment mean (bottom trimmed):", round(upper_treat_mean), "Ksh\n")
cat("Lower treatment mean (top trimmed):", round(lower_treat_mean), "Ksh\n")
cat("\n=== LEE BOUNDS ===\n")
cat("Lower bound:", round(lower_bound), "Ksh\n")
cat("Upper bound:", round(upper_bound), "Ksh\n")
cat("Naive ITT:", round(coef(naive_itt)["offered"]), "Ksh\n")
cat("Does bound contain zero?", lower_bound < 0 & upper_bound > 0, "\n")

#Question 12
# Key parameters
cost_per_participant_usd <- 1500    # Cost in USD
exchange_rate <- 79                  # 1 USD = 79 Ksh
monthly_discount_rate <- 0.01        # 1% monthly discount rate
n_months <- 24                       # 2 year timeframe

# Convert cost to Ksh
cost_per_participant_ksh <- cost_per_participant_usd * exchange_rate

# Our ToT estimates from Q7
tot_no_cov <- 4591      # IV ToT without covariates
tot_with_cov <- 4458    # IV ToT with covariates
itt_estimate <- 1774    # ITT from Q5 (pessimistic scenario)

cat("Cost per participant (USD):", cost_per_participant_usd, "\n")
cat("Cost per participant (Ksh):", cost_per_participant_ksh, "\n")
cat("Monthly discount rate:", monthly_discount_rate * 100, "%\n")
cat("Timeframe:", n_months, "months\n")

#step2

# Create sequence of months 1 to 24
months <- 1:n_months

# Calculate discount factor for each month
discount_factors <- 1 / (1 + monthly_discount_rate)^months

# Calculate total discounted benefits for each scenario
pv_no_cov <- sum(tot_no_cov * discount_factors)
pv_with_cov <- sum(tot_with_cov * discount_factors)
pv_itt <- sum(itt_estimate * discount_factors)

cat("Total discounted benefits (ToT no covariates):", round(pv_no_cov), "Ksh\n")
cat("Total discounted benefits (ToT with covariates):", round(pv_with_cov), "Ksh\n")
cat("Total discounted benefits (ITT pessimistic):", round(pv_itt), "Ksh\n")

#step3
# Net Present Benefits = Total Benefits - Cost
npb_no_cov <- pv_no_cov - cost_per_participant_ksh
npb_with_cov <- pv_with_cov - cost_per_participant_ksh
npb_itt <- pv_itt - cost_per_participant_ksh

# Benefit Cost Ratio = Total Benefits / Cost
bcr_no_cov <- pv_no_cov / cost_per_participant_ksh
bcr_with_cov <- pv_with_cov / cost_per_participant_ksh
bcr_itt <- pv_itt / cost_per_participant_ksh

cat("=== COST BENEFIT ANALYSIS RESULTS ===\n\n")
cat("Cost per participant:", cost_per_participant_ksh, "Ksh\n\n")

cat("--- Scenario 1: ToT No Covariates ---\n")
cat("Total discounted benefits:", round(pv_no_cov), "Ksh\n")
cat("Net present benefit:", round(npb_no_cov), "Ksh\n")
cat("Benefit-cost ratio:", round(bcr_no_cov, 3), "\n\n")

cat("--- Scenario 2: ToT With Covariates ---\n")
cat("Total discounted benefits:", round(pv_with_cov), "Ksh\n")
cat("Net present benefit:", round(npb_with_cov), "Ksh\n")
cat("Benefit-cost ratio:", round(bcr_with_cov, 3), "\n\n")

cat("--- Scenario 3: ITT Pessimistic ---\n")
cat("Total discounted benefits:", round(pv_itt), "Ksh\n")
cat("Net present benefit:", round(npb_itt), "Ksh\n")
cat("Benefit-cost ratio:", round(bcr_itt, 3), "\n")