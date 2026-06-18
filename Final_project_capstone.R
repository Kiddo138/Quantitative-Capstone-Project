# ============================================================
# CAPSTONE ANALYSIS: State EITC Effects on County Poverty
# ============================================================

library(tidyr)
setwd("C:/Users/abdir/OneDrive - UC San Diego/Documents/QM4/Final Capstone Paper Analysis")

library(dplyr)
library(ggplot2)
library(fixest)
library(did)

# ============================================================
# STEP 1: Load and inspect data
# ============================================================

data_original <- read.csv("final_data_ALL_CONTROLS.csv")
data_work <- data_original

head(data_work)
str(data_work)
dim(data_work)
names(data_work)

# ============================================================
# STEP 2: Check missing values
# ============================================================

colSums(is.na(data_work))

key_vars <- c("pov_rate", "EITC_on", "log_pop", "pct_under18", "fips", "year", "state_abbr")
colSums(is.na(data_work[key_vars]))

# ============================================================
# STEP 3: Check treatment timing
# ============================================================

treatment_timing <- data_work %>%
  filter(!is.na(adopt_year)) %>%
  select(state_abbr, adopt_year) %>%
  distinct() %>%
  arrange(adopt_year)
print(treatment_timing)

data_work %>%
  select(state_abbr, switcher_post_2000, treated_by_2000, never_treated) %>%
  distinct() %>%
  summarise(
    n_switchers = sum(switcher_post_2000),
    n_always_treated = sum(treated_by_2000),
    n_never_treated = sum(never_treated)
  )

# ============================================================
# STEP 4: Build clean estimation sample
# ============================================================

# 4a: Remove NC (treatment reversal - adopts 2008, drops 2015)
# 4a: Remove NC (treatment reversal) and CT (county restructuring mid-panel)
data_cs <- data_work %>%
  filter(!state_abbr %in% c("NC", "CT"))
# 4b: Impute missing pct_under18 for HI (1997-1999 missing from SEER source)
# Fill backwards using earliest available value (2000) - justified as pct_under18 
# is a slowly moving demographic variable
data_cs <- data_cs %>%
  group_by(fips) %>%
  arrange(year) %>%
  fill(pct_under18, .direction = "up") %>%
  ungroup()

drop_fips <- c(
  # Alaska - county reorganizations
  2063, 2066, 2105, 2158, 2195, 2198, 2201, 2230, 2232, 2261,
  2270, 2275, 2280,
  # Colorado
  8014,
  # South Dakota
  46102, 46113,
  # Virginia
  51515, 51560,
  # Hawaii - Kalawao County (19/27 years missing poverty data)
  15005
)

# 4c: Drop problematic FIPS and missing values
data_est <- data_cs %>%
  filter(!(fips %in% drop_fips)) %>%
  filter(
    !is.na(pov_rate),
    !is.na(log_pop),
    !is.na(pct_under18)
  )

# 4d: Balance the panel
data_balanced <- data_est %>%
  group_by(fips) %>%
  filter(n_distinct(year) == 27) %>%
  ungroup()

# 4e: Verify final sample
data_balanced %>%
  summarise(
    n_obs = n(),
    n_counties = n_distinct(fips),
    n_states = n_distinct(state_abbr),
    n_years = n_distinct(year)
  )

# 4f: Verify treatment groups
data_balanced %>%
  distinct(state_abbr, switcher_post_2000, treated_by_2000, never_treated) %>%
  summarise(
    n_switchers = sum(switcher_post_2000),
    n_early_adopters = sum(treated_by_2000),
    n_never_treated = sum(never_treated)
  )

# ============================================================
# STEP 5: Summary statistics on final sample
# ============================================================

summary_stats <- data_balanced %>%
  summarise(
    pov_rate_mean = mean(pov_rate, na.rm = TRUE),
    pov_rate_sd = sd(pov_rate, na.rm = TRUE),
    pov_rate_min = min(pov_rate, na.rm = TRUE),
    pov_rate_max = max(pov_rate, na.rm = TRUE),
    EITC_mean = mean(EITC_on, na.rm = TRUE),
    log_pop_mean = mean(log_pop, na.rm = TRUE),
    log_pop_sd = sd(log_pop, na.rm = TRUE),
    pct_under18_mean = mean(pct_under18, na.rm = TRUE),
    pct_under18_sd = sd(pct_under18, na.rm = TRUE),
    n_obs = n(),
    n_counties = n_distinct(fips),
    n_years = n_distinct(year)
  )
print(summary_stats)
summary(data_balanced[c("pov_rate", "EITC_on", "log_pop", "pct_under18")])

# ============================================================
# STEP 6: Balance check - switchers vs never-treated (1997-2001)
# ============================================================

balance_check <- data_balanced %>%
  filter(year >= 1997 & year <= 2001) %>%
  filter(switcher_post_2000 == 1 | never_treated == 1) %>%
  group_by(switcher_post_2000) %>%
  summarise(
    mean_poverty = mean(pov_rate, na.rm = TRUE),
    mean_log_pop = mean(log_pop, na.rm = TRUE),
    mean_pct_under18 = mean(pct_under18, na.rm = TRUE),
    n_counties = n_distinct(fips)
  ) %>%
  mutate(group = ifelse(switcher_post_2000 == 1, "Switchers", "Never Treated"))
print(balance_check)

# ============================================================
# STEP 7: Pre/post trends plot - switchers vs never-treated
# ============================================================

trend_data_clean <- data_balanced %>%
  filter(switcher_post_2000 == 1 | never_treated == 1) %>%
  group_by(year, switcher_post_2000) %>%
  summarise(mean_poverty = mean(pov_rate, na.rm = TRUE), .groups = "drop") %>%
  mutate(group = ifelse(switcher_post_2000 == 1, "Switchers (Post-2000)", "Never Treated"))

trends_plot <- ggplot(trend_data_clean, aes(x = year, y = mean_poverty, color = group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2002, linetype = "dashed", color = "black") +
  scale_linetype_manual(values = c("Never Treated" = "solid", 
                                   "Switchers (Post-2000)" = "solid")) +
  scale_x_continuous(breaks = seq(1997, 2023, by = 2)) +
  scale_y_continuous(breaks = seq(12, 22, by = 2)) +
  labs(
    title = "Average County Poverty Rates: Switchers vs Never-Treated",
    subtitle = "Dashed line marks first post-2000 EITC adoption (Oklahoma, 2002)",
    x = "Year",
    y = "Average County Poverty Rate (%)",
    color = "Group",
    linetype = "Group"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )
print(trends_plot)
# Save publication quality
ggsave("trends_plot.png", 
       plot = trends_plot,
       width = 8, 
       height = 5.5, 
       dpi = 300,
       bg = "white")



data_cs %>%
  filter(state_abbr == "HI", fips == 15001) %>%
  select(year, pov_rate, log_pop, pct_under18) %>%
  arrange(year)


#quick checks to see if data is balanced
# Check 1: Every county has exactly 27 years
data_balanced %>%
  group_by(fips) %>%
  summarise(n_years = n_distinct(year), .groups = "drop") %>%
  count(n_years)

# Check 2: Every year has the same number of counties
data_balanced %>%
  group_by(year) %>%
  summarise(n_counties = n_distinct(fips), .groups = "drop")

# Check 3: Final sample summary
data_balanced %>%
  summarise(
    n_obs = n(),
    n_counties = n_distinct(fips),
    n_states = n_distinct(state_abbr),
    n_years = n_distinct(year)
  )
#check graph
treated_counts_final <- data_balanced %>%
  filter(EITC_on == 1) %>%
  group_by(year) %>%
  summarise(n_treated = n_distinct(state_abbr), .groups = "drop")

# Treatment Rollout Plot
rollout_plot <- ggplot(treated_counts_final, aes(x = year, y = n_treated)) +
  geom_line(linewidth = 1.2, color = "#2C7BB6") +
  geom_point(size = 2.5, color = "#2C7BB6") +
  scale_x_continuous(breaks = seq(1997, 2023, by = 2)) +
  scale_y_continuous(breaks = seq(0, 35, by = 5), limits = c(0, 35)) +
  labs(
    title = "Number of States with Active EITC Programs Over Time",
    subtitle = "Staggered adoption across 16 post-2000 switcher states",
    x = "Year",
    y = "Number of States with Active EITC",
    caption = "Note: Includes all treated states in balanced panel sample."
  ) +
  theme_bw() +
  theme(
    panel.border = element_blank(),        # removes outer box
    axis.line = element_line(color = "black"),  # keeps clean axis lines
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    plot.caption = element_text(size = 9, color = "gray40", hjust = 0),
    panel.grid.minor = element_blank()
  )
print(rollout_plot)

# Save publication quality
ggsave("rollout_plot.png",
       plot = rollout_plot,
       width = 8,
       height = 5.5,
       dpi = 300,
       bg = "white")

#summary statistics
# ============================================================
# CAPSTONE ANALYSIS: State EITC Effects on County Poverty
# ============================================================

library(tidyr)
setwd("C:/Users/abdir/OneDrive - UC San Diego/Documents/QM4/Final Capstone Paper Analysis")

library(dplyr)
library(ggplot2)
library(fixest)
library(did)

# ============================================================
# STEP 1: Load and inspect data
# ============================================================

data_original <- read.csv("final_data_ALL_CONTROLS.csv")
data_work <- data_original

head(data_work)
str(data_work)
dim(data_work)
names(data_work)

# ============================================================
# STEP 2: Check missing values
# ============================================================

colSums(is.na(data_work))

key_vars <- c("pov_rate", "EITC_on", "log_pop", "pct_under18", "fips", "year", "state_abbr")
colSums(is.na(data_work[key_vars]))

# ============================================================
# STEP 3: Check treatment timing
# ============================================================

treatment_timing <- data_work %>%
  filter(!is.na(adopt_year)) %>%
  select(state_abbr, adopt_year) %>%
  distinct() %>%
  arrange(adopt_year)
print(treatment_timing)

data_work %>%
  select(state_abbr, switcher_post_2000, treated_by_2000, never_treated) %>%
  distinct() %>%
  summarise(
    n_switchers = sum(switcher_post_2000),
    n_always_treated = sum(treated_by_2000),
    n_never_treated = sum(never_treated)
  )

# ============================================================
# STEP 4: Build clean estimation sample
# ============================================================

# 4a: Remove NC (treatment reversal - adopts 2008, drops 2015)
# 4a: Remove NC (treatment reversal) and CT (county restructuring mid-panel)
data_cs <- data_work %>%
  filter(!state_abbr %in% c("NC", "CT"))
# 4b: Impute missing pct_under18 for HI (1997-1999 missing from SEER source)
# Fill backwards using earliest available value (2000) - justified as pct_under18 
# is a slowly moving demographic variable
data_cs <- data_cs %>%
  group_by(fips) %>%
  arrange(year) %>%
  fill(pct_under18, .direction = "up") %>%
  ungroup()

drop_fips <- c(
  # Alaska - county reorganizations
  2063, 2066, 2105, 2158, 2195, 2198, 2201, 2230, 2232, 2261,
  2270, 2275, 2280,
  # Colorado
  8014,
  # South Dakota
  46102, 46113,
  # Virginia
  51515, 51560,
  # Hawaii - Kalawao County (19/27 years missing poverty data)
  15005
)

# 4c: Drop problematic FIPS and missing values
data_est <- data_cs %>%
  filter(!(fips %in% drop_fips)) %>%
  filter(
    !is.na(pov_rate),
    !is.na(log_pop),
    !is.na(pct_under18)
  )

# 4d: Balance the panel
data_balanced <- data_est %>%
  group_by(fips) %>%
  filter(n_distinct(year) == 27) %>%
  ungroup()

# 4e: Verify final sample
data_balanced %>%
  summarise(
    n_obs = n(),
    n_counties = n_distinct(fips),
    n_states = n_distinct(state_abbr),
    n_years = n_distinct(year)
  )

# 4f: Verify treatment groups
data_balanced %>%
  distinct(state_abbr, switcher_post_2000, treated_by_2000, never_treated) %>%
  summarise(
    n_switchers = sum(switcher_post_2000),
    n_early_adopters = sum(treated_by_2000),
    n_never_treated = sum(never_treated)
  )

# ============================================================
# STEP 5: Summary statistics on final sample
# ============================================================

summary_stats <- data_balanced %>%
  summarise(
    pov_rate_mean = mean(pov_rate, na.rm = TRUE),
    pov_rate_sd = sd(pov_rate, na.rm = TRUE),
    pov_rate_min = min(pov_rate, na.rm = TRUE),
    pov_rate_max = max(pov_rate, na.rm = TRUE),
    EITC_mean = mean(EITC_on, na.rm = TRUE),
    log_pop_mean = mean(log_pop, na.rm = TRUE),
    log_pop_sd = sd(log_pop, na.rm = TRUE),
    pct_under18_mean = mean(pct_under18, na.rm = TRUE),
    pct_under18_sd = sd(pct_under18, na.rm = TRUE),
    n_obs = n(),
    n_counties = n_distinct(fips),
    n_years = n_distinct(year)
  )
print(summary_stats)
summary(data_balanced[c("pov_rate", "EITC_on", "log_pop", "pct_under18")])

# ============================================================
# STEP 6: Balance check - switchers vs never-treated (1997-2001)
# ============================================================

balance_check <- data_balanced %>%
  filter(year >= 1997 & year <= 2001) %>%
  filter(switcher_post_2000 == 1 | never_treated == 1) %>%
  group_by(switcher_post_2000) %>%
  summarise(
    mean_poverty = mean(pov_rate, na.rm = TRUE),
    mean_log_pop = mean(log_pop, na.rm = TRUE),
    mean_pct_under18 = mean(pct_under18, na.rm = TRUE),
    n_counties = n_distinct(fips)
  ) %>%
  mutate(group = ifelse(switcher_post_2000 == 1, "Switchers", "Never Treated"))
print(balance_check)

# ============================================================
# STEP 7: Pre/post trends plot - switchers vs never-treated
# ============================================================

trend_data_clean <- data_balanced %>%
  filter(switcher_post_2000 == 1 | never_treated == 1) %>%
  group_by(year, switcher_post_2000) %>%
  summarise(mean_poverty = mean(pov_rate, na.rm = TRUE), .groups = "drop") %>%
  mutate(group = ifelse(switcher_post_2000 == 1, "Switchers (Post-2000)", "Never Treated"))

ggplot(trend_data_clean, aes(x = year, y = mean_poverty, color = group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2002, linetype = "dashed", color = "black") +
  scale_linetype_manual(values = c("Never Treated" = "solid", 
                                   "Switchers (Post-2000)" = "solid")) +
  scale_x_continuous(breaks = seq(1997, 2023, by = 2)) +
  scale_y_continuous(breaks = seq(12, 22, by = 2)) +
  labs(
    title = "Average County Poverty Rates: Switchers vs Never-Treated",
    x = "Year",
    y = "Average County Poverty Rate (%)",
    color = "Group"
  ) +
  labs(
    title = "Average County Poverty Rates: Switchers vs Never-Treated",
    subtitle = "Dashed line marks first post-2000 EITC adoption (Oklahoma, 2002)",
    x = "Year",
    y = "Average County Poverty Rate (%)",
    color = "Group",
    linetype = "Group"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

# All switchers in original data_work
switchers_original <- data_work %>%
  filter(switcher_post_2000 == 1) %>%
  distinct(state_abbr) %>%
  arrange(state_abbr)

# Switchers in final balanced data
switchers_final <- data_balanced %>%
  filter(switcher_post_2000 == 1) %>%
  distinct(state_abbr) %>%
  arrange(state_abbr)

# Who is missing?
anti_join(switchers_original, switchers_final, by = "state_abbr")

data_cs %>%
  filter(state_abbr == "HI", fips == 15001) %>%
  select(year, pov_rate, log_pop, pct_under18) %>%
  arrange(year)


#quick checks to see if data is balanced
# Check 1: Every county has exactly 27 years
data_balanced %>%
  group_by(fips) %>%
  summarise(n_years = n_distinct(year), .groups = "drop") %>%
  count(n_years)

# Check 2: Every year has the same number of counties
data_balanced %>%
  group_by(year) %>%
  summarise(n_counties = n_distinct(fips), .groups = "drop")

# Check 3: Final sample summary
data_balanced %>%
  summarise(
    n_obs = n(),
    n_counties = n_distinct(fips),
    n_states = n_distinct(state_abbr),
    n_years = n_distinct(year)
  )
#check graph
treated_counts_final <- data_balanced %>%
  filter(EITC_on == 1) %>%
  group_by(year) %>%
  summarise(n_treated = n_distinct(state_abbr), .groups = "drop")

ggplot(treated_counts_final, aes(x = year, y = n_treated)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(1997, 2023, by = 2)) +
  scale_y_continuous(breaks = seq(0, 35, by = 5)) +
  labs(
    title = "Number of States with EITC Programs Over Time",
    x = "Year",
    y = "Number of States with EITC"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#summary statistics
table1 <- data_balanced %>%
  summarise(
    Mean = mean(pov_rate, na.rm = TRUE),
    SD = sd(pov_rate, na.rm = TRUE),
    Min = min(pov_rate, na.rm = TRUE),
    Max = max(pov_rate, na.rm = TRUE)
  ) %>%
  mutate(Variable = "Poverty Rate (%)") %>%
  bind_rows(
    data_balanced %>%
      summarise(
        Mean = mean(EITC_on, na.rm = TRUE),
        SD = sd(EITC_on, na.rm = TRUE),
        Min = min(EITC_on, na.rm = TRUE),
        Max = max(EITC_on, na.rm = TRUE)
      ) %>%
      mutate(Variable = "EITC Adoption (0/1)")
  ) %>%
  bind_rows(
    data_balanced %>%
      summarise(
        Mean = mean(log_pop, na.rm = TRUE),
        SD = sd(log_pop, na.rm = TRUE),
        Min = min(log_pop, na.rm = TRUE),
        Max = max(log_pop, na.rm = TRUE)
      ) %>%
      mutate(Variable = "Log Population")
  ) %>%
  bind_rows(
    data_balanced %>%
      summarise(
        Mean = mean(pct_under18, na.rm = TRUE),
        SD = sd(pct_under18, na.rm = TRUE),
        Min = min(pct_under18, na.rm = TRUE),
        Max = max(pct_under18, na.rm = TRUE)
      ) %>%
      mutate(Variable = "Percent Under 18")
  ) %>%
  select(Variable, Mean, SD, Min, Max) %>%
  mutate(across(where(is.numeric), ~round(., 3)))

print(table1)

#Table 2 — By treatment group in pre-period (1997–2001)
table2 <- data_balanced %>%
  filter(year >= 1997 & year <= 2001) %>%
  mutate(group = case_when(
    switcher_post_2000 == 1 ~ "Switchers",
    treated_by_2000 == 1   ~ "Early Adopters",
    never_treated == 1     ~ "Never Treated"
  )) %>%
  group_by(group) %>%
  summarise(
    mean_poverty    = round(mean(pov_rate, na.rm = TRUE), 3),
    sd_poverty      = round(sd(pov_rate, na.rm = TRUE), 3),
    mean_log_pop    = round(mean(log_pop, na.rm = TRUE), 3),
    sd_log_pop      = round(sd(log_pop, na.rm = TRUE), 3),
    mean_pct_under18 = round(mean(pct_under18, na.rm = TRUE), 3),
    sd_pct_under18  = round(sd(pct_under18, na.rm = TRUE), 3),
    n_counties      = n_distinct(fips),
    .groups = "drop"
  )

print(table2)
#======================================================================================
#  END OF PART 1, CLEAR AND DONE
#======================================================================================
# : Static TWFE Regression
twfe_static <- feols(
  pov_rate ~ EITC_on + log_pop + pct_under18 | fips + year,
  data = data_balanced,
  cluster = "state_abbr"
)

summary(twfe_static)

#dynamic twfe regression

# 9a: Create time to treat for switchers only
data_balanced_work <- data_balanced
data_balanced_work <- data_balanced_work %>%
  mutate(
    time_to_treat = case_when(
      switcher_post_2000 == 1 ~ year - adopt_year,
      TRUE ~ 0  # non-switchers get 0
    )
  )

# 9b: Bin extreme leads/lags to +/- 5 for cleaner plot
data_balanced_work <- data_balanced_work %>%
  mutate(time_to_treat = pmax(pmin(time_to_treat, 5), -5))

# 9c: Run dynamic TWFE
twfe_dynamic <- feols(
  pov_rate ~ i(time_to_treat, switcher_post_2000, ref = -1) + 
    log_pop + pct_under18 | fips + year,
  data = data_balanced_work,
  cluster = "state_abbr"
)

summary(twfe_dynamic)

# 9d: Plot event study
iplot(
  twfe_dynamic,
  xlab = "Years Relative to EITC Adoption",
  ylab = "Effect on County Poverty Rate (pp)",
  main = "Dynamic TWFE Event Study: State EITC Adoption on County Poverty",
  sub = "Coefficients relative to one year before adoption (t = -1)",
  ci_level = 0.95,
  zero.par = list(col = "black", lty = 2, lwd = 1.5),
  col = c("#E8735A", "#2C9EBF"),
  pt.pch = 20,
  pt.cex = 1.5,
  lwd = 2
)

# Add caption manually below plot
mtext(
  "Note: 95% confidence intervals shown. Standard errors clustered at state level. County and year fixed effects included.",
  side = 1,
  line = 5,
  cex = 0.75,
  col = "gray40",
  adj = 0
)

#Bacon decomposition
# Quick sanity check on state-level data
data_state %>%
  summarise(
    n_states = n_distinct(state_abbr),
    n_years = n_distinct(year),
    n_obs = n()
  )


# ============================================================
# STEP 10: Bacon Decomposition
# ============================================================
library(bacondecomp)
# Collapse to state-year level because treatment varies at state-year level
data_state <- data_balanced %>%
  group_by(state_abbr, year) %>%
  summarise(
    pov_rate = mean(pov_rate, na.rm = TRUE),
    EITC_on = max(EITC_on, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    state_id = as.integer(as.factor(state_abbr))
  )
# Quick sanity check on state-level data
data_state %>%
  summarise(
    n_states = n_distinct(state_abbr),
    n_years = n_distinct(year),
    n_obs = n()
  )

# Run Bacon decomposition on TWFE treatment indicator
bacon_out <- bacon(
  pov_rate ~ EITC_on,
  data = data_state,
  id_var = "state_id",
  time_var = "year"
)

print(bacon_out)

# Plot Bacon decomposition
bacon_plot <- ggplot(bacon_out, aes(x = weight, y = estimate, color = type)) +
  geom_point(size = 3.5, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
  labs(
    title = "Bacon Decomposition of Two-Way Fixed Effects Estimate",
    subtitle = "Each point represents a 2x2 DiD comparison weighted by sample share",
    x = "Weight in TWFE Estimate",
    y = "2×2 DiD Estimate",
    color = "Comparison Type",
    caption = "Note: Decomposition run at state level. Dashed line indicates zero effect."
  ) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "right",           # moves legend to right side
    legend.direction = "vertical",       # stacks items vertically
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    plot.caption = element_text(size = 9, color = "gray40", hjust = 0),
    panel.grid.minor = element_blank()
  )
print(bacon_plot)

ggsave("bacon_plot.png",
       plot = bacon_plot,
       width = 9,
       height = 5.5,
       dpi = 300,
       bg = "white")

# ============================================================
# STEP 11: Callaway & Sant'Anna ATT(g,t)
# ============================================================
library(did)
library(dplyr)
library(ggplot2)
# Build clean dataset
data_cs_final <- data_balanced %>%
  filter(switcher_post_2000 == 1 | never_treated == 1) %>%
  mutate(
    first_treat = ifelse(never_treated == 1, 3000, adopt_year)
  ) %>%
  as.data.frame()

# Verify
data_cs_final %>%
  distinct(state_abbr, first_treat, never_treated, switcher_post_2000) %>%
  arrange(first_treat) %>%
  print(n = Inf)

# Run att_gt - main spec (notyettreated)
cs_results_main <- att_gt(
  yname = "pov_rate",
  tname = "year",
  idname = "fips",
  gname = "first_treat",
  data = data_cs_final,
  control_group = "notyettreated",
  clustervars = "state_abbr",
  bstrap = TRUE,
  biters = 1000,
  panel = TRUE
)

# Run att_gt - robustness spec (nevertreated)
cs_results_robust <- att_gt(
  yname = "pov_rate",
  tname = "year",
  idname = "fips",
  gname = "first_treat",
  data = data_cs_final,
  control_group = "nevertreated",
  clustervars = "state_abbr",
  bstrap = TRUE,
  biters = 1000,
  panel = TRUE
)

# ============================================================
# STEP 12: Event study aggregation
# ============================================================

es_main <- aggte(cs_results_main, type = "dynamic",
                 min_e = -5, max_e = 10)
summary(es_main)
ggdid(es_main,
      title = "C&S Event Study: State EITC Adoption on County Poverty",
      ylab = "ATT (Poverty Rate %)",
      xlab = "Years Relative to EITC Adoption")

es_robust <- aggte(cs_results_robust, type = "dynamic",
                   min_e = -5, max_e = 10)
summary(es_robust)
ggdid(es_robust,
      title = "C&S Event Study: Robustness (Never-Treated Control)",
      ylab = "ATT (Poverty Rate %)",
      xlab = "Years Relative to EITC Adoption")

# Overall ATT
overall_main <- aggte(cs_results_main, type = "simple")
summary(overall_main)

overall_robust <- aggte(cs_results_robust, type = "simple")
summary(overall_robust)
# ============================================================
# STEP 13: Overall ATT
# ============================================================

overall_main <- aggte(cs_results_main, type = "simple")
summary(overall_main)

overall_robust <- aggte(cs_results_robust, type = "simple")
summary(overall_robust)

#regression tables 
library(modelsummary)
# TWFE without controls
twfe_no_controls <- feols(
  pov_rate ~ EITC_on | fips + year,
  data = data_balanced,
  cluster = "state_abbr"
)

# TWFE with controls
twfe_with_controls <- feols(
  pov_rate ~ EITC_on + log_pop + pct_under18 | fips + year,
  data = data_balanced,
  cluster = "state_abbr"
)

# Professional table
modelsummary(
  list("No Controls" = twfe_no_controls,
       "With Controls" = twfe_with_controls),
  title = "Table 1: TWFE Estimates of State EITC Adoption on County Poverty Rates",
  coef_rename = c("EITC_on" = "State EITC Adoption",
                  "log_pop" = "Log Population",
                  "pct_under18" = "Percent Under 18"),
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  notes = "Standard errors clustered at state level. County and year fixed effects included in all specifications.",
  output = "twfe_table.docx"
)

# Professional table
modelsummary(
  list("No Controls" = twfe_no_controls,
       "With Controls" = twfe_with_controls),
  title = "Table 1: TWFE Estimates of State EITC Adoption on County Poverty Rates",
  coef_rename = c("EITC_on" = "State EITC Adoption",
                  "log_pop" = "Log Population",
                  "pct_under18" = "Percent Under 18"),
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  notes = "Standard errors clustered at state level. County and year fixed effects included in all specifications.",
  output = "twfe_table.html"
)

#Callaway and Santa Anna results table
library(tibble)

# Extract results from overall ATT
cs_table <- tribble(
  ~term,                    ~`Main (Not-Yet-Treated)`,  ~`Robustness (Never-Treated)`,
  "State EITC Adoption",     "-0.072",                   "-0.059",
  "",                        "(0.260)",                  "(0.237)",
  "95% CI",                  "[-0.581, 0.437]",          "[-0.522, 0.405]",
  "Control Group",           "Not-Yet-Treated",          "Never-Treated",
  "Observations",            "61,344",                   "61,344",
  "Treated States",          "16",                       "16",
  "Never-Treated States",    "18",                       "18",
  "Estimation Method",       "Doubly Robust",            "Doubly Robust",
  "Bootstrap Iterations",    "1,000",                    "1,000"
)

# Save as html
library(knitr)
library(kableExtra)

cs_table %>%
  kable(
    format = "html",
    caption = "<b style='color:black; font-size:16px'>Table 2: Callaway & Sant'Anna ATT Estimates of State EITC Adoption on County Poverty Rates</b>",
    align = c("l", "c", "c"),
    escape = FALSE
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 16,
    html_font = "Arial"
  ) %>%
  row_spec(0, bold = TRUE, color = "black", background = "#D3D3D3") %>%
  row_spec(1:9, color = "black") %>%
  footnote(
    general = "Standard errors in parentheses. * p<0.1, ** p<0.05, *** p<0.01. Standard errors clustered at state level. Sample restricted to post-2000 switcher states and never-treated states.",
    general_title = "Notes:"
  ) %>%
  save_kable("cs_table2.html")

#professional summary statistics table
# ============================================================
# STEP 5: Professional Summary Statistics Table
# ============================================================

library(kableExtra)
library(dplyr)

# Build summary stats by group
summary_full <- data_balanced %>%
  summarise(
    across(c(pov_rate, EITC_on, log_pop, pct_under18, tot_pop), 
           list(
             Mean = ~round(mean(., na.rm = TRUE), 3),
             SD   = ~round(sd(., na.rm = TRUE), 3),
             Min  = ~round(min(., na.rm = TRUE), 3),
             Max  = ~round(max(., na.rm = TRUE), 3)
           ))
  ) %>%
  pivot_longer(everything(),
               names_to = c("Variable", ".value"),
               names_sep = "_(?=[^_]+$)")

# Rename variables cleanly
summary_full <- summary_full %>%
  mutate(Variable = case_when(
    Variable == "pov_rate"    ~ "Poverty Rate (%)",
    Variable == "EITC_on"     ~ "State EITC Active (0/1)",
    Variable == "log_pop"     ~ "Log Population",
    Variable == "pct_under18" ~ "Percent Under 18 (%)",
    Variable == "tot_pop"     ~ "Total Population",
    TRUE ~ Variable
  ))

# Add observation counts
summary_full <- summary_full %>%
  mutate(N = c(
    sum(!is.na(data_balanced$pov_rate)),
    sum(!is.na(data_balanced$EITC_on)),
    sum(!is.na(data_balanced$log_pop)),
    sum(!is.na(data_balanced$pct_under18)),
    sum(!is.na(data_balanced$tot_pop))
  )) %>%
  select(Variable, N, Mean, SD, Min, Max)

# Build professional table
summary_full %>%
  kable(
    format = "html",
    caption = "<b style='color:black; font-size:16px'>Table 1: Summary Statistics — County-Level Panel, 1997–2023</b>",
    align = c("l", "c", "c", "c", "c", "c"),
    escape = FALSE,
    col.names = c("Variable", "N", "Mean", "Std. Dev.", "Min", "Max")
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 16,
    html_font = "Arial"
  ) %>%
  row_spec(0, bold = TRUE, color = "black", background = "#D3D3D3") %>%
  row_spec(1:5, color = "black") %>%
  footnote(
    general = "Sample consists of 3,022 counties across 49 states observed annually from 1997 to 2023. N = 81,594 county-year observations. North Carolina and Connecticut excluded due to treatment reversal and county restructuring respectively.",
    general_title = "Notes:"
  ) %>%
  save_kable("summary_stats_table.html")
# Main spec C&S event study
cs_main_plot <- ggdid(es_main) +
  labs(
    title = "Callaway and Sant'Anna Event Study: State EITC Adoption on County Poverty",
    subtitle = "Main specification — not-yet-treated control group",
    x = "Years Relative to EITC Adoption",
    y = "ATT (Poverty Rate, pp)",
    caption = "Note: 95% simultaneous confidence bands shown. Standard errors clustered at state level.\nSample restricted to post-2000 switcher states and never-treated states. Doubly robust estimation."
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "bottom",
    legend.text = element_text(size = 11),
    legend.title = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    plot.caption = element_text(size = 9, color = "gray40", hjust = 0),
    panel.grid.minor = element_blank()
  )
print(cs_main_plot)
ggsave("cs_event_study_main.png",
       plot = cs_main_plot,
       width = 9,
       height = 5.5,
       dpi = 300,
       bg = "white")

# Robustness spec C&S event study
cs_robust_plot <- ggdid(es_robust) +
  labs(
    title = "Callaway and Sant'Anna Event Study: Robustness Check — Never-Treated Control Group",
    subtitle = "Robustness specification — never-treated control group only",
    x = "Years Relative to EITC Adoption",
    y = "ATT (Poverty Rate, pp)",
    caption = "Note: 95% simultaneous confidence bands shown. Standard errors clustered at state level.\nSample restricted to post-2000 switcher states and never-treated states. Doubly robust estimation."
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "bottom",
    legend.text = element_text(size = 11),
    legend.title = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    plot.caption = element_text(size = 9, color = "gray40", hjust = 0),
    panel.grid.minor = element_blank()
  )
print(cs_robust_plot)
ggsave("cs_event_study_robust.png",
       plot = cs_robust_plot,
       width = 9,
       height = 5.5,
       dpi = 300,
       bg = "white")