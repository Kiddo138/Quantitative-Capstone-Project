setwd("C:/Users/abdir/OneDrive - UC San Diego/Documents/GIS AND SPATIAL DATA ANALYSIS")
getwd()
library(readxl)
library(ggplot2)
# Read your Excel file (change the path to your file location)
distance_data <- read_excel("Zonal_Stats_Table_Final2.xlsx")

# Set correct order for distance bands (matching YOUR exact values)
distance_data$`Distance (km)` <- factor(distance_data$`Distance (km)`, 
                                        levels = c("0-10", "10-25", "25-50", 
                                                   "50-100", "100-360", "360+"))

# Create bar chart with single blue color
ggplot(distance_data, aes(x = `Distance (km)`, 
                          y = `Mean Population Density (persons/km²)`)) +
  geom_bar(stat = "identity", width = 0.7, fill = "steelblue") +
  labs(
    title = "Population Density by Distance to Jubba and Shabelle Rivers (2020)",
    x = "Distance from Rivers (km)",
    y = "Mean Population Density (persons/km²)"
    )+
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggsave("Figure1_Pop_Density_Distance.png", width = 8, height = 6, dpi = 300)

somalia_data <- read.csv("Somalia_OLS_Data.csv")
names(somalia_data)

# Check the data first
summary(somalia_data$X_mean)
summary(somalia_data$dist_mean)

# Check for any NA values
sum(is.na(somalia_data$X_mean))
sum(is.na(somalia_data$dist_mean))
# How many districts have zero population?


# Run OLS with LOG transformation (because of Mogadishu outlier)
# Distance in km instead of meters (divide by 1000)
somalia_data_ols <-somalia_data
somalia_data_ols$dist_km <- somalia_data_ols$dist_mean / 1000

# Log population density (add small value to avoid log(0))
somalia_data_ols$log_pop <- log(somalia_data_ols$X_mean + 0.01)

# RUN OLS REGRESSION
ols_model <- lm(log_pop ~ dist_km, data = somalia_data_ols)

# View results
summary(ols_model)

#ROUGHWORK
somalia_data_final <- read.csv("Somalia_Admin2_FINAL_OLS_R.csv")
names(somalia_data_final)
somalia <- read.csv("Somalia_Admin2_FINAL_OLS_R.csv")

# Distance in km
somalia_data_rough <- somalia_data_final
somalia_data_rough$dist_km <- somalia_data_rough$dist_mean / 1000

# Log population density (X_mean is mean pop density per district)
somalia_data_rough$log_pop <- log(somalia_data_rough$X_mean + 0.01)  # avoid log(0)

# Precipitation as factor (categories 0–5)
somalia_data_rough$precip_cat <- factor(
  somalia_data_rough$GRID_CODE,
  levels = 0:5,
  labels = c("very_low", "low", "low_med", "med", "med_high", "high")
)

# Conflict – log transform to reduce skew
somalia_data_rough$log_conflict <- log(somalia_data_rough$conflict_count + 1)
names(somalia_data_rough)
ols_full_rough <- lm(
  log_pop ~ dist_km + precip_cat + climate_mean + log_conflict,
  data = somalia_data_rough
)

summary(ols_full_rough)

library(stargazer)

stargazer(ols_model, ols_full_rough,
          type = "text",
          title = "OLS Regression Results",
          dep.var.labels = "Log Population Density",
          covariate.labels = c("Distance to River (km)",
                               "Precipitation: Low-Med",
                               "Precipitation: Med",
                               "Precipitation: Med-High",
                               "Precipitation: High",
                               "Climate Index",
                               "Log Conflict Count"),
          omit.stat = c("f", "ser"))
stargazer(ols_model, ols_full_rough,
          type = "text",
          title = "Determinants of Population Density in Somalia",
          dep.var.labels = "Log Population Density",
          column.labels = c("Distance Only", "Full Model"),
          covariate.labels = c("Distance to River (km)",
                               "Precipitation: Low-Med",
                               "Precipitation: Med",
                               "Precipitation: Med-High",
                               "Precipitation: High",
                               "Climate Index",
                               "Log Conflict Count"),
          digits = 3,
          no.space = TRUE,
          omit.stat = c("f", "ser"),
          notes = "Standard errors in parentheses.")
# Get residuals from the full model
somalia_data_rough$ols_residuals <- residuals(ols_full_rough)

# Check the residuals
summary(somalia_data_rough$ols_residuals)


# Save the data with residuals to CSV for QGIS
write.csv(somalia_data_rough, "Somalia_OLS_with_Residuals.csv", row.names = FALSE)

#Running Moran's I test

# Install and load spatial packages
library(spdep)

# Load the shapefile with residuals
library(sf)

somalia_sf <- st_read("C:/Users/abdir/Documents/GIS AND SPATIAL DATA ANALYSIS/Final Project/Somalia_Admin2_with_Climate_corrected.gpkg")

# Join the residuals to the shapefile
somalia_sf <- merge(somalia_sf, somalia_data_rough[, c("NAME_2", "ols_residuals")], by = "NAME_2")

# Create spatial weights (Queen contiguity - neighbors share border or corner)
neighbors <- poly2nb(somalia_sf, queen = TRUE)
weights <- nb2listw(neighbors, style = "W", zero.policy = TRUE)

# Run Global Moran's I test on residuals
moran_test <- moran.test(somalia_sf$ols_residuals, weights, zero.policy = TRUE)

# View results
print(moran_test)

# Local Moran's I (LISA)

# Run Local Moran's I (LISA)
lisa <- localmoran(somalia_sf$ols_residuals, weights, zero.policy = TRUE)

# View the results
head(lisa)

# Add LISA results to shapefile
somalia_sf$lisa_i <- lisa[,1]
somalia_sf$lisa_p <- lisa[,5]

# Create cluster categories
somalia_sf$lisa_cluster <- NA

# Get mean of residuals
mean_resid <- mean(somalia_sf$ols_residuals)
print(mean_resid)

# Get lagged residuals (neighbors' average)
somalia_sf$lag_resid <- lag.listw(weights, somalia_sf$ols_residuals, zero.policy = TRUE)

# Classify into 4 categories (only where p < 0.05)
somalia_sf$lisa_cluster[somalia_sf$ols_residuals > mean_resid & somalia_sf$lag_resid > mean_resid & somalia_sf$lisa_p < 0.05] <- "High-High"
somalia_sf$lisa_cluster[somalia_sf$ols_residuals < mean_resid & somalia_sf$lag_resid < mean_resid & somalia_sf$lisa_p < 0.05] <- "Low-Low"
somalia_sf$lisa_cluster[somalia_sf$ols_residuals > mean_resid & somalia_sf$lag_resid < mean_resid & somalia_sf$lisa_p < 0.05] <- "High-Low"
somalia_sf$lisa_cluster[somalia_sf$ols_residuals < mean_resid & somalia_sf$lag_resid > mean_resid & somalia_sf$lisa_p < 0.05] <- "Low-High"
somalia_sf$lisa_cluster[somalia_sf$lisa_p >= 0.05] <- "Not Significant"

# See how many in each category
table(somalia_sf$lisa_cluster)

# Save for QGIS mapping
st_write(somalia_sf, "Somalia_LISA_Results.gpkg", delete_dsn = TRUE)


#beautiful regression table with labels
stargazer(ols_model, ols_full_rough,
          type = "text",
          title = "OLS Regression Results - Determinants of Population Density in Somalia (2020)",
          dep.var.labels = "Log Population Density (persons/km²)",
          column.labels = c("Model 1: Baseline", "Model 2: Full Model"),
          covariate.labels = c("Distance to Nearest River (km)",
                               "Precipitation: Low-Medium (ref: Very Low)",
                               "Precipitation: Medium",
                               "Precipitation: Medium-High",
                               "Precipitation: High",
                               "Köppen Climate Index (4-6)",
                               "Log Conflict Events (2020-2024)"),
          digits = 3,
          no.space = TRUE,
          omit.stat = c("f", "ser"),
          add.lines = list(c("Control Variables", "No", "Yes")),
          notes = c("Standard errors in parentheses.",
                    "Significance levels: *p<0.1; **p<0.05; ***p<0.01",
                    "Precipitation baseline category: Very Low (GRID_CODE 0-1)"))

#Geographically weighted regression
# Install and load packages
library(spgwr)
library(sf)

# Load your shapefile with all the data
somalia_sf_gwr <- st_read("C:/Users/abdir/Documents/GIS AND SPATIAL DATA ANALYSIS/Final Project/Somalia_LISA_Results.gpkg")

# Check what columns you have
names(somalia_sf_gwr)

# Create the variables we need
somalia_sf_gwr$dist_km <- somalia_sf_gwr$dist_mean / 1000
somalia_sf_gwr$log_pop <- log(somalia_sf_gwr$X_mean + 0.01)

# For conflict, we need to get it from your original data
# If you have somalia_data_rough loaded, merge it:
somalia_sf_gwr <- merge(somalia_sf_gwr, somalia_data_rough[, c("NAME_2", "log_conflict")], by = "NAME_2")

# Check it worked
names(somalia_sf_gwr)

# Convert to Spatial object (required for spgwr)
somalia_sp <- as(somalia_sf_gwr, "Spatial")

# Find optimal bandwidth
bw <- gwr.sel(log_pop ~ dist_km + climate_mean + log_conflict, 
              data = somalia_sp, 
              adapt = TRUE)

print(bw)

# Run GWR
gwr_model <- gwr(log_pop ~ dist_km + climate_mean + log_conflict, 
                 data = somalia_sp, 
                 adapt = bw,
                 hatmatrix = TRUE,
                 se.fit = TRUE)

# View results
print(gwr_model)

# Extract results
gwr_results <- as.data.frame(gwr_model$SDF)

# View column names
names(gwr_results)

# Calculate t-values for distance coefficient
gwr_results$dist_t <- gwr_results$dist_km / gwr_results$dist_km_se

# Summary of t-values
summary(gwr_results$dist_t)

# Critical t-value at 90% confidence (~62 df)
critical_t <- 1.67

# Count significant coefficients
gwr_results$dist_sig <- ifelse(abs(gwr_results$dist_t) > critical_t, "Significant", "Not Significant")
table(gwr_results$dist_sig)

# Create coefficient column for significant only
gwr_results$dist_coef_sig <- ifelse(abs(gwr_results$dist_t) > critical_t, gwr_results$dist_km, NA)

# Add to shapefile
somalia_sf_gwr$gwr_dist_coef <- gwr_results$dist_km
somalia_sf_gwr$gwr_dist_t <- gwr_results$dist_t
somalia_sf_gwr$gwr_dist_sig <- gwr_results$dist_sig
somalia_sf_gwr$gwr_localR2 <- gwr_results$localR2

# Save for QGIS
st_write(somalia_sf_gwr, "Somalia_GWR_Results.gpkg", delete_dsn = TRUE)

