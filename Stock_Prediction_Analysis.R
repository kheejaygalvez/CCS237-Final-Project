# ==============================================================================
# PROJECT: Nonlinear Structural Changes in S&P 500: GLM vs GAM Comparison
# FILE: Stock_Prediction_Analysis.R
# AUTHORS: KHEE JAY GALVEZ, MARIANE FAITH TORREVERDE, GILLIE CALANUGA
# INSTITUTION: West Visayas State University
# DATE: December 7, 2025
# ==============================================================================

#### PHASE 0: DATA LOADING AND PREPROCESSING ####
cat("=== LOADING AND PREPROCESSING S&P 500 DATA ===\n")

library(dplyr)
library(mgcv)
library(ggplot2)
library(lubridate)
library(tidyr)

# Load and clean data
Data <- read.csv("StockData.csv", stringsAsFactors = FALSE)

# Clean numeric columns
clean_numeric <- function(x) {
  as.numeric(gsub("[.,]", "", x))
}

numeric_cols <- c("Open", "High", "Low", "Close", "Adj.Close", "Volume")
Data[numeric_cols] <- lapply(Data[numeric_cols], clean_numeric)

print(head(Data))

# Parse dates and sort
Data$Date <- as.Date(Data$Date, format = "%d-%b-%y")
Data <- Data[order(Data$Date), ]

# Keep data from 2000 onwards
Data <- Data[year(Data$Date) >= 2000, ]

#### FEATURE ENGINEERING ####
cat("\nCreating features for modeling...\n")

# Basic returns
Data$Return <- c(NA, diff(Data$Close) / Data$Close[-nrow(Data)]) * 100

# Lagged returns and prices
Data$Lag_Return_1 <- lag(Data$Return, 1)
Data$Lag_Return_2 <- lag(Data$Return, 2)
Data$Lag_Close_1 <- lag(Data$Close, 1)

# Rolling functions using only past data
roll_mean_past <- function(x, n) {
  result <- rep(NA, length(x))
  for(i in (n+1):length(x)) {
    result[i] <- mean(x[(i-n):(i-1)], na.rm = TRUE)
  }
  return(result)
}

roll_sd_past <- function(x, n) {
  result <- rep(NA, length(x))
  for(i in (n+1):length(x)) {
    result[i] <- sd(x[(i-n):(i-1)], na.rm = TRUE)
  }
  return(result)
}

# Technical indicators
Data$MA_12_Lag <- roll_mean_past(Data$Close, 12)
Data$Volatility_12 <- roll_sd_past(Data$Return, 12)

# Volume
Data$Log_Volume <- log(Data$Volume + 1)

# Month and seasonal
Data$Month <- factor(month(Data$Date), levels = 1:12, labels = month.abb)
Data$MonthNum <- month(Data$Date)

# Crisis indicators
Data$Crisis_2008 <- ifelse(Data$Date >= as.Date("2007-12-01") & 
                             Data$Date <= as.Date("2009-06-30"), 1, 0)
Data$Crisis_COVID <- ifelse(Data$Date >= as.Date("2020-01-01") & 
                              Data$Date <= as.Date("2020-12-31"), 1, 0)

# Time trend
Data$Time_Index <- 1:nrow(Data)

# Clean up NAs
Data_clean <- na.omit(Data)

#### TRAIN-TEST SPLIT ####
cat("\nCreating train-test split (80-20)...\n")

split_idx <- floor(0.8 * nrow(Data_clean))
Data_Train <- Data_clean[1:split_idx, ]
Data_Test <- Data_clean[(split_idx + 1):nrow(Data_clean), ]

cat(sprintf("Training set: %d rows\n", nrow(Data_Train)))
cat(sprintf("Test set: %d rows\n", nrow(Data_Test)))

#### PHASE 1: MODEL BUILDING ####
cat("\n=== PHASE 1: BUILDING MODELS ===\n")

# GLM Model
cat("\nBuilding GLM model...\n")
glm_model <- glm(
  Close ~ Lag_Close_1 + Lag_Return_1 + Month + Crisis_2008 + Crisis_COVID,
  data = Data_Train,
  family = gaussian()
)

# GAM Model
cat("\nBuilding GAM model...\n")
gam_model <- gam(
  Close ~ Lag_Close_1 + 
    s(Lag_Return_1, k = 8) +
    s(Volatility_12, k = 6) +
    Month + Crisis_2008 + Crisis_COVID,
  data = Data_Train,
  family = gaussian(),
  method = "REML"
)

#### PHASE 2: MODEL COMPARISON ####
cat("\n=== PHASE 2: MODEL COMPARISON ===\n")

# Make predictions
glm_predictions <- predict(glm_model, newdata = Data_Test)
gam_predictions <- predict(gam_model, newdata = Data_Test)

# Calculate metrics
calculate_metrics <- function(predictions, actual) {
  rmse <- sqrt(mean((actual - predictions)^2))
  mae <- mean(abs(actual - predictions))
  ss_res <- sum((actual - predictions)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  r2 <- 1 - (ss_res / ss_tot)
  return(list(rmse = rmse, mae = mae, r2 = r2))
}

glm_metrics <- calculate_metrics(glm_predictions, Data_Test$Close)
gam_metrics <- calculate_metrics(gam_predictions, Data_Test$Close)

# Print comparison
cat("\nModel Performance Comparison:\n")
cat(sprintf("GLM - RMSE: %.2f, R²: %.4f, AIC: %.1f\n", 
            glm_metrics$rmse, glm_metrics$r2, AIC(glm_model)))
cat(sprintf("GAM - RMSE: %.2f, R²: %.4f, AIC: %.1f\n", 
            gam_metrics$rmse, gam_metrics$r2, AIC(gam_model)))

# Statistical comparison
cat("\nStatistical Test (F-test):\n")
anova_test <- anova(glm_model, gam_model, test = "F")
print(anova_test)

if (!is.null(anova_test$`Pr(>F)`)) {
  p_val <- anova_test$`Pr(>F)`[2]
  if (!is.na(p_val)) {
    if (p_val < 0.05) {
      cat(sprintf("\n✓ GAM significantly improves over GLM (p = %.4f)\n", p_val))
    } else {
      cat(sprintf("\n✗ GAM does NOT significantly improve over GLM (p = %.4f)\n", p_val))
    }
  }
}

#### PHASE 3: NONLINEAR EFFECTS ANALYSIS ####
cat("\n=== PHASE 3: NONLINEAR EFFECTS ANALYSIS ===\n")

gam_summary <- summary(gam_model)

# Check smooth terms
if (!is.null(gam_summary$s.table)) {
  cat("\nGAM Smooth Terms Analysis:\n")
  smooth_terms <- as.data.frame(gam_summary$s.table)
  colnames(smooth_terms) <- c("EDF", "Ref_DF", "F", "p-value")
  print(smooth_terms)
  
  # Identify nonlinear terms (EDF > 2 suggests nonlinearity)
  nonlinear_vars <- rownames(smooth_terms)[smooth_terms$`p-value` < 0.05 & smooth_terms$EDF > 2]
  if (length(nonlinear_vars) > 0) {
    cat("\nSignificant nonlinear relationships found in:\n")
    for (var in nonlinear_vars) {
      cat("-", var, "\n")
    }
  } else {
    cat("\nNo significant nonlinear effects detected.\n")
  }
}

#### PHASE 4: SEASONAL EFFECTS ANALYSIS ####
cat("\n=== PHASE 4: SEASONAL EFFECTS ANALYSIS ===\n")

# Monthly return analysis
monthly_stats <- Data_clean %>%
  group_by(Month) %>%
  summarise(
    n = n(),
    Mean_Return = mean(Return, na.rm = TRUE),
    SD_Return = sd(Return, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    t_stat = Mean_Return / (SD_Return / sqrt(n)),
    p_value = 2 * pt(-abs(t_stat), df = n - 1)
  )

cat("\nMonthly Return Statistics:\n")
print(monthly_stats)

# Significant months
sig_months <- monthly_stats[monthly_stats$p_value < 0.05, ]
if (nrow(sig_months) > 0) {
  cat("\nMonths with statistically significant returns (p < 0.05):\n")
  print(sig_months[, c("Month", "Mean_Return", "p_value")])
}

#### PHASE 5: CRISIS IMPACT ANALYSIS ####
cat("\n=== PHASE 5: CRISIS IMPACT ANALYSIS ===\n")

# Define periods
periods <- list(
  "Pre-2008" = Data_clean$Date < as.Date("2007-12-01"),
  "2008 Crisis" = Data_clean$Date >= as.Date("2007-12-01") & 
    Data_clean$Date <= as.Date("2009-06-30"),
  "Post-2008" = Data_clean$Date > as.Date("2009-06-30") & 
    Data_clean$Date < as.Date("2020-01-01"),
  "COVID Crisis" = Data_clean$Date >= as.Date("2020-01-01") & 
    Data_clean$Date <= as.Date("2020-12-31"),
  "Post-COVID" = Data_clean$Date > as.Date("2020-12-31")
)

# Analyze each period
crisis_results <- data.frame()

for (period_name in names(periods)) {
  period_data <- Data_clean[periods[[period_name]], ]
  
  if (nrow(period_data) > 10) {
    # Calculate period statistics
    mean_return <- mean(period_data$Return, na.rm = TRUE)
    volatility <- sd(period_data$Return, na.rm = TRUE)
    
    # Model predictions
    pred_glm <- predict(glm_model, newdata = period_data)
    pred_gam <- predict(gam_model, newdata = period_data)
    
    # Calculate RMSE
    rmse_glm <- sqrt(mean((period_data$Close - pred_glm)^2))
    rmse_gam <- sqrt(mean((period_data$Close - pred_gam)^2))
    
    crisis_results <- rbind(crisis_results, data.frame(
      Period = period_name,
      n = nrow(period_data),
      Avg_Return = round(mean_return, 3),
      Volatility = round(volatility, 3),
      GLM_RMSE = round(rmse_glm, 2),
      GAM_RMSE = round(rmse_gam, 2),
      GAM_Improvement = round((rmse_glm - rmse_gam) / rmse_glm * 100, 1)
    ))
  }
}

cat("\nCrisis Period Analysis:\n")
print(crisis_results)

# Check post-crisis stabilization
if ("Post-2008" %in% crisis_results$Period && "Pre-2008" %in% crisis_results$Period) {
  pre_vol <- crisis_results$Volatility[crisis_results$Period == "Pre-2008"]
  post_vol <- crisis_results$Volatility[crisis_results$Period == "Post-2008"]
  
  cat(sprintf("\nVolatility comparison:\nPre-2008: %.3f\nPost-2008: %.3f\n", pre_vol, post_vol))
  
  if (abs(post_vol - pre_vol) < 1.0) {
    cat("✓ Market volatility stabilized after 2008 crisis\n")
  } else {
    cat("✗ Market volatility remained elevated post-2008\n")
  }
}

#### PHASE 6: VARIABLE IMPORTANCE ####
cat("\n=== PHASE 6: VARIABLE IMPORTANCE ===\n")

# GLM coefficients
coef_summary <- summary(glm_model)$coefficients
coef_data <- data.frame(
  Variable = rownames(coef_summary),
  Estimate = coef_summary[, "Estimate"],
  p_value = coef_summary[, "Pr(>|t|)"],
  stringsAsFactors = FALSE
) %>%
  filter(Variable != "(Intercept)") %>%
  mutate(
    Significance = ifelse(p_value < 0.001, "***",
                          ifelse(p_value < 0.01, "**",
                                 ifelse(p_value < 0.05, "*", "")))
  ) %>%
  arrange(desc(abs(Estimate)))

cat("\nTop Variables in GLM:\n")
print(head(coef_data, 10))

# GAM smooth term importance
if (!is.null(gam_summary$s.table)) {
  gam_importance <- as.data.frame(gam_summary$s.table) %>%
    arrange(desc(F))
  
  cat("\nGAM Smooth Term Importance:\n")
  print(gam_importance)
}

#### PHASE 7: GENERATE PAPER PLOTS ####
cat("\n=== PHASE 7: GENERATING PAPER PLOTS ===\n")

# Create directory
dir.create("paper_plots", showWarnings = FALSE)

#### PLOT 1: TIME SERIES WITH CRISIS PERIODS ####
cat("\nGenerating Plot 1: Time Series with Crisis Periods...\n")

crisis_periods <- data.frame(
  name = c("2008 Financial Crisis", "COVID-19 Pandemic"),
  start = as.Date(c("2007-12-01", "2020-01-01")),
  end = as.Date(c("2009-06-30", "2020-12-31"))
)

p1 <- ggplot(Data_clean, aes(x = Date, y = Close)) +
  geom_line(color = "#2c3e50", size = 0.8) +
  geom_rect(data = crisis_periods,
            aes(xmin = start, xmax = end, 
                ymin = min(Data_clean$Close), 
                ymax = max(Data_clean$Close)),
            fill = c("#FF6B6B", "#4ECDC4"), alpha = 0.2, inherit.aes = FALSE) +
  labs(title = "Figure 1: S&P 500 with Crisis Periods",
       x = "Year", y = "Closing Price (USD)") +
  theme_minimal()

ggsave("paper_plots/Fig1_TimeSeries_CrisisPeriods.png", p1, width = 10, height = 6, dpi = 300)

#### PLOT 2: MONTHLY SEASONAL PATTERNS ####
cat("\nGenerating Plot 2: Monthly Seasonal Patterns...\n")

p2 <- ggplot(monthly_stats, aes(x = Month, y = Mean_Return)) +
  geom_bar(stat = "identity", fill = "#3498db") +
  labs(title = "Figure 2: Monthly Return Patterns",
       x = "Month", y = "Average Return (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("paper_plots/Fig2_MonthlySeasonalPatterns.png", p2, width = 10, height = 6, dpi = 300)

#### PLOT 3: MODEL COMPARISON ####
cat("\nGenerating Plot 3: Model Comparison...\n")

comparison_data <- data.frame(
  Actual = Data_Test$Close,
  GLM = glm_predictions,
  GAM = gam_predictions
)

p3_glm <- ggplot(comparison_data, aes(x = Actual, y = GLM)) +
  geom_point(color = "#e74c3c", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "GLM: Actual vs Predicted",
       x = "Actual", y = "Predicted")

p3_gam <- ggplot(comparison_data, aes(x = Actual, y = GAM)) +
  geom_point(color = "#27ae60", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "GAM: Actual vs Predicted",
       x = "Actual", y = "Predicted")

library(gridExtra)
p3 <- grid.arrange(p3_glm, p3_gam, ncol = 2,
                   top = "Figure 3: Model Performance Comparison")
ggsave("paper_plots/Fig3_ModelComparison.png", p3, width = 12, height = 6, dpi = 300)

#### PLOT 4: CRISIS PERFORMANCE ####
cat("\nGenerating Plot 4: Crisis Performance...\n")

p4_data <- crisis_results[, c("Period", "GLM_RMSE", "GAM_RMSE")]
p4_data <- pivot_longer(p4_data, cols = c(GLM_RMSE, GAM_RMSE), 
                        names_to = "Model", values_to = "RMSE")
p4_data$Model <- gsub("_RMSE", "", p4_data$Model)

p4 <- ggplot(p4_data, aes(x = Period, y = RMSE, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("GLM" = "#e74c3c", "GAM" = "#27ae60")) +
  labs(title = "Figure 4: Model Performance Across Market Regimes",
       x = "Period", y = "RMSE") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("paper_plots/Fig4_CrisisPerformance.png", p4, width = 10, height = 6, dpi = 300)

#### PLOT 5: GAM SMOOTH TERMS ####
cat("\nGenerating Plot 5: GAM Smooth Terms...\n")

png("paper_plots/Fig5_GAM_SmoothTerms.png", width = 10, height = 6, units = "in", res = 300)
par(mfrow = c(1, 2))
plot(gam_model, select = 1, se = TRUE, seWithMean = TRUE,
     main = "Smooth: Lag_Return_1", ylab = "Effect")
plot(gam_model, select = 2, se = TRUE, seWithMean = TRUE,
     main = "Smooth: Volatility_12", ylab = "Effect")
mtext("Figure 5: GAM Smooth Term Estimates", side = 3, line = -2, outer = TRUE, 
      cex = 1.2, font = 2)
dev.off()

#### PLOT 6: VARIABLE IMPORTANCE ####
cat("\nGenerating Plot 6: Variable Importance...\n")

# Get top 8 variables
top_vars <- head(coef_data, 8)

p6 <- ggplot(top_vars, aes(x = reorder(Variable, Estimate), y = Estimate)) +
  geom_bar(stat = "identity", fill = "#3498db") +
  coord_flip() +
  labs(title = "Figure 6: GLM Variable Importance",
       x = "Variable", y = "Coefficient Estimate") +
  theme_minimal()

ggsave("paper_plots/Fig6_VariableImportance.png", p6, width = 10, height = 6, dpi = 300)

#### FINAL SUMMARY ####
cat("\n" %+% strrep("=", 60))
cat("\nANALYSIS COMPLETE - SUMMARY\n")
cat(strrep("=", 60))

cat("\n\nKEY FINDINGS:\n")
cat("1. Model Performance:\n")
cat(sprintf("   Best model: %s\n", ifelse(gam_metrics$rmse < glm_metrics$rmse, "GAM", "GLM")))
cat(sprintf("   GAM improvement: %.1f%% (RMSE)\n", 
            (glm_metrics$rmse - gam_metrics$rmse) / glm_metrics$rmse * 100))

cat("\n2. Nonlinear Effects:\n")
if (exists("nonlinear_vars") && length(nonlinear_vars) > 0) {
  cat("   Found in:", paste(nonlinear_vars, collapse = ", "), "\n")
} else {
  cat("   No strong nonlinear effects detected\n")
}

cat("\n3. Seasonal Patterns:\n")
if (nrow(sig_months) > 0) {
  cat("   Significant months:", paste(sig_months$Month, collapse = ", "), "\n")
} else {
  cat("   No statistically significant monthly patterns\n")
}

cat("\n4. Crisis Impacts:\n")
if (nrow(crisis_results) > 0) {
  crisis_improvement <- crisis_results$GAM_Improvement[crisis_results$Period %in% 
                                                         c("2008 Crisis", "COVID Crisis")]
  if (length(crisis_improvement) > 0) {
    cat("   GAM improvement during crises:", 
        paste(round(crisis_improvement, 1), "%", collapse = ", "), "\n")
  }
}

cat("\n5. Important Variables:\n")
if (nrow(coef_data) > 0) {
  cat("   Top predictors:", paste(head(coef_data$Variable, 3), collapse = ", "), "\n")
}

cat("\n6. Statistical Significance:\n")
if (exists("p_val") && !is.na(p_val)) {
  if (p_val < 0.05) {
    cat("   GAM significantly better than GLM (p =", round(p_val, 4), ")\n")
  } else {
    cat("   No significant difference between GAM and GLM (p =", round(p_val, 4), ")\n")
  }
}

cat("\n" %+% strrep("=", 60))
cat("\nAll plots saved in 'paper_plots' folder\n")
cat(strrep("=", 60), "\n")