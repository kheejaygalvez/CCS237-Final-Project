# ==============================================================================
# PROJECT: Nonlinear Structural Changes in S&P 500: GLM vs GAM Comparison
# FILE: Stock_Prediction_Analysis.R
# AUTHORS: KHEE JAY GALVEZ, MARIANE FAITH TORREVERDE, GILLIE CALANUGA
# INSTITUTION: West Visayas State University
# DATE: December 7, 2025
# ==============================================================================

#### PHASE 0: EXPLORATORY DATA ANALYSIS PLOTS FOR PAPER ####
cat("=== GENERATING PLOTS FOR PAPER ===\n")

# Create directory for paper plots
dir.create("paper_plots", showWarnings = FALSE)

if (!exists("glm_predictions") || !exists("gam_predictions")) {
  cat("Recalculating predictions...\n")
  
  # Recalculate GLM predictions
  glm_model <- glm(
    Close ~ Lag_Close_1 + Lag_Return_1 + Month + Crisis_2008 + Crisis_COVID,
    data = Data_Train,
    family = gaussian()
  )
  glm_predictions <- predict(glm_model, newdata = Data_Test)
  
  # Recalculate GAM predictions
  gam_model <- gam(
    Close ~ s(Lag_Close_1, k = 10) + 
      s(Lag_Return_1, k = 8) +
      Month + Crisis_2008 + Crisis_COVID,
    data = Data_Train,
    family = gaussian(),
    method = "REML"
  )
  gam_predictions <- predict(gam_model, newdata = Data_Test)
}

# Calculate performance metrics for reference
glm_rmse <- sqrt(mean((Data_Test$Close - glm_predictions)^2))
gam_rmse <- sqrt(mean((Data_Test$Close - gam_predictions)^2))

ss_res_glm <- sum((Data_Test$Close - glm_predictions)^2)
ss_tot <- sum((Data_Test$Close - mean(Data_Test$Close))^2)
glm_r2 <- 1 - (ss_res_glm / ss_tot)

gam_summary <- summary(gam_model)
gam_r2 <- gam_summary$r.sq

cat("\nModel Performance Metrics:\n")
cat(sprintf("GLM - RMSE: %.2f, R²: %.4f\n", glm_rmse, glm_r2))
cat(sprintf("GAM - RMSE: %.2f, R²: %.4f\n", gam_rmse, gam_r2))

#### PLOT 1: TIME SERIES WITH CRISIS PERIODS ####
cat("\nGenerating Plot 1: Time Series with Crisis Periods...\n")

# Define crisis periods for shading
crisis_periods <- data.frame(
  name = c("2008 Financial Crisis", "COVID-19 Pandemic"),
  start = as.Date(c("2007-12-01", "2020-01-01")),
  end = as.Date(c("2009-06-30", "2020-12-31")),
  color = c("#FF6B6B", "#4ECDC4")
)

p1 <- ggplot(Data, aes(x = Date, y = Close)) +
  geom_line(color = "#2c3e50", size = 0.8) +
  # Shade crisis periods
  geom_rect(data = crisis_periods,
            aes(xmin = start, xmax = end, 
                ymin = min(Data$Close, na.rm = TRUE), 
                ymax = max(Data$Close, na.rm = TRUE),
                fill = name),
            alpha = 0.2, inherit.aes = FALSE) +
  # Add crisis labels
  geom_text(data = data.frame(
    x = as.Date(c("2008-06-15", "2020-06-15")),
    y = c(2000, 3500),
    label = c("2008 Financial\nCrisis", "COVID-19\nPandemic")
  ), aes(x = x, y = y, label = label),
  size = 3.5, color = "black", fontface = "bold") +
  scale_fill_manual(values = c("#FF6B6B", "#4ECDC4")) +
  labs(
    title = "Figure 1: S&P 500 Closing Price (2000-2025) with Crisis Periods",
    x = "Year",
    y = "Closing Price (USD)",
    caption = "Source: S&P 500 Monthly Data"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "none",
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

ggsave("paper_plots/Fig1_TimeSeries_CrisisPeriods.png", p1, 
       width = 10, height = 6, dpi = 300)
cat("Saved: paper_plots/Fig1_TimeSeries_CrisisPeriods.png\n")

#### PLOT 2: MONTHLY SEASONAL PATTERNS ####
cat("\nGenerating Plot 2: Monthly Seasonal Patterns...\n")

# Calculate monthly statistics
monthly_data <- Data %>%
  group_by(Month) %>%
  summarise(
    Mean_Return = mean(Return, na.rm = TRUE) * 100,
    SD_Return = sd(Return, na.rm = TRUE) * 100,
    n = n(),
    SE_Return = SD_Return / sqrt(n)
  ) %>%
  mutate(
    Lower_CI = Mean_Return - 1.96 * SE_Return,
    Upper_CI = Mean_Return + 1.96 * SE_Return
  )

p2 <- ggplot(monthly_data, aes(x = Month, y = Mean_Return)) +
  geom_bar(stat = "identity", fill = "#3498db", alpha = 0.8) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), 
                width = 0.2, color = "#2c3e50", size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_text(aes(label = sprintf("%.2f%%", Mean_Return)), 
            vjust = -1, size = 3, fontface = "bold") +
  labs(
    title = "Figure 2: Monthly Return Patterns in S&P 500",
    subtitle = "Average monthly returns with 95% confidence intervals",
    x = "Month",
    y = "Average Monthly Return (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("paper_plots/Fig2_MonthlySeasonalPatterns.png", p2, 
       width = 10, height = 6, dpi = 300)
cat("Saved: paper_plots/Fig2_MonthlySeasonalPatterns.png\n")

#### PLOT 3: MODEL COMPARISON - ACTUAL VS PREDICTED ####
cat("\nGenerating Plot 3: Model Comparison - Actual vs Predicted...\n")

# Combine predictions
comparison_data <- data.frame(
  Date = Data_Test$Date,
  Actual = Data_Test$Close,
  GLM = glm_predictions,
  GAM = gam_predictions
)

# Melt for plotting
melted_data <- comparison_data %>%
  pivot_longer(cols = c(GLM, GAM), 
               names_to = "Model", 
               values_to = "Predicted")

# Create facet plot
p3 <- ggplot() +
  # GLM facet
  geom_point(data = subset(melted_data, Model == "GLM"),
             aes(x = Actual, y = Predicted), 
             color = "#e74c3c", alpha = 0.6, size = 2) +
  geom_abline(slope = 1, intercept = 0, color = "black", 
              linetype = "dashed", size = 0.8) +
  facet_wrap(~ Model, ncol = 2) +
  labs(
    title = "Figure 3: Model Performance Comparison",
    subtitle = "Actual vs Predicted Values on Test Set",
    x = "Actual Closing Price",
    y = "Predicted Closing Price"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    strip.text = element_text(face = "bold", size = 12)
  )

# Add GAM points separately
p3 <- p3 + geom_point(data = subset(melted_data, Model == "GAM"),
                      aes(x = Actual, y = Predicted), 
                      color = "#27ae60", alpha = 0.6, size = 2)

ggsave("paper_plots/Fig3_ModelComparison.png", p3, 
       width = 12, height = 6, dpi = 300)
cat("Saved: paper_plots/Fig3_ModelComparison.png\n")

#### PLOT 4: RESIDUAL DISTRIBUTION COMPARISON ####
cat("\nGenerating Plot 4: Residual Distribution Comparison...\n")

# Calculate residuals
residual_data <- data.frame(
  GLM_Residuals = Data_Test$Close - glm_predictions,
  GAM_Residuals = Data_Test$Close - gam_predictions
) %>%
  pivot_longer(cols = everything(), 
               names_to = "Model", 
               values_to = "Residual") %>%
  mutate(Model = gsub("_Residuals", "", Model))

# Create density plot
p4a <- ggplot(residual_data, aes(x = Residual, fill = Model)) +
  geom_density(alpha = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  scale_fill_manual(values = c("GLM" = "#e74c3c", "GAM" = "#27ae60")) +
  labs(
    title = "Figure 4a: Residual Distributions",
    x = "Residuals (Actual - Predicted)",
    y = "Density",
    fill = "Model"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "bottom"
  )

# Create Q-Q plot
p4b <- ggplot(residual_data, aes(sample = Residual, color = Model)) +
  stat_qq(size = 1.5, alpha = 0.6) +
  stat_qq_line(color = "black", linetype = "dashed") +
  scale_color_manual(values = c("GLM" = "#e74c3c", "GAM" = "#27ae60")) +
  facet_wrap(~ Model, ncol = 2) +
  labs(
    title = "Figure 4b: Q-Q Plots for Residuals",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "none"
  )

# Save plots
ggsave("paper_plots/Fig4a_ResidualDistribution.png", p4a, 
       width = 10, height = 6, dpi = 300)
ggsave("paper_plots/Fig4b_QQPlot.png", p4b, 
       width = 12, height = 6, dpi = 300)
cat("Saved: paper_plots/Fig4a_ResidualDistribution.png\n")
cat("Saved: paper_plots/Fig4b_QQPlot.png\n")

#### PLOT 5: CRISIS PERIOD PERFORMANCE ####
cat("\nGenerating Plot 5: Crisis Period Performance...\n")

# Function to calculate performance by period
calculate_period_performance <- function(period_name, condition) {
  period_data <- Data[condition, ]
  
  if (nrow(period_data) < 10) return(NULL)
  
  # Predictions
  pred_glm <- predict(glm_model, newdata = period_data)
  pred_gam <- predict(gam_model, newdata = period_data)
  
  data.frame(
    Period = period_name,
    GLM_RMSE = sqrt(mean((period_data$Close - pred_glm)^2)),
    GAM_RMSE = sqrt(mean((period_data$Close - pred_gam)^2))
  )
}

# Calculate for all periods
period_performance <- rbind(
  calculate_period_performance("Pre-2008", 
                               Data$Date < as.Date("2007-12-01")),
  calculate_period_performance("2008 Crisis", 
                               Data$Date >= as.Date("2007-12-01") & 
                                 Data$Date <= as.Date("2009-06-30")),
  calculate_period_performance("Post-2008 to Pre-COVID", 
                               Data$Date > as.Date("2009-06-30") & 
                                 Data$Date < as.Date("2020-01-01")),
  calculate_period_performance("COVID Crisis", 
                               Data$Date >= as.Date("2020-01-01") & 
                                 Data$Date <= as.Date("2020-12-31")),
  calculate_period_performance("Post-COVID", 
                               Data$Date > as.Date("2020-12-31"))
) %>%
  pivot_longer(cols = c(GLM_RMSE, GAM_RMSE), 
               names_to = "Model", 
               values_to = "RMSE") %>%
  mutate(Model = gsub("_RMSE", "", Model))

# Create bar plot
p5 <- ggplot(period_performance, aes(x = Period, y = RMSE, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), 
           alpha = 0.8) +
  geom_text(aes(label = round(RMSE, 1)), 
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3, fontface = "bold") +
  scale_fill_manual(values = c("GLM" = "#e74c3c", "GAM" = "#27ae60")) +
  labs(
    title = "Figure 5: Model Performance Across Market Regimes",
    subtitle = "RMSE comparison across different market conditions",
    x = "Market Period",
    y = "RMSE (Root Mean Squared Error)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

ggsave("paper_plots/Fig5_CrisisPerformance.png", p5, 
       width = 12, height = 7, dpi = 300)
cat("Saved: paper_plots/Fig5_CrisisPerformance.png\n")

#### PLOT 6: GAM SMOOTH TERMS VISUALIZATION ####
cat("\nGenerating Plot 6: GAM Smooth Terms...\n")

# Generate smooth term plots
p6 <- plot(gam_model, pages = 1, residuals = TRUE, rug = TRUE, 
           seWithMean = TRUE, shade = TRUE, shade.col = "lightblue")

# Save as PNG
png("paper_plots/Fig6_GAM_SmoothTerms.png", width = 12, height = 8, 
    units = "in", res = 300)
par(mfrow = c(1, 2))
plot(gam_model, select = 1, se = TRUE, seWithMean = TRUE,
     main = "Smooth: Lag_Close_1", ylab = "Effect on Close")
plot(gam_model, select = 2, se = TRUE, seWithMean = TRUE,
     main = "Smooth: Lag_Return_1", ylab = "Effect on Close")
mtext("Figure 6: GAM Smooth Term Estimates", side = 3, line = -2, outer = TRUE, 
      cex = 1.5, font = 2)
dev.off()
cat("Saved: paper_plots/Fig6_GAM_SmoothTerms.png\n")

#### PLOT 7: VARIABLE IMPORTANCE ####
cat("\nGenerating Plot 7: Variable Importance...\n")

# Prepare GLM coefficients - FIXED VERSION
coef_summary <- summary(glm_model)$coefficients
coef_data <- data.frame(
  Variable = rownames(coef_summary),
  Estimate = coef_summary[, "Estimate"],
  Std_Error = coef_summary[, "Std. Error"],
  t_value = coef_summary[, "t value"],
  p_value = coef_summary[, "Pr(>|t|)"],
  stringsAsFactors = FALSE
)

# Filter out intercept and process data
coef_data <- coef_data[coef_data$Variable != "(Intercept)", ]

# Create significance stars
coef_data$Significance <- ifelse(coef_data$p_value < 0.001, "***",
                                 ifelse(coef_data$p_value < 0.01, "**",
                                        ifelse(coef_data$p_value < 0.05, "*", "")))
coef_data$Color <- ifelse(coef_data$Estimate > 0, "Positive", "Negative")

# Create short names for display
coef_data$Variable_Short <- coef_data$Variable
coef_data$Variable_Short <- gsub("Month", "M:", coef_data$Variable_Short)
coef_data$Variable_Short <- gsub("Crisis_2008", "2008 Crisis", coef_data$Variable_Short)
coef_data$Variable_Short <- gsub("Crisis_COVID", "COVID Crisis", coef_data$Variable_Short)
coef_data$Variable_Short <- gsub("Lag_Close_1", "Lag Close", coef_data$Variable_Short)
coef_data$Variable_Short <- gsub("Lag_Return_1", "Lag Return", coef_data$Variable_Short)

# Order by absolute estimate
coef_data <- coef_data[order(-abs(coef_data$Estimate)), ]

# Filter to top 10 variables for readability
top_vars <- head(coef_data, 10)

p7 <- ggplot(top_vars, aes(x = reorder(Variable_Short, Estimate), 
                           y = Estimate, fill = Color)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = Significance), 
            vjust = ifelse(top_vars$Estimate > 0, -0.5, 1.2),
            size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Positive" = "#27ae60", "Negative" = "#e74c3c")) +
  coord_flip() +
  labs(
    title = "Figure 7: GLM Variable Importance",
    subtitle = "Top 10 predictors by coefficient magnitude",
    x = "Predictor Variable",
    y = "Coefficient Estimate",
    fill = "Effect Direction"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    legend.position = "bottom"
  )

ggsave("paper_plots/Fig7_VariableImportance.png", p7, 
       width = 12, height = 8, dpi = 300)
print(p7)
cat("Saved: paper_plots/Fig7_VariableImportance.png\n")

#### CREATE SUMMARY TABLE ####
cat("\n=== GENERATING SUMMARY TABLES ===\n")

# Performance comparison table - Use dplyr::select to avoid conflict
summary_table <- data.frame(
  Metric = c("RMSE", "R²", "AIC", "BIC"),
  GLM = c(
    sprintf("%.2f", glm_rmse),
    sprintf("%.4f", glm_r2),
    sprintf("%.1f", AIC(glm_model)),
    sprintf("%.1f", BIC(glm_model))
  ),
  GAM = c(
    sprintf("%.2f", gam_rmse),
    sprintf("%.4f", gam_r2),
    sprintf("%.1f", AIC(gam_model)),
    sprintf("%.1f", BIC(gam_model))
  ),
  Difference = c(
    sprintf("%+.2f", gam_rmse - glm_rmse),
    sprintf("%+.4f", gam_r2 - glm_r2),
    sprintf("%+.1f", AIC(gam_model) - AIC(glm_model)),
    sprintf("%+.1f", BIC(gam_model) - BIC(glm_model))
  )
)

# Print table
cat("\nTable 1: Performance Comparison\n")
print(summary_table)

# Save as CSV
write.csv(summary_table, "paper_plots/Table1_PerformanceComparison.csv", 
          row.names = FALSE)
cat("\nSaved: paper_plots/Table1_PerformanceComparison.csv\n")

# Variable significance table - FIXED VERSION (using base R)
var_table <- data.frame(
  Variable = coef_data$Variable,
  Estimate = round(coef_data$Estimate, 4),
  Std_Error = round(coef_data$Std_Error, 4),
  t_value = round(coef_data$t_value, 3),
  p_value = ifelse(coef_data$p_value < 0.001, "<0.001", 
                   sprintf("%.4f", coef_data$p_value)),
  Significance = coef_data$Significance,
  stringsAsFactors = FALSE
)

# Rename columns for better display
colnames(var_table) <- c("Variable", "Estimate", "Std. Error", "t value", "p-value", "Significance")

# Print table
cat("\nTable 2: Variable Significance\n")
print(var_table)

write.csv(var_table, "paper_plots/Table2_VariableSignificance.csv", 
          row.names = FALSE)
cat("\nSaved: paper_plots/Table2_VariableSignificance.csv\n")


cat("\n" %+% strrep("=", 60))
cat("\nALL PAPER PLOTS GENERATED SUCCESSFULLY\n")
cat(strrep("=", 60), "\n")
cat("\nPlots saved in 'paper_plots' folder:\n")
cat("1. Fig1_TimeSeries_CrisisPeriods.png - Overview with crisis periods\n")
cat("2. Fig2_MonthlySeasonalPatterns.png - Monthly return patterns\n")
cat("3. Fig3_ModelComparison.png - Actual vs predicted comparison\n")
cat("4. Fig4a_ResidualDistribution.png - Residual density plots\n")
cat("5. Fig4b_QQPlot.png - Q-Q plots for residuals\n")
cat("6. Fig5_CrisisPerformance.png - RMSE across market regimes\n")
cat("7. Fig6_GAM_SmoothTerms.png - GAM nonlinear effects\n")
cat("8. Fig7_VariableImportance.png - Variable importance plot\n")
cat("9. Fig8_RollingWindow.png - Rolling window performance\n")
cat("10. Table1_PerformanceComparison.csv - Performance metrics\n")
cat("11. Table2_VariableSignificance.csv - Variable significance\n")
#### EXPANDED PHASE: ADDITIONAL ANALYSES FOR OBJECTIVES ####
cat("\n=== ADDITIONAL ANALYSES FOR OBJECTIVES ===\n")

#### ANALYSIS 1: TEST FOR NONLINEAR IMPROVEMENT (Objective 1) ####
cat("\n1. Testing if GAM significantly improves over GLM...\n")

# F-test comparing GLM vs GAM
f_test <- anova(glm_model, gam_model, test = "F")
cat("\nF-test comparing GLM vs GAM:\n")
print(f_test)

if (!is.null(f_test$`Pr(>F)`)) {
  if (f_test$`Pr(>F)`[2] < 0.05) {
    cat("\n✓ GAM significantly improves over GLM (p =", 
        round(f_test$`Pr(>F)`[2], 4), ")\n")
  } else {
    cat("\n✗ GAM does NOT significantly improve over GLM (p =", 
        round(f_test$`Pr(>F)`[2], 4), ")\n")
  }
}

#### ANALYSIS 2: STRUCTURAL BREAK TESTING (Objective 4) ####
cat("\n2. Testing for structural breaks...\n")

# Simple Chow-like test for crises
cat("\nComparing model fits pre/during/post crises:\n")

# Function to calculate R² for subset
calculate_r2_subset <- function(model, data_subset) {
  pred <- predict(model, newdata = data_subset)
  actual <- data_subset$Close
  ss_res <- sum((actual - pred)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  1 - (ss_res / ss_tot)
}

# Calculate R² for different periods
periods_list <- list(
  "Pre-2008" = Data$Date < as.Date("2007-12-01"),
  "2008 Crisis" = Data$Date >= as.Date("2007-12-01") & Data$Date <= as.Date("2009-06-30"),
  "Post-2008" = Data$Date > as.Date("2009-06-30") & Data$Date < as.Date("2020-01-01"),
  "COVID Crisis" = Data$Date >= as.Date("2020-01-01") & Data$Date <= as.Date("2020-12-31"),
  "Post-COVID" = Data$Date > as.Date("2020-12-31")
)

crisis_r2 <- data.frame()
for (period_name in names(periods_list)) {
  period_data <- Data[periods_list[[period_name]], ]
  if (nrow(period_data) > 10) {
    glm_r2_period <- calculate_r2_subset(glm_model, period_data)
    gam_r2_period <- calculate_r2_subset(gam_model, period_data)
    
    crisis_r2 <- rbind(crisis_r2, data.frame(
      Period = period_name,
      n = nrow(period_data),
      GLM_R2 = round(glm_r2_period, 4),
      GAM_R2 = round(gam_r2_period, 4),
      GAM_Improvement = round(gam_r2_period - glm_r2_period, 4)
    ))
  }
}

cat("\nR² by Market Period:\n")
print(crisis_r2)

#### ANALYSIS 3: SEASONAL NONLINEARITY TEST (Objective 3) ####
cat("\n3. Testing for nonlinear seasonal effects...\n")

# Test if month effects are linear or nonlinear
# Create a GAM with smooth month effect (cyclic spline)
if (!require("mgcv")) install.packages("mgcv")

# Add numeric month with cyclic constraint
Data_Train$MonthNum <- as.numeric(Data_Train$Month)
Data_Test$MonthNum <- as.numeric(Data_Test$Month)

# Fit GAM with cyclic smooth for month
gam_seasonal <- gam(
  Close ~ s(Lag_Close_1, k = 10) + 
    s(Lag_Return_1, k = 8) +
    s(MonthNum, bs = "cc", k = 12) +  # Cyclic smooth for month
    Crisis_2008 + Crisis_COVID,
  data = Data_Train,
  family = gaussian(),
  method = "REML",
  knots = list(MonthNum = seq(1, 12, length = 12))
)

cat("\nGAM with cyclic month smooth summary:\n")
print(summary(gam_seasonal)$s.table)

# Compare with factor month model
seasonal_test <- anova(gam_model, gam_seasonal, test = "F")
cat("\nComparison: Factor month vs Cyclic smooth month:\n")
print(seasonal_test)

#### ANALYSIS 4: VOLUME EFFECTS (Objective 6) ####
cat("\n4. Analyzing volume effects...\n")

# Add volume features if not already present
if (!"Log_Volume" %in% colnames(Data)) {
  Data$Log_Volume <- log(Data$Volume + 1)
  Data_Train$Log_Volume <- log(Data_Train$Volume + 1)
  Data_Test$Log_Volume <- log(Data_Test$Volume + 1)
}

# Test GLM with volume
glm_volume <- glm(
  Close ~ Lag_Close_1 + Lag_Return_1 + Month + Crisis_2008 + Crisis_COVID + Log_Volume,
  data = Data_Train,
  family = gaussian()
)

cat("\nGLM with volume - Volume coefficient significance:\n")
vol_summary <- summary(glm_volume)$coefficients["Log_Volume", ]
print(vol_summary)

if (vol_summary[4] < 0.05) {
  cat("✓ Volume significantly affects S&P 500 closing price (p =", 
      round(vol_summary[4], 4), ")\n")
} else {
  cat("✗ Volume does NOT significantly affect S&P 500 closing price (p =", 
      round(vol_summary[4], 4), ")\n")
}

#### ANALYSIS 5: POST-CRISIS STABILIZATION (Objective 4) ####
cat("\n5. Analyzing post-crisis stabilization...\n")

# Check if patterns return to pre-crisis levels
cat("\nComparing volatility across periods:\n")
volatility_comparison <- Data %>%
  mutate(Period = case_when(
    Date < as.Date("2007-12-01") ~ "Pre-2008",
    Date >= as.Date("2007-12-01") & Date <= as.Date("2009-06-30") ~ "2008 Crisis",
    Date > as.Date("2009-06-30") & Date < as.Date("2020-01-01") ~ "Post-2008",
    Date >= as.Date("2020-01-01") & Date <= as.Date("2020-12-31") ~ "COVID Crisis",
    Date > as.Date("2020-12-31") ~ "Post-COVID"
  )) %>%
  group_by(Period) %>%
  summarise(
    n = n(),
    Mean_Return = mean(Return, na.rm = TRUE) * 100,
    Volatility = sd(Return, na.rm = TRUE) * 100,
    .groups = "drop"
  )

print(volatility_comparison)

# Test if post-crisis volatility returns to pre-crisis levels
if (nrow(volatility_comparison) >= 5) {
  pre_2008_vol <- volatility_comparison$Volatility[volatility_comparison$Period == "Pre-2008"]
  post_2008_vol <- volatility_comparison$Volatility[volatility_comparison$Period == "Post-2008"]
  post_covid_vol <- volatility_comparison$Volatility[volatility_comparison$Period == "Post-COVID"]
  
  cat("\nVolatility Comparison:\n")
  cat("Pre-2008 volatility:", round(pre_2008_vol, 2), "%\n")
  cat("Post-2008 volatility:", round(post_2008_vol, 2), "%\n")
  cat("Post-COVID volatility:", round(post_covid_vol, 2), "%\n")
  
  if (abs(post_2008_vol - pre_2008_vol) < 2) {
    cat("✓ Market stabilized after 2008 crisis (similar volatility)\n")
  } else {
    cat("✗ Market did NOT fully stabilize after 2008 crisis\n")
  }
}

#### ADDITIONAL PLOTS FOR PAPER ####
cat("\n=== GENERATING ADDITIONAL PLOTS ===\n")

#### PLOT 8: STRUCTURAL BREAK VISUALIZATION ####
cat("\nGenerating Plot 8: Structural Break Visualization...\n")

# Plot model performance over time (rolling window)
window_size <- 36  # 3-year window
dates <- Data$Date[(window_size+1):nrow(Data)]
rolling_r2 <- data.frame(Date = dates, GLM_R2 = NA, GAM_R2 = NA)

for (i in (window_size+1):nrow(Data)) {
  train_window <- Data[(i-window_size):(i-1), ]
  test_point <- Data[i, ]
  
  # Fit models on window
  glm_window <- glm(Close ~ Lag_Close_1 + Lag_Return_1 + Month + Crisis_2008 + Crisis_COVID,
                    data = train_window, family = gaussian())
  gam_window <- gam(Close ~ s(Lag_Close_1, k = 8) + s(Lag_Return_1, k = 6) + 
                      Month + Crisis_2008 + Crisis_COVID,
                    data = train_window, family = gaussian(), method = "REML")
  
  # Predict next point
  pred_glm <- predict(glm_window, newdata = test_point)
  pred_gam <- predict(gam_window, newdata = test_point)
  
  # Calculate R² for this window (use last 12 points for calculation)
  if (i >= (window_size + 12)) {
    test_window <- Data[(i-11):i, ]
    pred_glm_window <- predict(glm_window, newdata = test_window)
    pred_gam_window <- predict(gam_window, newdata = test_window)
    
    r2_glm <- 1 - (sum((test_window$Close - pred_glm_window)^2) / 
                     sum((test_window$Close - mean(test_window$Close))^2))
    r2_gam <- 1 - (sum((test_window$Close - pred_gam_window)^2) / 
                     sum((test_window$Close - mean(test_window$Close))^2))
    
    rolling_r2$GLM_R2[i-window_size] <- r2_glm
    rolling_r2$GAM_R2[i-window_size] <- r2_gam
  }
}

# Plot rolling R²
p8 <- ggplot(rolling_r2, aes(x = Date)) +
  geom_line(aes(y = GLM_R2, color = "GLM"), size = 1) +
  geom_line(aes(y = GAM_R2, color = "GAM"), size = 1) +
  geom_vline(xintercept = as.numeric(as.Date(c("2007-12-01", "2020-01-01"))), 
             linetype = "dashed", alpha = 0.5) +
  scale_color_manual(values = c("GLM" = "#e74c3c", "GAM" = "#27ae60")) +
  labs(
    title = "Figure 8: Rolling Window Model Performance",
    subtitle = "3-year rolling window R² showing structural changes",
    x = "Date",
    y = "R² (12-month prediction)",
    color = "Model"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("paper_plots/Fig8_RollingPerformance.png", p8, width = 12, height = 6, dpi = 300)
cat("Saved: paper_plots/Fig8_RollingPerformance.png\n")

#### PLOT 9: VOLUME EFFECTS ####
cat("\nGenerating Plot 9: Volume Effects...\n")

if ("Log_Volume" %in% colnames(Data)) {
  p9 <- ggplot(Data, aes(x = Log_Volume, y = Return * 100)) +
    geom_point(alpha = 0.3, color = "#3498db") +
    geom_smooth(method = "lm", color = "#e74c3c", se = TRUE) +
    geom_smooth(method = "gam", formula = y ~ s(x), color = "#27ae60", se = TRUE) +
    labs(
      title = "Figure 9: Trading Volume vs Returns",
      subtitle = "Linear (red) and nonlinear (green) relationships",
      x = "Log(Volume)",
      y = "Return (%)"
    ) +
    theme_minimal()
  
  ggsave("paper_plots/Fig9_VolumeEffects.png", p9, width = 10, height = 6, dpi = 300)
  cat("Saved: paper_plots/Fig9_VolumeEffects.png\n")
}

#### SUMMARY OF ADDITIONAL FINDINGS ####
cat("\n" %+% strrep("=", 60))
cat("\nADDITIONAL FINDINGS SUMMARY\n")
cat(strrep("=", 60))

# Summarize key additional insights
cat("\n\nKEY ADDITIONAL INSIGHTS:\n")

# 1. Nonlinear improvement
if (exists("f_test") && !is.null(f_test$`Pr(>F)`)) {
  p_value <- f_test$`Pr(>F)`[2]
  if (!is.na(p_value)) {
    if (p_value < 0.05) {
      cat("1. ✓ GAM provides statistically significant improvement over GLM (p =", 
          round(p_value, 4), ")\n")
    } else {
      cat("1. ✗ GAM does NOT provide statistically significant improvement over GLM (p =", 
          round(p_value, 4), ")\n")
    }
  }
}

# 2. Volume significance
if (exists("vol_summary")) {
  if (vol_summary[4] < 0.05) {
    cat("2. ✓ Trading volume significantly affects S&P 500 returns\n")
  } else {
    cat("2. ✗ Trading volume does NOT significantly affect S&P 500 returns\n")
  }
}

# 3. Seasonal nonlinearity
if (exists("seasonal_test")) {
  if (!is.null(seasonal_test$`Pr(>F)`)) {
    seasonal_p <- seasonal_test$`Pr(>F)`[2]
    if (!is.na(seasonal_p) && seasonal_p < 0.05) {
      cat("3. ✓ Seasonal effects show significant nonlinear patterns\n")
    }
  }
}

# 4. Structural stability
if (exists("pre_2008_vol") && exists("post_2008_vol")) {
  vol_change <- abs(post_2008_vol - pre_2008_vol)
  if (vol_change < 2) {
    cat("4. ✓ Market volatility stabilized post-2008 crisis\n")
  } else {
    cat("4. ✗ Market volatility remained elevated post-2008 crisis\n")
  }
}

cat("\n" %+% strrep("=", 60))
cat("\nALL ADDITIONAL ANALYSES COMPLETE\n")
cat(strrep("=", 60), "\n")