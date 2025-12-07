# ==============================================================================
# PROJECT: Predicting Stock Price Movements (GLM vs GAM)
# FILE: Finals_Stock_Analysis.R
# ==============================================================================

# --- 1. SETUP & LIBRARIES -----------------------------------------------------
rm(list = ls())
graphics.off()
cat("\014")

# Load libraries
library(readr)
library(dplyr)
library(lubridate)
library(mgcv)
library(ggplot2)
library(gratia)

# --- 2. DATA LOADING & ROBUST PREPROCESSING -----------------------------------

# 1. Load Data
# Ensure the file is named 'StockData.csv' in your folder
file_name <- "StockData.csv" 

if(!file.exists(file_name)) {
  stop("CRITICAL ERROR: 'StockData.csv' not found. Please rename your file.")
}

Data_Raw <- read_csv(file_name, show_col_types = FALSE)

# 2. Fix Column Names (Handles hidden spaces in 'Close ' columns)
colnames(Data_Raw) <- c("Date", "Open", "High", "Low", "Close", "Adj_Close", "Volume")

# 3. Process Data (With Robust Date Parsing)
Data <- Data_Raw %>%
  mutate(
    # TRY MULTIPLE DATE FORMATS (This fixes the data loss issue)
    # It checks YYYY-MM-DD, MM/DD/YYYY, and DD/MM/YYYY automatically
    Date_Parsed = parse_date_time(Date, orders = c("ymd", "mdy", "dmy")),
    Date = as.Date(Date_Parsed)
  ) %>%
  # Remove rows where Date failed to parse
  filter(!is.na(Date)) %>%
  arrange(Date) %>%
  mutate(
    # Feature Engineering
    Month = factor(
      month(Date, label = TRUE),
      levels = month.abb
    ), 
    Year = year(Date),
    Time_Index = as.numeric(Date),
    
    # Lags & Returns
    Lag_Close = lag(Close, 1),
    Lag_Volume = lag(Volume, 1),
    Log_Return = c(NA, diff(log(Close))),
    Lag_Return = lag(Log_Return, 1)
  ) %>%
  na.omit() # Remove only the first 2 rows (due to lags)

# Set factor levels to include all months
Data <- Data %>%
  mutate(
    Month = factor(month(Date, label = TRUE), 
                   levels = month.abb,
                   ordered = FALSE)
  )

# --- SAFETY CHECK -------------------------------------------------------------
# This block stops the code if data is still missing
cat("Rows loaded from CSV:", nrow(Data_Raw), "\n")
cat("Rows remaining after cleaning:", nrow(Data), "\n")

if(nrow(Data) < 50) {
  stop("ERROR: Almost all data was lost! Check your CSV date format.")
}

# --- 3. DATA SPLITTING --------------------------------------------------------
set.seed(2025)
train_size <- floor(0.80 * nrow(Data))
train_ind <- 1:train_size

Data_Train <- Data[train_ind, ]
Data_Test  <- Data[-train_ind, ]

cat("Training Set:", nrow(Data_Train), "observations\n")
cat("Testing Set: ", nrow(Data_Test), "observations\n\n")

# ==============================================================================
# --- 4. MODELING (GLM vs GAM) -------------------------------------------------
# ==============================================================================

# --- MODEL A: GENERALIZED LINEAR MODEL (GLM) ---
GLM_Model <- glm(
  Close ~ Lag_Close + Lag_Volume + Month + Lag_Return,
  family = gaussian(link = "identity"),
  data = Data_Train
)

# --- MODEL B: GENERALIZED ADDITIVE MODEL (GAM) ---
GAM_Model <- gam(
  Close ~ s(Lag_Close, bs = "cr") +      
    s(Lag_Volume, bs = "cr") +     
    s(Time_Index) +                
    Month,                         
  family = gaussian(link = "identity"),
  data = Data_Train,
  method = "REML"
)

# Predict wrapper to handle new factor levels
safe_predict <- function(model, newdata) {
  # Get training factor levels
  if("Month" %in% names(newdata)) {
    train_levels <- levels(model$model$Month)
    # Relevel any new factor levels to the first training level
    newdata$Month <- factor(newdata$Month, levels = train_levels)
    newdata$Month[is.na(newdata$Month)] <- train_levels[1]
  }
  predict(model, newdata = newdata)
}

# ==============================================================================
# --- 5. EVALUATION & VISUALIZATION --------------------------------------------
# ==============================================================================

# Predictions & Metrics
Pred_GLM <- safe_predict(GLM_Model, Data_Test)
Pred_GAM <- safe_predict(GAM_Model, Data_Test)

RMSE_GLM <- sqrt(mean((Data_Test$Close - Pred_GLM)^2))
RMSE_GAM <- sqrt(mean((Data_Test$Close - Pred_GAM)^2))

# Print Results
Comparison <- data.frame(
  Model = c("GLM", "GAM"),
  RMSE = c(RMSE_GLM, RMSE_GAM),
  AIC = c(AIC(GLM_Model), AIC(GAM_Model))
)
print(Comparison)

# Plots
# 1. GAM Smooths
draw(GAM_Model) + ggtitle("Objective (c): Nonlinear Structures")

# 2. Forecast Comparison
Results <- data.frame(Date = Data_Test$Date, Actual = Data_Test$Close, GLM = Pred_GLM, GAM = Pred_GAM)

ggplot(Results, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1) +
  geom_line(aes(y = GLM, color = "GLM"), linetype = "dashed") +
  geom_line(aes(y = GAM, color = "GAM"), linetype = "dotted", size = 1) +
  labs(title = "Forecast: GLM vs GAM", y = "Price", color = "Legend") +
  theme_minimal()