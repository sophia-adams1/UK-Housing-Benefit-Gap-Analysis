# ==============================================================================
# PROJECT: UK Housing Benefit Gap Analysis
# SCRIPT:  01_cleaning.R
# PURPOSE: Clean raw ONS/DWP data for analysis
# ==============================================================================

rm(list = ls())       
gc()                  
options(scipen = 999) 

# ==============================================================================

library(tidyverse) 
library(janitor)   
library(sf)        
library(fixest)    
library(skimr)    
library(here)      
library(readxl)    
# ==============================================================================

raw_rental_data <- read_excel(
  here("data-raw", "priceindexofprivaterentsukmonthlypricestatistics.xlsx"), 
  sheet = "Table 1", 
  skip = 2           
) %>%                
  clean_names()     
# ==============================================================================

required_cols <- c("time_period", "area_name", "index") 

missing_cols <- setdiff(required_cols, colnames(raw_rental_data))
if (length(missing_cols) > 0) {
  stop(paste("Validation failed: missing columns in source file:", 
             paste(missing_cols, collapse = ", ")))
}
# ==============================================================================

message("SUCCESS: Data loaded and validated. Row count: ", nrow(raw_rental_data))

# ==============================================================================

clean_data <- raw_rental_data %>%

  mutate(across(where(is.character), str_trim)) %>%

  mutate(across(where(is.character), ~ na_if(., "[x]"))) %>%
  
  mutate(across(where(is.character), ~ na_if(., "[z]"))) %>%
  
  mutate(index = as.numeric(index)) %>%
  mutate(across(starts_with("rental_price"), as.numeric))

if (!is.numeric(clean_data$index)) {
  stop("Validation failed: 'index' column is not numeric. Check for non-numeric characters.")
}
# ==============================================================================

audit_log <- list(              
  total_rows = nrow(clean_data), 
  nas_in_rental_price = sum(is.na(clean_data$rental_price)), 
  timestamp = Sys.time()         
)
print("--- DATA AUDIT REPORT ---")
print(audit_log)               

# ==============================================================================

market_trends <- clean_data %>%
  group_by(time_period) %>%
  summarize(avg_national_rent = mean(rental_price, na.rm = TRUE))

ggplot(market_trends, aes(x = time_period, y = avg_national_rent)) +
  geom_line(color = "steelblue", size = 1) +
  theme_minimal() +
  labs(title = "National Average Rental Trend", y = "Average Rent (£)", x = "Date")

# ==============================================================================

if (!dir.exists(here("data-processed"))) {
  dir.create(here("data-processed"))
}

write_csv(clean_data, here("data-processed", "clean_rental_data.csv"))

# ==============================================================================

print("Cleaning complete. Data saved to /data-processed.")

