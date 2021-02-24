# Paper Eight Data Cleaning

# Preliminaries -----------------------------------------------------------
rm(list = ls())
options("scipen" = 100, "digits" = "4")
library("tidyverse")
library("readxl")
library("lubridate")
library("rvest")
library("stringr")
library("rebus")
library("rlist")

# Functions ---------------------------------------------------------------
excel_import <- as_mapper(~ read_excel(file.path(data_path, .)))
date_rename <- as_mapper(~ rename(.x, Date = colnames(.x[1])))
export_csv<- function(list) {
  for (i in names(list)){
    form = sprintf('%s.csv', i)
    write.csv(list[[i]], sep = ",", file = form, row.names = FALSE)
  }
}

# Data Import -------------------------------------------------------------
data_path <- "./Data"
files <- list.files(data_path, pattern = "*.xlsx")
data <- files %>% map(excel_import)  # import excel into list
names(data) <- c("BRICS","CRISES", "CRSPR", "DJI", "G7", "SP500")

# Data Cleaning -----------------------------------------------------------
data <- data %>%  map(date_rename)  # rename date column
data$SP500 <- rename(data$SP500, SP500 = Close) #rename close column
data$CRSPR <-
  rename(data$CRSPR, CSRP_returns = `CRSP Returns`) #rename close column
names(data$G7) <-
  c(
    "Date",
    "MSCI_JAPAN_price_index",
    "MSCI_USA_price_index",
    "MSCI_UK_price_index",
    "MSCI_FRANCE_price_index",
    "MSCI_CANADA_price_index",
    "MSCI_GERMANY_price_index",
    "MSCI_ITALY_price_index"
  ) #rename columns
names(data$BRICS) <- c(
  "Date",
  "MSCI_BRICS_price_index",
  "MSCI_BRAZIL_price_index",
  "MSCI_CHINA_price_index",
  "MSCI_RUSSIA_price_index",
  "MSCI_INDIA_price_index"
) #rename columns

names(data$CRISES) <- c("Crisis", "Start", "End", "Country") #rename columns

data$DJI$Date <-
  parse_date_time(data$DJI$Date, orders = "m/d/Y") # datetime conversion
data$CRSPR$Date <-
  parse_date_time(data$CRSPR$Date, orders = "Ymd") # datetime conversion

data$CRISES$End <- paste0(data$CRISES$End,"1231")

data$CRISES$Start <-
  parse_date_time(data$CRISES$Start, orders = "Y") # datetime conversion

data$CRISES$End <-
  parse_date_time(data$CRISES$End, orders = "Ymd") # datetime conversion

summary <- data %>% map(summary) # checking for missing values

# Data Export -------------------------------------------------------------
export_csv(data) # writing to csv



