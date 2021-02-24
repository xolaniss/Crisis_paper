# Paper Eight - DJA

# Preliminaries -----------------------------------------------------------
options("scipen" = 100, "digits" = "4")
library("tidyverse")
library("readxl")
library("lubridate")
library("rvest")
library("stringr")
library("rebus")
library("rlist")
library("here")
library("xts")
library("quantmod")
library("PNWColors")
library("rlang")

# Functions ---------------------------------------------------------------
fx_plot <- function (data, plotname) {
  ggplot(fortify.zoo(data, melt = TRUE),
         aes(x = Index, y = Value, col = Series)) +
    geom_line() +
    facet_wrap (. ~ Series, scale = "free") +
    theme_bw() +
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    theme(
      text = element_text(size = 7),
      strip.background = element_rect(colour = "white", fill = "white")
    ) +
    labs(
      title = " ",
      subtitle = " ",
      x = "",
      y = plotname
    ) +
    scale_color_manual(values = pnw_palette("Moth", 12))
}
export_csv <- function(list) {
  for (i in names(list)) {
    form =  here("Crisis_DJI", sprintf('%s.csv', i))
    write.csv(list[[i]], file = form, row.names = FALSE)
  }
}
zoo_import <- function(datafile) {
  read.zoo(here(datafile),
           sep = ",",
           header = TRUE,
           format = "%Y-%m-%d")
}
crisis_group <- function(data_iteration, data) {
  two_year <-  years(2)
  start_date <- data_iteration$Start
  end_date <- data_iteration$End
  names <- rlang::sym(paste("Category"))
  result <- list() #empty list
  for (j in 1:nrow(data_iteration)) {
    result[[j]] <-  data %>% mutate(!!names := 
      case_when(
        Date >= start_date[j] & Date <= end_date[j] ~ "crisis",
        Date < start_date[j] &
          Date > start_date[j] - two_year ~ "pre crisis",
        Date > end_date[j] &
          Date < end_date[j] + two_year ~ "post crisis",
        TRUE ~ "no crisis"
      )
    )
  }
  return(result)
}

# Data Import -------------------------------------------------------------
DJI <- zoo_import("DJI.csv")
CRISES <-  read_csv(here("CRISES.csv"))
CRISES <- filter(CRISES, End >= "1885-02-17") # dropping crises before the advent of DJI

# Returns -----------------------------------------------------------------
DJI <- as.xts(DJI) #xts conversion
DJI_plot <- fx_plot(DJI, "Dow Jones Average Index") # Graphing index
DJI_returns <- ROC(DJI)  # returns calculation
DJI_index_returns <- cbind(DJI, DJI_returns)
DJI_index_returns <- na.omit(DJI_index_returns) # removing NAs
DJI_returns_plot <-
  fx_plot(DJI_index_returns, " ") # Graphing returns
DJI_returns <-
  tibble(Date = index(DJI_returns),
         DJI_returns = coredata(DJI_returns)[, 1]) # Converting back to tibble
DJI_returns <- na.omit(DJI_returns)  # removing NAs

# CRISES --------------------------------------------------------
DJI_crisis <- crisis_group(CRISES, DJI_returns) # implementing grouping function
crisis_names <- str_replace_all(CRISES$Crisis, " ", "_") # removing spaces from crisis names
names(DJI_crisis) <- crisis_names # naming lists

# Exporting ---------------------------------------------------------------
export_csv(DJI_crisis) # exporting lists
