# Title: DMV COVID-19 Analysis
# Author: Alexander Zakrzeski
# Date: September 26, 2024

# Part 1: Setup and Configuration

# Load to import, clean, and wrangle data
library(dplyr)
library(lubridate)
library(purrr)
library(readr)
library(stringr)
library(tidyr)

# Load to produce charts and tables
library(ggplot2)
library(gt)
library(scales)

# Define a function to perform aggregation and get summary statistics 
generate_summary_stats <- function(column) { 
  # Get summary statistics
  processed <- dmv_monthly |> 
    group_by(state) |>
    summarize(minimum = min(!!sym(column)), 
              maximum = max(!!sym(column)), 
              mean = mean(!!sym(column)),
              median = median(!!sym(column))) |>  
    # Modify values in certain columns and sort rows 
    mutate(across(c(median, mean), round)) |>
    arrange(desc(row_number())) 
  
  # Return the dataframe 
  return(processed) 
}

# Define a function to standardize the theme of ggplot outputs
theme_custom <- function() { 
  # Create an empty theme 
  empty <- theme_void() 
  
  # Add the various styling elements to the theme 
  custom <- empty + theme(  
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), 
    text = element_text(family = "Roboto"), 
    plot.title = element_text(margin = margin(0, 0, 15, 0), hjust = 0.5, 
                              size = 18, face = "bold"), 
    panel.grid.major.y = element_line(linetype = 3, linewidth = 0.3, 
                                      color = "#808080"),  
    axis.text.x = element_text(margin = margin(-2.5, 0, 0, 0), size = 16, 
                               color = "#000000"), 
    axis.text.y = element_text(margin = margin(0, 5, 0, 0), size = 16, 
                               color = "#000000"), 
    legend.position = "top", 
    legend.key.size = unit(0.625, "cm"), 
    legend.text = element_text(size = 16), 
    legend.spacing.x = unit(0.25, "cm"),
    legend.margin = margin(0, 0, 12.5, 0)    
    )
  
  # Return the custom theme 
  return(custom)  
}

# Define a function to display metrics in a table
generate_table <- function(value) { 
  # Filter based on the set condition, drop a column, and rename columns
  processed <- dmv_monthly |> 
    filter(state == value) |>
    select(-state) |>
    rename(Month = date, 
           Season = season, 
           Cases = cases, 
           `Cases per 100k People` = cases_per_100k, 
           `Cumulative Cases` = cum_cases, 
           Deaths = deaths, 
           `Deaths per 100k People` = deaths_per_100k, 
           `Cumulative Deaths` = cum_deaths) 
  
  # Create a table to display metrics  
  table <- gt(processed) |> 
    cols_align(align = "center", columns = -c(Month, Season)) |>
    fmt_number(columns = where(is.numeric), decimals = 1, 
               drop_trailing_zeros = TRUE) |>
    tab_options(table.width = "95%", table.font.names = "Roboto", 
                table.font.size = px(18), data_row.padding = px(12.5)) |>
    opt_stylize(style = 6)
  
  # Dynamically set the title of the table 
  if (value == "Washington, D.C.") { 
    table <- table |>
      tab_header(title = md("**Table 1: COVID-19 Metrics in Washington, D.C. 
                               (March 2020 – February 2021)**")) 
  } else if (value == "Maryland") { 
    table <- table |> 
      tab_header(title = md("**Table 2: COVID-19 Metrics in Maryland (March 2020
                               – February 2021)**"))
  } else if (value == "Virginia") {
    table <- table |>
      tab_header(title = md("**Table 3: COVID-19 Metrics in Virginia (March 2020
                               – February 2021)**"))
  }
  
  # Return the table 
  return(table) 
}

# Part 2: Data Preprocessing

# Iteratively load, process, and bind the dataframes
dmv_monthly <- map_dfr(c("DC-COVID-19-Data.csv", "MD-COVID-19-Data.csv", 
                         "VA-COVID-19-Data.csv"), function(file_name) { 
  # Load the data from the CSV file, rename and select columns
  processed <- read_csv(file_name) |> 
    rename_with(str_to_lower) |>
    rename(cum_deaths = death, 
           deaths = deathincrease, 
           cum_cases = positive, 
           cases = positiveincrease) |> 
    select(date, state, cases, cum_cases, deaths, cum_deaths) |> 
    # Modify values in a column, create a new column, filter, and sort rows
    mutate(date = mdy(date),  
           season = case_when(  
             month(date) %in% c(12, 1, 2) ~ "Winter",
             month(date) %in% c(3, 4, 5) ~ "Spring", 
             month(date) %in% c(6, 7, 8) ~ "Summer", 
             month(date) %in% c(9, 10, 11) ~ "Fall"   
             )) |> 
    filter(between(date, ymd("2020-03-01"), ymd("2021-02-28"))) |>
    arrange(date) |> 
    # Modify values in the columns and set factor levels
    mutate(date = format(date, "%b %Y") |> 
                  factor(levels = c("Mar 2020", "Apr 2020", "May 2020", 
                                    "Jun 2020", "Jul 2020", "Aug 2020", 
                                    "Sep 2020", "Oct 2020", "Nov 2020", 
                                    "Dec 2020", "Jan 2021", "Feb 2021")), 
           state = case_when( 
             state == "DC" ~ "Washington, D.C.",
             state == "MD" ~ "Maryland",  
             state == "VA" ~ "Virginia"   
             ) |>     
                   factor(levels = c("Virginia", "Maryland", 
                                     "Washington, D.C.")),  
           across(c(cum_cases, cum_deaths), ~ replace_na(.x, 0))) |> 
    # Generate the aggregated figures and additional new columns 
    group_by(state, date, season) |> 
    summarize(cases = sum(cases), 
              cum_cases = max(cum_cases), 
              deaths = sum(deaths), 
              cum_deaths = max(cum_deaths)) |>
    ungroup() |>
    mutate(cases_per_100k = case_when( 
      state == "Washington, D.C." ~ (cases / 689545) * 100000, 
      state == "Maryland" ~ (cases / 6177224) * 100000,
      state == "Virginia" ~ (cases / 8631393) * 100000 
      ),   
      deaths_per_100k = case_when( 
        state == "Washington, D.C." ~ (deaths / 689545) * 100000, 
        state == "Maryland" ~ (deaths / 6177224) * 100000,
        state == "Virginia" ~ (deaths / 8631393) * 100000 
        ), 
      across(c(cases_per_100k, deaths_per_100k), ~ if_else(  
        .x >= 0.5, round(.x), round(.x, 1)   
        ))) |>  
    # Change the position of the newly created columns 
    relocate(cases_per_100k, .after = cases) |>
    relocate(deaths_per_100k, .after = deaths)
}) 

# Part 3: Data Analysis

# Generate summary statistics
cases_summary_stats <- generate_summary_stats("cases") 

# Create a stacked bar chart to display sums
ggplot(dmv_monthly, aes(x = date, y = cases, fill = state)) +   
  geom_col(width = 0.6) + 
  geom_hline(yintercept = 0, linewidth = 1.5, color = "#000000") + 
  scale_x_discrete(labels = function(x) str_remove(x, "\\s.*")) +
  scale_y_continuous(limits = c(0, 241000), breaks = seq(0, 241000, by = 60000),
                     labels = comma) + 
  scale_fill_manual(values = c("#1e486a", "#4aa6ed", "#89cfec")) + 
  labs(title = str_squish("Figure 1: Monthly COVID-19 Cases in the DMV Region 
                           (March 2020 – February 2021)"), 
       x = "", y = "") +
  guides(fill = guide_legend(title = "", reverse = TRUE)) +
  theme_custom() 

# Create a line graph to display cumulative sums
ggplot(dmv_monthly, aes(x = date, y = cum_cases, group = state, 
                        color = state)) +  
  geom_line(linewidth = 2.15) +
  geom_point(size = 3.15) +
  geom_hline(yintercept = 0, linewidth = 1.5, color = "#000000") + 
  scale_x_discrete(labels = function(x) str_remove(x, "\\s.*")) +
  scale_y_continuous(limits = c(0, 576050), 
                     breaks = seq(0, 576050, by = 140000), labels = comma) + 
  scale_color_manual(values = c("#1e486a", "#4aa6ed", "#89cfec")) + 
  labs(title = str_squish("Figure 2: Cumulative COVID-19 Cases in the DMV Region 
                           (March 2020 – February 2021)"), 
       x = "", y = "") +
  guides(color = guide_legend(title = "", reverse = TRUE)) +
  theme_custom()

# Generate summary statistics
deaths_summary_stats <- generate_summary_stats("deaths")

# Create a stacked bar chart to display sums
ggplot(dmv_monthly, aes(x = date, y = deaths, fill = state)) +   
  geom_col(width = 0.6) + 
  geom_hline(yintercept = 0, linewidth = 1.5, color = "#000000") + 
  scale_x_discrete(labels = function(x) str_remove(x, "\\s.*")) +
  scale_y_continuous(limits = c(0, 2934), breaks = seq(0, 2934, by = 700), 
                     labels = comma) + 
  scale_fill_manual(values = c("#1e486a", "#4aa6ed", "#89cfec")) + 
  labs(title = str_squish("Figure 3: Monthly COVID-19 Deaths in the DMV Region 
                           (March 2020 – February 2021)"), 
       x = "", y = "") +
  guides(fill = guide_legend(title = "", reverse = TRUE)) +
  theme_custom() 

# Create a line graph to display cumulative sums
ggplot(dmv_monthly, aes(x = date, y = cum_deaths, group = state, 
                        color = state)) +  
  geom_line(linewidth = 2.15) +
  geom_point(size = 3.15) +
  geom_hline(yintercept = 0, linewidth = 1.5, color = "#000000") + 
  scale_x_discrete(labels = function(x) str_remove(x, "\\s.*")) +
  scale_y_continuous(limits = c(0, 8552), breaks = seq(0, 8552, by = 2100), 
                     labels = comma) + 
  scale_color_manual(values = c("#1e486a", "#4aa6ed", "#89cfec")) + 
  labs(title = str_squish("Figure 4: Cumulative COVID-19 Deaths in the DMV 
                           Region (March 2020 – February 2021)"), 
       x = "", y = "") + 
  guides(color = guide_legend(title = "", reverse = TRUE)) +
  theme_custom() 

# Output the tables with the metrics
dc_metrics_table <- generate_table("Washington, D.C.") 
md_metrics_table <- generate_table("Maryland") 
va_metrics_table <- generate_table("Virginia") 