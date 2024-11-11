##Assignment 1 -- SOCS0100

#Set up working directory and clear environment
setwd("~/Desktop")
rm(list = ls())

#Setup and load necessary packages
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse, 
  kableExtra,
  flextable,
  skimr,
  glue,
  ) 

options(scipen=999)

#Importing data in .csv format
data <- read.csv("~/Documents/GitHub/Assignment_1/Vaccine Coverage and Disease Burden - WHO (2017).csv", header = TRUE)

#Inspection of the dataset
skim(data)        #a short summary of key data aspects
str(data)         #show the struture of the dataset
summary(data)     #provide a summary of each variable

#Select variables for information analysis regarding measles and creating a new dataframe
df <- data %>% select(Entity, Year, Measles..MCV..immunization.coverage.among.1.year.olds..WHO.2017., Number.of.confirmed.measles.cases..WHO.2017.) %>%
  rename(coverage =  Measles..MCV..immunization.coverage.among.1.year.olds..WHO.2017., cases = Number.of.confirmed.measles.cases..WHO.2017.,
         country = Entity)

#Show an overview of the dataframe
glimpse(df)

#Creating a new binary variable in the original dataset to show if countries have high infection level in a given year
#Under the condition that high infection level is considered as having over 500 confimed cases
df <- df %>% mutate(high_infection = ifelse(cases > 500, 1, 0))

#Calculating means for numerical columns
df %>%
  select(-country, -Year) %>%  
  map_dbl(mean, na.rm = TRUE)

#Reshaping the dataset and pivoting it to a wide format where each year has its own column
df_1 <- df%>%
  pivot_wider(
    names_from = Year,
    values_from = c(coverage, cases, high_infection)
  )

#Saving the new dataset to an .rds format 
saveRDS(df_1, "df_1.rds")

#Function to summarize, sort, and display top results for a specific metric
#Filtering out regions and group data by countries to calculate averages
generate_top_10_table <- function(df, metric, top_n = 10, caption_text) {
  df %>%
    filter(!country %in% c("World", "Africa", "Western Pacific", "Europe", "South-East Asia", "Eastern Mediterranean")) %>%
    group_by(country) %>%
    summarise(
      avg_coverage = mean(coverage, na.rm = TRUE),
      avg_cases = mean(cases, na.rm = TRUE)
    ) %>%
    arrange(desc(.data[[metric]])) %>%
    head(top_n) %>%
    kable(caption = caption_text) %>%
    kable_styling("striped") %>%
    kable_classic(full_width = FALSE)
}

#Creating a box plot for indicating countries with high and low infection level
ggplot(df, aes(x = factor(high_infection), fill = factor(high_infection))) +
  geom_bar() +
  scale_x_discrete(labels = c("0" = "Low infection", "1" = "High infection")) +
  labs(x = "Infection level", y = "Number of countries", fill = "High infection") 

#Selecting 5 developing countries from the dataframe for analysing their trends in vaccine coverage and confirmed cases
filtered_df_1 <- df %>% filter(country %in% c("Afghanistan", "China", "India", "Nigeria", "Lebanon"))

#Creating the point plot with a trend line for the 5 selected developing countries
create_point_plot_1 <- function(i) {
  filtered_df_1 %>%
    ggplot(aes_string(x = names(filtered_df_1) [2], y = names(filtered_df_1) [i])) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, color = "blue") +
    labs(
      title = glue ("The trend in {names(df) [i]}"),
      y = glue("{names(df) [i]}")
    )
}

plots_list <- map(3:ncol(filtered_df_1), create_point_plot_1)
plots_grid <- gridExtra::grid.arrange(grobs = plots_list, ncol = 2)

#Selecting 5 developed countries from the dataframe for analysing their trends in vaccine coverage and confirmed cases
filtered_df_2 <- df %>% filter(country %in% c("United Kingdom", "Canada", "Japan", "France","Australia"))

#Creating the point plot with a trend line for the 5 selected developing countries
create_point_plot_2 <- function(i) {
  filtered_df_2 %>%
    ggplot(aes_string(x = names(filtered_df_2) [2], y = names(filtered_df_2) [i])) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, color = "red") +
    labs(
      title = glue ("The trend in {names(df) [i]}"),
      y = glue("{names(df) [i]}")
    )
}

plots_list <- map(3:ncol(filtered_df_2), create_point_plot_2)
plots_grid <- gridExtra::grid.arrange(grobs = plots_list, ncol = 2)

