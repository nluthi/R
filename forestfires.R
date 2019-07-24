install.packages(c("dplyr", "tidyr", "ggplot2", "purrr"))
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)

forest_fires <- read.csv("forestfires.csv")

#Count number of fires for each month

forest_fires_month <- forest_fires %>%
  mutate(month = factor(month, levels = c("jan", "feb", "mar", "apr", "may", "jun",
                                          "jul", "aug", "sep", "oct", "nov", "dec"))) %>%
  group_by(month) %>%
  summarize(n())
names(forest_fires_month) <- c("month", "fire_count")

ggplot(data = forest_fires_month) +
  aes(x = month, y = fire_count) +
  geom_bar(stat = "identity", color = "black", fill = "red") +
  labs(title = "Total Fires by Month")

#Count number of fires on each day of the week

forest_fires_day <- forest_fires %>%
  mutate(day = factor(day, levels = c("sun", "mon", "tue", "wed", "thu", "fri", "sat"))) %>%
  group_by(day) %>%
  summarize(n())
names(forest_fires_day) <- c("day", "fire_count")

ggplot(data = forest_fires_day) +
  aes(x = day, y = fire_count) +
  geom_bar(stat = "identity", color = "black", fill = "lightskyblue1") +
  labs(title = "Total Fires by Day of the Week") +
  theme(panel.background = element_rect(fill = "white"))

#Function to generate boxplots for each forest fire factor at the monthly and daily level

forest_fires <- forest_fires %>%
  mutate(month = factor(month, levels = c("jan", "feb", "mar", "apr", "may", "jun",
                                          "jul", "aug", "sep", "oct", "nov", "dec")),
         day = factor(day, levels = c("sun", "mon", "tue", "wed", "thu", "fri", "sat")))

create_boxplot <- function(x, y) {
  ggplot(data = forest_fires) +
    aes_string(x = x, y = y) +
    geom_boxplot() +
    theme(panel.background = element_rect(fill = "white"))
}

x_var_month <- names(forest_fires)[3]
x_var_day <- names(forest_fires)[4]
y_var <- names(forest_fires)[5:12]

box_month <- map2(x_var_month, y_var, create_boxplot)
box_day <- map2(x_var_day, y_var, create_boxplot)

#Function to generate scatterplots for each forest fire factor and fire severity

create_scatterplot <- function(x, y) {
  ggplot(data = forest_fires) +
    aes_string(x = x, y = y) +
    geom_point() +
    theme(panel.background = element_rect(fill = "white"))
}

x_var_scatter <- names(forest_fires)[5:12]
y_var_scatter <- names(forest_fires)[13]

scatters <- map2(x_var_scatter, y_var_scatter, create_scatterplot)

#Create histogram of the fire severity factor (area burned) to understand distribution

ggplot(data = forest_fires) +
  aes(x = area) +
  geom_histogram(color = "black", fill = "pink") +
  theme(panel.background = element_rect(fill = "white"))

#Drill into data relationships for less severe fires

forest_fires_less_severe <- forest_fires %>%
  mutate(month = factor(month, levels = c("jan", "feb", "mar", "apr", "may", "jun",
                                          "jul", "aug", "sep", "oct", "nov", "dec")),
         day = factor(day, levels = c("sun", "mon", "tue", "wed", "thu", "fri", "sat"))) %>%
  filter(area <= 100)

ggplot(data = forest_fires_less_severe) +
  aes(x = area) +
  geom_histogram(color = "black", fill = "pink") +
  theme(panel.background = element_rect(fill = "white"))

forest_fires_less_severe %>%
  group_by(month) %>%
  summarize(n())
  
ggplot(data = forest_fires_less_severe) +
  aes(x = month, y = area) +
  geom_bar(stat = "identity", color = "black", fill = "pink") +
  theme(panel.background = element_rect(fill = "white"))

forest_fires_less_severe %>%
  group_by(day) %>%
  summarize(n())

ggplot(data = forest_fires_less_severe) +
  aes(x = day, y = area) +
  geom_bar(stat = "identity", color = "black", fill = "pink") +
  theme(panel.background = element_rect(fill = "white"))

create_scatterplot_less_severe <- function(x, y) {
  ggplot(data = forest_fires_less_severe) +
    aes_string(x = x, y = y) +
    geom_point() +
    theme(panel.background = element_rect(fill = "white"))
}

x_var_scatter_less_severe <- names(forest_fires_less_severe)[5:12]
y_var_scatter_less_severe <- names(forest_fires_less_severe)[13]

scatters_less_severe <- map2(x_var_scatter_less_severe, y_var_scatter_less_severe, create_scatterplot_less_severe)

#Drill into data relationships for severe fires

forest_fires_severe <- forest_fires %>%
  mutate(month = factor(month, levels = c("jan", "feb", "mar", "apr", "may", "jun",
                                          "jul", "aug", "sep", "oct", "nov", "dec")),
         day = factor(day, levels = c("sun", "mon", "tue", "wed", "thu", "fri", "sat"))) %>%
  filter(area > 100)

ggplot(data = forest_fires_severe) +
  aes(x = area) +
  geom_histogram(color = "black", fill = "pink") +
  theme(panel.background = element_rect(fill = "white"))

forest_fires_severe %>%
  group_by(month) %>%
  summarize(n())

ggplot(data = forest_fires_severe) +
  aes(x = month, y = area) +
  geom_bar(stat = "identity", color = "black", fill = "pink") +
  theme(panel.background = element_rect(fill = "white"))

forest_fires_severe %>%
  group_by(day) %>%
  summarize(n())

ggplot(data = forest_fires_severe) +
  aes(x = day, y = area) +
  geom_bar(stat = "identity", color = "black", fill = "pink") +
  theme(panel.background = element_rect(fill = "white"))

create_scatterplot_severe <- function(x, y) {
  ggplot(data = forest_fires_severe) +
    aes_string(x = x, y = y) +
    geom_point() +
    theme(panel.background = element_rect(fill = "white"))
}

x_var_scatter_severe <- names(forest_fires_severe)[5:12]
y_var_scatter_severe <- names(forest_fires_severe)[13]

scatters_severe <- map2(x_var_scatter_severe, y_var_scatter_severe, create_scatterplot_severe)