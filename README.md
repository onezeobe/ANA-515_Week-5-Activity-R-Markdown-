---
title: "Assignment 2"
author: "NK Ezeobele"
date: "2024-06-16"
output: word_document
---

The dataset used is the "Global Power Plant Database" from the World Resources Institute. This dataset contains information about power plants around the world, including their location, type, capacity, and more. It is collected from various sources, including government reports, company disclosures, and other databases. I will be using this dataset to answer questions related to energy production, distribution, and the impact of power plants on the environment.

This dataset is saved in a CSV (Comma-Separated Values) file, which is a type of flat file. The data is delimited by commas, and CSV files can be opened using spreadsheet programs like Microsoft Excel or data analysis software such as R.

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

```


```{r section2, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
# Set a CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Install and load necessary packages
if (!require(dplyr)) install.packages("dplyr")
if (!require(readr)) install.packages("readr")
library(dplyr)
library(readr)

# Read the CSV file into a dataframe
power_plant_data <- read_csv("/Users/odinakaezeobele/Desktop/global_power_plant_database_v_1_3/global_power_plant_database.csv")

# Display the first few rows of the dataframe
head(power_plant_data)

```


```{r section3, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
# Load necessary package
library(dplyr)

# Rename columns for clarity (example)
cleaned_data <- power_plant_data %>%
  rename(
    Plant_ID = gppd_idnr,
    Plant_Name = name,
    Plant_Capacity = capacity_mw,
    Primary_Fuel = primary_fuel
  )

# Convert date columns to Date format if any (example)
# cleaned_data$operation_date <- as.Date(cleaned_data$operation_date, format = "%Y-%m-%d")

# Display the cleaned data
head(cleaned_data)

```



```{r data-characteristics}
# Number of rows and columns
num_rows <- nrow(cleaned_data)
num_columns <- ncol(cleaned_data)

# Inline R code to display in Markdown
cat("This dataframe has", num_rows, "rows and", num_columns, "columns.")


# Create a table with column names and descriptions
library(knitr)

# Specific descriptions for some columns
specific_descriptions <- c(
  "The ISO 3166-1 alpha-3 country code",
  "The full name of the country",
  "The name of the power plant",
  "The Global Power Plant Database identifier number",
  "The installed capacity of the power plant in megawatts (MW)",
  "The latitude coordinate of the power plant's location",
  "The longitude coordinate of the power plant's location",
  "The primary fuel used by the power plant",
  "The secondary fuel used by the power plant (if any)",
  "The tertiary fuel used by the power plant (if any)",
  "The quaternary fuel used by the power plant (if any)",
  "The year the power plant was commissioned",
  "The owner of the power plant",
  "The source of the power plant data",
  "The URL to the source of the power plant data",
  "The source of the geolocation data",
  "The World Electric Power Plants database identifier",
  "The year of the capacity data",
  "The power generation in GWh for the year 2013",
  "The power generation in GWh for the year 2014",
  "The power generation in GWh for the year 2015",
  "The power generation in GWh for the year 2016",
  "The power generation in GWh for the year 2017",
  "The power generation in GWh for the year 2018",
  "The power generation in GWh for the year 2019",
  "The source of the generation data",
  "The estimated power generation in GWh for the year 2013",
  "The estimated power generation in GWh for the year 2014",
  "The estimated power generation in GWh for the year 2015",
  "The estimated power generation in GWh for the year 2016",
  "The estimated power generation in GWh for the year 2017",
  "Notes on the estimated generation data for 2013",
  "Notes on the estimated generation data for 2014",
  "Notes on the estimated generation data for 2015",
  "Notes on the estimated generation data for 2016",
  "Notes on the estimated generation data for 2017"
)

# Generate descriptions for remaining columns
if (length(specific_descriptions) < num_columns) {
  descriptions <- c(
    specific_descriptions,
    rep("Additional column with relevant information", num_columns - length(specific_descriptions))
  )
} else {
  descriptions <- specific_descriptions
}

# Create a dataframe for the column information
column_info <- data.frame(
  Column_Name = colnames(cleaned_data),
  Description = descriptions
)

kable(column_info, col.names = c("Column Name", "Description"))

```


```{r summary-statistics}
knitr::opts_chunk$set(echo = TRUE)
# Select three columns for summary statistics
selected_columns <- cleaned_data %>%
  select(Plant_Capacity, Primary_Fuel, Plant_ID)  # Example columns

# Summary statistics
summary_stats <- selected_columns %>%
  summarise(
    Min_Capacity = min(Plant_Capacity, na.rm = TRUE),
    Max_Capacity = max(Plant_Capacity, na.rm = TRUE),
    Mean_Capacity = mean(Plant_Capacity, na.rm = TRUE),
    Missing_Capacity = sum(is.na(Plant_Capacity)),
    Unique_Fuels = n_distinct(Primary_Fuel),
    Missing_Fuels = sum(is.na(Primary_Fuel)),
    Total_Plants = n_distinct(Plant_ID)
  )

# Display the summary statistics
summary_stats
```


```{r visualization}
install.packages(c("ggplot2", "sf", "rnaturalearth", "rnaturalearthdata", "ggspatial", "ggplot2"))

# Load required libraries
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)

# Load the dataset
power_plant_data <- read.csv("/Users/odinakaezeobele/Desktop/global_power_plant_database_v_1_3/global_power_plant_database.csv")

# Convert the data to an sf object
power_plant_data_sf <- st_as_sf(power_plant_data, coords = c("longitude", "latitude"), crs = 4326)

# Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

```

```{r location}
# Plot the world map with power plant locations
ggplot(data = world) +
  geom_sf(fill = "lightgray") +
  geom_sf(data = power_plant_data_sf, aes(color = primary_fuel), size = 1, alpha = 0.7) +
  labs(title = "Global Distribution of Power Plants",
       x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(legend.position = "bottom")
```


```{r heatmap}
# Plot heat map for density visualization
ggplot() +
  geom_bin2d(data = st_coordinates(power_plant_data_sf), aes(X, Y), bins = 100) +
  geom_sf(data = world, fill = NA, color = "black") +
  scale_fill_gradient(low = "yellow", high = "red", trans = "log") +
  labs(title = "Heat Map of Global Power Plant Density (plants/sq.km)",
       x = "Longitude", y = "Latitude", fill = "Density") +
  theme_minimal()

```



























  


