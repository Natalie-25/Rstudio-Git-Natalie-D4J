
setwd("C:/Users/Administrator/Documents/data")

options(timeout = 600)

install.packages("readxl")
install.packages("readr")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("janitor")
install.packages("sf")
install.packages("viridis")
install.packages("viridisLite")
install.packages("RColorBrewer")
install.packages("stringi")
install.packages("ggspatial")
install.packages("stringr")
install.packages("leaflet")
install.packages("leaflet.providers")
install.packages("leaflet.extras")
install.packages("htmlwidgets")
install.packages("patchwork")


library(readxl)
library(readr)
library (ggplot2)
library (dplyr)
library(tidyr)
library(janitor)
library(sf)
library(viridis)
library(RColorBrewer)
library(stringi)
library(ggspatial)
library(stringr)
library(leaflet)
library(leaflet.providers)
library(leaflet.extras)
library(htmlwidgets)
library(patchwork)

hdi_data <- read_excel("hdi_trends_data.xlsx")

To clean the variables headings in the dataset


hdi_raw <- read_excel("hdi_trends_data.xlsx",
                      skip = 3)

head(hdi_raw)

hdi_clean <- hdi_raw %>%
  clean_names()

glimpse(hdi_clean)

hdi_clean$country <- hdi_clean[[2]]

hdi_clean <- hdi_clean %>%
  filter(!is.na(country))


hdi_clean <- hdi_clean %>%
  mutate(across(starts_with("x"), as.numeric))

glimpse(hdi_clean)


Reshape to long format for modelling and plotting

hdi_long <- hdi_clean %>%
  pivot_longer(
    cols = starts_with("x"),
    names_to = "year",
    values_to = "hdi"
  )

hdi_long <- hdi_long %>%
  mutate(year = as.integer(gsub("x", "", year)))

glimpse(hdi_long)

names(hdi_long)

hdi_frame <- hdi_long %>%
  select(country, year, hdi) %>%
  arrange(country, year)

head(hdi_frame)
glimpse(hdi_frame)


To fix the year whose rows are existing as integers in the dataset

hdi_long_fixed <- hdi_clean %>%
  pivot_longer(
    cols = matches("^x"),
    names_to = "year",
    values_to = "hdi"
  ) %>%
  mutate(
    year = as.integer(gsub("x", "", year)),
    hdi = as.numeric(hdi)
  ) %>%
  filter(!is.na(hdi))

unique(hdi_long_fixed$year)

head(sort(unique(hdi_frame$country)), 30)
tail(sort(unique(hdi_frame$country)), 30)


year_map <- c(
  `1`  = 1990,
  `5`  = 1995,
  `7`  = 2000,
  `9`  = 2005,
  `11` = 2010,
  `13` = 2011,
  `15` = 2012,
  `17` = 2013,
  `19` = 2014,
  `23` = 2018,
  `25` = 2020,
  `27` = 2023
)


hdi_long_fixed <- hdi_long_fixed %>%
  mutate(year = recode(as.character(year), !!!year_map))

sort(unique(hdi_long_fixed$year))

Selecting the African countries that will be used in developing the visualisations

africa_names <- c(
  "Algeria","Angola","Benin","Botswana","Burkina Faso","Burundi",
  "Cameroon","Central African Republic","Chad","Comoros","Congo",
  "Côte d'Ivoire","Democratic Republic of the Congo","Djibouti",
  "Egypt","Equatorial Guinea","Eritrea","Eswatini","Ethiopia",
  "Gabon","Gambia","Ghana","Guinea","Guinea-Bissau","Kenya",
  "Lesotho","Liberia","Libya","Madagascar","Malawi","Mali",
  "Mauritania","Mauritius","Morocco","Mozambique","Namibia",
  "Niger","Nigeria","Rwanda","Senegal","Seychelles","Sierra Leone",
  "Somalia","South Africa","South Sudan","Sudan",
  "Tanzania (United Republic of)","Togo","Tunisia","Uganda",
  "Zambia","Zimbabwe"
)

africa_dta <- hdi_long_fixed %>%
  filter(
    year == 2023,
    country %in% africa_names
  )

glimpse(africa_dta)

n_distinct(africa_dta$country)
sort(africa_dta$country)

the next tidy step is simply to drop any rows where HDI or year is missing

hdi_valid <- hdi_long_fixed %>%
  filter(!is.na(hdi) & !is.na(year))

glimpse(hdi_valid)

sum(is.na(hdi_valid$hdi))
sum(is.na(hdi_valid$year))
n_distinct(hdi_valid$country)

Further targetting of obly Southern African region countries

southern_africa <- c(
  "Angola",
  "Botswana",
  "Eswatini",
  "Lesotho",
  "Malawi",
  "Mozambique",
  "Namibia",
  "South Africa",
  "Zambia",
  "Zimbabwe"
)

southern_dta <- hdi_long_fixed %>%
  filter(country %in% southern_africa)

glimpse(southern_dta)

unique(southern_dta$country)
range(southern_dta$year)

developing the bar graph

ggplot(southern_dta, aes(x = year, y = hdi, group = country)) +
  geom_col(fill = "#003f5c", position = "dodge") +
  labs(
    x = "Year",
    y = "Human Development Index (HDI)",
    title = "HDI Trends Across Southern African Countries (1990–2023)"
  ) +
  theme_minimal()

reordering the y-axis

reorder(southern_dta, hdi_clean)

ggplot(southern_dta, aes(x = hdi, y = reorder(country, hdi))) +
  geom_col(fill = "#003f5c") +
  labs(
    x = "Human Development Index (HDI)",
    y = "Country",
    title = "HDI Levels Across Southern African Countries"
  ) +
  theme_minimal()

To add hdi values alongside the graph for the entire periods 1990=2023

ggplot(southern_dta, aes(x = hdi, y = reorder(country, hdi))) +
  geom_col(fill = "#003f5c") +
  geom_text(
    aes(label = round(hdi, 3)),
    hjust = -0.1,
    size = 3
  ) +
  scale_x_continuous(limits = c(0, 1)) +
  labs(
    x = "Human Development Index (HDI)",
    y = "Country",
    title = "HDI Levels Across Southern African Countries"
  ) +
  theme_minimal()

Because it is not clear with all the years lumped up, we choose to compare 1990 to 2023


southern_dta %>% 
  filter(year == 2023) %>% 
  ggplot(aes(x = hdi, y = reorder(country, hdi))) +
  geom_col(fill = "#003f5c") +
  geom_text(aes(label = round(hdi, 3)), hjust = -0.1)

southern_compare <- southern_dta %>%
  filter(year %in% c(1990, 2023))

ggplot(southern_compare,
       aes(x = reorder(country, hdi), y = hdi, fill = factor(year))) +
  geom_col(position = "dodge") +
  scale_fill_manual(
    values = c("1990" = "#7a5195", "2023" = "#003f5c"),
    name = "Year"
  ) +
  coord_flip() +
  labs(
    x = "Country",
    y = "Human Development Index (HDI)",
    title = "HDI in Southern Africa: 1990 vs 2023"
  ) +
  theme_minimal()


to highight Mozambique which has the highest hdi_2023


southern_compare <- southern_dta %>%
  filter(year %in% c(1990, 2023))

ggplot(southern_compare,
       aes(x = reorder(country, hdi), y = hdi, fill = factor(year))) +
  
  # Base bars for all countries
  geom_col(position = "dodge") +
  
  # Highlight Mozambique bars only
  geom_col(
    data = southern_compare %>% filter(country == "Mozambique"),
    aes(x = reorder(country, hdi), y = hdi),
    fill = "#ffa600",
    position = "dodge"
  ) +
  
  scale_fill_manual(
    values = c("1990" = "#7a5195", "2023" = "#003f5c"),
    name = "Year"
  ) +
  
  coord_flip() +
  labs(
    x = "Country",
    y = "Human Development Index (HDI)",
    title = "HDI in Southern Africa: 1990 vs 2023"
  ) +
  theme_minimal()


To order the bar graphs

southern_compare <- southern_dta %>%
  filter(year %in% c(1990, 2023)) %>%
  group_by(country) %>%
  mutate(
    order_2023 = hdi[year == 2023][1]
  ) %>%
  ungroup()


to add HDI Values

ggplot(
  southern_compare,
  aes(
    x = reorder(country, -order_2023),
    y = hdi,
    fill = factor(year)
  )
) +
  geom_col(position = "dodge", width = 0.85) +
  
  # HDI value labels
  geom_text(
    aes(label = round(hdi, 3)),
    position = position_dodge(width = 0.85),
    hjust = -0.1,
    size = 3.5
  ) +
  
  coord_flip() +
  
  scale_fill_manual(
    values = c("1990" = "#7a5195", "2023" = "#003f5c"),
    name = "Year"
  ) +
  
  labs(
    x = "Country",
    y = "Human Development Index (HDI)",
    title = "HDI in Southern Africa: 1990 vs 2023"
  ) +
  
  theme_minimal()


Country names placed inside the bars

because angola had no value for 2023 the nearest hdi value is used

country_labels <- southern_compare %>%
  group_by(country) %>%
  slice_max(year, with_ties = FALSE) %>%   # takes latest available year
  ungroup()


ggplot(
  southern_compare,
  aes(
    x = reorder(country, -order_2023),
    y = hdi,
    fill = factor(year)
  )
) +
  
  geom_col(position = "dodge", width = 0.85) +
  
  geom_col(
    data = southern_compare %>% filter(country == "Mozambique"),
    aes(x = reorder(country, -order_2023), y = hdi),
    fill = "#ffa600",
    position = "dodge",
    width = 0.85
  ) +
  
  # Country names inside bars
  geom_text(
    data = country_labels,
    aes(
      x = reorder(country, -order_2023),
      y = 0.02,
      label = country
    ),
    hjust = 0,
    color = "white",
    size = 3.5
  ) +
  
  # HDI value labels
  geom_text(
    aes(label = round(hdi, 3)),
    position = position_dodge(width = 0.85),
    hjust = -0.1,
    size = 4.5
  ) +
  
  coord_flip() +
  
  scale_fill_manual(
    values = c("1990" = "#7a5195", "2023" = "#003f5c"),
    name = "Year"
  ) +
  
  labs(
    x = "Country",
    y = "Human Development Index (HDI)",
    title = "HDI in Southern Africa: 1990 vs 2023"
  ) +
  
  theme_minimal()

To change theme and remove grid lines, etc

country_labels <- southern_compare %>%
  group_by(country) %>%
  slice_max(year, with_ties = FALSE) %>%   
  ungroup()


ggplot(
  southern_compare,
  aes(
    x = reorder(country, -order_2023),
    y = hdi,
    fill = factor(year)
  )
) +
  
  geom_col(position = "dodge", width = 0.85) +
  
  geom_col(
    data = southern_compare %>% filter(country == "Mozambique"),
    aes(x = reorder(country, -order_2023), y = hdi),
    fill = "#ffa600",
    position = "dodge",
    width = 0.85
  ) +
  
  # Country names inside bars
  geom_text(
    data = country_labels,
    aes(
      x = reorder(country, -order_2023),
      y = 0.02,
      label = country
    ),
    hjust = 0,
    color = "white",
    size = 3.5
  ) +
  
  # HDI value labels
  geom_text(
    aes(label = round(hdi, 3)),
    position = position_dodge(width = 0.85),
    hjust = -0.1,
    size = 4.5
  ) +
  
  coord_flip() +
  
  scale_fill_manual(
    values = c("1990" = "#7a5195", "2023" = "#003f5c"),
    name = "Year"
  ) +
  
  labs(
    x = "Country",
    y = "Human Development Index (HDI)",
    title = "HDI in Southern Africa: 1990 vs 2023"
  ) +
  # Minimalistic theme but retain axis text
  theme_minimal() +
  theme(
    panel.grid = element_blank(),           
    panel.border = element_blank(),         
    axis.ticks = element_blank(),           
    axis.text.y = element_text(size = 10),  
    axis.title = element_text(size = 12),   
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  ) 

the hdi values for both years are not properly scaled and so this needs to be corrected

southern_compare %>%
  group_by(year) %>%
  summarise(
    min = min(hdi, na.rm = TRUE),
    max = max(hdi, na.rm = TRUE)
  ) 

southern_compare <- southern_compare %>%
  mutate(
    hdi = ifelse(hdi > 1, hdi / 100, hdi)
  )

ggplot(
  southern_compare,
  aes(
    x = reorder(country, -order_2023),
    y = hdi,
    fill = factor(year)
  )
) +
  
  # Base bars
  geom_col(position = "dodge", width = 0.85) +
  
  # Highlight Mozambique
  geom_col(
    data = southern_compare %>% filter(country == "Mozambique"),
    aes(x = reorder(country, -order_2023), y = hdi),
    fill = "#ffa600",
    position = "dodge",
    width = 0.85
  ) +
  
  # Country names inside bars (once per country)
  geom_text(
    data = country_labels,
    aes(
      x = reorder(country, -order_2023),
      y = 0.02,
      label = country
    ),
    hjust = 0,
    color = "white",
    size = 4.5
  ) +
  
  # HDI value labels
  geom_text(
    aes(label = round(hdi, 3)),
    position = position_dodge(width = 0.85),
    hjust = -0.1,
    size = 3.5
  ) +
  
  coord_flip() +
  
  scale_fill_manual(
    values = c("1990" = "#7a5195", "2023" = "#003f5c"),
    name = "Year"
  ) +
  
  labs(
    x = "Country",
    y = "Human Development Index (HDI)",
    title = "HDI in Southern Africa: 1990 vs 2023"
  ) +
  
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "top"
  )


Clean the bar further so that there is clear contrast between years 1990 and 2023 particularly for Mozambique

order_1990 <- southern_compare %>%
  filter(year == 1990) %>%
  arrange(desc(hdi)) %>%   
  pull(country)

southern_compare <- southern_compare %>%
  mutate(country = factor(country, levels = order_1990))

ggplot(
  southern_compare,
  aes(
    x = country,
    y = hdi,
    fill = factor(year)
  )
) +
  
  # Base bars (proper dodging)
  geom_col(
    position = position_dodge(width = 0.85),
    width = 0.85
  ) +
  
  # Highlight Mozambique — ALSO dodged by year
  geom_col(
    data = southern_compare %>% filter(country == "Mozambique"),
    aes(fill = factor(year)),
    position = position_dodge(width = 0.85),
    width = 0.85,
    colour = "black",
    linewidth = 0.3
  ) +
  
  # Country labels inside bars (once per country)
  geom_text(
    data = southern_compare %>% 
      filter(year == 1990),   # ensures only one label per country
    aes(
      y = 0.02,
      label = country
    ),
    hjust = 0,
    color = "white",
    size = 4.5
  ) +
  
  # HDI value labels
  geom_text(
    aes(label = round(hdi, 3)),
    position = position_dodge(width = 0.85),
    hjust = -0.1,
    size = 3.5
  ) +
  
  coord_flip() +
  
  scale_fill_manual(
    values = c("1990" = "#7a5195", "2023" = "#003f5c"),
    name = "Year"
  ) +
  
  labs(
    x = "Country",
    y = "Human Development Index (HDI)",
    title = "HDI in Southern Africa: 1990 vs 2023"
  ) +
  
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "top"
  )

Making the bar chart look nicer and properly scaling the hdi values


order_1990 <- southern_compare %>%
  filter(year == 1990) %>%
  arrange(desc(hdi)) %>%
  pull(country)

southern_compare <- southern_compare %>%
  mutate(country = factor(country, levels = order_1990))


ggplot(
  southern_compare,
  aes(
    x = country,
    y = hdi,
    fill = factor(year)
  )
) +
  
  # Base bars
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.75
  ) +
  
  # Strong highlight for Mozambique
  geom_col(
    data = southern_compare %>% filter(country == "Mozambique"),
    position = position_dodge(width = 0.8),
    width = 0.85,
    colour = "black",
    linewidth = 0.8
  ) +
  
  # Country labels (once)
  geom_text(
    data = southern_compare %>% filter(year == 1990),
    aes(y = 0.02, label = country),
    hjust = 0,
    color = "white",
    size = 4.6,
    fontface = "bold"
  ) +
  
  # HDI value labels
  geom_text(
    aes(label = round(hdi, 3)),
    position = position_dodge(width = 0.8),
    hjust = -0.1,
    size = 3.6
  ) +
  
  coord_flip() +
  
  # HIGH CONTRAST colours
  scale_fill_manual(
    values = c(
      "1990" = "#f28e2b",   
      "2023" = "#1f77b4"    
    ),
    name = "Year"
  ) +
  
  labs(
    x = "Country",
    y = "Human Development Index (HDI)",
    title = "HDI in Southern Africa (Ranked by 1990 Levels)",
    subtitle = "Mozambique highlighted — comparison between 1990 and 2023"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "top",
    plot.title = element_text(face = "bold")
  )


Order by HDI 2023

southern_compare$country <- as.character(southern_compare$country)

# Create clean ranking
order_2023 <- southern_compare %>%
  filter(year == 2023) %>%
  arrange(desc(hdi)) %>%
  pull(country)

# Remove any possible duplicates from order vector
order_2023 <- unique(order_2023)

# Recreate factor safely
southern_compare$country <- factor(southern_compare$country,
                                   levels = order_2023)


ggplot(
  southern_compare,
  aes(x = country, y = hdi, fill = factor(year))
) +
  
  # Bars
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.75
  ) +
  
  # Highlight Mozambique with black border
  geom_col(
    data = southern_compare %>% filter(country == "Mozambique"),
    position = position_dodge(width = 0.8),
    width = 0.75,
    colour = "black",
    linewidth = 0.8
  ) +
  
  # HDI value labels
  geom_text(
    aes(label = round(hdi, 3)),
    position = position_dodge(width = 0.8),
    hjust = -0.1,
    size = 3.6
  ) +
  
  coord_flip() +
  
  scale_fill_manual(
    values = c(
      "1990" = "#f28e2b",   
      "2023" = "#1f77b4"    
    ),
    name = "Year"
  ) +
  
  scale_y_continuous(limits = c(0, 1)) +
  
  labs(
    x = "Country",
    y = "Human Development Index (HDI)",
    title = "HDI in Southern Africa: 1990 vs 2023",
    subtitle = "Countries ordered by 2023 HDI"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "top",
    plot.title = element_text(face = "bold")
  )


To remove NA values from the hdi data and widen the bars

southern_compare <- southern_compare %>%
  filter(!is.na(country) & country != "NA")


ggplot(
  southern_compare,
  aes(x = country, y = hdi, fill = factor(year))
) +
  
  # Wider bars
  geom_col(
    position = position_dodge(width = 0.9),
    width = 0.9
  ) +
  
  # HDI value labels
  geom_text(
    aes(label = round(hdi, 3)),
    position = position_dodge(width = 0.9),
    hjust = -0.1,
    size = 3.8
  ) +
  
  coord_flip() +
  
  scale_fill_manual(
    values = c(
      "1990" = "#f28e2b",
      "2023" = "#1f77b4"
    ),
    name = "Year"
  ) +
  
  scale_y_continuous(limits = c(0, 1)) +
  
  labs(
    x = "Country",
    y = "Human Development Index (HDI)",
    title = "HDI in Southern Africa: 1990 vs 2023",
    subtitle = "Countries ordered by 2023 HDI"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "top",
    plot.title = element_text(face = "bold")
  )

To return the 1990 values that are not displaying

southern_compare <- southern_dta %>%
  filter(year %in% c(1990, 2023)) %>%
  
  # Correct HDI scaling
  mutate(
    hdi = ifelse(hdi > 1, hdi / 100, hdi)
  ) %>%
  
  # Remove missing countries
  filter(!is.na(country) & country != "NA")

# Order countries by 2023 HDI
order_2023 <- southern_compare %>%
  filter(year == 2023) %>%
  distinct(country, .keep_all = TRUE) %>% 
  arrange(desc(hdi)) %>%
  pull(country)

southern_compare$country <- factor(
  southern_compare$country,
  levels = order_2023
)

To ensure the plotting of both 1990 and 2023 bars on the chart

ggplot(
  southern_compare,
  aes(
    x = country,
    y = hdi,
    fill = factor(year)
  )
) +
  
  # Side-by-side bars
  geom_col(
    position = position_dodge(width = 0.9),
    width = 0.85
  ) +
  
  # HDI labels
  geom_text(
    aes(label = round(hdi, 3)),
    position = position_dodge(width = 0.9),
    hjust = -0.15,
    size = 3.8
  ) +
  
  # Flip coordinates
  coord_flip() +
  
  # Colors
  scale_fill_manual(
    values = c(
      "1990" = "#f28e2b",
      "2023" = "#1f77b4"
    ),
    name = "Year"
  ) +
  
  # Proper HDI scale
  scale_y_continuous(
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0.08))
  ) +
  
  # Labels
  labs(
    x = "Country",
    y = "Human Development Index (HDI)",
    title = "HDI in Southern Africa: 1990 vs 2023",
    subtitle = "Countries ordered by 2023 HDI"
  ) +
  
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "top",
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold")
  )

southern_compare <- southern_dta %>%
  
  filter(year %in% c(1990, 2023)) %>%
  
  # Fix Hdi Scale
  mutate(
    hdi = case_when(
      hdi > 10 ~ hdi / 1000,   # 193 -> 0.193
      hdi > 1  ~ hdi / 100,    # 75 -> 0.75
      TRUE ~ hdi
    )
  ) %>%
  
  filter(!is.na(hdi)) %>%
  filter(!is.na(country) & country != "NA")

order_2023 <- southern_compare %>%
  filter(year == 2023) %>%
  distinct(country, .keep_all = TRUE) %>%
  arrange(desc(hdi)) %>%
  pull(country)

southern_compare$country <- factor(
  southern_compare$country,
  levels = order_2023
)

ggplot(
  southern_compare,
  aes(
    x = country,
    y = hdi,
    fill = factor(year)
  )
) +
  
  geom_col(
    position = position_dodge(width = 0.9),
    width = 0.85
  ) +
  
  geom_text(
    aes(label = round(hdi, 3)),
    position = position_dodge(width = 0.9),
    hjust = -0.15,
    size = 3.8
  ) +
  
  coord_flip() +
  
  scale_fill_manual(
    values = c(
      "1990" = "#f28e2b",   # orange
      "2023" = "#1f77b4"    # blue
    ),
    name = "Year"
  ) +
  
  scale_y_continuous(
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0.08))
  ) +
  
  labs(
    x = "Country",
    y = "Human Development Index (HDI)",
    title = "HDI in Southern Africa: 1990 vs 2023",
    subtitle = "Countries ordered by 2023 HDI"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "top",
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold")
  )


To make sure the 1990 bars are displayed on the chart as they are incorrectly scaled again

southern_compare <- southern_compare %>%
  filter(!is.na(country) & country != "NA")


southern_compare <- southern_compare %>%
  mutate(
    hdi = ifelse(year == 1990, hdi / 10, hdi)
  )

southern_compare %>%
  filter(year == 1990) %>%
  select(country, hdi)


ggplot(
  southern_compare,
  aes(x = country, y = hdi, fill = factor(year))
) +
  
  geom_col(
    position = position_dodge(width = 0.9),
    width = 0.85
  ) +
  
  geom_text(
    aes(label = round(hdi, 3)),
    position = position_dodge(width = 0.9),
    hjust = -0.15,
    size = 3.6
  ) +
  
  coord_flip() +
  
  scale_fill_manual(
    values = c(
      "1990" = "#f28e2b",
      "2023" = "#1f77b4"
    ),
    name = "Year"
  ) +
  
  scale_y_continuous(
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0.08))
  ) +
  
  labs(
    x = "Country",
    y = "Human Development Index (HDI)",
    title = "HDI in Southern Africa: 1990 vs 2023",
    subtitle = "Countries ordered by 2023 HDI"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "top",
    plot.title = element_text(face = "bold")
  )


To widen the bars to better readibilty


ggplot(
  southern_compare,
  aes(x = country, y = hdi, fill = factor(year))
) +
  
  # Wider bars
  geom_col(
    position = position_dodge(width = 1),
    width = 0.98
  ) +
  
  # HDI labels
  geom_text(
    aes(label = round(hdi, 3)),
    position = position_dodge(width = 1),
    hjust = -0.15,
    size = 3.8
  ) +
  
  coord_flip() +
  
  scale_fill_manual(
    values = c(
      "1990" = "#f28e2b",
      "2023" = "#1f77b4"
    ),
    name = "Year"
  ) +
  
  scale_y_continuous(
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0.08))
  ) +
  
  labs(
    x = "Country",
    y = "Human Development Index (HDI)",
    title = "HDI in Southern Africa: 1990 vs 2023",
    subtitle = "Countries ordered by 2023 HDI"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "top",
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold", size = 11)
  )

Ceating the geospatial map of Africa where each country is colored according to its HDI in 2023

Read the African shapefile


africa_map <- st_read("C:/Users/Administrator/Documents/data/wb/WB_GAD_ADM0_complete.shp")

st_crs(africa_map)


centroids<-read.csv("C:/Users/Administrator/Documents/data/centroids_countries.csv")

summary(centroids$lon)
summary(centroids$lat)

ggplot() +
  geom_point(data = centroids, aes(longitude, latitude), size = 0.125) +
  coord_sf(crs = 4326, datum = NA) +
  theme_bw()

after replacing the datum = NA with just datum

coord_sf(crs = 4326)

ggplot() +
  geom_point(data = centroids, aes(longitude, latitude), size = 0.125) +
  coord_sf(crs = 4326) +
  theme_bw()

adding centroids to the data

hdi_geo <- hdi_valid %>%
  left_join(
    centroids[, c("COUNTRY", "longitude", "latitude")],
    by = c("country" = "COUNTRY")
  ) %>%
  rename(
    long = longitude,
    lat  = latitude
  )

hdi_africa_geo <- hdi_geo %>%
  filter(country %in% africa_names)

summary(hdi_africa_geo$long)
summary(hdi_africa_geo$lat)

hdi_map_years <- hdi_africa_geo %>%
  filter(year %in% c(1990, 2023))

hdi_geo <- hdi_valid %>%
  left_join(
    centroids[, c("COUNTRY", "longitude", "latitude")],
    by = c("country" = "COUNTRY")
  ) %>%
  rename(
    end_long = longitude,
    end_lat  = latitude
  )

head(hdi_geo)

filter the data to specific subset

hdi_subset <- hdi_geo %>%
  filter(
    country %in% africa_names,
    !is.na(hdi),
    year %in% c(1990, 2023)
  )

hdi_subset <- hdi_subset %>%
  mutate(hdi_size = hdi * 10)

aes(size = hdi_size)

map_world <- st_read("C:/Users/Administrator/Documents/data/wb/WB_GAD_ADM0_complete.shp")

m <- ggplot() +
  geom_sf(
    data = map_world,
    color = "black",
    linewidth = 0.05,
    fill = "grey"
  )

m

To make an african map and make it look nice- choropleth

hdi_2023 <- hdi_valid %>%
  filter(
    year == 2023,
    country %in% africa_names,
    !is.na(hdi)
  )


africa_hdi_map <- africa_map %>%
  left_join(hdi_2023, by = c("NAM_0" = "country"))


ggplot(africa_hdi_map) +
  geom_sf(aes(fill = hdi), color = "white", linewidth = 0.2) +
  
  scale_fill_viridis_c(
    option = "plasma",
    name = "HDI (2023)",
    na.value = "grey85"
  ) +
  
  labs(
    title = "Human Development Index (HDI) Across Africa — 2023",
    subtitle = "Higher values indicate higher human development",
    caption = "Source: UNDP HDI Data"
  ) +
  
  coord_sf(crs = 4326) +
  
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14)
  )


Compare 1990 and 2023 side by Side

hdi_compare <- hdi_valid %>%
  filter(
    year %in% c(1990, 2023),
    country %in% africa_names,
    !is.na(hdi)
  )

africa_hdi_compare <- africa_map %>%
  left_join(
    hdi_compare,
    by = c("NAM_0" = "country"),
    relationship = "many-to-many"
  )

ggplot(africa_hdi_compare) +
  geom_sf(aes(fill = hdi), color = "white", linewidth = 0.2) +
  
  facet_wrap(~year, ncol = 2) +
  
  scale_fill_viridis_c(
    option = "plasma",
    name = "HDI",
    limits = range(africa_hdi_compare$hdi, na.rm = TRUE),
    na.value = "grey85"
  ) +
  
  labs(
    title = "Human Development Index (HDI) in Africa: 1990 vs 2023",
    subtitle = "Same color scale used for accurate comparison",
    caption = "Source: UNDP HDI Data"
  ) +
  
  coord_sf(crs = 4326) +
  
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 14)
  )


africa_hdi_compare <- africa_hdi_compare %>%
  filter(!is.na(hdi))

ggplot(africa_hdi_compare) +
  
  geom_sf(aes(fill = hdi), color = "white", linewidth = 0.15) +
  
  facet_wrap(~year, ncol = 2) +
  
  scale_fill_viridis_c(
    option = "plasma",
    limits = c(0,1),
    name = "HDI"
  ) +
  
  coord_sf(datum = NA) +
  
  labs(
    title = "Human Development Index Across Africa",
    subtitle = "1990 vs 2023",
    caption = "Source: UNDP HDI",
    x = NULL, y = NULL
  ) +
  
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14)
  )

Add centrion to the map to show data points

glimpse(hdi_subset)


hdi_subset <- hdi_subset %>%
  rename(
    long = end_long,
    lat  = end_lat
  )

geom_point(
  data = hdi_subset,
  aes(long, lat),
  size = 0.6,
  colour = "black"
)

ggplot() +
  geom_sf(
    data = africa_hdi_compare,
    aes(fill = hdi),
    color = "white",
    linewidth = 0.1
  ) +
  
  geom_point(
    data = hdi_subset,
    aes(long, lat),
    size = 0.6,
    colour = "black"
  ) +
  
  facet_wrap(~year) +
  
  scale_fill_viridis_c(option = "plasma") +
  coord_sf() +
  theme_minimal()


Add legend + title - directional lines


hdi_flows <- hdi_subset %>%
  select(country, year, long, lat) %>%
  pivot_wider(
    names_from = year,
    values_from = c(long, lat),
    names_glue = "{.value}_{year}"
  )

glimpse(hdi_flows)


geom_segment(
  data = hdi_flows,
  aes(
    x = long_1990, y = lat_1990,
    xend = long_2023, yend = lat_2023
  ),
  arrow = arrow(length = unit(0.12, "cm")),
  linewidth = 0.3,
  color = "black",
  alpha = 0.6
)


ggplot() +
  
  # HDI choropleth
  geom_sf(
    data = africa_hdi_compare,
    aes(fill = hdi),
    color = "white",
    linewidth = 0.1
  ) +
  
  # Country centroids
  geom_point(
    data = hdi_subset,
    aes(long, lat),
    size = 0.6,
    colour = "black"
  ) +
  
  # Directional change arrows (1990 → 2023)
  geom_segment(
    data = hdi_flows,
    aes(
      x = long_1990, y = lat_1990,
      xend = long_2023, yend = lat_2023
    ),
    arrow = arrow(length = unit(0.12, "cm")),
    linewidth = 0.35,
    color = "black",
    alpha = 0.6
  ) +
  
  # Side-by-side years
  facet_wrap(~year) +
  
  # HDI color scale
  scale_fill_viridis_c(
    option = "plasma",
    name = "HDI"
  ) +
  
  coord_sf() +
  
  labs(
    title = "Human Development Index in Africa (1990 vs 2023)",
    subtitle = "Country shading shows HDI levels; arrows indicate change over time",
    caption = "Source: UNDP HDI data"
  ) +
  
  theme_minimal() +
  
  # Clean map look (no gridlines)
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

Adding country labels to the graph

ggplot() +
  
  # HDI choropleth
  geom_sf(
    data = africa_hdi_compare,
    aes(fill = hdi),
    color = "white",
    linewidth = 0.1
  ) +
  
  # Centroid dots
  geom_point(
    data = hdi_subset,
    aes(long, lat),
    size = 0.6,
    colour = "black"
  ) +
  
  # Country labels
  geom_text(
    data = hdi_subset,
    aes(long, lat, label = country),
    size = 2.3,
    colour = "black",
    check_overlap = TRUE
  ) +
  
  # Directional arrows (1990 → 2023)
  geom_segment(
    data = hdi_flows,
    aes(
      x = long_1990, y = lat_1990,
      xend = long_2023, yend = lat_2023
    ),
    arrow = arrow(length = unit(0.12, "cm")),
    linewidth = 0.35,
    color = "black",
    alpha = 0.6
  ) +
  
  # Compare years
  facet_wrap(~year) +
  
  # HDI color scale
  scale_fill_viridis_c(
    option = "plasma",
    name = "HDI"
  ) +
  
  coord_sf() +
  
  labs(
    title = "Human Development Index in Africa (1990 vs 2023)",
    subtitle = "Country shading shows HDI levels; arrows indicate development change",
    caption = "Source: UNDP HDI data"
  ) +
  
  theme_minimal() +
  
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

Adding lat/ long axes

ggplot() +
  
  # HDI choropleth
  geom_sf(
    data = africa_hdi_compare,
    aes(fill = hdi),
    color = "white",
    linewidth = 0.1
  ) +
  
  # Centroid points
  geom_point(
    data = hdi_subset,
    aes(x = long, y = lat),
    size = 1,
    colour = "black"
  ) +
  
  # Country labels
  geom_text(
    data = hdi_subset,
    aes(x = long, y = lat, label = country),
    size = 2.2,
    nudge_y = 0.6,
    check_overlap = TRUE
  ) +
  
  # Directional arrows
  geom_segment(
    data = hdi_flows,
    aes(
      x = long_1990, y = lat_1990,
      xend = long_2023, yend = lat_2023
    ),
    arrow = arrow(length = unit(0.12, "cm")),
    linewidth = 0.35,
    color = "black",
    alpha = 0.6
  ) +
  
  facet_wrap(~year) +
  
  scale_fill_viridis_c(
    option = "plasma",
    name = "HDI"
  ) +
  
  coord_sf() +
  
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Human Development Index in Africa (1990 vs 2023)",
    subtitle = "Shading shows HDI; dots are country centroids; arrows show change",
    caption = "Source: UNDP HDI Data"
  ) +
  
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 11)
)


Adding a scalebar and classifying HDI into development groups

africa_hdi_compare <- africa_hdi_compare %>%
  mutate(
    hdi_class = case_when(
      hdi < 0.55 ~ "Low",
      hdi < 0.70 ~ "Medium",
      hdi < 0.80 ~ "High",
      TRUE       ~ "Very High"
    )
)


ggplot() +
  
  # Classified HDI choropleth
  geom_sf(
    data = africa_hdi_compare,
    aes(fill = hdi_class),
    color = "white",
    linewidth = 0.1
  ) +
  
  # Centroids
  geom_point(
    data = hdi_subset,
    aes(long, lat),
    size = 1,
    colour = "black"
  ) +
  
  # Labels
  geom_text(
    data = hdi_subset,
    aes(long, lat, label = country),
    size = 2.2,
    nudge_y = 0.6,
    check_overlap = TRUE
  ) +
  
  # HDI change arrows
  geom_segment(
    data = hdi_flows,
    aes(
      x = long_1990, y = lat_1990,
      xend = long_2023, yend = lat_2023
    ),
    arrow = arrow(length = unit(0.12, "cm")),
    linewidth = 0.35,
    color = "black",
    alpha = 0.6
  ) +
  
  facet_wrap(~year) +
  
  # Nice development colors
  scale_fill_manual(
    values = c(
      "Low" = "#d73027",
      "Medium" = "#fc8d59",
      "High" = "#91bfdb",
      "Very High" = "#4575b4"
    ),
    name = "HDI Level"
  ) +
  
  # Scale bar + north arrow feel
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    text_cex = 0.7
  ) +
  
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    height = unit(1.1, "cm"),
    width = unit(1.1, "cm"),
    style = north_arrow_fancy_orienteering
  ) +
  
  coord_sf() +
  
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Human Development Index in Africa (1990 vs 2023)",
    subtitle = "HDI grouped by development level with country centroids and change direction",
    caption = "Source: UNDP HDI Data"
  ) +
  
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14)
  )


Correct Accuracy of scaleBar

africa_hdi_compare <- africa_hdi_compare %>%
  mutate(
    hdi_class = case_when(
      hdi < 0.55 ~ "Low",
      hdi < 0.70 ~ "Medium",
      hdi < 0.80 ~ "High",
      TRUE       ~ "Very High"
    )
  )

# Transform to metric CRS for accurate scale bar (UTM 35S covers most southern Africa)
africa_sf_utm <- st_transform(africa_hdi_compare, crs = 32735)  

# Plot Africa HDI map
ggplot(africa_sf_utm) +
  geom_sf(aes(fill = hdi_class), color = "white", size = 0.2) +
  
  # Scale bar and north arrow
  annotation_scale(location = "bl", width_hint = 0.3, line_col = "black") +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         style = north_arrow_fancy_orienteering()) +
  
  # Fill colors for HDI classes
  scale_fill_manual(
    values = c(
      "Low" = "#d73027",
      "Medium" = "#fc8d59",
      "High" = "#fee08b",
      "Very High" = "#1a9850"
    ),
    name = "HDI Class"
  ) +
  
  # Minimal theme
  theme_minimal(base_size = 13) +
  labs(
    title = "Human Development Index (HDI) in Africa",
    subtitle = "Classified into Low, Medium, High, Very High"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12)
  )


Side by Side map with accurate scalebar

africa_hdi_compare <- africa_hdi_compare %>%
  filter(year %in% c(1990, 2023)) %>%   
  mutate(
    hdi_class = case_when(
      hdi < 0.55 ~ "Low",
      hdi < 0.70 ~ "Medium",
      hdi < 0.80 ~ "High",
      TRUE       ~ "Very High"
    )
  )

# Transform to metric CRS for accurate scale bars
africa_sf_utm <- st_transform(africa_hdi_compare, crs = 32735)  

# Map for 1990 
map_1990 <- africa_sf_utm %>%
  filter(year == 1990) %>%
  ggplot() +
  geom_sf(aes(fill = hdi_class), color = "white", size = 0.2) +
  annotation_scale(location = "bl", width_hint = 0.3, line_col = "black") +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  scale_fill_manual(
    values = c(
      "Low" = "#d73027",
      "Medium" = "#fc8d59",
      "High" = "#fee08b",
      "Very High" = "#1a9850"
    ),
    name = "HDI Class"
  ) +
  labs(title = "1990") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14)
  )

# Map for 2023
map_2023 <- africa_sf_utm %>%
  filter(year == 2023) %>%
  ggplot() +
  geom_sf(aes(fill = hdi_class), color = "white", size = 0.2) +
  annotation_scale(location = "bl", width_hint = 0.3, line_col = "black") +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  scale_fill_manual(
    values = c(
      "Low" = "#d73027",
      "Medium" = "#fc8d59",
      "High" = "#fee08b",
      "Very High" = "#1a9850"
    ),
    name = "HDI Class"
  ) +
  labs(title = "2023") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14)
  )

# Combine side by side
map_1990 + map_2023 + plot_layout(guides = "collect")


Add title to the graph


# HDI classes
africa_hdi_compare <- africa_hdi_compare %>%
  filter(year %in% c(1990, 2023)) %>%
  mutate(
    hdi_class = case_when(
      hdi < 0.55 ~ "Low",
      hdi < 0.70 ~ "Medium",
      hdi < 0.80 ~ "High",
      TRUE       ~ "Very High"
    )
  )

# Project to UTM for accurate scale bars
africa_sf_utm <- st_transform(africa_hdi_compare, crs = 32735)

# Map for 1990
map_1990 <- africa_sf_utm %>%
  filter(year == 1990) %>%
  ggplot() +
  geom_sf(aes(fill = hdi_class), color = "white", size = 0.2) +
  annotation_scale(location = "bl", width_hint = 0.3, line_col = "black") +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  scale_fill_manual(
    values = c(
      "Low" = "#d73027",
      "Medium" = "#fc8d59",
      "High" = "#fee08b",
      "Very High" = "#1a9850"
    ),
    name = "HDI Class"
  ) +
  labs(title = "1990") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14)
  )

# Map for 2023
map_2023 <- africa_sf_utm %>%
  filter(year == 2023) %>%
  ggplot() +
  geom_sf(aes(fill = hdi_class), color = "white", size = 0.2) +
  annotation_scale(location = "bl", width_hint = 0.3, line_col = "black") +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  scale_fill_manual(
    values = c(
      "Low" = "#d73027",
      "Medium" = "#fc8d59",
      "High" = "#fee08b",
      "Very High" = "#1a9850"
    ),
    name = "HDI Class"
  ) +
  labs(title = "2023") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14)
  )

# Combine maps side by side with a main title
map_1990 + map_2023 + 
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Human Development Index (HDI) in Africa: 1990 vs 2023",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
    )
  )


Add a data source


africa_hdi_compare <- africa_hdi_compare %>%
  filter(year %in% c(1990, 2023)) %>%
  mutate(
    hdi_class = case_when(
      hdi < 0.55 ~ "Low",
      hdi < 0.70 ~ "Medium",
      hdi < 0.80 ~ "High",
      TRUE       ~ "Very High"
    )
  )

# Project to UTM for accurate scale bars
africa_sf_utm <- st_transform(africa_hdi_compare, crs = 32735)

# Common HDI colors
hdi_colors <- c(
  "Low" = "#d73027",
  "Medium" = "#fc8d59",
  "High" = "#fee08b",
  "Very High" = "#1a9850"
)

# Map for 1990
map_1990 <- africa_sf_utm %>%
  filter(year == 1990) %>%
  ggplot() +
  geom_sf(aes(fill = hdi_class), color = "white", size = 0.2) +
  annotation_scale(location = "bl", width_hint = 0.3, line_col = "black") +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  scale_fill_manual(values = hdi_colors, name = "HDI Class") +
  labs(title = "1990") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14)
  )

# Map for 2023
map_2023 <- africa_sf_utm %>%
  filter(year == 2023) %>%
  ggplot() +
  geom_sf(aes(fill = hdi_class), color = "white", size = 0.2) +
  annotation_scale(location = "bl", width_hint = 0.3, line_col = "black") +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  scale_fill_manual(values = hdi_colors, name = "HDI Class") +
  labs(title = "2023") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14)
  )

# Combine maps side by side with a main title and caption
map_1990 + map_2023 +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Human Development Index (HDI) in Africa: 1990 vs 2023",
    caption = "Data source: UNDP HDI REPORTS",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.caption = element_text(size = 8, hjust = 0, face = "italic")
    )
  )

Shared Scalebar

map_1990 <- africa_sf_utm %>%
  filter(year == 1990) %>%
  ggplot() +
  geom_sf(aes(fill = hdi_class), color = "white", size = 0.2) +
  annotation_scale(location = "bl", width_hint = 0.3, line_col = "black") +  # only here
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  scale_fill_manual(values = hdi_colors, name = "HDI Class") +
  labs(title = "1990") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14)
  )

# Map for 2023 (without scale bar)
map_2023 <- africa_sf_utm %>%
  filter(year == 2023) %>%
  ggplot() +
  geom_sf(aes(fill = hdi_class), color = "white", size = 0.2) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  scale_fill_manual(values = hdi_colors, name = "HDI Class") +
  labs(title = "2023") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14)
  )

# Combine maps side by side
map_1990 + map_2023 +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Human Development Index (HDI) in Africa: 1990 vs 2023",
    caption = "Data source: UNDP HDI REPORTS",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.caption = element_text(size = 8, hjust = 0, face = "italic")
    )
  )


Developing the Interactive Map

leaflet() %>%
  addTiles()

adding a location

leaflet() %>%
  addTiles() %>%
  addMarkers(
    lng = 28.3228,
    lat = -15.3875,
    popup = "Lusaka — Capital of Zambia"
  )

Adding a basemap


leaflet() %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addMarkers(
    lng = 28.3228,
    lat = -15.3875,
    popup = "Lusaka — Capital of Zambia"
  )

To make some spatial data- Adding GPS to the map
Centroids will be used to avoid a fake gps map


leaflet(hdi_subset) %>%
  addTiles() %>%
  addCircles(lng = ~long, lat = ~lat)


To make clusters of points

leaflet(hdi_subset) %>%
  addTiles() %>%
  addMarkers(
    lng = ~long,
    lat = ~lat,
    popup = ~paste0(country, "<br>HDI: ", round(hdi, 3)),
    clusterOptions = markerClusterOptions()
  )

leaflet(hdi_subset) %>%
  addTiles() %>%
  addCircles(
    lng = ~long,          
    lat = ~lat,           
    radius = 100000,      
    color = "#003f5c",
    fillOpacity = 0.7,
    
    popup = ~paste0(
      "<b>", country, "</b><br>",
      "Year: ", year, "<br>",
      "HDI: ", round(hdi, 3)
    ),
    
    label = ~country
  )

To change the color of the dots

hdi_subset <- hdi_subset %>%
  mutate(
    hdi_group = ifelse(hdi < 0.6, "Low HDI", "High HDI")
  )

pal <- colorFactor(
  c("yellow", "red"),
  domain = c("Low HDI", "High HDI")
)

leaflet(hdi_subset) %>%
  addTiles() %>%
  addCircles(
    lng = ~long,
    lat = ~lat,
    radius = 100000,
    color = ~pal(hdi_group),
    fillOpacity = 0.7,
    popup = ~paste0(
      "<b>", country, "</b><br>",
      "Year: ", year, "<br>",
      "HDI: ", round(hdi, 3)
    ),
    label = ~country
  )
Loading shapefiles into leaflet

africa_hdi_compare <- st_transform(africa_hdi_compare, 4326)


pal <- colorNumeric(
  palette = "plasma",
  domain = africa_hdi_compare$hdi,
  na.color = "transparent"
)


leaflet(africa_hdi_compare) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  
  addPolygons(
    fillColor = ~pal(hdi),
    fillOpacity = 0.85,
    color = "white",
    weight = 0.4,
    
    popup = ~paste0(
      "<strong>", NAM_0, "</strong><br>",
      "Year: ", year, "<br>",
      "HDI: ", round(hdi, 3), "<br>",
      "Class: ", hdi_class
    )
  ) %>%
  
  addLegend(
    pal = pal,
    values = ~hdi,
    title = "Human Development Index"
  ) %>%
  
  addScaleBar()

# Remove missing HDI values
africa_hdi_plot <- africa_hdi_compare %>%
  filter(!is.na(hdi))

# Colour palette
pal <- colorNumeric(
  palette = "plasma",
  domain = africa_hdi_plot$hdi,
  na.color = "transparent"
)

# Leaflet map
leaflet(africa_hdi_plot) %>%
  addProviderTiles("CartoDB.Positron") %>%
  
  addPolygons(
    fillColor = ~pal(hdi),
    fillOpacity = 0.85,
    color = "white",
    weight = 0.4,
    
    popup = ~paste0(
      "<strong>", NAM_0, "</strong><br>",
      "Year: ", year, "<br>",
      "HDI: ", round(hdi, 3), "<br>",
      "Class: ", hdi_class
    ),
    
    highlightOptions = highlightOptions(
      weight = 2,
      color = "black",
      bringToFront = TRUE
    )
  ) %>%
  
  addLegend(
    pal = pal,
    values = ~hdi,
    title = "Human Development Index",
    opacity = 1
  ) %>%
  
  addScaleBar(position = "bottomleft")

Mapping HDI in Zambia

# Filter Zambia
zambia_hdi <- africa_hdi_compare %>%
  filter(NAM_0 == "Zambia") %>%
  filter(!is.na(hdi))

# Ensure numeric
zambia_hdi$hdi <- as.numeric(zambia_hdi$hdi)

# Palette
pal <- colorNumeric(
  palette = "Blues",
  domain = zambia_hdi$hdi,
  na.color = "#f0f0f0"
)

leaflet(data = zambia_hdi) %>%   
  addTiles() %>%
  
  addPolygons(
    fillColor = ~pal(hdi),
    fillOpacity = 1,
    stroke = TRUE,
    color = "white",
    weight = 0.3,
    
    popup = ~paste(
      "<b>Zambia HDI</b><br>",
      "Year:", year, "<br>",
      "HDI:", round(hdi, 3)
    ),
    
    label = ~paste("HDI:", round(hdi, 3))
  ) %>%
  
  addLegend(
    pal = pal,
    values = ~hdi,
    title = "Human Development Index",
    opacity = 1
  ) %>%
  
  addScaleBar(position = "bottomleft")


Colouring the quantiles


# Subset Zambia HDI
zambia_hdi <- africa_hdi_compare %>%
  filter(NAM_0 == "Zambia") %>%
  filter(!is.na(hdi))

# Make sure HDI is numeric
zambia_hdi$hdi <- as.numeric(zambia_hdi$hdi)

# Create quantile palette (5 classes)
qpal <- colorQuantile(
  palette = "Blues",
  domain = zambia_hdi$hdi,
  n = 5
)

leaflet(data = zambia_hdi) %>%
  addTiles() %>%
  
  addPolygons(
    stroke = FALSE,
    smoothFactor = 0.2,
    fillOpacity = 0.9,
    
    fillColor = ~qpal(hdi), 
    
    popup = ~paste(
      "Year:", year,
      "<br>HDI:", round(hdi, 3)
    ),
    
    label = ~paste("HDI:", round(hdi, 3))
  ) %>%
  
  addLegend(
    pal = qpal,
    values = ~hdi,
    title = "Zambia HDI (quantiles)",
    opacity = 1
  ) %>%
  
  addScaleBar(position = "bottomleft")

highlighting the polygon

zambia_hdi <- africa_hdi_compare %>%
  filter(NAM_0 == "Zambia") %>%
  filter(!is.na(hdi))

# Ensure numeric
zambia_hdi$hdi <- as.numeric(zambia_hdi$hdi)

# Quantile palette (5 classes)
qpal <- colorQuantile(
  palette = "Blues",
  domain = zambia_hdi$hdi,
  n = 5
)

leaflet(data = zambia_hdi) %>%
  addTiles() %>%
  
  addPolygons(
    stroke = TRUE,          
    weight = 2,             
    fillOpacity = 0.9,
    
    fillColor = ~qpal(hdi), 
    
    popup = ~paste(
      "Year:", year,
      "<br>HDI:", round(hdi, 3)
    ),
    
    label = ~paste("HDI:", round(hdi, 3)),
    
    highlightOptions = highlightOptions(
      color = "#666",
      weight = 4,
      bringToFront = TRUE
    )
  ) %>%
  
  addLegend(
    pal = qpal,
    values = ~hdi,
    title = "Zambia HDI (quantiles)",
    opacity = 1
  ) %>%
  
  addScaleBar(position = "bottomleft")


Making Sure HDI is properly scaled on the interactive map

Corrected Africa HDI Interactive map


africa_hdi_compare <- st_transform(africa_hdi_compare, 4326)

# Ensure HDI is numeric
africa_hdi_compare$hdi <- as.numeric(africa_hdi_compare$hdi)

# Remove missing HDI values 
africa_hdi_plot <- africa_hdi_compare %>%
  filter(!is.na(hdi))


pal <- colorNumeric(
  palette = "plasma",
  domain = c(0, 1),     
  na.color = "#bdbdbd"
)


leaflet(data = africa_hdi_plot) %>%
  
  # Clean light basemap
  addProviderTiles("CartoDB.Positron") %>%
  
  # HDI polygons
  addPolygons(
    fillColor = ~pal(hdi),
    fillOpacity = 0.85,
    color = "white",
    weight = 0.5,
    smoothFactor = 0.2,
    
    popup = ~paste0(
      "<strong>", NAM_0, "</strong><br>",
      "Year: ", year, "<br>",
      "HDI: ", round(hdi, 3), "<br>",
      "Class: ", hdi_class
    ),
    
    highlightOptions = highlightOptions(
      weight = 2,
      color = "black",
      bringToFront = TRUE
    )
  ) %>%
  
  # Fixed legend scale
  addLegend(
    pal = pal,
    values = c(0, 1),
    title = "Human Development Index (0–1)",
    opacity = 1,
    position = "bottomright"
  ) %>%
  
  # Scale bar
  addScaleBar(position = "bottomleft")

Corrected Zambia HDI Interactive map

africa_hdi_compare <- africa_hdi_compare %>%
  mutate(
    value = na_if(value, ".."),
    hdi = as.numeric(value)
  )

africa_hdi_compare$hdi


zambia_hdi <- africa_hdi_compare %>%
  filter(NAM_0 == "Zambia",
         year == 2023) %>%
  filter(!is.na(hdi))

zambia_hdi$hdi <- as.numeric(zambia_hdi$hdi)

# Fixed HDI scale
pal <- colorNumeric(
  palette = "Blues",
  domain = c(0, 1),
  na.color = "#f0f0f0"
)

# Build map properly
leaflet(data = zambia_hdi) %>%
  
  addProviderTiles("CartoDB.Positron") %>%
  
  addPolygons(
    fillColor = ~pal(hdi),
    fillOpacity = 1,
    stroke = TRUE,
    color = "white",
    weight = 0.6,
    
    # Click popup
    popup = ~paste0(
      "<b>Zambia HDI</b><br>",
      "Year: ", year, "<br>",
      "HDI: ", round(hdi, 3)
    ),
    
    # Hover label
    label = ~paste0("HDI: ", round(hdi, 3)),
    
    labelOptions = labelOptions(
      style = list(
        "font-weight" = "bold",
        "font-size" = "14px"
      ),
      direction = "auto"
    ),
    
    highlightOptions = highlightOptions(
      weight = 3,
      color = "black",
      bringToFront = TRUE
    )
  ) %>%
  
  addLegend(
    pal = pal,
    values = c(0,1),
    title = "Human Development Index (0–1)",
    opacity = 1
  ) %>%
  
  addScaleBar(position = "bottomleft")


saving the interactive map using html


zambia_hdi_map <- leaflet(data = zambia_hdi) %>%
  addTiles() %>%
  addPolygons(
    stroke = TRUE,          
    weight = 2,             
    fillOpacity = 0.9,
    fillColor = ~qpal(hdi), 
    popup = ~paste(
      "Year:", year,
      "<br>HDI:", round(hdi, 3)
    ),
    label = ~paste("HDI:", round(hdi, 3)),
    highlightOptions = highlightOptions(
      color = "#666",
      weight = 4,
      bringToFront = TRUE
    )
  ) %>%
  addLegend(
    pal = qpal,
    values = ~hdi,
    title = "Zambia HDI (quantiles)",
    opacity = 1
  ) %>%
  addScaleBar(position = "bottomleft")


saveWidget(
  zambia_hdi_map, 
  file = "C:/Users/Administrator/Documents/data/zambia_hdi_interactive.html",
  selfcontained = TRUE
)

browseURL("C:/Users/Administrator/Documents/data/zambia_hdi_interactive.html")



