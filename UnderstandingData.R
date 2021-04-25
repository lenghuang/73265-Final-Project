# CMU 73-265 EDS Final Project
# Analyzing World Happiness Report
# https://www.kaggle.com/unsdsn/world-happiness

library("tidyverse")
library("tidyr")
library("ggplot2")
library("ggthemes")

# Set Working Directory + Import Data
rm(list=ls())
setwd("~/Coding/School/73265/Final_Project")
data2015 <- read_csv("archive/2015.csv")
data2019 <- read_csv("archive/2019.csv")


#######################################
###         DATA WRANGLING          ###
#######################################

# Make Strings Categorical Data
#data2015 <- data2015 %>% mutate_if(is.character, as.factor)
#data2019 <- data2019 %>% mutate_if(is.character, as.factor)


# Rename Columns for Convenience
names(data2015) <- c("Country", "Region", "Rank", "Happiness", "SE",
                     "Economy", "Social", "Health", "Freedom", "Trust",
                     "Generosity", "Dystopia")
names(data2019) <- c("Rank", "Country", "Happiness", "Economy", "Social",
                     "Health", "Freedom", "Generosity", "Trust")

# Only Keep 6 Factors + Metadata
data2015 <- data2015 %>% select("Country", "Happiness", "Economy", 
                                "Health", "Social", "Freedom", 
                                "Trust", "Generosity", "Region")
data2019 <- data2019 %>% select("Country", "Happiness", "Economy", 
                                "Health", "Social", "Freedom", 
                                "Trust", "Generosity")

# Create Region Column for 2019 based off of 2015 Regions
data2019$Region <- 
  ifelse(data2019$Country %in% data2015$Country,
         data2015$Region[data2019$Country %in% data2015$Country], NA)

# Drop Countries with Missing Regions
data2019 <- drop_na(data2019)
# Match 2015 data with countries in 2019
data2015 <- data2015 %>% filter(Country %in% data2019$Country)
data2019 <- data2019 %>% filter(Country %in% data2015$Country)

# Rescale the Categories to be out of Happiness Score 
data2015pct <- data2015
data2019pct <- data2019
# Get the Categorical Total
data2015pct$CategoryTotal <- rowSums(data2015pct[c(3,4,5,6,7,8)])
data2019pct$CategoryTotal <- rowSums(data2019pct[c(3,4,5,6,7,8)])
# Loop through each category
for(i in 3:8){
  # Divide By "Extent of Contribution"
  data2015pct[,i] <- data2015pct[,i] / data2015pct$CategoryTotal
  data2019pct[,i] <- data2019pct[,i] / data2019pct$CategoryTotal
  # Multiply By "Happiness", or total
  data2015pct[,i] <- data2015pct[,i] * data2015pct$Happiness
  data2019pct[,i] <- data2019pct[,i] * data2019pct$Happiness
}

# Store the Individual Categories in a Longer Dataset
data2015_longer <- 
  data2015pct %>% 
  select(-c("CategoryTotal")) %>%
  arrange(desc(Happiness)) %>%
  pivot_longer(cols = c("Happiness", "Economy", "Health", "Social", 
                        "Freedom", "Trust", "Generosity"),
               names_to = "Category",
               values_to = "Score")

###############################################
###         UNDERSTANDING THE DATA          ###
###############################################

# The 6 categories add up to a total happiness score on a scale of 0-10, 0
# being the worst and 10 being the best. The individual scores for each
# category then represents how much each category contributes the overall
# happiness score.

exampleCountries <- c("Egypt", "China", "United States", "Finland")

data2015_longer %>% 
  filter(Category != "Happiness") %>%
  filter(Country %in% exampleCountries) %>% 
  ggplot(aes(x=Score, y=Country, fill=Category)) + 
    geom_bar(position="stack", stat="identity") + 
    theme_bw() + 
    scale_fill_brewer(palette="Spectral") +
    scale_x_continuous(limits = c(0, 10), expand = c(0, 0)) +
    scale_y_discrete(limits = exampleCountries) +
    labs(x = "Happiness Score",
         y = "",
         title = "Happiness Scores and Contributing Factors",
         subtitle = "Categories are a Percentage of Happiness Score") +
    theme(plot.title = element_text(size = 16), 
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
    ) 