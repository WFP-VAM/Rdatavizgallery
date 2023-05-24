#------------------------------------------------------------------------------#

#	                        WFP RAM Standardized Scripts
#                      Calculating and Visualizing FCSN

#------------------------------------------------------------------------------#

## Last updated:  May 24 2023

## Load Packages --------------------------------------------------------------#

library(tidyverse)
library(dplyr)
library(labelled)
library(expss)
library(haven)
library(officer)
library(gtsummary)
# WFP themes package will be updated frequently - reinstall it everytime for now
library(devtools)
install_github("WFP-VAM/wfpthemes")
library(wfpthemes)

# Load Sample Data ------------------------------------------------------------#

data <- read_sav("data/sampledataenglish.sav")

# Calculate FCSN --------------------------------------------------------------# 
# Script copied and pasted from 
# https://github.com/WFP-VAM/RAMResourcesScripts/blob/main/Indicators/Food-consumption-score-nutrition/FCSN_indicator_tidyverse.R

#recode "n/a" values to 0 and change to numeric
vars2recode <- c("FCSNPrMeatF","FCSNPrMeatO","FCSNPrFish","FCSNPrEggs",
                 "FCSNVegOrg","FCSNVegGre","FCSNFruiOrg")

data <- data %>% mutate_at(vars2recode, ~replace(., . == "n/a", "0"))
data <- data %>% mutate_at(vars2recode, as.numeric)

# Compute aggregates of key micronutrient consumption -------------------------#

## Vitamin A-Rich Foods -------------------------------------------------------#

data <- data %>% mutate(FGVitA = FCSDairy +FCSNPrMeatO +FCSNPrEggs +
                                 FCSNVegOrg +FCSNVegGre +FCSNFruiOrg)
var_label(data$FGVitA) <- "Consumption of vitamin A-rich foods"

## Protein-Rich Foods ---------------------------------------------------------#

data <- data %>% mutate(FGProtein = FCSPulse +FCSDairy +FCSNPrMeatF +
                                    FCSNPrMeatO +FCSNPrFish +FCSNPrEggs)
var_label(data$FGProtein) <- "Consumption of protein-rich foods"

## Iron-Rich Foods ------------------------------------------------------------#

data <- data %>% mutate(FGHIron = FCSNPrMeatF +FCSNPrMeatO +FCSNPrFish)
var_label(data$FGHIron) <- "Consumption of heme iron-rich foods"

## recode into nutritious groups  ---------------------------------------------#

data <- data %>% mutate(FGVitACat = case_when(FGVitA == 0 ~ 1,
                                    between(FGVitA,1,6)   ~ 2, 
                                    FGVitA >= 7           ~ 3),
                        FGProteinCat = case_when(FGProtein == 0 ~ 1, 
                                       between(FGProtein,1,6)   ~ 2,
                                       FGProtein >= 7           ~ 3),
                        FGHIronCat = case_when(FGHIron == 0 ~ 1,
                                     between(FGHIron,1,6)   ~ 2,
                                     FGHIron >= 7           ~ 3))

# define variables labels and properties for FGVitACat FGProteinCat FGHIronCat

data <- data %>%
  mutate(across(c(FGVitACat, FGProteinCat, FGHIronCat), 
                ~labelled(., labels = c(
                  "Never consumed" = 1,
                  "Consumed sometimes" = 2,
                  "Consumed at least 7 times" = 3
                ))))

data <- data %>%
  mutate(across(c(FGVitACat, FGProteinCat, FGHIronCat),
                ~factor(., levels = c(1, 2, 3),
                        labels = c("Never consumed", "Consumed sometimes", 
                                   "Consumed at least 7 times"))))

# Create table of FGVitACat, FGProteinCat, FGHIronCat by ADMIN1 ---------------#

data$ADMIN1Name <- haven::as_factor(data$ADMIN1Name)

fcsn_table <- data %>%
  tbl_summary(by = ADMIN1Name, 
              include = c(FGVitACat, FGProteinCat, FGHIronCat),
              type = list(where(is.numeric) ~ "continuous",
                          where(is.factor)  ~ "categorical"),
              statistic = list(all_continuous() ~ "{mean}",
                               all_categorical() ~ "{n}({p}%)"),
              missing = "no")%>%
  modify_header(update = list(label ~ "**Variable**"))%>%
  bold_labels()%>%
  italicize_labels()%>%
  italicize_levels() %>%
  modify_spanning_header(c("stat_1","stat_2","stat_3","stat_4","stat_5",
                           "stat_6","stat_7","stat_8","stat_9","stat_10") 
                        ~ "**n%**") # adjust based on number of ADMIN1
fcsn_table

# Create bar graph of VitACat, FGProteinCat, FGHIronCat -----------------------# 

## adjust data format ---------------------------------------------------------#

data_long <- tidyr::pivot_longer(data, 
                                 cols = c(FGVitACat, FGProteinCat, FGHIronCat), 
                                 names_to = "Variable")

## Create the bar graph -------------------------------------------------------# 

ggplot(data_long, 
       aes(x = Variable, 
           fill = factor(value))) +
#  facet_wrap(~ ADMIN1Name, ncol = 1) +
  geom_bar(width = 0.5, 
           position = "stack") +
  scale_fill_manual(values = c("#C00000","#E46C0A","#92D050"), 
                   labels = c("Never consumed", 
                              "Consumed sometimes", 
                              "Consumed at least 7 times")
                   ) +
  geom_text(aes(label = scales::percent(..count../tapply(..count.., ..x.., sum)
                                        [..x..], accuracy = 0.1), 
                group = factor(value)), 
            stat = "count", 
            position = position_stack(vjust = 0.5), 
            color = "white",
            size = 3.5) +
  scale_color_manual(values = c(main_white, main_black, main_white)) +
  labs(
    title = "Household Food Consumption Score for Nutrition",
    subtitle = "Vitamin A, Protein and Iron-Rich Foods by State (n = 3,000)",
    caption = "Source: Emergency Food Security Assessment, data collected May 2023",
  ) +
  scale_x_discrete(labels = c("Vitamin A-rich foods", 
                              "Protein-rich foods", 
                              "Heme iron-rich foods")) + 
  theme_wfp(grid = "XY",
            axis = F,
            axis_title = F)
