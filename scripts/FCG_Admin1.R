#------------------------------------------------------------------------------#

#	                        WFP RAM Standardized Scripts
#                      Calculating and Visualising FCS

#------------------------------------------------------------------------------#

## Last updated:  May 25 2023

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

data <- haven::read_sav("data/sampledataenglish.sav")

# Calculate FCS & FCG ---------------------------------------------------------# 
# script copied and pasted from 
# https://github.com/WFP-VAM/RAMResourcesScripts/blob/main/Indicators/Food-consumption-score/FCS_indicator_tidyverse.R

data <- data %>% mutate(FCS = (FCSStap  * 2) + 
                              (FCSPulse * 3) +
                              (FCSPr    * 4) +
                              (FCSDairy * 4) + 
                               FCSVeg +
                               FCSFruit +
                              (FCSFat   * 0.5) +
                              (FCSSugar * 0.5))
var_label(data$FCS) <- "Food Consumption Score"

# Create FCG groups based on 21/55 or 28/42 thresholds
# Use this when analyzing a country with low consumption of sugar and oil - thresholds 21-35
data <- data %>% mutate(FCSCat21 = case_when(
                                   FCS <= 21 ~ 1, 
                                   between(FCS, 21.5, 35) ~ 2, 
                                   FCS > 35 ~ 3),
                        FCSCat28 = case_when(
                                   FCS <= 28 ~ 1, 
                                   between(FCS, 28.5, 42) ~ 2, 
                                   FCS > 42 ~ 3))
val_lab(data$FCSCat21) = num_lab("
             1 Poor
             2 Borderline
             3 Acceptable
")
var_label(data$FCSCat21) <- "FCS Categories: 21/35 thresholds"

val_lab(data$FCSCat28) = num_lab("
             1 Poor
             2 Borderline
             3 Acceptable
")
var_label(data$FCSCat28) <- "FCS Categories: 28/42 thresholds"

#make table of FCG by admin1 - where should this script go - in with indicator calculation or with the viz?

## adjust data format ---------------------------------------------------------#
fcscat21_admin1_table_long <- data %>% 
  group_by(ADMIN1Name_lab = to_factor(ADMIN1Name)) %>%
  count(FCSCat21 = as.character(FCSCat21)) %>%
  mutate(perc = 100 * n / sum(n)) %>%
  ungroup() %>% select(-n) %>% mutate_if(is.numeric, round, 1) 

## Create the bar graph -------------------------------------------------------# 

fcscat21_barplot <- fcscat21_admin1_table_long %>% 
  ggplot() +
  geom_col(aes(x = fct_reorder2(ADMIN1Name_lab, 
                                perc, 
                                FCSCat21, 
                                \(x,y) sum(x*(y=="Acceptable"))), 
               y = perc,
               fill = FCSCat21), 
           width = 0.7) +
  geom_text(aes(x = ADMIN1Name_lab,
                y = paste0(as.character(perc), "%"),
                color = FCSCat21,
                label = perc),
            position = position_stack(vjust = 0.5),
            show.legend = FALSE,
            size = 10/.pt,
            ) +
  scale_color_manual(values = c(main_white, main_black, main_white)) +
  labs(title = "Household Food Consumption Score Classification by State | April 2023",
       subtitle = "Relative Proportion of Households per FCS Classification by State in Fake Country",
       caption = "Source: Emergency Food Security Assessment, data collected April 2023",
       tag = "Figure 1"
       ) +
  scale_fill_wfp_b(palette = "pal_stoplight_3pt") +
  theme_wfp(grid = "Y",
            axis = FALSE,
            axis_title = FALSE)
