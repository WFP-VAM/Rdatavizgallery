#------------------------------------------------------------------------------#

#	                        WFP RAM Standardized Scripts
#                      Calculating and Visualizing FCSN

#------------------------------------------------------------------------------#

rm(list = ls())

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

# Create bar graph of FGVitACat, FGProteinCat, FGHIronCat -----------------------# 

#set ordering
order_fcsn <- c("Consumed at least 7 times","Consumed sometimes","Never consumed")
pal_fcsn <- setNames(pal_fcsn, order_fcsn )

## Create the bar graph FGVitACat ---------------------------------------------# 
percFGVitA_admin1_table_long <- data %>%
  group_by(ADMIN1Name_lab = to_factor(ADMIN1Name), FGVitACat_lab = as.character(FGVitACat)) %>% 
  summarize(count = n()) %>%
  group_by(ADMIN1Name_lab) %>%
  mutate(perc = round(count/sum(count) * 100, 1))

percFGVitA_barplot <- percFGVitA_admin1_table_long %>% 
  ggplot() +
  geom_col(
    aes(x = fct_reorder2(ADMIN1Name_lab,
                         perc,  
                         FGVitACat_lab,
                         \(x,y) sum(x*(y=="Consumed at least 7 times"))), 
        y = perc,
        fill = factor(FGVitACat_lab, level=order_fcsn)), 
    width = 0.7) +
  geom_text(aes(x = ADMIN1Name_lab,
                y = perc,
                color = factor(FGVitACat_lab, level=order_fcsn),
                label = paste0(perc, "%")),
            position = position_stack(vjust = 0.5),
            show.legend = FALSE,
            size = 10/.pt) +
  scale_color_manual(
    values = c(main_white, main_black, main_white)
  ) +
  labs(tag = "Figure 2",
       title = "Household Food Consumption Nutritional Analysis by State | April 2023",
       subtitle = "Percentage of Households Consuming Vitamin-A Rich Foods per State in Example Country",
       caption = "Source: Emergency Food Security Assessment, data collected April 2023"
  ) +  scale_fill_manual(values = pal_fcsn) + theme_wfp(grid = FALSE, axis_text = "x", axis = F, axis_title = F) 

percFGVitA_barplot


## Create the bar graph FGProteinCat ------------------------------------------#

percFGProteinCat_admin1_table_long <- data %>%
  group_by(ADMIN1Name_lab = to_factor(ADMIN1Name), FGProteinCat_lab = as.character(FGProteinCat)) %>% 
  summarize(count = n()) %>%
  group_by(ADMIN1Name_lab) %>%
  mutate(perc = round(count/sum(count) * 100, 1))

percFGProteinCat_barplot <- percFGProteinCat_admin1_table_long %>% 
  ggplot() +
  geom_col(
    aes(x = fct_reorder2(ADMIN1Name_lab,
                         perc,  
                         FGProteinCat_lab,
                         \(x,y) sum(x*(y=="Consumed at least 7 times"))), 
        y = perc,
        #fill = FCSCat21_lab),
        fill = factor(FGProteinCat_lab, level=order_fcsn)), 
    width = 0.7) +
  geom_text(aes(x = ADMIN1Name_lab,
                y = perc,
                color = factor(FGProteinCat_lab, level=order_fcsn),
                label = paste0(perc, "%")),
            position = position_stack(vjust = 0.5),
            show.legend = FALSE,
            size = 10/.pt) +
  scale_color_manual(
    values = c(main_white, main_black, main_white)
  ) +
  labs(tag = "Figure 3",
       title = "Household Food Consumption Nutritional Analysis by State | April 2023",
       subtitle = "Percentage of Households Consuming Protein Rich Foods per  by State in Example Country",
       caption = "Source: Emergency Food Security Assessment, data collected April 2023"
  ) +  scale_fill_manual(values = pal_fcsn) + theme_wfp(grid = FALSE, axis_text = "x", axis = F, axis_title = F) 

percFGProteinCat_barplot

## Create the bar graph FGHIronCat ------------------------------------------#

percFGHIronCat_admin1_table_long <- data %>%
  group_by(ADMIN1Name_lab = to_factor(ADMIN1Name), FGHIronCat_lab = as.character(FGHIronCat)) %>% 
  summarize(count = n()) %>%
  group_by(ADMIN1Name_lab) %>%
  mutate(perc = round(count/sum(count) * 100, 1))

FGHIronCat_barplot <- percFGHIronCat_admin1_table_long %>% 
  ggplot() +
  geom_col(
    aes(x = fct_reorder2(ADMIN1Name_lab,
                         perc,  
                         FGHIronCat_lab,
                         \(x,y) sum(x*(y=="Consumed at least 7 times"))), 
        y = perc,
        fill = factor(FGHIronCat_lab, level=order_fcsn)), 
    width = 0.7) +
  geom_text(aes(x = ADMIN1Name_lab,
                y = perc,
                color = factor(FGHIronCat_lab, level=order_fcsn),
                label = paste0(perc, "%")),
            position = position_stack(vjust = 0.5),
            show.legend = FALSE,
            size = 10/.pt) +
  scale_color_manual(
    values = c(main_white, main_black, main_white)
  ) +
  labs(tag = "Figure 4",
       title = "Household Food Consumption Nutritional Analysis by State | April 2023",
       subtitle = "Percentage of Households Consuming Heme Iron Rich Foods per  by State in Example Country",
       caption = "Source: Emergency Food Security Assessment, data collected April 2023"
  ) +  scale_fill_manual(values = pal_fcsn) + theme_wfp(grid = FALSE, axis_text = "x", axis = F, axis_title = F) 

FGHIronCat_barplot
