library(tidyverse)
library(labelled)
library(expss)
library(haven)


#library(devtools)
#install_github("WFP-VAM/wfpthemes")
library(wfpthemes)


#load data set
data <- haven::read_sav("data/sampledataenglish.sav")

#calculate FCS & FCG
#calculate FCS
data <- data %>% mutate(FCS = (2 * FCSStap) +(3 * FCSPulse) +(4*FCSPr) +(4*FCSDairy) +FCSVeg  +FCSFruit +(0.5*FCSFat) +(0.5*FCSSugar))
var_label(data$FCS) <- "Food Consumption Score"
#create FCG groups based on 21/25 or 28/42 thresholds
#Use this when analyzing a country with low consumption of sugar and oil - thresholds 21-35
data <- data %>% mutate(FCSCat21 = case_when(
  FCS <= 21 ~ 1, between(FCS, 21.5, 35) ~ 2, FCS > 35 ~ 3))
val_lab(data$FCSCat21) = num_lab("
             1 Poor
             2 Borderline
             3 Acceptable
")
var_label(data$FCSCat21) <- "FCS Categories"


#make table of FCG by admin1
fcscat21_admin1_table_long <- data %>% 
  group_by(ADMIN1Name = to_factor(ADMIN1Name)) %>%
  count(FCSCat21 = as.character(FCSCat21)) %>%
  mutate(perc = 100 * n / sum(n)) %>%
  ungroup() %>% select(-n) %>% mutate_if(is.numeric, round, 1) 


#stacked bar chart of fcg by Admin1
fcg_colors = c("Acceptable" = "#27AE60","Borderline" = "#F1C40F","Poor" = "#C0392B")
fcscat21_barplot <- fcscat21_admin1_table_long  %>%  ggplot(aes(fill=FCSCat21, y=perc, x=ADMIN1Name, label = perc)) +geom_bar(position="fill", stat="identity") +theme_wfp(grid = FALSE, axis = "y", axis_title = FALSE, axis_text = "y")
fcscat21_barplot <- fcscat21_barplot +scale_y_continuous(labels = scales::percent) +scale_fill_manual(values=fcg_colors) +  labs(
  title = "Percentage of Households with Poor/Borderline Food Consumption by Region | April 2023",
  subtitle = "",
  x = "",
  y = "",
  caption = "Source: WFP VAM Unit \n© United Nations World Food Programme"
)


