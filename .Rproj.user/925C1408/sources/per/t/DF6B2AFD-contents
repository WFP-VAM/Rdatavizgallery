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


fcscat21_barplot <- fcscat21_admin1_table_long %>% 
  ggplot() +geom_col(aes(x = ADMIN1Name, y = perc,fill = FCSCat21), width = 0.7) +geom_text(aes(x = ADMIN1Name,
                  y = perc,
                  color = FCSCat21,
                  label = perc),
              position = position_stack(vjust = 0.5),
              show.legend = FALSE,
              size = 10/.pt,
  )+
  labs(
    title = "Household Food Consumption Score Classification by State | 2022",
    subtitle = "",
    caption = "Source: WFP VAM Unit \n© United Nations World Food Programme"
  )  +  scale_fill_manual(values=fcg_colors) + theme_wfp(grid = "XY",
                                                                    axis = FALSE,
                                                                    axis_title = FALSE)             
                
                

