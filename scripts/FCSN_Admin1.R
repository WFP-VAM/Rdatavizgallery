


library(tidyverse)
library(labelled)
library(expss)
library(haven)
library(officer)

#wfp themes package will be updated frequently - so its good to reinstall it everytime for now
library(devtools)
install_github("WFP-VAM/wfpthemes")
library(wfpthemes)


#load sample data set
data <- haven::read_sav("data/sampledataenglish.sav")

#calculate FCSN - script copied and pasted from https://github.com/WFP-VAM/RAMResourcesScripts/blob/main/Indicators/Food-consumption-score-nutrition/FCSN_indicator_tidyverse.R

#recode "n/a" values to 0
vars2recode <- c("FCSNPrMeatF","FCSNPrMeatO","FCSNPrFish","FCSNPrEggs","FCSNVegOrg","FCSNVegGre","FCSNFruiOrg")

data <- data %>% mutate_at(vars2recode, ~replace(., . == "n/a", "0"))

data <- data %>% mutate_at(vars2recode, as.numeric)


#compute aggregates of key micronutrient consumption of vitamin, iron and protein 
data <- data %>% mutate(FGVitA = FCSDairy +FCSNPrMeatO +FCSNPrEggs +FCSNVegOrg +FCSNVegGre +FCSNFruiOrg)
var_label(data$FGVitA) <- "Consumption of vitamin A-rich foods"

data <- data %>% mutate(FGProtein = FCSPulse +FCSDairy +FCSNPrMeatF +FCSNPrMeatO +FCSNPrFish +FCSNPrEggs)
var_label(data$FGProtein) <- "Consumption of protein-rich foods"

data <- data %>% mutate(FGHIron = FCSNPrMeatF +FCSNPrMeatO +FCSNPrFish)
var_label(data$FGHIron) <- "Consumption of hem iron-rich foods"

#recode into nutritious groups  
data <- data %>% mutate(FGVitACat = case_when(FGVitA == 0 ~ 1, between(FGVitA,1,6) ~ 2, FGVitA >= 7 ~ 3),
                        FGProteinCat = case_when(FGProtein == 0 ~ 1, between(FGProtein,1,6) ~ 2,  FGProtein >= 7 ~ 3),
                        FGHIronCat = case_when(FGHIron == 0 ~ 1, between(FGHIron,1,6) ~ 2,  FGHIron >= 7 ~ 3)
)


# define variables labels and properties for FGVitACat FGProteinCat FGHIronCat
data <- data %>%
  mutate(across(c(FGVitACat, FGProteinCat, FGHIronCat), ~labelled(., labels = c(
    "Never consumed" = 1,
    "Consumed sometimes" = 2,
    "Consumed at least 7 times" = 3
  ))))


#create table of FGVitACat, FGProteinCat, FGHIronCat by ADM1

#make plot
rcsi_barplot <- rcsi_admin1_table_long %>% 
ggplot() +
  geom_col(aes(x = ADMIN1Name, 
               y = perc,
               fill = FCSCat21), 
               width = 0.7) +
  geom_text(aes(x = ADMIN1Name,
                y = perc,
                color = FCSCat21,
                label = perc),
                position = position_stack(vjust = 0.5),
                show.legend = FALSE,
                size = 10/.pt,) + 
  scale_color_manual(values = c(main_white, main_black, main_white)) +
  labs(
    title = "Household Food Consumption Score Classification by State | April 2023",
    subtitle = "Relative Proportion of Households per FCS Classification by State in Fake Country",
    caption = "Source: Emergency Food Security Assessment, data collected April 2023"
  ) + 
  scale_fill_wfp_b(palette = "pal_stoplight_3pt") + 
  theme_wfp(grid = "XY",
            axis = FALSE,
            axis_title = FALSE)
            