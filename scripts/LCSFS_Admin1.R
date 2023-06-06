#------------------------------------------------------------------------------#

#	                        WFP RAM Standardized Scripts
#                      Calculating and Visualizing LCS-FS

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

data <- haven::read_sav("data/sampledataenglish.sav")

# Assign variable and value labels --------------------------------------------#

## Variable labels
var_label(data$Lcs_stress_DomAsset)   <- "Sold household assets/goods (radio, furniture, refrigerator, television, jewellery etc.) due to lack of food"
var_label(data$Lcs_stress_Saving)     <- "Spent savings due to lack of food"
var_label(data$Lcs_stress_EatOut)     <- "Sent household members to eat elsewhere/live with family or friends due to lack of food"
var_label(data$Lcs_stress_CrdtFood)   <- "Purchased food/non-food on credit (incur debts) due to lack of food"
var_label(data$Lcs_crisis_ProdAssets) <- "Sold productive assets or means of transport (sewing machine, wheelbarrow, bicycle, car, etc.)  due to lack of food"
var_label(data$Lcs_crisis_Health)     <- "Reduced expenses on health (including drugs)"
var_label(data$Lcs_crisis_OutSchool)  <- "Withdrew children from school due to lack of food"
var_label(data$Lcs_em_ResAsset)       <- "Mortgaged/Sold house or land due to lack of food"
var_label(data$Lcs_em_Begged)         <- "Begged and/or scavenged (asked strangers for money/food) due to lack of food"
var_label(data$Lcs_em_IllegalAct)     <- "Engaged in illegal income activities (theft, prostitution) due to lack of food"

## Value labels
data <- data %>%
  mutate(across(c(Lcs_stress_DomAsset,Lcs_stress_Saving,Lcs_stress_EatOut,
                  Lcs_stress_CrdtFood,Lcs_crisis_ProdAssets,Lcs_crisis_Health,
                  Lcs_crisis_OutSchool,Lcs_em_ResAsset,Lcs_em_Begged,
                  Lcs_em_IllegalAct), 
                ~labelled(., labels = c(
    "No, because I did not need to" = 10,
    "No, because I already sold those assets or have engaged in this activity within the last 12 months and cannot continue to do it" = 20,
    "Yes" = 30,
    "Not applicable (don’t have access to this strategy)" = 9999
  ))))

# Create coping strategies variables by severity ------------------------------#

## Stress
data <- data %>% mutate(Stress_coping_FS = case_when(
  Lcs_stress_DomAsset == 20 |  Lcs_stress_DomAsset == 30 ~ 1,
  Lcs_stress_Saving   == 20 | Lcs_stress_Saving    == 30 ~ 1,
  Lcs_stress_EatOut   == 20 | Lcs_stress_EatOut    == 30 ~ 1,
  Lcs_stress_CrdtFood == 20 | Lcs_stress_CrdtFood  == 30 ~ 1,
  TRUE ~ 0))
var_label(data$Stress_coping_FS) <- "Household engaged in stress coping strategies"

## Crisis
data <- data %>% mutate(Crisis_coping_FS = case_when(
  Lcs_crisis_ProdAssets == 20 | Lcs_crisis_ProdAssets == 30 ~ 1,
  Lcs_crisis_Health     == 20 | Lcs_crisis_Health     == 30 ~ 1,
  Lcs_crisis_OutSchool  == 20 | Lcs_crisis_OutSchool  == 30 ~ 1,
  TRUE ~ 0))
var_label(data$Crisis_coping_FS) <- "Household engaged in crisis coping strategies"

## Emergency
data <- data %>% mutate(Emergency_coping_FS = case_when(
  Lcs_em_ResAsset   == 20 |  Lcs_em_ResAsset  == 30 ~ 1,
  Lcs_em_Begged     == 20 | Lcs_em_Begged     == 30 ~ 1,
  Lcs_em_IllegalAct == 20 | Lcs_em_IllegalAct == 30 ~ 1,
  TRUE ~ 0))
var_label(data$Emergency_coping_FS) <- "Household engaged in emergency coping strategies"

# Calculate Max_coping_behaviour ----------------------------------------------#

data <- data %>% mutate(Max_coping_behaviour_FS = case_when(
  Emergency_coping_FS == 1 ~ 4,
  Crisis_coping_FS    == 1 ~ 3,
  Stress_coping_FS    == 1 ~ 2,
  TRUE ~ 1))

var_label(data$Max_coping_behaviour_FS) <- "Summary of asset depletion"

val_lab(data$Max_coping_behaviour_FS) = num_lab("
             1 Household not adopting coping strategies
             2 Stress coping strategies
             3 Crisis coping strategies
             4 Emergencies coping strategies
")

#creates a table of the weighted percentage of Max_coping_behaviour_FS by
#creating a temporary variable to display value labels 
#and providing the option to use weights if needed

lcsfs_admin1_table_long <- data %>% 
  group_by(ADMIN1Name_lab = to_factor(ADMIN1Name)) %>%
  count(Max_coping_behaviour_FS_lab = as.factor(Max_coping_behaviour_FS)) %>%
  mutate(perc = 100 * n / sum(n)) %>%
  ungroup() %>% select(-n) %>% mutate_if(is.numeric, round, 1) 


#make plot
lcsfs_barplot <- lcsfs_admin1_table_long %>% 
  ggplot() +geom_col(aes(x = ADMIN1Name_lab, y = perc,fill = Max_coping_behaviour_FS_lab), width = 0.7) +geom_text(aes(x = ADMIN1Name_lab,
                                                                                                y = perc,
                                                                                                color = Max_coping_behaviour_FS_lab,
                                                                                                label = paste0(perc,"%")),
                                                                                            position = position_stack(vjust = 0.5),
                                                                                            show.legend = FALSE,
                                                                                            size = 10/.pt,
  ) + scale_color_manual(values = c(main_black, main_black, main_white, main_white)) +
  labs(
    tag = "Figure 3",
    title = "Households using Livelihood Coping Stragies by State | April 2023",
    subtitle = "Relative Proportion of Households using livelihood strategies by State in Fake Country",
    caption = "Source: Emergency Food Security Assessment, data collected April 2023"
  )  +
  scale_fill_wfp_b(palette = "pal_lcs") +
  theme_wfp(grid = FALSE,
            axis = FALSE,
            axis_title = FALSE,
            axis_text = "x")

lcsfs_barplot
