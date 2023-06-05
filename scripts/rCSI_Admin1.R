#------------------------------------------------------------------------------#

#	                        WFP RAM Standardized Scripts
#                      Calculating and Visualizing rCSI

#------------------------------------------------------------------------------#

## Last updated:  May 25 2023

## Load Packages --------------------------------------------------------------#

library(tidyverse)
library(labelled)
library(haven)
library(scales)

# WFP themes package will be updated frequently - reinstall it everytime for now
library(devtools)
install_github("WFP-VAM/wfpthemes")
library(wfpthemes)

# Load Sample Data ------------------------------------------------------------#
data <- haven::read_sav("data/sampledataenglish.sav")

# Calculate rCSI --------------------------------------------------------------# 
#Script copied and pasted from 
#https://github.com/WFP-VAM/RAMResourcesScripts/blob/main/Indicators/Reduced-coping-strategy-index/rCSI_tidyverse.R

data <- data %>% mutate(rCSI = rCSILessQlty + 
                              (rCSIBorrow * 2) + 
                               rCSIMealNb + 
                               rCSIMealSize + 
                              (rCSIMealAdult * 3))
var_label(data$rCSI) <- "Reduced coping strategies index (rCSI)"

# Create table of rCSI by ADMIN1 ----------------------------------------------#

## unweighted mean rCSI by ADMIN1
rcsi_admin1_table_long <- data %>% 
  mutate(ADMIN1Name_lab = to_factor(ADMIN1Name)) %>% 
  group_by(ADMIN1Name_lab) %>% 
  drop_na(rCSI) %>%   # check before proceed?
  summarise(meanrCSI = round(mean(rCSI),1))

# Create bar graph of rCSI ----------------------------------------------------# 
rcsi_admin_barplot <- rcsi_admin1_table_long %>% ggplot() +
  geom_col(aes(
    x = meanrCSI,
    y = reorder(ADMIN1Name_lab, meanrCSI),
  ),
  fill = wfp_pal(n = 1, "pal_blue"),
  width = 0.8
  ) +
  labs(
    title = "Consumption Based Reduced Coping Strategy Index (rCSI) by State | April 2023",
    subtitle = "Average rCSI  per Household by State",
    x = "rCSI",
    y = "State",
    caption = "Source: Emergency Food Security Assessment, data collected April 2023"
  ) + geom_text(aes(x = meanrCSI,
                    y = ADMIN1Name_lab, label = meanrCSI),
                hjust = -0.5,
                size = 8 / .pt
  ) +
  scale_x_continuous(
    expand = expansion(c(0, 0.1)),
    breaks = pretty_breaks(n = 7),
    labels = label_number()
  ) + theme_wfp(grid = FALSE, axis = "y", axis_title = FALSE, axis_text = "y")


rcsi_admin_barplot 
