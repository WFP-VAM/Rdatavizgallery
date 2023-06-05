#------------------------------------------------------------------------------#

#	                        WFP RAM Standardized Scripts
#                      Calculating and Visualizing rCSI

#------------------------------------------------------------------------------#

## Last updated:  May 25 2023

## Load Packages --------------------------------------------------------------#

library(tidyverse)
library(labelled)
library(haven)
# WFP themes package will be updated frequently - reinstall it everytime for now
library(devtools)
install_github("WFP-VAM/wfpthemes")
library(wfpthemes)

# Load Sample Data ------------------------------------------------------------#
data <- haven::read_sav("Documents/GitHub/Rdatavizgallery/data/sampledataenglish.sav")

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
ggplot(rcsi_admin1_table_long) +
  geom_col(aes(
    x = reorder(ADMIN1Name_lab, meanrCSI),
    y = meanrCSI,
  ),
  fill = wfp_pal(n = 1, "pal_blue"),
  width = 0.5
  ) +
  labs(
    title = "Mean rCSI by State | April 2023",
    subtitle = "Mean reduced Coping Strategy Index (rCSI) by State",
    y = "rCSI",
    caption = "Source: Emergency Food Security Assessment, data collected April 2023",
  ) + 
  geom_text(aes(x = ADMIN1Name_lab,
                y = meanrCSI, 
                label = meanrCSI),
            vjust = -0.5,
            size = 10/.pt,
  ) +
  theme_wfp(grid = "Y", 
                axis_text = "XY", 
                axis_title = "Y", 
                axis_ticks = "Y") 
