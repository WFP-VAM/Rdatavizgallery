#------------------------------------------------------------------------------#

#	                        WFP RAM Standardized Scripts
#                      Calculating and Visualizing rCSI

#------------------------------------------------------------------------------#

## Last updated:  June 06 2023

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
data <- haven::read_sav("Rdatavizgallery/data/sampledataenglish.sav")

# Calculate rCSI --------------------------------------------------------------# 
#Script copied and pasted from 
#https://github.com/WFP-VAM/RAMResourcesScripts/blob/main/Indicators/Reduced-coping-strategy-index/rCSI_tidyverse.R

data <- data %>% mutate(rCSI = rCSILessQlty + 
                              (rCSIBorrow * 2) + 
                               rCSIMealNb + 
                               rCSIMealSize + 
                              (rCSIMealAdult * 3))

var_label(data$rCSILessQlty)  <- "Relied on less preferred and less expensive food"
var_label(data$rCSIBorrow)    <- "Borrowed food or relied on help from a relative or friend"
var_label(data$rCSIMealNb)    <- "Reduce number of meals eaten in a day"
var_label(data$rCSIMealSize)  <- "Limit portion size of meals at meal times"
var_label(data$rCSIMealAdult) <- "Restricted consumption by adults for small children to eat"
var_label(data$rCSI)          <- "Reduced coping strategies index (rCSI)"

# Create table of FGVitACat, FGProteinCat, FGHIronCat by ADMIN1 ---------------#

data$ADMIN1Name <- haven::as_factor(data$ADMIN1Name)

rcsi_table <- data %>%
  tbl_summary(by = ADMIN1Name, 
              include = c(rCSILessQlty, rCSIBorrow, rCSIMealNb, rCSIMealSize, 
                          rCSIMealAdult, rCSI),
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
rcsi_table

# Create table of rCSI by ADMIN1 ----------------------------------------------#

## unweighted mean rCSI by ADMIN1
rcsi_admin1_table_long <- data %>% 
  mutate(ADMIN1Name_lab = to_factor(ADMIN1Name)) %>% 
  group_by(ADMIN1Name_lab) %>% 
  drop_na(rCSI) %>%   # check before proceed?
  summarise(meanrCSI = round(mean(rCSI),1))

# Create bar graph of rCSI ----------------------------------------------------# 
rcsi_admin_barplot <- rcsi_admin1_table_long %>% 
  ggplot() +
  geom_col(aes(
    x = meanrCSI,
    y = reorder(ADMIN1Name_lab, meanrCSI)
    ),
    fill = wfp_pal(n = 1, "pal_blue"),
    width = 0.8
    ) +
  labs(
    title = "Reduced Coping Strategy Index (rCSI) by State | April 2023",
    subtitle = "Average rCSI per Household by State",
    caption = "Source: Emergency Food Security Assessment, data collected May 2023",
    x = "rCSI",
    y = "State"
  ) + 
  geom_vline(xintercept = 4, 
             color = "#e74a53", 
             linetype = "dashed") + ## cut-off point?
  geom_text(aes(x = meanrCSI,
                y = ADMIN1Name_lab,
                label = meanrCSI),
            hjust = -0.5,
            size = 8 /.pt
            ) +
  scale_x_continuous(
    expand = expansion(c(0, 0.1)),
    breaks = pretty_breaks(n = 7),
    labels = label_number()
  ) + 
  theme_wfp(grid = "XY",
            axis = F,
            axis_title = F)

rcsi_admin_barplot 
