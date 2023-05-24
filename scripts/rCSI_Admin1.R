library(tidyverse)
library(labelled)
library(haven)
#wfp themes package will be updated frequently - so its good to reinstall it everytime for now
library(devtools)
install_github("WFP-VAM/wfpthemes")
library(wfpthemes)


#load sample data set
data <- haven::read_sav("data/sampledataenglish.sav")

#calculate rCSI - script copied and pasted from https://github.com/WFP-VAM/RAMResourcesScripts/blob/main/Indicators/Reduced-coping-strategy-index/rCSI_tidyverse.R
#calculate reduced Coping Strategy Index (rCSI)
data <- data %>% mutate(rCSI = rCSILessQlty + (2 * rCSIBorrow) + rCSIMealNb + rCSIMealSize + (3 * rCSIMealAdult))
var_label(data$rCSI) <- "Reduced coping strategies index (rCSI)"


#creates a table of the weighted mean of rCSI


#unweighted mean rCSI by ADM1
rcsi_admin1_table_long <- data %>% mutate(ADMIN1Name_lab = to_factor(ADMIN1Name)) %>% group_by(ADMIN1Name_lab) %>% 
  drop_na(rCSI) %>% 
  summarise(meanrCSI = round(mean(rCSI),1))


#make plot
  rcsi_admin1_barplot <- rcsi_admin1_table_long %>% ggplot() +
  geom_col(aes(
    x = reorder(ADMIN1Name_lab, meanrCSI),
    y = meanrCSI,
  ),
  fill = wfp_pal(n = 1, "pal_blue"),
  width = 0.8
  ) +
  labs(
    tag = "Figure 2",
    title = "Mean rCSI by State | April 2023",
    subtitle = "Mean reduced Coping Strategy Index (rCSI) by State in Fake Country",
    y = "rCSI",
    caption = "Source: Emergency Food Security Assessment, data collected April 2023",
    
  ) + geom_text(aes(x = ADMIN1Name_lab,
                    y = meanrCSI, label = meanrCSI),
                    vjust = -0.5
  ) + theme_wfp(grid = "Y", axis_text = "XY", axis_title = "Y", axis_ticks = "Y")  
