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

#calculate rCSI - script copied and pasted from https://github.com/WFP-VAM/RAMResourcesScripts/blob/main/Indicators/Reduced-coping-strategy-index/rCSI_tidyverse.R
#calculate reduced Coping Strategy Index (rCSI)
data <- data %>% mutate(rCSI = rCSILessQlty + (2 * rCSIBorrow) + rCSIMealNb + rCSIMealSize + (3 * rCSIMealAdult))
var_label(data$rCSI) <- "Reduced coping strategies index (rCSI)"


#creates a table of the unweighted mean of rCSI


#unweighted mean rCSI by ADM1
rcsi_admin1_table_long <- data %>% group_by(ADMIN1Name) %>% 
  drop_na(rCSI) %>% 
  summarise(meanrCSI = mean(rCSI))


#make plot
rcsi_barplot <- rcsi_admin1_table_long %>% 
  ggplot() +geom_col(aes(x = ADMIN1Name, y = perc,fill = FCSCat21), width = 0.7) +geom_text(aes(x = ADMIN1Name,
                                                                                                y = perc,
                                                                                                color = FCSCat21,
                                                                                                label = perc),
                                                                                            position = position_stack(vjust = 0.5),
                                                                                            show.legend = FALSE,
                                                                                            size = 10/.pt,
  )+ scale_color_manual(values = c(main_white, main_black, main_white)) +
  labs(
    title = "Household Food Consumption Score Classification by State | April 2023",
    subtitle = "Relative Proportion of Households per FCS Classification by State in Fake Country",
    caption = "Source: Emergency Food Security Assessment, data collected April 2023"
  )  +  scale_fill_wfp_b(palette = "pal_stoplight_3pt") + theme_wfp(grid = "XY",
                                                                    axis = FALSE,
                                                                    axis_title = FALSE)


