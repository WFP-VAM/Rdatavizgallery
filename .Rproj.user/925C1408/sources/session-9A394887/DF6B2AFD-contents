library(tidyverse)
library(labelled)
library(expss)
library(haven)
library(officer)


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


#add fcg_colors
fcg_colors = c("Acceptable" = "#27AE60","Borderline" = "#F1C40F","Poor" = "#C0392B")
main_white <- "#000000"
main_black <- "#FFFFFF"


fcscat21_barplot <- fcscat21_admin1_table_long %>% 
  ggplot() +geom_col(aes(x = ADMIN1Name, y = perc,fill = FCSCat21), width = 0.7) +geom_text(aes(x = ADMIN1Name,
                  y = perc,
                  color = FCSCat21,
                  label = perc),
                  position = position_stack(vjust = 0.5),
              show.legend = FALSE,
              size = 10/.pt,
  )+ scale_color_manual(values = c(main_white, main_black, main_white)) +
  labs(
    title = "Household Food Consumption Score Classification by State | 2022",
    subtitle = "Relative Proportion of Households per FCS Classification by State",
    caption = "Source: WFP VAM Unit \n© United Nations World Food Programme"
  )  +  scale_fill_manual(values=fcg_colors) + theme_wfp(grid = "XY",
                                                                    axis = T,
                                                                    axis_title = T)             
                
#Export as editable ppt                
#copied from https://www.pipinghotdata.com/posts/2020-09-22-exporting-editable-ggplot-graphics-to-powerpoint-with-officer-and-purrr/
#create list of all the plots
listofplots <- list(fcscat21_barplot)

#create_dml, converts the ggplot objects to dml objects.
create_dml <- function(plot){
  rvg::dml(ggobj = plot)
}
#Apply this function to the list of ggplot objects to create a list of dml objects with the same dimension.
plots_dml <- purrr::map(listofplots, create_dml)
# function to export plot to PowerPoint ----
create_pptx <- function(plot, path, left = 0.5, top = 0.5, width = 9, height = 7){
  # if file does not yet exist, create new PowerPoint ----
  if (!file.exists(path)) {
    out <- officer::read_pptx()
  }
  # if file exist, append slides to exisiting file ----
  else {
    out <- officer::read_pptx(path)
  }
  out %>% 
    officer::add_slide() %>% 
    officer::ph_with(plot, location = officer::ph_location(
      width = width, height = height, left = left, top = top)) %>% 
    base::print(target = path)
}
##now fire away!
purrr::map(
  # dml plots to export ----
  plots_dml, 
  # exporting function ----
  create_pptx, 
  # additional fixed arguments in create_pptx ----
  path = "FCS_ADM1graphics.pptx"
)
