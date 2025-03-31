## Load packages ##

# Package list
pkg_list <- c("tidyverse", "lme4", "emmeans", "knitr", "car")

# Load packages
pacman::p_load(pkg_list, character.only = TRUE)

# Formatting for RTs
apa_rt <- function(rt_tab) {
  
  if (is.data.frame(rt_tab) == FALSE) {
    
    rt_tab <- data.frame(rt_tab %>% summary(type = "response"))
    
  }
  
  tab_cols <- colnames(rt_tab)
  tab_end <- which(tab_cols == "emmean") - 1
  tabs <- tab_cols[1:tab_end]
  
  if ("lower.CL" %in% tabs) {
    
    rt_tab <- rt_tab %>% 
      mutate(rt = -1000/emmean,
             rt_low = -1000/lower.CL,
             rt_high = -1000/upper.CL)
    
    rt_tab %>%
      select(all_of(tabs), rt, rt_low, rt_high)
    
  } else {
    
    rt_tab <- rt_tab %>% 
      mutate(rt = -1000/emmean,
             rt_low = -1000/asymp.LCL,
             rt_high = -1000/asymp.UCL)
    
    rt_tab %>%
      select(all_of(tabs), rt, rt_low, rt_high)
    
  }
  
}
