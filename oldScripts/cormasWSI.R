library(dplyr)
library(tidyr)
library(ggplot2)

optWsi <- read.csv("data/Sw_lai_mat.csv") %>%
  tbl_df()

minWsi <- min(optWsi, na.rm = T)
maxWsi <- max(optWsi, na.rm = T)

missedDays <- 1:5

cormasWSI <- function(missedDays = 0){
  return(pmax(maxWsi - (maxWsi - minWsi) / 25 * missedDays, minWsi, na.rm = T))
}
  
  
optWsi %>% 
  mutate(time = seq(291)) %>%
  mutate(missedDays = time - 225) %>%
  mutate(cormasPlot = cormasWSI(missedDays)) %>%
  select(-missedDays) %>%
  gather("plot","wsi",- time) %>%
  filter(time > 225) %>%
  ggplot() +
  geom_line(aes(x=time, y=wsi, color=plot)) 

optWsi %>% 
  mutate(time = seq(291)) %>%
  gather("plot","wsi",- time) %>%
  filter(time > 225) %>%
  ggplot() +
  geom_histogram(aes(x=wsi))

