library(igraph)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

minArea <- 10

hrus <- read.table("superjams/data/J2K_cowat/parameter/hru_cowat_10_cor_grand_buech.par", 
                   skip = 5,
                   sep= "\t",
                   dec = ".") %>%
  tbl_df()
#nb correspondance collonnes: V1, V2, V6, V7, V9,    V13,    V14
                              #id,area,x,y,subbassin,to_poly,to_reach

toosmall_hrus <- hrus %>% 
  filter(V2 < minArea) %>% 
  tbl_df()

valide_hrus <- hrus %>% 
  filter(V2 >= minArea) %>% 
  tbl_df()

areaToAdd <- toosmall_hrus %>% 
  select(V2, V13) %>%
  rename(areaToAdd = V2, id = V13) %>%
  group_by(id) %>%
  summarise(areaToAdd = sum(areaToAdd)) %>%
  filter(id >0)

corrected_hrus <- valide_hrus %>% 
  rename(id = V1) %>%
  left_join(areaToAdd) %>%
  mutate(areaToAdd = replace_na(0)) %>%
  mutate(V2 = V2 + areaToAdd) %>%
  select(-areaToAdd)

write.table(corrected_hrus,
            "hru_cowat_withplots.par.dat",
            sep = "\t",
            dec= ".",
            row.names = F)

hrus_big <- read.table("superjams/data/J2K_cowat/parameter/hru_cor_grand_buech.par", 
                   skip = 5,
                   sep= "\t",
                   dec = ".") %>%
  tbl_df() %>%
  select(V1, V2, V6, V7, V9,    V13,    V14)

colnames(hrus_big) <- c("id","area","x","y","subbassin","to_poly","to_reach")

hrus_big %>% filter(area < 2000)



