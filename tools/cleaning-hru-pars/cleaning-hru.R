library(igraph)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

hrus <- read.table("superjams/data/J2K_cowat/parameter/hru_cowat_10_cor_grand_buech.par", 
                   skip = 5,
                   sep= "\t",
                   dec = ".") %>%
  tbl_df()
#nb correspondance collonnes: V1, V2, V6, V7, V9,    V13,    V14
                              #id,area,x,y,subbassin,to_poly,to_reach

valide_hrus <- hrus %>% 
  filter(V2 > 100) %>% 
  tbl_df()

write.table(valide_hrus,
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



