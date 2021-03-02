bigHrusTb <<- read.table("superjams/data/J2K_cowat/parameter/hru.par",
                      sep="\t", 
                      dec= ".", 
                      header = F,
                      skip = 5) %>% tbl_df()

colnames(bigHrusTb) <- c("ID","area","elevation","slope","aspect","x","y","watershed","subbasin","hgeoID","landuseID","soilID","to_poly","to_reach")

bigHrusTb %>% filter(is.na(to_poly))

hrus <<- read.table("superjams/data/J2K_cowat/parameter/hru_cowat_10_ok2.par",
                      sep="\t", 
                      dec= ".", 
                      header = F,
                      skip = 5) %>% tbl_df()

colnames(hrus) <- c("ID","area","elevation","slope","aspect","x","y","watershed","subbasin","hgeoID","landuseID","soilID","to_poly","to_reach")

plots2Hrus <<- read.table("hru-plot-to-big-hru.csv",
                         sep=";",
                         dec=".",
                         header = T) %>% 
  mutate(isPlot = !is.na(ID_PARCEL)) %>%
  #filter(isPlot) %>%
  rename(motherHRU = cat) %>%
  tbl_df()

plotsInHrus <<- hrus %>% 
  left_join(# Pour les plots, la HRU mère est l'exutoire du HRUplot
              plots2Hrus %>%
              filter(!isPlot) %>%
              rename(to_poly = cat_new) %>%
              select(to_poly, motherHRU)) %>%
  left_join(# Pour les nons plots, la HRU mère est l'anciène HRU..
              plots2Hrus %>%
              rename(ID = cat_new) %>%
              rename(bigHRUID = motherHRU) %>%
              select(ID, bigHRUID, isPlot)) %>%
              mutate(motherHRU = replace_na(motherHRU, 0)) %>%
              mutate(bigHRUID = replace_na(bigHRUID, 0)) %>%
  mutate(bigHRUid = isPlot * motherHRU + (!isPlot) * bigHRUID)

# 
# hruPlotData <- hrus
# 
# 
# hrus
# hrus %>% filter(duplicated(area))
# 
# length(hrus$area)
# length(unique(paste0(hrus$area)))
# hist(hrus$area)
# hrus %>% 
#   mutate(uid = paste0(x,y,area,landuseID))%>%
#   group_by(uid) %>% 
#   filter(n()>1)
# 
# bigHrus %>% 
#   mutate(uid = paste0(x,y,area,landuseID))%>%
#   group_by(uid) %>% 
#   filter(n()>1)
# 
# hrus %>% filter(to_poly == 13841)
