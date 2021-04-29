library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

# Global
globalWaterSummaryBigH <- read.table(paste0("output/globalwaterBalance-bigTRUE_GB.csv"),
              header = T,
              sep = ";",
              dec=".") %>%
  tbl_df() %>% 
  mutate(hruType = "Big")

globalWaterSummaryWithPlots <- read.table(paste0("output/globalwaterBalance-bigFALSE_GB.csv"),
              header = T,
              sep = ";",
              dec=".") %>%
  tbl_df() %>% 
  mutate(hruType = "WithHruPlots")

globalWaterSummary <- union(globalWaterSummaryBigH, globalWaterSummaryWithPlots)

globalWaterSummary %>% 
  ggplot() +
  geom_line(aes(y = runoff, x = day, color = hruType))

globalWaterSummary %>% 
  select(day, runoff, hruType) %>% 
  spread(hruType, runoff) %>%
  filter(day > 100) %>%
  mutate(diffRunOff =  (Big - WithHruPlots) / Big * 100) %>%
  ggplot() +
  geom_line(aes(y = diffRunOff, x = day))

globalWaterSummary %>% 
  select(day, runoff, hruType) %>% 
  spread(hruType, runoff) %>%
  mutate(diffRunOff =  (Big - WithHruPlots)) %>%
  ggplot() +
  geom_line(aes(y = diffRunOff, x = day))


# Local
localWaterSummaryBigH <- read.table(paste0("output/localwaterBalance-bigTRUE_GB.csv"),
                                     header = T,
                                     sep = ";",
                                     dec=".") %>%
  tbl_df() %>% 
  mutate(hruType = "Big")

localWaterSummaryWithPlots <- read.table(paste0("output/localwaterBalance-bigFALSE_GB.csv"),
                                          header = T,
                                          sep = ";",
                                          dec=".") %>%
  tbl_df() %>% 
  mutate(hruType = "WithHruPlots")

localWaterSummary <- union(localWaterSummaryBigH, localWaterSummaryWithPlots)

localWaterSummary %>% 
  ggplot() +
 # geom_line(aes(y = outflow, x = day, color = hruType)) +
  geom_line(aes(y = hruStorage, x = day, color = hruType))

localWaterSummary %>% 
  select(day, outflow, hruType) %>% 
  spread(hruType, outflow) %>%
  filter(day > 100) %>%
  mutate(diffoutFlow =  (Big - WithHruPlots) / Big * 100) %>%
  ggplot() +
  geom_line(aes(y = diffoutFlow, x = day))

#Plotting global water loss for verification
globalWaterSummaryBigH %>%
  arrange(day) %>%
  mutate(inWater = rain + snow) %>%
  mutate(outWater = etact + runoff) %>%
  mutate(storage = hruStorage + reachStorage) %>%
  mutate(storageNextDay = lead(storage)) %>%
  mutate(deltaS = storageNextDay - storage) %>%
  mutate(waterBalance =  inWater - outWater) %>%
  mutate(waterLoss = storageNextDay - storage - waterBalance) %>%
  ggplot() +
  geom_line(aes(x = day, y = waterLoss))

globalWaterSummaryWithPlots %>%
  arrange(day) %>%
  mutate(inWater = rain + snow) %>%
  mutate(outWater = etact + runoff) %>%
  mutate(storage = hruStorage + reachStorage) %>%
  mutate(storageNextDay = lead(storage)) %>%
  mutate(deltaS = storageNextDay - storage) %>%
  mutate(waterBalance =  inWater - outWater) %>%
  mutate(waterLoss = storageNextDay - storage - waterBalance) %>%
  ggplot() +
  geom_line(aes(x = day, y = waterLoss))

#Plotting local water loss for verification
localWaterSummaryBigH %>%
  arrange(day) %>%
  mutate(inWater = rain + snow) %>%
  mutate(outWater = etact + outflow) %>%
  mutate(storage = hruStorage) %>%
  mutate(storageNextDay = lead(storage)) %>%
  mutate(deltaS = storageNextDay - storage) %>%
  mutate(waterBalance =  inWater - outWater) %>%
  mutate(deltaS = storageNextDay - storage) %>%
  mutate(waterLoss = deltaS - waterBalance) %>%
  filter(day > 0) %>%
  mutate(cumWaterLoss = cumsum(waterLoss)) %>%
  ggplot() +
  geom_line(aes(x = day, y = waterLoss, color = "loss")) + 
  ggtitle("Bilan Hrus 16637")

localWaterSummaryWithPlots %>%
  arrange(day) %>%
  mutate(inWater = rain + snow) %>%
  mutate(outWater = etact + outflow) %>%
  mutate(storage = hruStorage) %>%
  mutate(storageNextDay = lead(storage)) %>%
  mutate(deltaS = storageNextDay - storage) %>%
  mutate(waterBalance =  inWater - outWater) %>%
  mutate(deltaS = storageNextDay - storage) %>%
  mutate(waterLoss = deltaS - waterBalance) %>%
  filter(day > 0) %>%
  mutate(cumWaterLoss = cumsum(waterLoss)) %>%
  ggplot() +
  geom_line(aes(x = day, y = waterLoss, color = "loss")) + 
  ggtitle("Bilan Hrus 8560,16637t,8563,11104,12464")
