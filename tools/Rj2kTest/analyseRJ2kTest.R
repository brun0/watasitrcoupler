library(dplyr)
library(tidyr)
library(ggplot2)

#waterSummary <- read.csv("tools/Rj2kTest/newHruTestRes.csv") %>%
#waterSummary <- read.csv("tools/Rj2kTest/baseTestRes.csv") %>%
  #tbl_df()

waterSummary <- testRes #%>%
  #select(ends_with("Bis"),t) %>%
  #rename_all(sub, pattern = "Bis", replacement = "") %>%
  #tbl_df()

#graphName <- "waterBalanceHruPlots"
graphName <- "waterBalanceBaseTest"

WS <- waterSummary %>%  
  mutate(inWater = rain + snow) %>%
  rename(day = t) %>%
  arrange(day) %>%
  #mutate(runoff = runoffBis) %>%
  mutate(outRunoff = lead(runoff,n=0)) %>%
  #mutate(outRunoff = runoff) %>%
  mutate(eTR = etact) %>%
  mutate(balance = inWater - outRunoff - eTR) %>%
  mutate(storage = reachStorage + hruStorage) %>%
  mutate(storageNextDay = lead(storage)) %>%
  mutate(massConservation = storageNextDay - (storage + balance)) %>%
  mutate(deltaStock = storageNextDay - storage) %>% 
  mutate(deltaStock = lag(deltaStock)) %>%
  mutate(P = rain + snow)

WSBilan <- WS %>%
  select(P, deltaStock,eTR,outRunoff, day) %>%
  mutate(PmoinsETMoinsD = P - eTR - outRunoff) %>%
  gather("Variable", "VolInL", -day)

WSBilan %>% 
  filter(Variable != "deltaStock") %>%
  ggplot() +
  geom_line(aes(color=Variable, x= day, y=VolInL)#, 
   #         position = position_jitter(height=0.4e+9)
            ) +
  #geom_point(aes(color=Variable, x= day, y=VolInL, shape = Variable)) +
  geom_point(data = WSBilan %>% filter(Variable == "deltaStock"), aes(x= day, y=VolInL, shape = "deltaStock")) +
  #coord_cartesian(ylim=c(-2e+10, 4e+10)) +
  ggsave(paste0("tools/Rj2kTest/", graphName, "-", 2, ".pdf"), height = 13, width= 19, units ="cm")
