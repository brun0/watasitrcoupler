library(dplyr)
library(tidyr)
library(ggplot2)
#res <- read.csv("tools/Rj2kTest/newHruTestRes.csv") %>%
res <- read.csv("tools/Rj2kTest/hruTestRes.csv") %>%
  tbl_df()

res %>% 
  mutate(S = hruStorage + reachStorage) %>%
  mutate(Stp1 = lead(S)) %>%
  mutate(dS = Stp1 - S) %>%
  mutate(etact = etact) %>%
  mutate(bilan = dS - (rain + snow - etact - lead(runoff))) %>%
  ggplot(aes(x=t)) +
  #geom_line(aes(y=rain + snow + 0, color ="P")) +
  #geom_line(aes(y=rain, color = "rain")) +
  #geom_line(aes(y=snow, color = "snow")) +
  geom_line(aes(y=runoff, color = "D")) +
  geom_line(aes(y=dS, color = "dS")) +
  geom_line(aes(y=etact, color = "etact")) +
  geom_line(aes(y= dS - rain - snow + runoffBis + etact, color = "flows")) #+
#  geom_line(aes(y= 0 + dS, color = "dS"))

