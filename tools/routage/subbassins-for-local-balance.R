library(igraph)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

#hrus <- read.table("superjams/data/J2K_cowat/parameter/hru.par",
#hrus <- read.table("superjams/data/J2K_cowat/parameter/hru_cowat_10_ok2.par",
#hrus <- read.table("superjams/data/J2K_cowat/parameter/hru_cowat_10_cor_grand_buech.par",
hrus <- read.table("superjams/data/J2K_cowat/parameter/hru_cowat_smallminarea_grand_buech.par", 
                    skip = 5,
                    sep= "\t",
                    dec = ".") %>%
  tbl_df() %>% 
  select(V1, V2, V6, V7, V9, V13, V14)

colnames(hrus) <- c("id", "area", "x", "y", "subbassin", "to_poly", "to_reach")


#reachs <- read.table("superjams/data/J2K_cowat/parameter/reach_cor2_delete_duplicate.par",
reachs <- read.table("superjams/data/J2K_cowat/parameter/reachs_grand_buech.par", 
                   skip = 5,
                   sep= "\t",
                   dec = ".") %>%
  tbl_df() %>% 
  select(V1, V2, V7)

colnames(reachs) <- c("id", "to_reach", "width")

# On prend toutes les hrus de ces sous-bassins
selectedHrus <- hrus

# On crée les arcs entre HRUs
sub1HruEdges <- selectedHrus %>%
  filter(to_poly > 0) %>%
  select(id, to_poly) %>% 
  rename(flows_in = to_poly) %>%
  mutate_all(as.character)

hruNtw <- graph_from_edgelist(sub1HruEdges %>% as.matrix())

# La liste des noeuds du réseaux comprend les Hrus
vertexList <- V(hruNtw)

# Les Hrus sont positionnées au niveau de leurs coordonées
hrusPositions <- selectedHrus %>%
  select(id, x,y) %>%
  mutate(vertexId = as.character(id))

vertexPositions <- hrusPositions %>% 
                           select(x,y, vertexId)

#vertexPositions <- rbind(reachsPositions %>% 
#                           select(x,y, vertexId),
#                         hrusPositions %>% 
#                           select(x,y, vertexId))

# On stocke dans cette table les attributs des noeuds
# Attention ils doivent être dans le même ordre que lea liste des noueds (vertexList)
# Assuré par le le "left_join"
vertexAtributes <- vertexList$name %>% as.data.frame() %>%
  mutate_("id"=".") %>%
  tbl_df() %>%
  mutate(vertexId = as.character(id)) %>%
  select(vertexId) %>%
  left_join(vertexPositions,  by="vertexId")  %>%
  left_join(selectedHrus %>% 
    mutate(vertexId = as.character(id)) %>%
      select(vertexId, area, subbassin), by="vertexId") %>%
  mutate(ishru = !str_detect(vertexId, "reach")) %>%
  mutate(area = replace_na(area,1000000))

#subnum = "north-west-hru-ploy"
subnum = "subbassin-GB-smallminarea-hru"
pdf(paste0("topology_", subnum,".pdf"), height = 16, width = 11) #en A3 pour tout le bassin
#pdf(paste0("topology_", subnum,".pdf"), paper ="a4")
plot(hruNtw, 
     edge.arrow.size=.2,
     vertex.size = vertexAtributes %>% 
       pull(area) / 250000,
     vertex.label.cex=0.25,
     vertex.label.dist=0, #en A3 on met les noms des noeuds dans les noeuds.
     #vertex.label.dist=0.3,#en A4 on met les noms des noeuds au dessus des noeuds.
     #layout = vertexAtributes %>% # Commenter cette ligne et les deux suivante si on ne veux pas les coordonnées
    #   select(x,y) %>%        # intéressant si on veut regarder seulement la topologie sur certains sous-bassins par exemple
    #   as.matrix(),
     vertex.color= !vertexAtributes %>% 
       pull(ishru)
    )
dev.off()

pdf(paste0("bassin_", subnum,".pdf"), paper ="a4")
plot(hruNtw, 
     edge.arrow.size=.2,
     vertex.size = vertexAtributes %>% 
       pull(area) / 250000,
     vertex.label.cex=0.25,
     #vertex.label.dist=0, #en A3 on met les noms des noeuds dans les noeuds.
     vertex.label.dist=0.3,#en A4 on met les noms des noeuds au dessus des noeuds.
     layout = vertexAtributes %>% # Commenter cette ligne et les deux suivante si on ne veux pas les coordonnées
        select(x,y) %>%        # intéressant si on veut regarder seulement la topologie sur certains sous-bassins par exemple
        as.matrix(),
     vertex.color= !vertexAtributes %>% 
       pull(ishru)
)
dev.off()

hrusSubbassins <- NULL
bassinsList <- hruNtw %>% 
  decompose()

for (i in 1:length(bassinsList)) {
  testSubBassin <- bassinsList[[i]]
  subbass <- NULL
  subbass$Hrus <- c(V(testSubBassin)$name)
  subbass <- subbass %>% as.data.frame()
  subbass$isLast <- c(degree(testSubBassin, mode = "out")) == 0
  subbass$bassin <- i 
  hrusSubbassins <- rbind(hrusSubbassins, subbass)
}

hrusSubbassins <-  hrusSubbassins %>% 
  as.data.frame() %>%
  tbl_df()

hrusSubbassins %>% write.table("minareahrus-subassins-for-balance-test.csv",
                               dec= ".",
                               sep=";",
                               row.names = F)

#hrusSubbassins <- read.table("hrus-subassins-for-balance-test.csv",
#                               dec= ".",
#                               sep=";",
#                               header = T) %>% 
#  tbl_df()

hrusSubbassins %>% 
  group_by(bassin) %>%
  count() %>% filter(n< 10)

hrusSubbassins %>% 
  filter(bassin == 45) %>% 
  pull(Hrus) %>% paste(sep=",",collapse = ",")

plot(bassinsList[[45]],
     edge.arrow.size=.2)

testVertexAtributes <- testVertexList$name %>% as.data.frame() %>%
  mutate_("id"=".") %>%
  tbl_df() %>%
  mutate(vertexId = as.character(id)) %>%
  select(vertexId) %>%
  left_join(vertexPositions,  by="vertexId")  %>%
  left_join(selectedHrus %>% 
              mutate(vertexId = as.character(id)) %>%
              select(vertexId, area, subbassin), by="vertexId") %>%
  mutate(ishru = !str_detect(vertexId, "reach")) %>%
  mutate(area = replace_na(area,1000000))

plot(testSubBassin, 
     edge.arrow.size=.2,
     vertex.size = testVertexAtributes %>% 
       pull(area) / 250000,
     vertex.label.cex=0.25,
     #vertex.label.dist=0, #en A3 on met les noms des noeuds dans les noeuds.
     vertex.label.dist=0.3,#en A4 on met les noms des noeuds au dessus des noeuds.
     layout = testVertexAtributes %>% # Commenter cette ligne et les deux suivante si on ne veux pas les coordonnées
       select(x,y) %>%        # intéressant si on veut regarder seulement la topologie sur certains sous-bassins par exemple
       as.matrix(),
     vertex.color= !testVertexAtributes %>% 
       pull(ishru)
)

plot(testSubBassin, 
     edge.arrow.size=.2,
     vertex.size = testVertexAtributes %>% 
       pull(area) / 250000,
     vertex.label.cex=0.25,
     vertex.label.dist=0, #en A3 on met les noms des noeuds dans les noeuds.
     #vertex.label.dist=0.3,#en A4 on met les noms des noeuds au dessus des noeuds.
     #layout = vertexAtributes %>% # Commenter cette ligne et les deux suivante si on ne veux pas les coordonnées
     #   select(x,y) %>%        # intéressant si on veut regarder seulement la topologie sur certains sous-bassins par exemple
     #   as.matrix(),
     vertex.color= !testVertexAtributes %>% 
       pull(ishru)
)

