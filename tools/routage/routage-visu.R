library(igraph)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
hrus <- read.table("superjams/data/J2K_cowat/parameter/hru.par", 
                    skip = 5,
                    sep= "\t",
                    dec = ".") %>%
  tbl_df() %>% 
  select(V1, V2, V6, V7, V9, V13, V14)

colnames(hrus) <- c("id", "area", "x", "y", "subbassin", "to_poly", "to_reach")

# On plote tout le buech
sub1hrus <- hrus #%>% (sinon on peut ici ne choisir que certains sous-bassins)
  #filter(subbassin == 52001)

# On crée les arcs entre HRUs
sub1HruEdges <- sub1hrus %>%
  filter(to_poly > 0) %>%
  select(id, to_poly) %>% 
  rename(flows_in = to_poly) %>%
  mutate_all(as.character)

# On crée les arcs entre HRU et reachs
sub1ReachEdges <- sub1hrus %>%
  filter(to_reach > 0) %>%
  select(id, to_reach) %>%
  rename(flows_in = to_reach) %>%
  mutate_all(as.character) %>%
  mutate(flows_in = paste("reach", flows_in, sep="_"))

# le réseau est la cominaison des deux
sub1Edges <- rbind(sub1HruEdges, 
                    sub1ReachEdges)

sub1G <- graph_from_edgelist(sub1Edges %>% as.matrix())

# La liste des noeuds du réseaux comprend les Hrus et les reachs
vertexList <- V(sub1G)

# Il y a un reach par sous-bassin! On le positionne au "centre" du sous-bassin..
reachsPositions <- hrus %>% 
  group_by(subbassin) %>%
  summarise(x = mean(x), y=mean(y)) %>%
  mutate(vertexId = paste("reach", subbassin, sep="_"))

# Les Hrus sont positionnées au niveau de leurs coordonées
hrusPositions <- hrus %>%
  select(id, x,y) %>%
  mutate(vertexId = as.character(id))

vertexPositions <- union(reachsPositions %>% 
                           select(x,y, vertexId),
                         hrusPositions %>% 
                           select(x,y, vertexId))
# On stocke dans cette table les attributs des noeuds
# Attention ils doivent être dans le même ordre que lea liste des noueds (vertexList)
# Assuré par le le "left_join"
vertexAtributes <- vertexList$name %>% as.data.frame() %>%
  mutate_("id"=".") %>%
  tbl_df() %>%
  mutate(vertexId = as.character(id)) %>%
  select(vertexId) %>%
  left_join(vertexPositions,  by="vertexId")  %>%
  left_join(sub1hrus %>% 
    mutate(vertexId = as.character(id)) %>%
      select(vertexId, area, subbassin), by="vertexId") %>%
  mutate(ishru = !str_detect(vertexId, "reach")) %>%
  mutate(area = replace_na(area,500000))

subnum = "all"
pdf(paste0("subassin_", subnum,".pdf"), height = 16, width = 11)
plot(sub1G, 
     edge.arrow.size=.2,
     vertex.size = vertexAtributes %>% 
       pull(area) / 500000,
     vertex.label.cex=0.25,
     vertex.label.dist=0,
     layout = vertexAtributes %>%
       select(x,y) %>%
       as.matrix(),
     vertex.color= !vertexAtributes %>% 
       pull(ishru)
    )
dev.off()



