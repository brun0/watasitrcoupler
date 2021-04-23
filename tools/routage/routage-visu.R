library(igraph)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

#hrus <- read.table("superjams/data/J2K_cowat/parameter/hru.par",
#hrus <- read.table("superjams/data/J2K_cowat/parameter/hru_cowat_10_ok2.par",
hrus <- read.table("superjams/data/J2K_cowat/parameter/hru_cowat_10_cor_grand_buech.par", 
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

# On choisit les HRUS  sur des parties du bassin définies comme des fenêtres 
# en x et y des sous-bassins (on ne prend que des sous-bassins entiers, cf script ci-dessous)

xmin <- min(hrus$x)
xmax <- max(hrus$x)
xL <- xmax - xmin

ymin <- min(hrus$y)
ymax <- max(hrus$y)
yL <- ymax - ymin

# Ici on prend juste le petit bassin nord-ouest
#ymin <- 6394625
#xmax <- 918612

# On regarde tous les sous-bassins apparaissant dans de cette fenetre
windowsSubssasins <- hrus %>%
  filter((x < xmax) &
           (x > xmin) &
           (y < ymax) &
           (y > ymin )) %>%
  select(subbassin) %>%
  distinct()

# On prend toutes les hrus de ces sous-bassins
selectedHrus <- hrus %>% 
  inner_join(windowsSubssasins)

selectedReaches <- reachs %>% 
  mutate(subbassin = id) %>% 
  inner_join(windowsSubssasins) %>%
  select(-subbassin)
  
# On crée les arcs entre HRUs
sub1HruEdges <- selectedHrus %>%
  filter(to_poly > 0) %>%
  select(id, to_poly) %>% 
  rename(flows_in = to_poly) %>%
  mutate_all(as.character)

# On crée les arcs entre HRU et reachs
sub1ReachEdges <- selectedHrus %>%
  filter(to_reach > 0) %>%
  select(id, to_reach) %>%
  rename(flows_in = to_reach) %>%
  mutate_all(as.character) %>%
  mutate(flows_in = paste("reach", flows_in, sep="_"))

# On crée les arcs entre reachs et reachs
sub1ReachReachEdges <- selectedReaches %>%
  select(id, to_reach) %>%
  rename(flows_in = to_reach) %>%
  mutate_all(as.character) %>%
  mutate(flows_in = paste("reach", flows_in, sep="_")) %>%
  mutate(id = paste("reach",id, sep="_"))

# le réseau est la cominaison des trois
sub1Edges <- rbind(sub1HruEdges, 
                    sub1ReachEdges,
                   sub1ReachReachEdges)

sub1G <- graph_from_edgelist(sub1Edges %>% as.matrix())

hruNtw <- graph_from_edgelist(sub1HruEdges %>% as.matrix())

# La liste des noeuds du réseaux comprend les Hrus et les reachs
vertexList <- V(sub1G)

# La liste des arêtes du réseaux comprend tous les liens
edgeList <- E(sub1G)

# Il y a un reach par sous-bassin! On le positionne au "centre" du sous-bassin..
reachsPositions <- selectedHrus %>% 
  group_by(subbassin) %>%
  summarise(x = mean(x), y=mean(y)) %>%
  mutate(vertexId = paste("reach", subbassin, sep="_"))

# Les Hrus sont positionnées au niveau de leurs coordonées
hrusPositions <- selectedHrus %>%
  select(id, x,y) %>%
  mutate(vertexId = as.character(id))

vertexPositions <- union(reachsPositions %>% 
                           select(x,y, vertexId),
                         hrusPositions %>% 
                           select(x,y, vertexId))

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

# Attention en rajoutant le graphe des reachs on arrive dans les sous-bassins du dessous
## on verse en bas au milieu des reachs existants..
mx <- reachsPositions$x %>% mean()

vertexAtributes <- vertexAtributes %>% 
  mutate(x = replace_na(x, mx)) %>%
  mutate(y = replace_na(y,ymin))

#subnum = "north-west-hru-ploy"
subnum = "all-GB"
pdf(paste0("topology_", subnum,".pdf"), height = 16, width = 11) #en A3 pour tout le bassin
#pdf(paste0("topology_", subnum,".pdf"), paper ="a4")
plot(sub1G, 
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
plot(sub1G, 
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


