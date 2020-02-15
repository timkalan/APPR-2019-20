skupineDelez <- kmeans(delezi$delez, 3, nstart = 1500)
zemCluster <- tm_shape(merge(svet, data.frame(country = delezi$country, skupina = factor(skupineDelez$cluster)), 
                             by.x = "NAME", by.y = "country")) + 
  tm_polygons("skupina")



# # 4. faza: Analiza podatkov
# 
# podatki <- obcine %>% transmute(obcina, povrsina, gostota,
#                                 gostota.naselij=naselja/povrsina) %>%
#   left_join(povprecja, by="obcina")
# row.names(podatki) <- podatki$obcina
# podatki$obcina <- NULL
# 
# # Število skupin
# n <- 5
# skupine <- hclust(dist(scale(podatki))) %>% cutree(n)
