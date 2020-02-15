skupineDelez <- kmeans(delezi$delez, 5, nstart = 1500)
zemCluster <- tm_shape(merge(svet, data.frame(country = delezi$country, skupina = factor(skupineDelez$cluster)), 
                             by.x = "NAME", by.y = "country")) + 
  tmap_options(max.categories = 5) + 
  tm_polygons("skupina")

# korelacijski oblak
grafBdp <- grafBdp + geom_smooth(method = "lm")
# to morda logaritmiraš?
korelacija <- cor(master$BDP, y = master$imigracija, use = "na.or.complete")



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
