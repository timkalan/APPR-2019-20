# ZEMLJEVID - CLUSTERING ####
skupineDelez <- kmeans(delezi$delez, 5, nstart = 1500)
zemljevidCluster <- tm_shape(merge(svet, data.frame(country = delezi$country, skupina = factor(skupineDelez$cluster)), 
                             by.x = "NAME", by.y = "country")) + 
  tmap_options(max.categories = 5) + 
  tm_polygons("skupina")

napakaClust <- skupineDelez$tot.withinss



# DRŽAVE - korelacijski oblak (v shiny) ####
grafBdp <- grafBdp + geom_smooth(method = "lm")
korelacija <- cor(master$BDP, y = master$imigracija, use = "na.or.complete")



# NAPOVEDOVANJE MIGRCIJE ####
grafMigracija <- ggplot(master %>% group_by(leto) %>%
                           summarise(povp = mean(imigracija, na.rm = TRUE)), 
                         aes(x = leto, y = povp)) + 
  geom_point() + geom_point(data = master %>% group_by(leto) %>%
                              summarise(povp = mean(emigracija, na.rm = TRUE)), 
                            aes(x = leto, y = povp), col = "red") 

cor(imigracija$Year, y = imigracija$number, use = "na.or.complete")

linImigracija <- lm(data = master, imigracija ~ leto)
napoved <- data.frame(leto = seq(1990, 2025, 1))
napoved$imigracija <- predict(lin, napoved) # kaj model napove

grafMigracija <- grafMigracija + xlim(1990, 2025) + 
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE, col = "pink") 

linEmigracija <- lm(data = master, emigracija ~ leto)
napoved$emigracija <- predict(linEmigracija, napoved)

# to dej v en kjut facetek ane


# RAZVRŠČANJE DRŽAV V SKUPINE ####