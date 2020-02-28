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
grafMigracija <- ggplot(master %>% gather(preselitev, stevilo, 8:9) %>%
                                            group_by(leto, preselitev) %>% 
                                            summarise(stevilo = mean(stevilo)),
                         aes(x = leto, y = stevilo)) + geom_point() +
  facet_grid(~preselitev) + xlim(1990, 2025) + 
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE) + 
  xlab("Leto") + ylab("Število") + 
  ggtitle("Povprečna emigracija in imigracija v obdobju 1990-2019") + 
  geom_smooth(method = "loess", se = FALSE, fullrange = TRUE, col = "yellow")


cor(imigracija$Year, y = imigracija$number, use = "na.or.complete")

linImigracija <- lm(data = master, imigracija ~ leto)
napoved <- data.frame(leto = seq(1990, 2025, 1))
napoved$imigracija <- predict(linImigracija, napoved) # kaj model napove

linEmigracija <- lm(data = master, emigracija ~ leto)
napoved$emigracija <- predict(linEmigracija, napoved)
# loh bi še kej o vsaki državi posebi. 

loessImigracija <- loess(data = master, imigracija ~ leto)
napoved$lImi <- predict(loessImigracija, napoved)

loessEmigracija <- loess(data = master, emigracija ~ leto)
napoved$lEmi <- predict(loessEmigracija, napoved)


# napake <- sapply(list(linImigracija, kv, mls, mgam), function(x) sum(x$residuals^2))


# RAZVRŠČANJE DRŽAV V SKUPINE ####

