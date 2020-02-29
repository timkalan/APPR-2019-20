# ZEMLJEVID - CLUSTERING ####
skupineDelez <- kmeans(delezi$delez, 5, nstart = 1500) 
centers <- sort(skupineDelez$centers) # uredimo skupine po deležu
skupineDelez <- kmeans(delezi$delez, centers = centers, nstart = 1500)

zemljevidCluster <- tm_shape(merge(svet, data.frame(country = delezi$country, 
                                                    skupina = factor(skupineDelez$cluster)), 
                             by.x = "NAME", by.y = "country")) + 
  tmap_options(max.categories = 5) + 
  tm_polygons("skupina", title = "Skupina") +
  tm_layout(main.title = "Države razdeljene v skupine glede na delež priseljencev") + 
  tm_legend(position = c("left", "center"))

napakaClust1 <- skupineDelez$tot.withinss



# DRŽAVE - korelacijski oblak (v shiny) ####
grafBdp <- grafBdp + geom_smooth(method = "lm")
korelacija <- cor(master$BDP, y = master$imigracija, use = "na.or.complete")



# NAPOVEDOVANJE MIGRCIJE ####
grafMigracija <- ggplot(master %>% gather(preselitev, stevilo, 8:9) %>%
                                            group_by(leto, preselitev) %>% 
                                            summarise(stevilo = mean(stevilo)),
                         aes(x = leto, y = stevilo)) + 
  facet_grid(~preselitev) + xlim(1990, 2025) + 
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE) + 
  xlab("Leto") + ylab("Število") + 
  ggtitle("Povprečna emigracija in imigracija v obdobju 1990-2019") + 
  geom_smooth(method = "loess", se = FALSE, fullrange = TRUE, col = "yellow") + 
  geom_point()
grafMigracija

grafTest <- ggplot(master %>% gather(preselitev, stevilo, 8:9), 
                   aes(x = leto, y = stevilo, group = leto)) + 
  facet_grid(~preselitev) +
  geom_boxplot() + scale_y_log10()

cor(imigracija$Year, y = imigracija$number, use = "na.or.complete")

linImigracija <- lm(data = master, imigracija ~ leto)
napoved <- data.frame(leto = seq(1990, 2025, 1))
napoved$imigracija <- predict(linImigracija, napoved) # kaj model napove

linEmigracija <- lm(data = master, emigracija ~ leto)
napoved$emigracija <- predict(linEmigracija, napoved)

loessImigracija <- loess(data = master, imigracija ~ leto)
napoved$lImi <- predict(loessImigracija, napoved)

loessEmigracija <- loess(data = master, emigracija ~ leto)
napoved$lEmi <- predict(loessEmigracija, napoved)

# napake <- sapply(list(linImigracija, kv, mls, mgam), function(x) sum(x$residuals^2))



# RAZVRŠČANJE DRŽAV V SKUPINE ####
povprecje <- master %>% group_by(country) %>% 
  summarise(emigracija = mean(emigracija, na.rm = TRUE),
            imigracija = mean(imigracija, na.rm = TRUE), 
            populacija = mean(populacija, na.rm = TRUE)) %>%
  mutate(emigracija = emigracija / populacija, imigracija = imigracija / populacija) %>%
  select(-"populacija")

skupineEmigracija <- kmeans(povprecje$emigracija, 6, nstart = 1500)
centersEmi <- sort(skupineEmigracija$centers)
skupineEmigracija <- kmeans(povprecje$emigracija, centers = centersEmi, nstart = 1500)

zemljevidEmi <- tm_shape(merge(svet, data.frame(country = povprecje$country, 
                                                    skupina = factor(skupineEmigracija$cluster)), 
                                   by.x = "NAME", by.y = "country")) + 
  tmap_options(max.categories = 6) + 
  tm_polygons("skupina", title = "Skupina") + 
  tm_layout(main.title = "Države razdeljene v skupine glede na povprečno emigracijo") + 
  tm_legend(position = c("left", "center"))


skupineImigracija <- kmeans(povprecje$imigracija, 6, nstart = 1500)
centersImi <- sort(skupineImigracija$centers)
skupineImigracija <- kmeans(povprecje$imigracija, centers = centersImi, nstart = 1500)

zemljevidImi <- tm_shape(merge(svet, data.frame(country = povprecje$country, 
                                                skupina = factor(skupineImigracija$cluster)), 
                               by.x = "NAME", by.y = "country")) + 
  tmap_options(max.categories = 6) + 
  tm_polygons("skupina", title = "Skupina") + 
  tm_layout(main.title = "Države razdeljene v skupine glede na povprečno imigracijo") + 
  tm_legend(position = c("left", "center"))

