# ZEMLJEVIDI ####
delezi <- stock %>%  
  filter(age == "total", year == "2019") %>%
  select(-"year", -"age") %>% 
  group_by(country) %>% 
  summarise(number = sum(number, na.rm = TRUE)) %>%
  inner_join(drzave[c("country", "leto", "populacija")] %>% 
               filter(leto == "2018")) %>%
  mutate(delez = number / populacija) %>%
  select(-"leto", -"number", -"populacija")

zemStock <- tm_shape(merge(svet, delezi, 
                                 by.x = "NAME", by.y = "country")) + 
  tm_polygons("delez", midpoint = 0.4)


# GRAFI ####
starostneSkupine <- stock %>% 
  filter(age != "total") %>%
  group_by(year, age, gender) %>%
  summarise(number = sum(number, na.rm = TRUE))
starostGraf <- ggplot(starostneSkupine, aes(x = year, y = number)) + 
  geom_line() + facet_grid(age~gender)

dve <- ggplot(starostneSkupine, aes(x = age, y = number)) + 
  geom_point() + facet_grid(year~gender)

starostLeta <- ggplot(starostneSkupine %>% 
                        group_by(year, age) %>%
                        summarise(number = sum(number, na.rm = TRUE)), 
                      aes(x = age, y = number)) + 
  geom_boxplot()


# # Izračun neto migracije za posamezne države skozi čas
# izhod <- skupno %>% group_by(origin_country, decade) %>%
#   summarise(izhod=sum(number, na.rm=TRUE)) %>%
#   rename(country = origin_country)
# 
# prihod <- skupno %>% group_by(dest_country, decade) %>%
#   summarise(prihod=sum(number, na.rm=TRUE)) %>%
#   rename(country = dest_country)
# 
# # to zdaj ni tidy data
# neto <- inner_join(izhod, prihod, by = c("country", "decade")) %>%
#   mutate(neto_imigracija = prihod - izhod)
# 
# # kot zgoraj, samo razdeljeno po spolih
# izhodS <- poSpolih %>% group_by(origin_country, decade, gender) %>%
#   summarise(izhod=sum(number, na.rm=TRUE)) %>%
#   rename(country = origin_country)
# 
# prihodS <- poSpolih %>% group_by(dest_country, decade, gender) %>%
#   summarise(prihod=sum(number, na.rm=TRUE)) %>%
#   rename(country = dest_country)
# 
# netoSpoli <- inner_join(izhodS, prihodS, by = c("country", "decade", "gender")) %>%
#   mutate(neto_imigracija = prihod - izhod)
# 
# kolicina <- netoSpoli %>% group_by(country, gender) %>%
#   summarise(priselitev = sum(prihod, na.rm = TRUE))
# 
# 
# # GRAFI
# 
# desetletja <- ggplot(neto, aes(x = decade, y = neto_imigracija)) + geom_point()
# 
# # se izkaže za nezanimiv graf
# kolicina <- kolicina %>% inner_join(drzave %>% filter(leto == 2000), by = c("country")) %>%
#   select(-"leto")
# 
# zemPriselitev <- tm_shape(merge(svet, kolicina %>% group_by(country, BDP) %>% 
#                  summarise(priselitev = sum(priselitev, na.rm = TRUE)), by.x = "NAME", by.y = "country")) + tm_polygons("priselitev")



