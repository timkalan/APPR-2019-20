# ZEMLJEVID - delež ####
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



# GRAF - starostne skupine ####
starostneSkupine <- stock %>% 
  filter(age != "total") %>%
  group_by(year, age, gender) %>%
  summarise(number = sum(number, na.rm = TRUE))
starostGraf <- ggplot(starostneSkupine, aes(x = year, y = number)) + #eh
  geom_line() + facet_grid(age~gender)

dve <- ggplot(starostneSkupine, aes(x = age, y = number)) + 
  geom_point() + facet_grid(year~gender)

starostLeta <- ggplot(starostneSkupine %>% 
                        group_by(year, age) %>%
                        summarise(number = sum(number, na.rm = TRUE)), 
                      aes(x = age, y = number)) + 
  geom_boxplot() +
  geom_line(data = starostneSkupine %>% group_by(age, year) %>%
                               summarise(povp = sum(number, na.rm = TRUE)) %>%
               group_by(age) %>% summarise(med = median(povp)),
                             aes(x = age, y = med, group = 1, color = "red"), 
            show.legend = FALSE) + 
  xlab("Starost (leta)") + ylab("Število migrantov") + 
  ggtitle("Število migrantov glede na starost v obdobju 1990-2019")



# GRAF - države (to je v shiny) ####
emigracija <- migranti %>% group_by(origin, Year) %>%
  summarise(number = sum(number, na.rm = TRUE))
imigracija <- migranti %>% group_by(destination, Year) %>%
  summarise(number = sum(number, na.rm = TRUE))

drzave$leto[drzave$leto == 2018] <- 2019 #manjša guljufija
master <- drzave %>% 
  inner_join(emigracija, by = c("country" = "origin", "leto" = "Year")) %>%
  rename(emigracija = number) %>%
  inner_join(imigracija, by = c("country" = "destination", "leto" = "Year")) %>%
  rename(imigracija = number)

grafBdp <- ggplot(master, aes(x = BDP, y = imigracija)) + geom_point() + 
  scale_x_log10() + scale_y_log10()
#  facet_grid(rows = "leto") + geom_smooth(method = "lm")

grafBdppc <- ggplot(master, aes(x = BDPpc, y = emigracija)) + geom_point() + 
  scale_y_log10() + scale_x_log10()

# trash
grafIzobrazba <- ggplot(master, aes(x = izobrazenost, y = imigracija)) + geom_point() +
  scale_y_log10()
grafHDI <- ggplot(master, aes(x = HDI, y = imigracija)) + geom_point() +
  scale_y_log10()



# RELIGIJE (2019) ####
religigranti <- religije %>% inner_join(emigracija %>%
                                          filter(Year == "2019"),
                                        by = c("country" = "origin")) %>%
  select(-"Year", emigracija = number) %>% inner_join(imigracija %>% filter(Year == "2019"),
                                 by = c("country" = "destination")) %>%
  select(-"Year", imigracija = number) %>%
  inner_join(drzave %>% filter(leto == "2019") %>%
               select(-"leto", -"BDP", -"izobrazenost", -"HDI", -"BDPpc"),
             by = c("country")) %>%
  arrange(country, desc(procent))

dominantna <- distinct(religigranti, country, .keep_all = TRUE) %>% 
  mutate(emigracija = emigracija / populacija, imigracija = imigracija / populacija)

grafDominantna <- ggplot(dominantna %>% select(-"procent", -"populacija") %>%
                 gather(preseljevanje, delez, 3:4), aes(x = delez, fill = preseljevanje)) + 
  geom_histogram() + facet_grid(religija~.)
 
grafDrreg <- ggplot(dominantna %>% arrange(religija, desc(emigracija)) %>% 
                      mutate(country = 1:168), aes(x = country, y = emigracija, fill = religija) ) + geom_col()


# muslimani <- ggplot(religigranti, 
#                     aes(x = muslims, y = emigracija)) + 
#   geom_point() + scale_x_log10() + scale_y_log10()
# #   geom_smooth(method = "gam")
# # cor(religigranti$muslims, y = religigranti$emigracija, use = "na.or.complete")

