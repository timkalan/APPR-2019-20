# tu je več grafov, kot jih je na koncu vključeno v poročilo,
# a sem želel prikazati tudi fazo "eksploratorne analize"

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

zemljevidStock <- tm_shape(merge(svet, delezi, 
                                 by.x = "NAME", by.y = "country")) + 
  tm_polygons("delez", midpoint = 0.4, title = "Delež") + 
  tm_layout(main.title = "Delež priseljencev glede na populacijo države") + 
  tm_legend(position = c("left", "center"))



# GRAF - starostne skupine ####
starostneSkupine <- stock %>% 
  filter(age != "total") %>%
  group_by(year, age, gender) %>%
  summarise(number = sum(number, na.rm = TRUE))
grafStarostleta <- ggplot(starostneSkupine, aes(x = year, y = number)) + #eh
  geom_line() + facet_grid(age~gender)

grafStarostskupine <- ggplot(starostneSkupine, aes(x = age, y = number)) + 
  geom_point() + facet_grid(year~gender)

grafStarostskatla <- ggplot(starostneSkupine %>% 
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
  geom_histogram() + facet_grid(Religija~.)
 
država <- dominantna$country

grafDrzavarelig <- ggplot(dominantna %>% arrange(Religija, desc(emigracija)) %>% 
                      mutate(country = 1:174), aes(x = country, y = - emigracija, fill = Religija, država = država)) + 
  geom_col() + geom_col(data = dominantna %>% arrange(Religija, desc(imigracija)) %>% 
                          mutate(country = 1:174), aes(x = country, y = imigracija, fill = Religija)) + 
  geom_hline(yintercept = 0) + ylab("Delež emigracije / Delež imigracije") + xlab("Država") + 
  ggtitle("Delež imigrantov/emigrantov glede na religijo (2019)") + theme(axis.text.x = element_blank(),
                                                                   axis.ticks.x = element_blank())

grafDrzavarelig <- ggplotly(grafDrzavarelig, tooltip = "država", width = 1000, height = 600)



# SLOVENIJA ####
slovenijaEmi <- migranti %>% filter(origin == "Slovenia") %>%
  select(-"origin")
slovenijaImi <- migranti %>% filter(destination == "Slovenia") %>%
  select(-"destination")
slovenijaStock <- stock %>% filter(country == "Slovenia") %>%
  select(-"country")
slovenijaPodatki <- master %>% filter(country == "Slovenia") %>%
  select(-"country")

grafSlodrzave <- ggplot(slovenijaImi %>% group_by(origin) %>%
                          summarise(number = sum(number, na.rm = TRUE)) %>%
                          arrange(desc(number)), 
                        aes(x = origin, y = number)) + 
  geom_col()

grafSlostock <- ggplot(slovenijaStock %>% filter(age != "total") %>%
                         rename(Starost = age), 
                       aes(x = year, y = number)) + 
  geom_col(position = "dodge", aes(fill = Starost)) +
  geom_line(data = slovenijaStock %>% filter(age != "total") %>%
              group_by(year, gender) %>% summarise(number = mean(number)),
            aes(x = year, y = number)) + 
  facet_grid(~gender) + xlab("Leto") + ylab("Število") + 
  ggtitle("Število imigrantov glede na spol in starost v obdobju 1990-2019")
