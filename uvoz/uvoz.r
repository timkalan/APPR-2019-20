# MIGRACIJA ####
stock <- read_xlsx("podatki/UNstock.xlsx", sheet = 2, range = "A15:BE1997", na = "..")
stock <- stock[!is.na(stock$`Type of data (a)`),] %>% 
  select(-"Type of data (a)", -"Notes", -"Code", country="Major area, region, country or area of destination")
stock[2] <- NULL

stockM <- stock[, c(1:2, 20:36)]
colnames(stockM) <- c("year", "country", 1:17)
stockM <- stockM %>%  
  gather(age, number, "1":"17") %>%
  arrange(year, country) %>%
  mutate(gender="male")

stockF <- stock[, c(1:2, 37:53)]
colnames(stockF) <- c("year", "country", 1:17)
stockF <- stockF %>%  
  gather(age, number, "1":"17") %>%
  arrange(year, country) %>% 
  mutate(gender="female")

stock <- rbind(stockM, stockF) %>% arrange(year, country)
stock <- stock[c(2, 1, 3, 5, 4)]
stock$number <- as.numeric(stock$number)
stock$age <- factor(stock$age, levels = 1:17,
                    labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
                             "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75+", "total"))


migranti <- read_xlsx("podatki/UNdestination.xlsx", sheet = 2, skip = 14, na = "..")
imena <- migranti[1,] %>% 
  select(-"Type of data (a)", -"Notes", -"Code", destination="Major area, region, country or area of destination")
migranti <- migranti[!is.na(migranti$`Type of data (a)`),] %>%
  select(-"Type of data (a)", -"Notes", -"Code", destination="Major area, region, country or area of destination")
migranti[2] <- NULL
imena[2] <- NULL
colnames(migranti)[3:length(colnames(migranti))] <- imena[3:length(colnames(migranti))]
migranti <- migranti %>% gather(origin, number, "Total":"Zimbabwe") %>% 
  arrange(Year, origin, destination)
migranti <- migranti[c(3, 2, 1, 4)] %>% mutate(number = as.numeric(number))



# BDP (wikipedia) ####
url <- "https://en.wikipedia.org/wiki/List_of_countries_by_past_and_projected_GDP_(PPP)"
stran <- read_html(url)

bdpji <- stran %>%
  html_nodes(xpath = "//table[@class='sortable wikitable']//td") %>%
  html_text() %>% str_replace_all(',', '')

bdp <- as.data.frame(matrix(bdpji, ncol = 11, byrow = TRUE))
bdp <- data.frame(lapply(bdp, as.character), stringsAsFactors=FALSE)
bdp <- bdp[1:768, ]
bdp[bdp==""] <- NA
bdp[, 2:11] <- sapply(bdp[, 2:11], as.numeric)

osemdeseta <- bdp[1:192,]
colnames(osemdeseta) <- c("country", 1980:1989)
devetdeseta <- bdp[193:384,]
colnames(devetdeseta) <- c("country", 1990:1999)
dvatisoca <- bdp[385:576,]
colnames(dvatisoca) <- c("country", 2000:2009)
dvadeseta <- bdp[577:768,]
colnames(dvadeseta) <- c("country", 2010:2019)

osemdeseta <- osemdeseta %>% gather(leto, BDP, "1980":"1989")
devetdeseta <- devetdeseta %>% gather(leto, BDP, "1990":"1999")
dvatisoca <- dvatisoca %>% gather(leto, BDP, "2000":"2009")
dvadeseta <- dvadeseta %>% gather(leto, BDP, "2010":"2019")

bdp <- rbind(osemdeseta, devetdeseta, dvatisoca, dvadeseta)
bdp$BDP <- bdp$BDP * 1000000
bdp[, 2] <- sapply(bdp[, 2], as.numeric)



# POPULACIJA ####
pop <- read.csv2("podatki/populacija.csv", skip = 16) %>% 
  filter(Type=="Country/Area") %>%
  select(-"Index", -"Variant", -"Notes", -"Country.code", -"Parent.code", -"Type") 
  
colnames(pop) <- c("country", 1950:2020)
pop[, 2:72] <- as.data.frame(apply(pop[, 2:72],2,function(x)gsub('\\s+', '',x)))
pop[, 1:72] <- sapply(pop[, 1:72], as.character)
pop[, 2:72] <- sapply(pop[, 2:72], as.numeric)
pop <- pop[, c(1, 12:72)] %>% arrange(country) %>%
  gather(leto, populacija, "1960":"2020")
pop$populacija <- pop$populacija * 1000
pop$leto <- as.numeric(pop$leto)



# RELIGIJE ####
religije <- read_csv("podatki/religije.csv", na = c("5000")) %>%
# 5000 v podatkih pomeni zanemarljivo malo
  rename(country = name, christians = chistians) %>%
  mutate(pop2019 = pop2019 * 1000)
colnames(religije) <- c("country", "Kristjani", "Muslimani", "Neopredeljeni", "Hinduisti", 
                        "Budisti", "Pogani", "Drugo", "Judje", "pop2019")
religije <- cbind(religije[, 1], round(religije[, 2:9] / religije$pop2019, digits = 4) * 100) %>%
  gather(Religija, procent, 2:9) %>% arrange(country)



# IZOBRAZBA in HDI####
izobrazba <- read_csv("podatki/education_index.csv", skip = 1, n_max = 189, na = c(".."))
izobrazba <- Filter(function(x)!all(is.na(x)), izobrazba) %>% 
  select(-"HDI Rank (2018)") %>% gather(leto, izobrazenost, "1990":"2018")
izobrazba$leto <- as.numeric(izobrazba$leto)

hdi <- read_csv("podatki/hdi.csv", skip = 1, n_max = 189, na = c(".."))
hdi <- Filter(function(x)!all(is.na(x)), hdi) %>% 
  select(-"HDI Rank (2018)") %>% gather(leto, HDI, "1990":"2018")
hdi$leto <- as.numeric(hdi$leto)



# POPRAVLJANJE IMEN ####
# to bi lahko bilo lepše napisano, a je v trenutni verziji paketa napaka
bdp$country <- standardize.countrynames(bdp$country, suggest = "auto", print.changes = FALSE)
pop$country <- standardize.countrynames(pop$country, suggest = "auto", print.changes = FALSE)
religije$country <- standardize.countrynames(religije$country, suggest = "auto", print.changes = FALSE)
izobrazba$Country <- standardize.countrynames(izobrazba$Country, suggest = "auto", print.changes = FALSE)
hdi$Country <- standardize.countrynames(hdi$Country, suggest = "auto", print.changes = FALSE)
stock$country <- standardize.countrynames(stock$country, suggest = "auto", print.changes = FALSE)
migranti$origin <- standardize.countrynames(migranti$origin, suggest = "auto", print.changes = FALSE)
migranti$destination <- standardize.countrynames(migranti$destination, suggest = "auto", print.changes = FALSE)



# DRZAVE ####
drzave <- filter(bdp, leto > 1989 & leto < 2019) %>%
  inner_join(pop %>% filter(leto > 1989 & leto < 2019), by = c("country", "leto")) %>%
  inner_join(izobrazba, by = c("country" = "Country", "leto")) %>%
  inner_join(hdi, by = c("country" = "Country", "leto")) %>%
  mutate(BDPpc=BDP/populacija)


rm(devetdeseta, dvadeseta, dvatisoca, osemdeseta, 
   stran, bdpji, url, bdp, izobrazba, hdi, pop, stockF, stockM, imena)



# ZEMLJEVID ####
svet <- uvozi.zemljevid(
  "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
  "ne_50m_admin_0_countries", encoding="UTF-8")
svet$NAME <- standardize.countrynames(svet$NAME, suggest = "auto", print.changes = FALSE) 
