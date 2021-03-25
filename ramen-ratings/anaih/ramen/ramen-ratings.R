library(dplyr)
library(ggplot2)
library(forcats)
library(hrbrthemes)


# Arrumando BD ------------------------------------------------------------

dados <- read.csv(file = "../data/ramen-ratings.csv",stringsAsFactors = F)
str(dados)

# Vendo pq o Score nao esta numerico = 3 ramens nao scorados
dados %>% 
  count(Stars) %>% 
  as.data.frame()

#Transformando os nao scorados para NA
dados[dados=="Unrated"] <- NA

#Transformando o Score em numerico
dados <- dados %>% 
            mutate(Stars=as.numeric(Stars))

#Olhando quantos niveis tem cada var
dados %>%  
  mutate_if(is.character,as.factor) %>% 
  str()

#tem 10 no top 10? nop
dados %>% 
  count(Top.Ten) %>% 
  as.data.frame() %>% 
  pull(Top.Ten)

#Sera que eh o top 10 por ano? parece que sim
table(dados$Top.Ten)


#arrumando  var Top.Ten
dados <- dados %>% 
          mutate(Top.Ten2=ifelse(Top.Ten %in% c("","\n"),0,1)) 

dados %>% 
  count(Top.Ten,Top.Ten2) %>% 
  as.data.frame()

str(dados)

#Arrumando a var pais para maiusculo 
dados <- dados %>% 
  mutate_if(is.character, toupper)


# BD pais e continente ----------------------------------------------------

dados2 <- read.csv(file = "continents/countries.csv",stringsAsFactors = F)
str(dados2)

#Arrumando a var pais para maiusculo 
dados2 <- dados2 %>% 
  mutate_if(is.character, toupper)

# merge bd ramen e continente ---------------------------------------------

dados3 <- inner_join(dados, dados2, by=c("Country","Country"))


# Arrumar Paises que nao bateram ------------------------------------------

arrumar_pais <- anti_join(dados, dados2, by=c("Country","Country")) 

table(arrumar_pais$Country)
# DUBAI HOLLAND MYANMAR SARAWAK      UK     USA 
# "ASIA","EUROPE","ASIA","ASIA","EUROPE","NORTHERN_AMERICA"
#qual o ome dessa paises na base nova? 
# UNITED KINGDOM	EUROPE	
# UNITED STATES	NORTHERN_AMERICA	
# ASIA

arrumar_pais <- 
  arrumar_pais %>% 
    mutate(Region=ifelse(Country %in% c("DUBAI","MYANMAR","SARAWAK"),"ASIA",ifelse(Country %in% c("HOLLAND","UK"),"EUROPE","NORTHERN_AMERICA")))



# Completar BD com os paises que nao deram match --------------------------

BD_final <- bind_rows(dados3, arrumar_pais) 

#Olhando quantos niveis tem cada var
BD_final %>%  
  mutate_if(is.character,as.factor) %>% 
  str()
# 'data.frame':	2580 obs. of  10 variables:
# $ Review..  : int  
# $ Brand     : Factor w/ 353 levels 
# $ Variety   : Factor w/ 2406 levels 
# $ Style     : Factor w/ 8 levels 
# $ Country   : Factor w/ 38 levels 
# $ Stars     : num  
# $ Top.Ten   : Factor w/ 39 levels 
# $ Top.Ten2  : num  
# $ Region    : Factor w/ 6 levels 
# $ Population: int  



# Ver se tem ramens repetidos ---------------------------------------------

BD_final %>% 
  count(Brand,Variety,Country,Style,Top.Ten2,sort = T) %>% 
  filter(n>1) %>% 
  as.data.frame()
#14 linhas duplicadas e 12 no top 10

BD_final <- 
  BD_final %>% 
    arrange(desc(Review..)) %>% 
    distinct(Brand,Variety,Country,Style, .keep_all = T) 
#2580-14 rep = 2566 rows (excluindo as rep mais antigas, olhando o review)




# descritiva --------------------------------------------------------------

BD_final %>% 
  count(Region,Style) %>% 
  as.data.frame()

BD_final %>% 
  mutate( Style = fct_lump(Style, n = 3),
          Region = fct_lump(Region, n = 3)) %>% 
  count(Region,Style) %>% 
  as.data.frame()%>% 
  ggplot(aes(x=Region, y=n)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() +
  facet_wrap(~Style)

BD_final %>% 
  mutate( Style = fct_lump(Style, n = 3),
          Region = fct_lump(Region, n = 3)) %>% 
  filter(Top.Ten2==1) %>% 
  count(Region,Style) %>% 
  as.data.frame()%>% 
  ggplot(aes(x=Region, y=n)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() +
  facet_wrap(~Style)


BD_final %>% 
  ggplot(aes(Stars)) + 
  geom_histogram() +
  theme_minimal()


BD_final %>% 
  count(Stars,Top.Ten2) %>% 
  as.data.frame() %>% 
  ggplot(aes(Stars, n, col=Top.Ten2, group=Top.Ten2)) + 
  geom_line() +
  theme_minimal()



BD_final %>% 
  count(Stars,Top.Ten2) %>% 
  filter(Top.Ten2==1) %>% 
  as.data.frame() %>% 
  ggplot(aes(Stars, n, col=Top.Ten2, group=Top.Ten2)) + 
  geom_line() +
  theme_minimal()

BD_final %>% 
  mutate( Style = fct_lump(Style, n = 3),
          Region = fct_lump(Region, n = 3)) %>% 
  filter(Top.Ten2==1,Region=="ASIA") %>% 
  count(Country) %>% 
  as.data.frame()%>% 
  ggplot(aes(x=Country, y=n)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() 



# analise -----------------------------------------------------------------


classifica_score <- function(score){
  case_when(score <= .5 ~ "0-0.5",
            score > .5 & score <= 1 ~ "0.5-1",
            score > 1 & score <= 1.5 ~ "1-1.5",
            score > 1.5 & score <= 2 ~ "1.5-2",
            score > 2 & score <= 2.5 ~ "2-2.5",
            score > 2.5 & score <= 3 ~ "2.5-3",
            score > 3 & score <= 3.5 ~ "3-3.5",
            score > 3.5 & score <= 4 ~ "3.5-4",
            score > 4 & score <= 4.5 ~ "4-4.5",
            T ~ "4.5-5")
}

df_acumulado <- 
  BD_final %>% 
  mutate(Stars = as.numeric(Stars)) %>% 
  filter(!is.na(Stars),
         !is.na(Style)) %>%
  mutate(classe_score = classifica_score(Stars),
         Style = fct_lump(Style, n = 3),
         Region = fct_lump(Region, n = 3)) %>% 
  group_by(Style,Region, classe_score) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(classe_score) %>% 
  mutate(classe_score=factor(classe_score,rev(unique(classe_score)))) %>% 
  arrange(Style, Region,classe_score) %>% 
  group_by(Style,Region) %>% 
  mutate(n_acum = cumsum(n),
         pct_acum = n_acum / sum(n)) %>% 
  as.data.frame()


df_acumulado %>% 
  ggplot(aes(classe_score, pct_acum,
             col = Region, group = Region)) + 
  geom_line() +
  facet_wrap(~Style)+
  theme_minimal()

