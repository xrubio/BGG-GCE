library(RSQLite)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(ggrepel)


library(tidyverse)

conn <- dbConnect(RSQLite::SQLite(), "/home/xrubio/workspace/bgg/db/historical_games.db")

##### procesos preliminares

# Vista de juegos que pasan durante el siglo XX. Solo hace falta ejecutarlo una vez, si no está creado

# create view games20 as select * from game where game.type=='game' and cast(owned as integer)>10 and year>=1970 and year<=2021 and game.id in (select gamePeriod.id_game from gamePeriod where gamePeriod.id_period in (select period.id from period where begin>1899 and begin<2000))

# parejas game-placename para exportar datos a QGIS y realizar el análisis espacial de juegos del siglo XX

games20WithPlace <- dbGetQuery(conn, "select  *  from ( select gamePlace.*, place.continent, place.country, place.region, place.city from gamePlace inner join place on gamePlace.id_place=place.id) as placeDetails inner join (select id, title, year, owned from games20) as basicGames20 on basicGames20.id=placeDetails.id_game")

# drop duplicated column
games20WithPlace$id <- NULL

write.csv(games20WithPlace, "games20WithPlace.csv", row.names=F)

##### FIGURA 5 - comparativa juegos publicados históricos vs otros/zombies

# scifi
scifi <- dbGetQuery(conn, "select * from game where game.type=='game' and cast(owned as integer)>10 and year>=1970 and year<=2021 and game.id in (select gameCategory.id_game from gameCategory where gameCategory.id_category in (select category.id from category where name=='Science Fiction'))")
# zombies
zombies <- dbGetQuery(conn, "select * from game where game.type=='game' and cast(owned as integer)>10 and year>=1970 and year<=2021 and game.id in (select gameCategory.id_game from gameCategory where gameCategory.id_category in (select category.id from category where name=='Zombies'))")
# fantasy
fantasy <- dbGetQuery(conn, "select * from game where game.type=='game' and cast(owned as integer)>10 and year>=1970 and year<=2021 and game.id in (select gameCategory.id_game from gameCategory where gameCategory.id_category in (select category.id from category where name=='Fantasy'))")
# historical
historical <- dbGetQuery(conn, "select * from game where game.type=='game' and cast(owned as integer)>10 and year>=1970 and year<=2021 and  game.id in (select id_game from gamePeriod)")

scifi$genre <- "scifi"
zombies$genre <- "zombies"
fantasy$genre <- "fantasy"
historical$genre <- "historical"
genres <- rbind(scifi, zombies, fantasy, historical)
freqGenres <- genres %>% group_by(year,genre)  %>% summarise(freq=n())


pdf("05_genres.pdf", width=16, height=8)   

# hack; se han modificado los valores para las etiquetas de fantasía/historia en -10/+10 para evitar solapamientos (los valores son demasiado parecidos)
ggplot(freqGenres, aes(x=year, y=freq, col=genre)) + geom_line() + geom_point() + theme_bw() + scale_color_manual(values=c("skyblue3","indianred2","palegreen4", "goldenrod2")) + theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) + xlab("año") +  ylab("juegos publicados")+ annotate("label", x = 2022, y = freqGenres[freqGenres$genre=="fantasy" & freqGenres$year==2021,]$freq-10, label = "fantasía", fill="skyblue3", col="white", size=5, hjust=0) + annotate("label", x = 2022, y =  freqGenres[freqGenres$genre=="scifi" & freqGenres$year==2021,]$freq, label = "scifi", fill="palegreen4", col="white", size=5, hjust=0) + annotate("label", x = 2022, y =  freqGenres[freqGenres$genre=="zombies" & freqGenres$year==2021,]$freq, label = "zombies", fill="goldenrod2", col="white", size=5, hjust=0) + annotate("label", x = 2022, y = freqGenres[freqGenres$genre=="historical" & freqGenres$year==2021,]$freq+10, label = "histórico", fill="indianred2", col="white", size=5, hjust=0)  + scale_x_continuous(breaks=seq(1970,2021,5), limits=c(1970, 2025)) + theme(legend.position="none") 
dev.off()


##### FIGURA 6: producción juegos Spain y SCW

## juegos sobre Spain
spainGames <- dbGetQuery(conn, "select game.* from game where type='game' and cast(owned as integer)>10 and year>1970 and year<2022 and game.id in (select distinct gamePlace.id_game from gamePlace where gamePlace.id_place in (select id from place where country like 'Spain'))")
## juegos SCW

scwGames <- dbGetQuery(conn, "select game.* from game where type='game' and cast(owned as integer)>10 and year>1970 and year<2022 and game.id in (select distinct gamePeriod.id_game from gamePeriod where gamePeriod.id_period in (select id from period where name like 'Spanish Civil War'))")


spainGames$topic <- "Spain"
scwGames$topic <- "Spanish Civil War"

allGames <- rbind(spainGames, scwGames)

pdf("06_spain_scw.pdf", width=10, height=6)   
ggplot(allGames, aes(x=year, fill=topic)) + geom_histogram(binwidth=5, col="white") + theme_bw() + theme(legend.position="none") + scale_fill_manual(values=c("skyblue3","indianred2")) + theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +xlab("año") + ylab("juegos publicados") + facet_wrap(~topic, ncol=2)
dev.off()

##### Figura 7 - TRI Siglo XX

## tabla juegos-períodos

games20Periods <- dbGetQuery(conn, "select * from ( select gamePeriod.*, period.* from gamePeriod inner join period on gamePeriod.id_period=period.id) as periodDetails inner join games20 on games20.id=periodDetails.id_game")
# drop 2 columns
games20Periods$id <- NULL

# añadir begin y end a cada período de la tabla
weighted <- games20Periods %>% 
  group_by(r=row_number()) %>% 
  mutate(custom = list(begin:end)) %>% 
  ungroup %>% select(-r) %>% 
  unnest()

weighted$weight <- 1/(1+weighted$end-weighted$begin)

weighted <- subset(weighted, custom>1899 & custom<=2000)

a <- subset(weighted, custom<=1913)
a$periodSplit <- "" 
b <- subset(weighted, custom>=1914 & custom<=1918)
b$periodSplit <- "1 Guerra Mundial" 
c <- subset(weighted, custom>=1919 & custom<=1935)
c$periodSplit <- "" 
d <- subset(weighted, custom>=1936 & custom<=1938)
d$periodSplit <- "Guerra Civil"
e <- subset(weighted, custom>=1939 & custom<=1945)
e$periodSplit <- "2 Guerra Mundial"
f <- subset(weighted, custom>=1946 & custom<=1949)
f$periodSplit <- "" 
g <- subset(weighted, custom>=1950 & custom<=1953)
g$periodSplit <- "Guerra de Corea"
h <- subset(weighted, custom>=1954 & custom<=2000)
h$periodSplit <- "" 
weighted <- rbind(a,b,c,d,e,f,g,h)


weightedSum <- weighted %>% group_by(custom,periodSplit) %>% summarise_at(vars(weight), list(sum = sum)) 


pdf("07_tri_siglo20.pdf", width=20, height=6)
ggplot(weightedSum, aes(x=custom, y=sum, col=periodSplit, fill=periodSplit)) + geom_bar(stat="identity") + theme_bw() + xlab("año") + ylab("TRI (siglo XX)") + theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))+ theme(legend.position="none") + scale_fill_manual(values=c("grey70", "skyblue3","indianred2","palegreen4","goldenrod2")) + scale_color_manual(values=c("grey70", "skyblue3","indianred2","palegreen4","goldenrod2")) + scale_x_continuous(breaks=c(1900, 1914, 1918, 1936, 1939, 1945, 1950, 1953, 1975, 2000)) + theme(legend.position="none") 
dev.off()

##### Figura 9: TRI para Spain

gamesSpain <- dbGetQuery(conn, "select  *  from ( select gamePeriod.*, period.* from gamePeriod inner join period on gamePeriod.id_period=period.id) as periodDetails inner join ( select game.* from game where type='game' and cast(owned as integer)>10 and year>=1970 and game.id in (select gamePlace.id_game from gamePlace inner join (select * from place where country=='Spain') as spanishPlaces on spanishPlaces.id==gamePlace.id_place )) as gameSpain on gameSpain.id=periodDetails.id_game")

gamesSpain$id <- NULL

weighted <- gamesSpain%>% 
  group_by(r=row_number()) %>% 
  mutate(custom = list(begin:end)) %>% 
  ungroup %>% select(-r) %>% 
  unnest()

weighted$weight <- 1/(1+weighted$end-weighted$begin)

a <- subset(weighted, custom<=1935)
a$periodSplit <- ""
b <- subset(weighted, custom>=1936 & custom<=1939)
b$periodSplit <- "GCE"
c <- subset(weighted, custom>=1940)
c$periodSplit <- "" 

weighted <- rbind(a,b,c)

weighted <- subset(weighted, custom>1500 & custom<=2000)

weightedSum <- weighted %>% group_by(custom,periodSplit) %>% summarise_at(vars(weight), list(sum = sum)) 

pdf("09_tri_spain.pdf", width=20, height=6)

ggplot(weightedSum, aes(x=custom, y=sum, col=periodSplit, fill=periodSplit)) + geom_bar(stat="identity") + theme_bw() + xlab("año") + ylab("TRI (Spain)") + theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))+ scale_fill_manual(values=c("grey70", "indianred2")) + scale_color_manual(values=c("grey70","indianred2")) + scale_x_continuous(breaks=c(1500,1600,1700,1800,1900,1936,2000)) + theme(legend.position="none") 
dev.off()

##### Figura 10, valoracion juegos

pdf("10_ratings.pdf", width=15, height=12)    
ggplot(escalas, aes(y=meanRating, x=copias, col=escala, label=title)) + geom_label_repel(fill="white", na.rm=T, min.segment.length = 0) + geom_point(aes(size=copias))+ theme_bw() + theme(legend.position="bottom") + theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) + xlab("num. copias registradas") + ylab("puntuación media") 
dev.off()


