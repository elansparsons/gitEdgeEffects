library(reshape2)
library(ggplot2)
library(forcats)
library(dplyr)
library(stringr)
begin <- dcast(mergedrefined7, Article.ID ~ variable, fun=toString, value.var="data")
names(begin)[names(begin) == "focal area of research (ecophysiology, population ecology, community ecology ecosystem ecology, animal behavior)"] <- "focal.area.of.research"
cats <- withbroad
begin <- cbind(begin,cats$broad)
names(begin)[names(begin) == "cats$broad"] <- "broad"
names(begin)[names(begin) == "accession number"] <- "accession.n"
names(begin)[names(begin) == "Article.ID"] <- "article.id"
names(begin)[c(9,10)] <- c("archive","arch.y.n")

#data cleaning
begin$Municipality[63] <- "Manaus"
begin[begin=="NA"] <- NA
begin[begin==""] <- NA
begin[begin=="SFRD"] <- "SRFD"
begin[begin=="AF"] <- "WS"
begin[begin=="TF"] <- "TP"
begin[begin=="GT"] <- NA
begin[begin=="IR"] <- "LIT"
begin[begin=="plant, plant"] <- "plant"
remove <- is.na(begin$focal.subject.of.research)
remove2 <- is.na(begin$layout)
begin$focal.subject.of.research[remove==TRUE] <- "plant"
begin$layout[remove2==TRUE] <- "fixed.point"
begin[begin=="transect"] <- "perpendicular"
begin$citations <- as.numeric(begin$citations)
begin$biome <- tolower(begin$biome)



#location data
length(unique(begin$Country))
sort(begin$Country)
a <- ggplot(data = begin, aes(begin$Country))
a + geom_bar()
length(begin[begin$Country=="Mexico"])
popular_countries <-  as.data.frame(sort(table(begin$Country), decreasing=T))
names(popular_countries)[1] <- "Countries"

popular_countries$noper <- gsub("\\."," ",popular_countries$Countries)
#export for python
write.csv(popular_countries,"popular_countries.csv",row.names = FALSE)



b <- ggplot(data = begin, aes(x = fct_infreq(Country))) +
  geom_bar(aes(fill=Country)) + scale_y_continuous(expand=c(0,0), limits=c(0,27))
b <- b + theme_classic() + theme(legend.position="none")
b <- b + theme(axis.text.x = element_text(angle=90, vjust=0, hjust=1))
b

popular.cities <- as.data.frame(head(sort(table(begin$Municipality), decreasing=T)))
popular.cities
#export mergedrefined7 to add simple latitude, first listed only
write.csv(begin,"mergedrefined8.csv",row.names=FALSE)
simplat <- mergedrefined8[,c(1,41)]



#variables data
g <- begin[,c("var.1","var.2","var.3","var.4","var.5","var.6","var.7")]


e <- g[,-c(1)] 
f <- as.data.frame(sort(table(unlist(e)), decreasing = T)) #sorts strings from different variables into correct bins, singular

k <- apply(g,1,sort, na.last=TRUE) #sorts groups of strings
k <- t(k)
j <- apply(format(k),1,paste, collapse=" ")
m <- as.data.frame(sort(table(unlist(k)),decreasing=T)) #new, correct version of f
l <- as.data.frame(sort(table(j), decreasing =T))


g$varcombine <- paste(g$var.1,g$var.2,g$var.3,g$var.4,g$var.5,g$var.6,g$var.7)
i <- g[str_detect(g$varcombine,"(?=.*AT)(?=.*RH)(?=.*VPD)"),] #search by specific string type

#transect info
h <- begin$layout
transect.types = as.data.frame(sort(table(unlist(h)),decreasing = T))
transect.types

#habitat info
sorted.hab <- as.data.frame(sort(table(unlist(begin$biome)),decreasing=T))
biomes <- as.data.frame(sort(table(unlist(begin$broad)),decreasing=T))
biomes$percent <- round(((biomes$Freq/71)*100), digits = 0)
biomes$withdata <- c(24,14,1)
biomes$percentwith <- round(((biomes$withdata/39)*100),digits=0)

biopal <- c("#5DADE2","#F9E79F","#1E8449","#EC7063")

cited.biomes <- ggplot(begin, aes(x=broad, y=citations, fill=broad)) + geom_col() + theme_classic()
cited.biomes + scale_fill_manual(values=biopal) + theme(legend.position = "none")

###citations
#by biome
length(mergedrefined8[mergedrefined8$`was cited before`,]) #96
cite.biome <- mergedrefined8 %>% group_by(broad) %>% summarize(sum(citations)) #all studies

length(unique(mergedrefined8$article.id[mergedrefined8$broad=="temperate"]))
length(unique(mergedrefined8$article.id[mergedrefined8$broad=="tropical"]))
length(unique(mergedrefined8$article.id[mergedrefined8$broad=="boreal"]))

#by country
cite.country <- mergedrefined8 %>% group_by(Country) %>% summarize(sum(citations))

studies.country <-  as.data.frame(sort(table(mergedrefined8$Country), decreasing=T))
names(studies.country) <- c("Country","No_Studies")

cite.country <- merge(cite.country,studies.country,by="Country")

attach(cite.country)
cite.country <- cite.country[order(-`sum(citations)`),]
detach(cite.country)

per.study <- cite.country %>% mutate(cite.per.study = round(`sum(citations)`/No_Studies))

ggplot(per.study,aes(x=No_Studies,y=cite.per.study)) +
  geom_text(aes(label=Country)) +
  coord_cartesian(xlim=c(-5,30)) +
  xlab("Number of Studies")+
  ylab("Citations/Number of Studies")+
  geom_text(data=per.study, aes(x=21,y=41,label="Brazil",color="red"))

portuspan <- c(7,11,12,53,58)
english <- mergedrefined8[!mergedrefined8$article.id %in% portuspan,]

english.cite <- english %>% group_by(Country) %>% summarize(sum(citations))
english.country <-  as.data.frame(sort(table(english$Country), decreasing=T))
#Without Portuguese, 864 citations for Brazil, 21 studies ** x=21,y=41


##years/time
hist(mergedrefined8$Year,breaks=10)
mergedrefined8$Year <- as.factor(mergedrefined8$Year)
years <- mergedrefined8 %>% count(Year)


mergedrefined8$end.date <- as.Date(mergedrefined8$end.date, format = "%m/%d/%Y")
mergedrefined8$end.date.1 <- as.Date(mergedrefined8$end.date.1, format = "%m/%d/%Y")
mergedrefined8$end.date.2 <- as.Date(mergedrefined8$end.date.2, format = "%m/%d/%Y")
mergedrefined8$start.date <- as.Date(mergedrefined8$start.date, format = "%m/%d/%Y")
mergedrefined8$start.date.1 <- as.Date(mergedrefined8$start.date.1, format = "%m/%d/%Y")
mergedrefined8$start.date.2 <- as.Date(mergedrefined8$start.date.2, format = "%m/%d/%Y")

dates <- mergedrefined8[,c(1,17,18,19,71,72,73)]
dates$days <- ifelse(!is.na(dates$start.date), dates$end.date - dates$start.date, 
                     ifelse(!is.na(dates$start.date.1), (dates$end.date.1 - dates$start.date.1)+(dates$end.date.2 - dates$start.date.2),NA))
dates$days[13] <- 12 #fix entry issue where end.date and start.date were swapped

hist(dates$days)
hist(dates$days,breaks=60,xlim=c(0,100))

dates$year <- ifelse(!is.na(dates$end.date), substring(dates$end.date,1,4), 
                     substring(dates$end.date.2,1,4))
dates$year <- as.integer(dates$year)
hist(dates$year)


##archives?
count(mergedrefined8[mergedrefined8$arch.y.n == "Y",1]) #2 of 71

##journal
journals <- as.data.frame(sort(table(mergedrefined8$Journal),decreasing=T)) #most common = FEM 6, BC 5

##area, subject of research
barplot(sort(table(mergedrefined8$focal.area.of.research),decreasing=T))
barplot(sort(table(mergedrefined8$focal.subject.of.research),decreasing=T))


##replicates
hist(mergedrefined8$replicates.habitat.1,breaks=10)
hist(mergedrefined8$replicates.habitat.2)
sort(table(mergedrefined8$replicates.habitat.1),decreasing=T) # 22 of 71 had only one replicate of first habitat
sort(table(mergedrefined8$replicates.habitat.2),decreasing=T)
sort(table(mergedrefined8$replicates.habitat.3),decreasing=T)
sort(table(mergedrefined8$replicates.habitat.4),decreasing=T)

replicates <- mergedrefined8[,c(1,55,56,57,58,59)]
replicates$article.id <- as.factor(replicates$article.id)
replicates$replicates.habitat.1 <- as.factor(replicates$replicates.habitat.1)
replicates$replicates.habitat.2 <- as.factor(replicates$replicates.habitat.2)
replicates$replicates.habitat.3 <- as.factor(replicates$replicates.habitat.3)
replicates$replicates.habitat.4 <- as.factor(replicates$replicates.habitat.4)
replicates$replicates.habitat.5 <- as.factor(replicates$replicates.habitat.5)
reps <- replicates %>% count(replicates.habitat.1)
