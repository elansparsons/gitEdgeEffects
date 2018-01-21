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

biopal <- c("#5DADE2","#F9E79F","#1E8449","#EC7063")

cited.biomes <- ggplot(begin, aes(x=broad, y=citations, fill=broad)) + geom_col() + theme_classic()
cited.biomes + scale_fill_manual(values=biopal) + theme(legend.position = "none")



