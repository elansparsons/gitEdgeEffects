#Quantitative data only

library(dplyr) #Version 0.7.4
library(tidyr) #Version 0.7.2
library(ggplot2) #Version 2.2.1
library(stringr) #Version 1.2.0
library(gridExtra) #Version 2.3
library(lme4) #Version 1.1-14
library(MASS) #Version 7.3-47
library(readr) #Version 1.1.1
library(MuMIn)

AT <- read_csv("./Data/AT_seg.csv")

###########air temperature ####
rem <- c(-30,-25,-20,-15)
AT <- AT[!AT$dist %in% rem,] #all -30,-20,-15 distances unnecessary

length(unique(AT$article.id))


#distances
distances <- as.data.frame(table(AT$dist))
mean(interiors$dist)
#find interior point
interiors <- AT %>% group_by(article.id) %>% summarize(dist = max(dist))
top <- AT %>% group_by(article.id,segment_n) %>% slice(which.max(dist))
zero <- AT %>% group_by(article.id, segment_n) %>% slice(which(dist==0))

sep <- merge(AT, top[,c(1,2,3,4,5)], by = c("article.id","segment_n"))
colnames(sep)[c(3,4,5,16,17,18)] <- c("just.temp","just.dist","just.diff","max.airtemp","max.dist", "max.diff")


#find temp differences
#so <- sep$article.id == 71
#sep <- sep[!so,]

sep$full_diff <- ifelse(is.na(sep$just.diff), sep$full_diff <- sep$just.temp - sep$max.airtemp, NA)
sep$other.diff <- ifelse(sep$notes == "comparison to edge",sep$other.diff <- sep$just.diff - sep$max.diff, NA)


#combine columns of correct data into single
sep$other.diff <- ifelse(sep$notes == "comparison to edge", sep$other.diff, sep$other.diff <- sep$just.diff)
sep$full_diff <- ifelse(!is.na(sep$other.diff), sep$other.diff,sep$full_diff)
#fix id 55
sep$full_diff[sep$article.id == 55] <- sep$just.diff[sep$article.id == 55]
#for later comparison to variances
sepAT <- sep
#divide to make everything relative
sep$percent_diff <- round((sep$full_diff/sep$max.airtemp)*100)
#minimize to single transect per article
sep$idseg <- paste(sep[,1],sep[,2])
grouped <- sep %>% group_by(article.id,just.dist) %>% mutate(avg.diff = round(mean(full_diff),2),avg.var = round(mean(SD_SE_CI_V_n),2))
oneonly <- grouped[is.na(sep$segment_n)|sep$segment_n == "a",]
  


#TO ANALYZE, SHORT
short <- sep[,c(1,2,4,6,7,8,9,10,11,13,14,19)]
short$article.id <- as.factor(short$article.id)

short$idseg <- paste(short[,1],short[,2])

#no interior points shown
noint <- sep[!sep$just.dist == sep$max.dist,]
noint <- noint[,c(1,2,4,6,7,8,9,10,11,13,14,19)]
noint$idseg <- paste(noint[,1],noint[,2])





#matrices
matrices <- as.data.frame(sort(table(AT$matrix_type),decreasing = T))
length(unique(AT$article.id[AT$matrix_type=="pasture"])) #number of studies using matrix type
bit <- AT %>% group_by(article.id, matrix_type) %>% summarize()
bitn <- as.data.frame(sort(table(bit$matrix_type), decreasing = T)) #number of studies over all matrix types

#data to spread
clip <- AT[-c(423,495,155),-c(6,7,12,13,14,15)]
wide <- clip %>% spread(dist,air_temp,fill=NA,convert=FALSE)

#simple graphs
#simple AT
ggplot(AT, aes(x=dist, y=air_temp)) + geom_point()
ggplot(short, aes(x=just.dist, y=full_diff)) + geom_point(aes(color=article.id)) + 
  geom_smooth(method = "auto") + scale_x_continuous(breaks=pretty(short$just.dist,n=40)) + scale_y_continuous(breaks=pretty(short$full_diff,n=10)) + 
  coord_cartesian(ylim=c(-5,10),xlim=c(-10,200)) + geom_hline(yintercept = 0)
#by each plot per article
plot <- ggplot(short, aes(x=just.dist, y=full_diff,group=idseg)) + scale_x_continuous(breaks=pretty(short$just.dist,n=40)) + 
  scale_y_continuous(breaks=pretty(short$full_diff,n=10)) + coord_cartesian(ylim=c(-5,10),xlim=c(-10,200))
plot + geom_line(color = "blue",alpha=.2)
#relative to interior point
relative <- ggplot(subset(short,!is.na(percent_diff)), aes(x=just.dist, y=percent_diff))
relative + geom_point() + geom_smooth(method="loess",formula=y~x) + coord_cartesian(ylim=c(-10,100),xlim=c(-10,200)) +
  scale_x_continuous(breaks=pretty(short$just.dist,n=30))
#without interior points
no_interior <- ggplot(subset(noint,!is.na(percent_diff)), aes(x=just.dist, y=percent_diff))
no_interior + geom_point() + geom_smooth(method="loess",formula=y~x) + coord_cartesian(ylim=c(-10,90),xlim=c(-10,140)) +
  scale_x_continuous(breaks=pretty(short$just.dist,n=30)) + scale_y_continuous(breaks=pretty(short$percent_diff,n=10)) + xlab("Distance") +
  ylab("% difference from interior") + geom_point(data=data.frame(x=104,y=0),aes(x,y),color="red",size=4)
#average of values based on article.id
avg.id <- ggplot(subset(oneonly,!is.na(percent_diff)), aes(x=just.dist,y = percent_diff))
avg.id + geom_point() + geom_smooth(method="loess",formula = y~x) + coord_cartesian(ylim=c(-10,100),xlim=c(-10,200)) + 
  scale_x_continuous(breaks=pretty(short$just.dist,n=30)) + scale_y_continuous(breaks=pretty(short$percent_diff,n=10))
 

#######################relative humidity ####

RH <- read_csv("./Data/RH_seg.csv")

rem <- c(-30,-25,-20,-15)
RH <- RH[!RH$dist %in% rem,] #all -30,-20,-15 distances unnecessary
names(RH)[names(RH) == "SD_SE_CI_V"] <-"RH_var"
names(RH)[names(RH) == "SD_SE_CI_V_n"] <- "RH_var_n"

length(unique(RH$article.id))

#distances
distances <- as.data.frame(table(RH$dist))
round(mean(interiors$dist))
#find interior point
interiors <- RH %>% group_by(article.id) %>% summarize(dist = max(dist))
top <- RH %>% group_by(article.id,segment_n) %>% slice(which.max(dist))
zero <- RH %>% group_by(article.id, segment_n) %>% slice(which(dist==0))

sep <- merge(RH, top[,c(1,2,3,4,5)], by = c("article.id","segment_n"))
colnames(sep)[c(3,4,5,16,17,18)] <- c("just.humid","just.dist","just.diff","max.humid","max.dist", "max.diff")

sep$fullrh_diff <- ifelse(is.na(sep$just.diff), sep$fullrh_diff <- sep$just.humid - sep$max.humid, NA)
sep$other.diff <- ifelse(sep$notes == "comparison to edge",sep$other.diff <- sep$just.diff - sep$max.diff, NA)

#combine columns of correct data into single
sep$other.diff <- ifelse(sep$notes == "comparison to edge", sep$other.diff, sep$other.diff <- sep$just.diff)
sep$fullrh_diff <- ifelse(!is.na(sep$other.diff), sep$other.diff,sep$fullrh_diff)

#for later comparison to tolerances
sepRH <- sep

#divide to make everything relative
sep$percentrh_diff <- ifelse(!is.na(sep$percent_diff),sep$percent_diff,round((sep$fullrh_diff/sep$max.humid)*100))
#minimize to single transect per article
sep$idseg <- paste(sep[,1],sep[,2])
groupedrh <- sep %>% group_by(article.id,just.dist) %>% mutate(avgrh.diff = round(mean(fullrh_diff),2),avg.rhvar = round(mean(RH_var_n),2))
oneonlyrh <- groupedrh[is.na(sep$segment_n)|sep$segment_n == "a",]

#TO ANALYZE, SHORT
short.rh <- sep[,c(1,2,4,6,8,9,10,11,13,14,19,21)]
short.rh$article.id <- as.factor(short.rh$article.id)

short.rh$idseg <- paste(short.rh[,1],short.rh[,2])

#no interior points
noint <- sep[!sep$just.dist == sep$max.dist,]
noint <- noint[,c(1,2,4,6,8,9,10,11,13,14,19)]
noint$idseg <- paste(noint[,1],noint[,2])

#graphs
#relative to interior point
relative <- ggplot(subset(short,!is.na(percentrh_diff)), aes(x=just.dist, y=percentrh_diff))
relative + geom_point() + geom_smooth(method="loess",formula=y~x) + coord_cartesian(ylim=c(-30,5),xlim=c(-10,250)) +
  scale_x_continuous(breaks=pretty(short$just.dist,n=30))
#average of values based on article.id
avg.id <- ggplot(subset(oneonlyrh,!is.na(percentrh_diff)), aes(x=just.dist,y = percentrh_diff))
avg.id + geom_point() + geom_smooth(method="loess",formula = y~x) + coord_cartesian(ylim=c(-30,5),xlim=c(-10,250)) + 
  scale_x_continuous(breaks=pretty(short$just.dist,n=30)) + scale_y_continuous(breaks=pretty(short$percentrh_diff,n=10))



###########################soil temperature ####

ST <- read_csv("./Data/ST_seg.csv")

rem <- c(-30,-25,-20,-15)
ST <- ST[!ST$dist %in% rem,] #all -30,-20,-15 distances unnecessary
names(ST)[names(ST) == "SD_SE_CI_V"] <-"ST_var"
names(ST)[names(ST) == "SD_CI_V_n"] <- "ST_var_n"

length(unique(ST$article.id))

interiors <- ST %>% group_by(article.id) %>% summarize(dist = max(dist))
top <- ST %>% group_by(article.id,segment_n) %>% slice(which.max(dist))
zero <- ST %>% group_by(article.id, segment_n) %>% slice(which(dist==0))

sep <- merge(ST, top[,c(1,2,3,4,5)], by = c("article.id","segment_n"))
colnames(sep)[c(3,4,5,16,17,18)] <- c("just.temp","just.dist","just.diff","max.temp","max.dist", "max.diff")

sep$fullst_diff <- ifelse(is.na(sep$just.diff), sep$fullst_diff <- sep$just.temp - sep$max.temp, NA)
sep$other.diff <- ifelse(sep$notes == "comparison to edge",sep$other.diff <- sep$just.diff - sep$max.diff, NA)

#combine columns of correct data into single
sep$other.diff <- ifelse(sep$notes == "comparison to edge", sep$other.diff, sep$other.diff <- sep$just.diff)
sep$fullst_diff <- ifelse(!is.na(sep$other.diff), sep$other.diff,sep$fullst_diff)

#for later comparison to tolerances
sepST <- sep

#divide to make everything relative
sep$percentst_diff <- ifelse(!is.na(sep$percent_diff),sep$percent_diff,round((sep$fullst_diff/sep$max.temp)*100))
#minimize to single transect per article
sep$idseg <- paste(sep[,1],sep[,2])
groupedst <- sep %>% group_by(article.id,just.dist) %>% mutate(avgst.diff = round(mean(fullst_diff),2),avg.stvar = round(mean(ST_var_n),2))
oneonlyst <- groupedst[is.na(sep$segment_n)|sep$segment_n == "a",]

#TO ANALYZE, SHORT
short.st <- sep[,c(1,2,4,6,8,9,10,11,13,14,19,21)]
short.st$article.id <- as.factor(short.st$article.id)

short.st$idseg <- paste(short.st[,1],short.st[,2])

#no interior points
noint <- sep[!sep$just.dist == sep$max.dist,]
noint <- noint[,c(1,2,4,6,8,9,10,11,13,14,19)]
noint$idseg <- paste(noint[,1],noint[,2])

#graphs
#relative to interior point
relative <- ggplot(subset(short,!is.na(percentst_diff)), aes(x=just.dist, y=percentst_diff))
relative + geom_point() + geom_smooth(method="loess",formula=y~x) + coord_cartesian(ylim=c(-20,90),xlim=c(-10,250)) +
  scale_x_continuous(breaks=pretty(short$just.dist,n=30))
#average of values based on article.id
avg.id <- ggplot(subset(oneonlyst,!is.na(percentst_diff)), aes(x=just.dist,y = percentst_diff))
avg.id + geom_point() + geom_smooth(method="loess",formula = y~x) + coord_cartesian(ylim=c(-20,90),xlim=c(-10,250)) + 
  scale_x_continuous(breaks=pretty(short$just.dist,n=30)) + scale_y_continuous(breaks=pretty(short$percentst_diff,n=10))


############################soil moisture ####

SM <- read_csv("./Data/SM_seg.csv")

rem <- c(-30,-25,-20,-15)
SM <- SM[!SM$dist %in% rem,] #all -30,-20,-15 distances unnecessary
names(SM)[names(SM) == "SD_SE_CI_V"] <-"SM_var"
names(SM)[names(SM) == "SD_SE_CI_V_n"] <- "SM_var_n"

length(unique(SM$article.id))

interiors <- SM %>% group_by(article.id) %>% summarize(dist = max(dist))
top <- SM %>% group_by(article.id,segment_n) %>% slice(which.max(dist))
zero <- SM %>% group_by(article.id, segment_n) %>% slice(which(dist==0))

sep <- merge(SM, top[,c(1,2,3,4,5)], by = c("article.id","segment_n"))
colnames(sep)[c(3,4,5,16,17,18)] <- c("just.humid","just.dist","just.diff","max.humid","max.dist", "max.diff")

sep$fullsm_diff <- ifelse(is.na(sep$just.diff), sep$fullsm_diff <- sep$just.humid - sep$max.humid, NA)
sep$other.diff <- ifelse(sep$notes == "comparison to edge",sep$other.diff <- sep$just.diff - sep$max.diff, NA)

#combine columns of correct data into single
sep$other.diff <- ifelse(sep$notes == "comparison to edge", sep$other.diff, sep$other.diff <- sep$just.diff)
sep$fullsm_diff <- ifelse(!is.na(sep$other.diff), sep$other.diff,sep$fullsm_diff)

#for later comparison to tolerances
sepSM <- sep

#divide to make everything relative
sep$percentsm_diff <- ifelse(!is.na(sep$percent_diff),sep$percent_diff,round((sep$fullsm_diff/sep$max.humid)*100))
#minimize to single transect per article
sep$idseg <- paste(sep[,1],sep[,2])
groupedsm <- sep %>% group_by(article.id,just.dist) %>% mutate(avgsm.diff = round(mean(fullsm_diff),2),avg.smvar = round(mean(SM_var_n),2))
oneonlysm <- groupedsm[is.na(sep$segment_n)|sep$segment_n == "a",]

#TO ANALYZE, SHORT
short.sm <- sep[,c(1,2,4,6,8,9,10,11,13,14,19,21)]
short.sm$article.id <- as.factor(short.sm$article.id)

short.sm$idseg <- paste(short.sm[,1],short.sm[,2])

#no interior points shown
noint <- sep[!sep$just.dist == sep$max.dist,]
noint <- noint[,c(1,2,4,6,8,9,10,11,13,14,19)]
noint$idseg <- paste(noint[,1],noint[,2])

#graphs
#relative to interior point
relative <- ggplot(subset(short,!is.na(percentsm_diff)), aes(x=just.dist, y=percentsm_diff))
relative + geom_point() + geom_smooth(method="loess",formula = y~x) + coord_cartesian(xlim=c(-10,250)) +
  scale_x_continuous(breaks=pretty(short$just.dist,n=30))
#average of values based on article.id
avg.id <- ggplot(subset(oneonlysm,!is.na(percentsm_diff)), aes(x=just.dist,y = percentsm_diff))
avg.id + geom_point() + geom_smooth(method="loess",formula = y~x) + coord_cartesian(xlim=c(-10,250)) + 
  scale_x_continuous(breaks=pretty(short$just.dist,n=30)) + scale_y_continuous(breaks=pretty(short$percentsm_diff,n=10))



###########################photosynthetically active radiation ####

PAR <- read_csv("./Data/PAR_seg.csv")

rem <- c(-30,-25,-20,-15)
PAR <- PAR[!PAR$dist %in% rem,] #all -30,-20,-15 distances unnecessary
names(PAR)[names(PAR) == "SD_SE_CI_V"] <-"PAR_var"
names(PAR)[names(PAR) == "SD_SE_CI_V_n"] <- "PAR_var_n"

length(unique(PAR$article.id))

interiors <- PAR %>% group_by(article.id) %>% summarize(dist = max(dist))
top <- PAR %>% group_by(article.id,segment_n) %>% slice(which.max(dist))
zero <- PAR %>% group_by(article.id, segment_n) %>% slice(which(dist==0))

sep <- merge(PAR, top[,c(1,2,3,4,5,6)], by = c("article.id","segment_n"))
colnames(sep)[c(3,5,6,17,18,19,20)] <- c("just.PAR","just.dist","just.diff","max.PAR","log.max.PAR","max.dist", "max.diff")

sep$notes <- as.character(sep$notes)
sep$notes <- str_split_fixed(sep$notes,"\\.",n=2)


sep$fullPAR_diff <- ifelse(is.na(sep$just.diff), sep$fullPAR_diff <- sep$just.PAR - sep$max.PAR, NA)
sep$other.diff <- ifelse(sep$notes == "comparison to edge",sep$other.diff <- sep$just.diff - sep$max.diff, NA)


#combine columns of correct data into single
sep$other.diff <- ifelse(sep$notes == "comparison to edge", sep$other.diff, sep$other.diff <- sep$just.diff)
sep$fullPAR_diff <- ifelse(!is.na(sep$other.diff), sep$other.diff,sep$fullPAR_diff)
sep$fullPAR_diff <- ifelse(!is.na(sep$fullPAR_diff), sep$fullPAR_diff,sep$log_value.x)
sep$fullPAR_diff <- ifelse(!is.na(sep$fullPAR_diff), sep$fullPAR_diff,sep$percent_diff)

#for later comparison to tolerances
sepPAR <- sep

#divide to make everything relative
sep$percentPAR_diff <- ifelse(!is.na(sep$percent_diff),sep$percent_diff,round((sep$fullPAR_diff/sep$max.PAR)*100))
#minimize to single transect per article
sep$idseg <- paste(sep[,1],sep[,2])
groupedPAR <- sep %>% group_by(article.id,just.dist) %>% mutate(avgPAR.diff = round(mean(fullPAR_diff),2),avg.PARvar = round(mean(PAR_var_n),2))


#TO ANALYZE, SHORTS
short.par <- sep[,c(1,2,5,6,9,10,11,12,13,14,15,23)]
short.par$article.id <- as.factor(short.par$article.id)

short.par$idseg <- paste(short.par[,1],short.par[,2])

#no interior points shown
noint <- sep[!sep$just.dist == sep$max.dist,]
noint <- noint[,c(1,2,4,6,8,9,10,11,13,14,19)]
noint$idseg <- paste(noint[,1],noint[,2])

#graphs
#relative to interior point
relative <- ggplot(subset(short,!is.na(percentPAR_diff)), aes(x=just.dist, y=percentPAR_diff))
relative + geom_point() + geom_smooth(method="loess",formula = y~x) + coord_cartesian(xlim=c(-10,250)) +
  scale_x_continuous(breaks=pretty(short$just.dist,n=30))


##########################vapor pressure deficit ####

VPD <- read_csv("./Data/VPD_seg.csv")

rem <- c(-30,-25,-20,-15)
VPD <- VPD[!VPD$dist %in% rem,] #all -30,-20,-15 distances unnecessary
names(VPD)[names(VPD) == "SD_SE_CI_V"] <-"VPD_var"
names(VPD)[names(VPD) == "SD_SE_CI_V_n"] <- "VPD_var_n"

length(unique(VPD$article.id))

interiors <- VPD %>% group_by(article.id) %>% summarize(dist = max(dist))
top <- VPD %>% group_by(article.id,segment_n) %>% slice(which.max(dist))
zero <- VPD %>% group_by(article.id, segment_n) %>% slice(which(dist==0))

sep <- merge(VPD, top[,c(1,2,3,4,5)], by = c("article.id","segment_n"))
colnames(sep)[c(3,4,5,16,17,18)] <- c("just.vpd","just.dist","just.diff","max.vpd","max.dist", "max.diff")

sep$fullVPD_diff <- ifelse(is.na(sep$just.diff), sep$fullVPD_diff <- sep$just.vpd - sep$max.vpd, NA)
sep$other.diff <- ifelse(sep$notes == "comparison to edge",sep$other.diff <- sep$just.diff - sep$max.diff, NA)

#combine columns of correct data into single
sep$other.diff <- ifelse(sep$notes == "comparison to edge", sep$other.diff, sep$other.diff <- sep$just.diff)
sep$fullVPD_diff <- ifelse(!is.na(sep$other.diff), sep$other.diff,sep$fullVPD_diff)

#for later comparison to tolerances
sepVPD <- sep

#divide to make everything relative
sep$percentVPD_diff <- ifelse(!is.na(sep$percent_diff),sep$percent_diff,round((sep$fullVPD_diff/sep$max.vpd)*100))
#minimize to single transect per article
sep$idseg <- paste(sep[,1],sep[,2])
groupedvpd <- sep %>% group_by(article.id,just.dist) %>% mutate(avgvpd.diff = round(mean(fullVPD_diff),2),avg.vpdvar = round(mean(VPD_var_n),2))
oneonlyvpd <- groupedvpd[is.na(sep$segment_n)|sep$segment_n == "a",]

#TO ANALYZE, SHORT
short.vpd <- sep[,c(1,2,4,6,8,9,10,11,13,14,19,21)]
short.vpd$article.id <- as.factor(short.vpd$article.id)

short.vpd$idseg <- paste(short.vpd[,1],short.vpd[,2])

#no interior points shown
noint <- sep[!sep$just.dist == sep$max.dist,]
noint <- noint[,c(1,2,4,6,8,9,10,11,13,14,19)]
noint$idseg <- paste(noint[,1],noint[,2])

#graphs
#relative to interior point
relative <- ggplot(subset(short,!is.na(percentVPD_diff)), aes(x=just.dist, y=percentVPD_diff))
relative + geom_point() + geom_smooth(method="loess",formula = y~x) + coord_cartesian(xlim=c(-10,250)) +
  scale_x_continuous(breaks=pretty(short$just.dist,n=30))
#average of values based on article.id
avg.id <- ggplot(subset(oneonlyvpd,!is.na(percentVPD_diff)), aes(x=just.dist,y = percentVPD_diff))
avg.id + geom_point() + geom_smooth(method="loess",formula = y~x) + coord_cartesian(xlim=c(-10,250)) + 
  scale_x_continuous(breaks=pretty(short$just.dist,n=30))



##########################wind speed ####

WS <- read_csv("./Data/WS_seg.csv")

rem <- c(-30,-25,-20,-15)
WS <- WS[!WS$dist %in% rem,] #all -30,-20,-15 distances unnecessary
names(WS)[names(WS) == "SD_SE_CI_V"] <-"WS_var"
names(WS)[names(WS) == "SD_SE_CI_V_n"] <- "WS_var_n"

length(unique(WS$article.id))

interiors <- WS %>% group_by(article.id) %>% summarize(dist = max(dist))
top <- WS %>% group_by(article.id,segment_n) %>% slice(which.max(dist))
zero <- WS %>% group_by(article.id, segment_n) %>% slice(which(dist==0))

sep <- merge(WS, top[,c(1,2,3,4,5)], by = c("article.id","segment_n"))
colnames(sep)[c(3,4,5,16,17,18)] <- c("just.ws","just.dist","just.diff","max.ws","max.dist", "max.diff")

sep$fullws_diff <- ifelse(is.na(sep$just.diff), sep$fullws_diff <- sep$just.ws - sep$max.ws, NA)
sep$other.diff <- ifelse(sep$notes == "comparison to edge",sep$other.diff <- sep$just.diff - sep$max.diff, NA)

#combine columns of correct data into single
sep$other.diff <- ifelse(sep$notes == "comparison to edge", sep$other.diff, sep$other.diff <- sep$just.diff)
sep$fullws_diff <- ifelse(!is.na(sep$other.diff), sep$other.diff,sep$fullws_diff)

#for later comparison to tolerances
sepWS <- sep

#divide to make everything relative
sep$percentws_diff <- ifelse(!is.na(sep$percent_diff),sep$percent_diff,round((sep$fullws_diff/sep$max.ws)*100))
#remove article 56 because it can't be converted to percentages
fivesix <- 56
sep <- sep[!sep$article.id %in% fivesix,]


#minimize to single transect per article
sep$idseg <- paste(sep[,1],sep[,2])
groupedws <- sep %>% group_by(article.id,just.dist) %>% mutate(avgws.diff = round(mean(fullws_diff),2),avg.wsvar = round(mean(WS_var_n),2))
oneonlyws <- groupedws[is.na(sep$segment_n)|sep$segment_n == "a",]

#TO ANALYZE, SHORT
short.ws <- sep[,c(1,2,4,6,8,9,10,11,13,14,19,21,22)]
short.ws$article.id <- as.factor(short.ws$article.id)

short.ws$idseg <- paste(short.ws[,1],short.ws[,2])

#no interior points shown
noint <- sep[!sep$just.dist == sep$max.dist,]
noint <- noint[,c(1,2,4,6,8,9,10,11,13,14,19)]
noint$idseg <- paste(noint[,1],noint[,2])

#graphs
#relative to interior point
relative <- ggplot(subset(short,!is.na(percentws_diff)), aes(x=just.dist, y=percentws_diff))
relative + geom_point() + geom_smooth(method="loess",formula = y~x) + coord_cartesian(xlim=c(-10,250)) +
  scale_x_continuous(breaks=pretty(short$just.dist,n=30))
#average of values based on article.id
avg.id <- ggplot(subset(oneonlyws,!is.na(percentws_diff)), aes(x=just.dist,y = percentws_diff))
avg.id + geom_point() + geom_smooth(method="loess",formula = y~x) + coord_cartesian(xlim=c(-10,250)) + 
  scale_x_continuous(breaks=pretty(short$just.dist,n=30))



#################COMBINE ALL ####
combined <- merge(short, short.par[,c(1,2,3,10,11,12,13)], by = c("article.id","segment_n","just.dist","idseg"), all=TRUE)
combined1 <- merge(combined, short.rh[,c(1,2,3,9,10,12,13)], by = c("article.id","segment_n","just.dist","idseg"), all=TRUE)
combined2 <- merge(combined1, short.sm[,c(1,2,3,9,10,12,13)], by = c("article.id","segment_n","just.dist","idseg"), all=TRUE)
combined3 <- merge(combined2, short.st[,c(1,2,3,9,10,12,13)], by = c("article.id","segment_n","just.dist","idseg"), all=TRUE)
combined4 <- merge(combined3, short.vpd[,c(1,2,3,9,10,12,13)], by = c("article.id","segment_n","just.dist","idseg"), all=TRUE)
combined5 <- merge(combined4, short.ws[,c(1,2,3,9,10,12,13)], by = c("article.id","segment_n","just.dist","idseg"), all=TRUE)

#check - how many rows with all variables?
full <- data.frame(combined5[!is.na(combined5[,6]) & !is.na(combined5[,16]) & !is.na(combined5[,19]) &
  !is.na(combined5[,22]) & !is.na(combined5[,25]) & !is.na(combined5[,28]) &
  !is.na(combined5[,31]),])
  
#export combined dataset
write.csv(combined5,"./Outputs/vardata.csv",row.names = F)

#number articles with data = 39
length(unique(vardata$article.id))


#graphs of all variables ####
ggplot(combined5,aes(x = just.dist)) +
  geom_smooth(aes(y=percent_diff), color = "green",alpha=0) +
  geom_smooth(aes(y=percentrh_diff), color = "red",alpha=0) +
  geom_smooth(aes(y=percentPAR_diff), color = "blue",alpha=0) +
  geom_smooth(aes(y=percentsm_diff), color = "cyan",alpha=0) +
  geom_smooth(aes(y=percentst_diff), color = "orange",alpha=0) +
  geom_smooth(aes(y=percentVPD_diff), color = "purple",alpha=0) +
  geom_smooth(aes(y=percentws_diff), color = "yellow",alpha=0) +
  coord_cartesian(xlim=c(-10,250))

ggplot(combined5,aes(x = just.dist)) +
  geom_smooth(aes(y=percent_diff), color = "green",alpha=0) +
  geom_smooth(aes(y=percentrh_diff), color = "red",alpha=0) +
  geom_smooth(aes(y=percentsm_diff), color = "cyan",alpha=0) +
  geom_smooth(aes(y=percentst_diff), color = "orange",alpha=0) +
  coord_cartesian(xlim=c(-10,250))


aa <- ggplot(combined5,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Temperature and humidity")+
  geom_smooth(aes(y=percent_diff), color = "lightslateblue",alpha=0) +
  geom_smooth(aes(y=percentrh_diff), color = "indianred3",alpha=0) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  coord_cartesian(xlim=c(-10,250))

aab <- ggplot(combined5,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Temperature and humidity")+
  geom_point(aes(y=percent_diff), color = "lightslateblue") +
  geom_point(aes(y=percentrh_diff), color = "indianred3") +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  coord_cartesian(xlim=c(-10,250))

aabb <- ggplot(combined5,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("VPD")+
  geom_point(aes(y=percentVPD_diff), color = "lightslateblue") +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  coord_cartesian(xlim=c(-10,250))

ab <- ggplot(combined5,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Soil")+
  geom_smooth(aes(y=percentsm_diff), color = "turquoise3",alpha=0) +
  geom_smooth(aes(y=percentst_diff), color = "goldenrod2",alpha=0) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  coord_cartesian(xlim=c(-10,250))

abb <- ggplot(combined5,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Soil")+
  geom_point(aes(y=percentsm_diff), color = "turquoise3") +
  geom_point(aes(y=percentst_diff), color = "goldenrod2") +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  coord_cartesian(xlim=c(-10,250))

ac <- ggplot(combined5,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Light")+
  geom_smooth(aes(y=percentPAR_diff), color = "maroon2",alpha=0) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  coord_cartesian(xlim=c(-10,250))

acb <- ggplot(combined5,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Light")+
  geom_point(aes(y=percentPAR_diff), color = "maroon2") +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  coord_cartesian(xlim=c(-10,250))

ad <- ggplot(combined5,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Wind")+
  geom_smooth(aes(y=percentws_diff), color = "thistle4",alpha=0) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  coord_cartesian(xlim=c(-10,250))

adb <- ggplot(combined5,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Wind")+
  geom_point(aes(y=percentws_diff), color = "thistle4") +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  coord_cartesian(xlim=c(-10,250))

grid.arrange(aa,ab,ac,ad,ncol=2,nrow=2)

ggplot(combined5,aes(y=percent_diff,x=just.dist)) + geom_point(color="green") +
  geom_smooth(color="blue")

ae <- ggplot(combined5,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Soil")+
  geom_smooth(aes(y=percentsm_diff), color = "turquoise3",alpha=0) +
  geom_point(aes(y=percentsm_diff, fill = article.id)) +
  geom_text(aes(y=percentsm_diff, label= article.id)) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  coord_cartesian(xlim=c(-10,250))

#individually graphed
ea <- ggplot(combined5,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Air temperature")+
  geom_point(aes(y=percent_diff),color="lightslateblue",alpha=0.2) +
  geom_line(aes(y=percent_diff,group=idseg),color="lightslateblue",alpha=0.2)+
  geom_smooth(aes(y=percent_diff), color = "thistle4",alpha=0,size=1.5) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  coord_cartesian(xlim=c(-10,250))

eb <- ggplot(combined5,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Relative humidity")+
  geom_point(aes(y=percentrh_diff),color="indianred3",alpha=0.2) +
  geom_line(aes(y=percentrh_diff,group=idseg),color="indianred3",alpha=0.2)+
  geom_smooth(aes(y=percentrh_diff), color = "thistle4",alpha=0,size=1.5) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  coord_cartesian(xlim=c(-10,250))

ec <- ggplot(combined5,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("VPD")+
  geom_point(aes(y=percentVPD_diff),color="darkviolet",alpha=0.2) +
  geom_line(aes(y=percentVPD_diff,group=idseg),color="darkviolet",alpha=0.2)+
  geom_smooth(aes(y=percentVPD_diff), color = "thistle4",alpha=0,size=1.5) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  coord_cartesian(xlim=c(-10,250))

ed <- ggplot(combined5,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Soil temperature")+
  geom_point(aes(y=percentst_diff),color="goldenrod2",alpha=0.2) +
  geom_line(aes(y=percentst_diff,group=idseg),color="goldenrod2",alpha=0.2)+
  geom_smooth(aes(y=percentst_diff), color = "thistle4",alpha=0,size=1.5) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  coord_cartesian(xlim=c(-10,250))

ee <- ggplot(combined5,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Soil moisture")+
  geom_point(aes(y=percentsm_diff),color="turquoise3",alpha=0.2) +
  geom_line(aes(y=percentsm_diff,group=idseg),color="turquoise3",alpha=0.2)+
  geom_smooth(aes(y=percentsm_diff), color = "thistle4",alpha=0,size=1.5) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  coord_cartesian(xlim=c(-10,250))

ef <- ggplot(combined5,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Light (PAR)")+
  geom_point(aes(y=percentPAR_diff),color="maroon2",alpha=0.2) +
  geom_line(aes(y=percentPAR_diff,group=idseg),color="maroon2",alpha=0.2)+
  geom_smooth(aes(y=percentPAR_diff), color = "thistle4",alpha=0,size=1.5) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  coord_cartesian(xlim=c(-10,250))

eg <- ggplot(combined5,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Wind")+
  geom_point(aes(y=percentws_diff),color="thistle4",alpha=0.2) +
  geom_line(aes(y=percentws_diff,group=idseg),color="thistle4",alpha=0.2)+
  geom_smooth(aes(y=percentws_diff), color = "thistle4",alpha=0,size=1.5) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  coord_cartesian(xlim=c(-10,250))


grid.arrange(ea,eb,ec,ed,ee,ef,eg,ncol=2,nrow=4)

#with other smooth types

fa <- ggplot(combined5,aes(x = log1p(just.dist))) +
  theme_classic()+
  ggtitle("Air temperature")+
  geom_point(aes(y=log1p(percent_diff/100)),color="lightslateblue",alpha=0.2) +
  geom_smooth(aes(y=log1p(percent_diff/100)), color = "darksalmon",alpha=0) +
  geom_smooth(aes(y=log1p(percent_diff/100)),method = "lm", color = "green4",alpha=0) +
  geom_smooth(aes(y=log1p(percent_diff/100)), method = "glm", color = "orange4",alpha=0) +
  geom_smooth(aes(y=log1p(percent_diff/100)), method = "gam", color = "maroon4") +
  xlab("log(Distance from edge)")+
  ylab("log(% difference from interior point)")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0)

fb <- ggplot(combined5,aes(x = log1p(just.dist))) +
  theme_classic()+
  ggtitle("Relative humidity")+
  geom_point(aes(y=log1p(percentrh_diff/100)),color="indianred3",alpha=0.2) +
  geom_smooth(aes(y=log1p(percentrh_diff/100)), color = "darksalmon",alpha=0) +
  geom_smooth(aes(y=log1p(percentrh_diff/100)),method = "lm", color = "green4",alpha=0) +
  geom_smooth(aes(y=log1p(percentrh_diff/100)), method = "glm", color = "orange4",alpha=0) +
  geom_smooth(aes(y=log1p(percentrh_diff/100)), method = "gam", color = "maroon4") +
  xlab("log(Distance from edge)")+
  ylab("log(% difference from interior point)")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0)

fc <- ggplot(combined5,aes(x = log1p(just.dist))) +
  theme_classic()+
  ggtitle("VPD")+
  geom_point(aes(y=log1p(percentVPD_diff/100)),color="darkviolet",alpha=0.2) +
  geom_smooth(aes(y=log1p(percentVPD_diff/100)), color = "darksalmon",alpha=0) +
  geom_smooth(aes(y=log1p(percentVPD_diff/100)),method = "lm", color = "green4",alpha=0) +
  geom_smooth(aes(y=log1p(percentVPD_diff/100)), method = "glm", color = "orange4",alpha=0) +
  geom_smooth(aes(y=log1p(percentVPD_diff/100)), method = "gam", color = "maroon4") +
  xlab("log(Distance from edge)")+
  ylab("log(% difference from interior point)")+
  geom_line(aes(y=0),color="black")

fd <- ggplot(combined5,aes(x = log1p(just.dist))) +
  theme_classic()+
  ggtitle("Soil temperature")+
  geom_point(aes(y=log1p(percentst_diff/100)),color="goldenrod2",alpha=0.2) +
  geom_smooth(aes(y=log1p(percentst_diff/100)), color = "darksalmon",alpha=0) +
  geom_smooth(aes(y=log1p(percentst_diff/100)),method = "lm", color = "green4",alpha=0) +
  geom_smooth(aes(y=log1p(percentst_diff/100)), method = "glm", color = "orange4",alpha=0) +
  geom_smooth(aes(y=log1p(percentst_diff/100)), method = "gam", color = "maroon4") +
  xlab("log(Distance from edge)")+
  ylab("log(% difference from interior point)")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) 

fe <- ggplot(combined5,aes(x = log1p(just.dist))) +
  theme_classic()+
  ggtitle("Soil moisture")+
  geom_point(aes(y=log1p(percentsm_diff/100)),color="turquoise3",alpha=0.2) +
  geom_smooth(aes(y=log1p(percentsm_diff/100)), color = "darksalmon",alpha=0) +
  geom_smooth(aes(y=log1p(percentsm_diff/100)),method = "lm", color = "green4",alpha=0) +
  geom_smooth(aes(y=log1p(percentsm_diff/100)), method = "glm", color = "orange4",alpha=0) +
  geom_smooth(aes(y=log1p(percentsm_diff/100)), method = "gam", color = "maroon4") +
  xlab("log(Distance from edge)")+
  ylab("log(% difference from interior point)")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) 

ff <- ggplot(combined5,aes(x = log1p(just.dist))) +
  theme_classic()+
  ggtitle("Light (PAR)")+
  geom_point(aes(y=log1p(percentPAR_diff/100)),color="maroon2",alpha=0.2) +
  geom_smooth(aes(y=log1p(percentPAR_diff/100)), color = "darksalmon",alpha=0) +
  geom_smooth(aes(y=log1p(percentPAR_diff/100)),method = "lm", color = "green4",alpha=0) +
  geom_smooth(aes(y=log1p(percentPAR_diff/100)), method = "glm", color = "orange4",alpha=0) +
  geom_smooth(aes(y=log1p(percentPAR_diff/100)), method = "gam", color = "maroon4") +
  xlab("log(Distance from edge)")+
  ylab("log(% difference from interior point)")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) 

fg <- ggplot(combined5,aes(x = log1p(just.dist))) +
  theme_classic()+
  ggtitle("Wind")+
  geom_point(aes(y=log1p(percentws_diff/100)),color="thistle4",alpha=0.2) +
  geom_smooth(aes(y=log1p(percentws_diff/100)), color = "darksalmon",alpha=0) +
  geom_smooth(aes(y=log1p(percentws_diff/100)),method = "lm", color = "green4",alpha=0) +
  geom_smooth(aes(y=log1p(percentws_diff/100)), method = "glm", color = "orange4",alpha=0) +
  geom_smooth(aes(y=log1p(percentws_diff/100)), method = "gam", color = "maroon4") +
  xlab("log(Distance from edge)")+
  ylab("log(% difference from interior point)")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) 

grid.arrange(fa,fb,fc,fd,fe,ff,fg,ncol=2,nrow=4)




#remove article 29 from SM due to high influence on LOESS
sminf <- c(29,43)

smrem <- combined5[!(combined5$article.id %in% sminf),]

aea <- ggplot(smrem,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Soil")+
  geom_smooth(aes(y=percentsm_diff), color = "turquoise3",alpha=0) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  coord_cartesian(xlim=c(-10,250))





########initial modeling ####

## READ IN
read.csv("./Outputs/vardata.csv")

#distributions not normal, log after + 1 to remove all negative
#normalize everything
forcart <- vardata[,-c(1,2,4,5,11,12,13,14,15,17,18,20,21,23,24,26,27,29,30)]
fractions <- (forcart[,-c(1,3,4,5,6)]/100)
logvar <- log1p(fractions) #remove negatives, take log

normalize <- function(x) {
  return((x-min(x,na.rm=TRUE))/(max(x,na.rm=TRUE)-min(x,na.rm=TRUE)))
} #min-max normalization between 0 and 1

norms <- as.data.frame(lapply(logvar,normalize))


forglmm <- cbind(vardata[,c(1,2,3,10)],logvar) #new, now any analysis can be done
forglmm$edge_age_years <- log(forglmm$edge_age_years) #log years to linearize response




##GLM
sum(forglm[(!is.na(forglm$percentws_diff) & !is.na(forglm$edge_orient.f)),]) #check, no rows contain both wind and orientation

#is effect due to missing data?
nrow(forglmm[!is.na(forglmm$edge_age_years),])/900
nrow(forglmm[!is.na(forglmm$season.f),])/900
nrow(forglmm[!is.na(forglmm$matrix_type.f),])/900
nrow(forglmm[!is.na(forglmm$edge_orient.f),])/900


#graphing

#single variable,dist
atglm.sing <- glm(percent_diff ~ just.dist,family = gaussian, data = forglmm)
rhglm.sing <- glm(percentrh_diff ~ just.dist,family = gaussian, data = forglmm)
vpdglm.sing <- glm(percentVPD_diff ~ just.dist,family = gaussian, data = forglmm)
smglm.sing <-  glm(percentsm_diff ~ just.dist,family = gaussian, data = forglmm)
stglm.sing <-  glm(percentst_diff ~ just.dist,family = gaussian, data = forglmm)
parglm.sing <- glm(percentPAR_diff ~ just.dist,family = gaussian, data = forglmm)
wsglm.sing <-  glm(percentws_diff ~ just.dist,family = gaussian, data = forglmm)

#is a log curve a better predictor?
propor <- vardata
propor$atpro <- propor$percent_diff/100
logfit <- lm(atpro[just.dist >=0]~log1p(just.dist[just.dist >= 0]), data = propor)
newy <- predict(logfit,list(just.dist=distrange),interval="confidence")
matlines(distrange,newy,lwd=2)
regfit <- lm(atpro~just.dist,data=propor)

newnewy <- predict(logfit,list(just.dist=distrange))
regy <- predict(regfit,list(just.dist=distrange))
plot(propor$just.dist,propor$atpro,pch=1,ylim=c(-0.1,0.3),col="blue",xlab="Distance from edge",ylab="Change in AT compared to interior point")
lines(distrange,newnewy,col="red")
lines(distrange,regy,col="green")
# yup.


atrange <- seq(-1,1,0.1)
rhrange <- seq(-0.5,0.5,0.1)
smrange <- seq(-1,1,0.1)
parrange <- seq(-2.4,4,0.2)
strange <- seq(-0.5,1,0.1)
vpdrange <- seq(-1,2,0.2)
wsrange <- seq(-1,2.2,0.2)
distrange <- seq(-10,500,1)
distrange <- seq(0,500,1)

aty <- predict(atglm.sing,list(just.dist = distrange),type="response")
plot(forglmm$just.dist,forglmm$percent_diff,pch=1,xlab="Distance from edge",ylab="Change in AT compared to interior point")
lines(distrange,aty)

rhy <- predict(rhglm.sing,list(just.dist = distrange),type="response")
plot(forglmm$just.dist,forglmm$percentrh_diff,pch=1,xlab="Distance from edge",ylab="Change in RH compared to interior point")
lines(distrange,rhy)

vpdy <- predict(vpdglm.sing,list(just.dist = distrange),type="response")
plot(forglmm$just.dist,forglmm$percentVPD_diff,pch=1,xlab="Distance from edge",ylab="Change in VPD compared to interior point")
lines(distrange,vpdy)

smy <- predict(smglm.sing,list(just.dist = distrange),type="response")
plot(forglmm$just.dist,forglmm$percentsm_diff,pch=1,xlab="Distance from edge",ylab="Change in SM compared to interior point")
lines(distrange,smy)


sty <- predict(stglm.sing,list(just.dist = distrange),type="response")
plot(forglm$just.dist,forglm$percentst_diff,pch=1,xlab="Distance from edge",ylab="Change in ST compared to interior point")
lines(distrange,sty)


pary <- predict(parglm.sing,list(just.dist = distrange),type="response")
plot(forglmm$just.dist,forglmm$percentPAR_diff,pch=1,xlab="Distance from edge",ylab="Change in PAR compared to interior point")
lines(distrange,pary)

wsy <- predict(wsglm.sing,list(just.dist = distrange),type="response")
plot(forglmm$just.dist,forglmm$percentws_diff,pch=1,xlab="Distance from edge",ylab="Change in WS compared to interior point")
lines(distrange,wsy)

#all together
ggplot(vardata,aes(x = just.dist)) +
  geom_smooth(aes(y=percent_diff),method="glm", color = "green",alpha=0) +
  geom_smooth(aes(y=percentrh_diff),method="glm", color = "red",alpha=0) +
  geom_smooth(aes(y=percentPAR_diff),method="glm", color = "blue",alpha=0) +
  geom_smooth(aes(y=percentsm_diff),method="glm", color = "cyan",alpha=0) +
  geom_smooth(aes(y=percentst_diff),method="glm", color = "orange",alpha=0) +
  geom_smooth(aes(y=percentVPD_diff),method="glm", color = "purple",alpha=0) +
  geom_smooth(aes(y=percentws_diff),method="glm", color = "yellow",alpha=0) +
  coord_cartesian(xlim=c(-10,250),ylim=c(-300,400))

ggplot(vardata,aes(x = just.dist)) +
  geom_smooth(aes(y=percent_diff),method="glm", color = "green",alpha=0) +
  geom_smooth(aes(y=percentrh_diff),method="glm", color = "red",alpha=0) +
  geom_smooth(aes(y=percentsm_diff),method="glm", color = "cyan",alpha=0) +
  geom_smooth(aes(y=percentst_diff),method="glm", color = "orange",alpha=0) +
  geom_line(aes(y=0),color="black")+
  labs(y="Percent change from interior point",x="Distance from edge")+
  coord_cartesian(xlim=c(-10,250))

ggplot(vardata,aes(x = just.dist)) +
  geom_smooth(aes(y=percent_diff),method="glm", color = "green",alpha=0) +
  geom_smooth(aes(y=percentrh_diff),method="glm", color = "red",alpha=0) +
  geom_line(aes(y=0),color="black")+
  labs(y="Percent change from interior point",x="Distance from edge")+
  coord_cartesian(xlim=c(-10,250))



###does removing studies with 1ha plots change anything?
###without 1 ha plots
#ids with 1ha plots: 7,66,45,44
oneha <- c(7,66,45,44)
withoutone <- vardata[!vardata$article.id %in% oneha,]

withoutforglmm <- forglmm[!forglmm$article.id %in% oneha,]




###grouping matrices into similar ####
#grass, meadow, clearing, pasture, field, grassland = "grass"
broadmat <- vardata
broadmat$matrix_type[broadmat$matrix_type == "meadow"] <- "grass"
broadmat$matrix_type[broadmat$matrix_type == "clearing"] <- "grass"
broadmat$matrix_type[broadmat$matrix_type == "field"] <- "grass"
broadmat$matrix_type[broadmat$matrix_type == "grassland"] <- "grass"
broadmat$matrix_type[broadmat$matrix_type == "pasture"] <- "grass"

#sugarcane field = "ag.field"
broadmat$matrix_type[broadmat$matrix_type == "sugarcane.field"] <- "ag.field"
#prairie = "savanna"
broadmat$matrix_type[broadmat$matrix_type == "prairie"] <- "savanna"
#pine plantation, eucalyptus farm= "plantation"
broadmat$matrix_type[broadmat$matrix_type == "pine.plantation"] <- "plantation"
broadmat$matrix_type[broadmat$matrix_type == "eucalyptus.farm"] <- "plantation"
broadmat$matrix_type[broadmat$matrix_type == "stand"] <- "plantation"

# ag vs non-ag
broadmat <- vardata
agri_list <- c("field","pasture","sugarcane.field","pine.plantation","eucalyptus.farm","stand","conifer.clearcut","plantation","grass")
broadmat$matrix_type[broadmat$matrix_type %in% agri_list] <- "ag"

not_agri_list <- c("shrubland","young.successional.forest","oak.forest","sandscrub","savanna","grassland","prairie")
broadmat$matrix_type[broadmat$matrix_type %in% not_agri_list] <- "non_ag"

# remove neither
neither <- c("highway","powerline","meadow","clearing",NA)
broadmat <- broadmat[!(broadmat %in% neither)]



forcart <- broadmat[,-c(1,2,4,5,11,12,13,14,15,17,18,20,21,23,24,26,27,29,30)]
fractions <- (forcart[,-c(1,3,4,5,6)]/100) ##remove negative percentages
logvar <- log1p(fractions)
mixedvar <- cbind(fractions[,c(3,4)],logvar[,c(1,2,3,4,5,6,7)])


broadmat$season.f <- factor(broadmat$season)
broadmat$matrix_type.f <- factor(broadmat$matrix_type)
broadmat$edge_orient.f <- factor(broadmat$edge_orient)

matglmm <- cbind(broadmat[,c(1,2,3,10,32,33,34)],mixedvar)
#matglmm <- matglmm[!matglmm$article.id %in% oneha,] #remove 1 ha plots

#prep for GLMMs, remove matrix points and log dist(?)

matglmm <- matglmm[matglmm$just.dist>=0,]

matglmm$just.dist.l <- log1p(matglmm$just.dist)


#AT GLMS ####
ggqqplot(vardata$percent_diff)

atglm <- lmer(percent_diff ~ (1|article.id), #2nd best
              data = matglmm, REML=F)
atglm2 <- lmer(percent_diff ~ just.dist.l + (1|article.id), #bestbest
              data = matglmm, REML=F)
atglm3 <- lmer(percent_diff ~ just.dist.l + matrix_type.f + (1|article.id), #3rd
              data = matglmm, REML=F)
atglm4 <- lmer(percent_diff ~ just.dist.l * matrix_type.f + (1|article.id),
               data = matglmm, REML=F)
atglm5 <- lmer(percent_diff ~ just.dist.l + matrix_type.f + edge_orient.f + (1|article.id),
               data = matglmm, REML=F)
atglm6 <- lmer(percent_diff ~ just.dist.l + edge_orient.f + (1|article.id),
               data = matglmm, REML=F)
atglm7 <- lmer(percent_diff ~ just.dist.l * edge_orient.f + (1|article.id),
               data = matglmm, REML=F)
atglm8 <- lmer(percent_diff ~ just.dist.l + matrix_type.f + edge_age_years + (1|article.id),
               data = matglmm, REML=F)
atglm9 <- glm(percent_diff ~ just.dist.l,
               data = matglmm)

anova(atglm,atglm2)

qqp(vardata$percent_diff, "lnorm")
qqp(vardata$percent_diff, "norm")
nbinom <- fitdistr(na.exclude(vardata$percent_diff), "Negative Binomial")
qqp(a.exclude(vardata$percent_diff), "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
poisson <- fitdistr(na.exclude(vardata$percent_diff), "Poisson")
qqp(vardata$percent_diff, "pois", poisson$estimate)

bestat <- glm(percent_diff ~ just.dist.l, data = matglmm)

AIC(atglm,atglm2,atglm3,atglm4,atglm5,atglm6,atglm7,atglm8,atglm9)
AICc(atglm,atglm2,atglm3,atglm4,atglm5,atglm6,atglm7,atglm8,atglm9)

generalized.at <- glmer(percent_diff ~ just.dist + (1|article.id), data = matglmm) #true generalized linear mixed model, defaults to lmer due to Gaussian dist

qqnorm(resid(bestat))
qqline(resid(bestat))   #model fairly normal, but somewhat heavy at tails
#compare to loglinear model
qqnorm(resid(logfit))
qqline(resid(logfit))
#basically the same

ggqqplot(matglmm$percent_diff)

 #polynomial models
polyat <- lm(vardata$percent_diff ~ poly(vardata$just.dist,2,raw=TRUE))
polyat2 <- lm(vardata$percent_diff ~ poly(vardata$just.dist,3,raw=TRUE))
polyat3 <- lm(vardata$percent_diff ~ poly(vardata$just.dist,4,raw=TRUE))
linear <- lm(vardata$percent_diff ~ vardata$just.dist)

#4th order polynomial best, log curve worse fit than linear



#RH GLMMS ####
ggqqplot(vardata$percentrh_diff)

rhglm <- lmer(percentrh_diff ~ (1|article.id), #2nd best
              data = matglmm, REML=F)
rhglm2 <- lmer(percentrh_diff ~ just.dist.l + (1|article.id), #bestbest
               data = matglmm, REML=F)
rhglm3 <- lmer(percentrh_diff ~ just.dist.l + edge_orient.f + (1|article.id), #3rd
               data = matglmm, REML=F) 
rhglm4 <- lmer(percentrh_diff ~ just.dist.l * edge_orient.f + (1|article.id),
               data = matglmm, REML=F)
rhglm5 <- lmer(percentrh_diff ~ just.dist.l + matrix_type.f + edge_orient.f + (1|article.id),
               data = matglmm, REML=F) #3rd
rhglm6 <- lmer(percentrh_diff ~ just.dist.l + matrix_type.f + edge_age_years + (1|article.id),
               data = matglmm, REML=F)
rhglm7 <- glm(percentrh_diff ~ just.dist.l, 
                         data = matglmm)

AIC(rhglm,rhglm2,rhglm3,rhglm4,rhglm5,rhglm6,rhglm7)
AICc(rhglm,rhglm2,rhglm3,rhglm4,rhglm5,rhglm6,rhglm7)

bestrh <- lmer(percentrh_diff ~ just.dist.l + (1|article.id), #bestbest
               data = matglmm, REML=F)

qqnorm(resid(bestrh))
qqline(resid(bestrh))


#ST GLMMS ####
ggqqplot(vardata$percentst_diff)

stglm <- lmer(percentst_diff ~ (1|article.id), #2nd best
              data = matglmm, REML=F)
stglm2 <- lmer(percentst_diff ~ just.dist.l + (1|article.id), #best
               data = matglmm, REML=F)
stglm3 <- lmer(percentst_diff ~ just.dist.l + matrix_type.f + (1|article.id), #3rd
               data = matglmm, REML=F)
stglm4 <- lmer(percentst_diff ~ just.dist.l * matrix_type.f + (1|article.id),
               data = matglmm, REML=F)
stglm5 <- lmer(percentst_diff ~ just.dist.l + matrix_type.f + edge_orient.f + (1|article.id),
               data = matglmm, REML=F)
stglm6 <- lmer(percentst_diff ~ just.dist.l + matrix_type.f + edge_age_years + (1|article.id),
               data = matglmm, REML=F)
stglm7 <- lmer(percentst_diff ~ just.dist.l + edge_orient.f + (1|article.id),
               data = matglmm, REML=F)
stglm8 <- lmer(percentst_diff ~ just.dist.l * edge_orient.f + (1|article.id),
               data = matglmm, REML=F)
stglm9 <- glm(percentst_diff ~ just.dist.l, 
              data = matglmm)

AIC(stglm,stglm2,stglm3,stglm4,stglm5,stglm6,stglm7,stglm8,stglm9)
AICc(stglm,stglm2,stglm3,stglm4,stglm5,stglm6,stglm7,stglm8,stglm9)

bestst <- glm(percentst_diff ~ just.dist.l, data = matglmm)

qqnorm(resid(bestst))
qqline(resid(bestst))

#SM GLMMS ####

ggqqplot(vardata$percentsm_diff)

smglm <- lmer(percentsm_diff ~ (1|article.id),
              data = matglmm, REML=F)
smglm2 <- lmer(percentsm_diff ~ just.dist.l + (1|article.id),
               data = matglmm, REML=F)
smglm3 <- lmer(percentsm_diff ~ just.dist.l + matrix_type.f + (1|article.id), #best, but not diff from smglm4
               data = matglmm, REML=F)
smglm4 <- lmer(percentsm_diff ~ just.dist.l * matrix_type.f + (1|article.id), #2nd
               data = matglmm, REML=F)
smglm5 <- lmer(percentsm_diff ~ just.dist.l + matrix_type.f + edge_age_years + (1|article.id),
               data = matglmm, REML=F) 
smglm6 <- lmer(percentsm_diff ~ just.dist.l + matrix_type.f + edge_orient.f + (1|article.id),
               data = matglmm, REML=F)
smglm7 <- lmer(percentsm_diff ~ just.dist.l + edge_age_years + (1|article.id),
               data = matglmm, REML=F) 
smglm8 <- glm(percentsm_diff ~ just.dist.l, 
              data = matglmm)

AIC(smglm,smglm2,smglm3,smglm4,smglm5,smglm6,smglm7,smglm8)
AICc(smglm,smglm2,smglm3,smglm4,smglm5,smglm6,smglm7,smglm8)

bestsm <- glm(percentsm_diff ~ just.dist.l * matrix_type.f, data = matglmm)

distlm <- glm(percentsm_diff ~ just.dist.l,data=matglmm)

qqnorm(resid(distlm))
qqline(resid(distlm))

#PAR GLMMS ####
ggqqplot(vardata$percentPAR_diff)

parglm <- lmer(percentPAR_diff ~ (1|article.id), 
               data = matglmm, REML=F)
parglm2 <- lmer(percentPAR_diff ~ just.dist.l + (1|article.id), 
                data = matglmm, REML=F)
parglm3 <- lmer(percentPAR_diff ~ just.dist.l + edge_orient.f + (1|article.id), #best
                data = matglmm, REML=F)
parglm4 <- lmer(percentPAR_diff ~ just.dist.l * edge_orient.f + (1|article.id), #2nd, but not sig different
                data = matglmm, REML=F)
parglm5 <- lmer(percentPAR_diff ~ just.dist.l + matrix_type.f + edge_orient.f + (1|article.id), #best
                data = matglmm, REML=F) 
parglm6 <- lmer(percentPAR_diff ~ just.dist.l + matrix_type.f + (1|article.id),
                data = matglmm, REML=F) 
parglm7 <- glm(percentPAR_diff ~ just.dist.l, 
               data = matglmm)
parglm8 <- lmer(percentPAR_diff ~ just.dist.l + edge_age_years + (1|article.id),
                data = matglmm, REML=F) #not enough data to run

AIC(parglm,parglm2,parglm3,parglm4,parglm5,parglm6,parglm7)
AICc(parglm,parglm2,parglm3,parglm4,parglm5,parglm6,parglm7)

bestpar <- glm(percentPAR_diff ~ just.dist.l + edge_orient.f, data = matglmm)

distlm2 <- glm(percentPAR_diff ~ just.dist.l,data=matglmm)

qqnorm(resid(distlm2))
qqline(resid(distlm2))


#VPD GLMMS ####

ggqqplot(vardata$percentVPD_diff)

vpdglm <- lmer(percentVPD_diff ~ (1|article.id),
               data = matglmm, REML=F) 
vpdglm2 <- lmer(percentVPD_diff ~ just.dist.l + (1|article.id), 
                data = matglmm, REML=F) 
vpdglm3 <- lmer(percentVPD_diff ~ just.dist.l + edge_age_years + (1|article.id),
                data = matglmm, REML=F) 
vpdglm4 <- lmer(percentVPD_diff ~ just.dist.l * edge_age_years + (1|article.id),
                data = matglmm, REML=F) 
vpdglm5 <- lmer(percentVPD_diff ~ just.dist.l + matrix_type.f + edge_age_years + (1|article.id),
                data = matglmm, REML=F)
vpdglm6 <- lmer(percentVPD_diff ~ just.dist.l + matrix_type.f + edge_orient.f + (1|article.id), #3rd
                data = matglmm, REML=F)
vpdglm7 <- lmer(percentVPD_diff ~ just.dist.l + matrix_type.f + (1|article.id), 
                data = matglmm, REML=F)
vpdglm8 <- lmer(percentVPD_diff ~ just.dist.l + edge_orient.f + (1|article.id), #2nd
                data = matglmm, REML=F)
vpdglm9 <- lmer(percentVPD_diff ~ just.dist.l * edge_orient.f + (1|article.id), #best
                data = matglmm, REML=F)
vpdglm10 <- glm(percentVPD_diff ~ just.dist.l, 
                data = matglmm)

AIC(vpdglm,vpdglm2,vpdglm3,vpdglm4,vpdglm5,vpdglm6,vpdglm7,vpdglm8,vpdglm9,vpdglm10)
AICc(vpdglm,vpdglm2,vpdglm3,vpdglm4,vpdglm5,vpdglm6,vpdglm7,vpdglm8,vpdglm9,vpdglm10)

bestvpd <- lmer(percentVPD_diff ~ just.dist.l * edge_orient.f + (1|article.id), #best
                data = matglmm, REML=F)
qqnorm(resid(bestvpd))
qqline(resid(bestvpd))

distlm3 <- glm(percentVPD_diff ~ just.dist.l,data=matglmm) #better than both log

qqnorm(resid(distlm3))
qqline(resid(distlm3))


#WS GLMMS ####
wsdf <- matglmm[c("percentws_diff","just.dist.l","article.id","matrix_type.f")] %>% drop_na()

ggqqplot(vardata$percentws_diff)

wsglm <- lmer(percentws_diff ~ (1|article.id),
              data = matglmm, REML=F)
wsglm2 <- lmer(percentws_diff ~ just.dist.l + (1|article.id),
               data = matglmm, REML=F)
wsglm3 <- lmer(percentws_diff ~ just.dist.l + matrix_type.f + (1|article.id),
               data = matglmm, REML=F) #2nd
wsglm4 <- lmer(percentws_diff ~ just.dist.l + matrix_type.f + edge_orient.f + (1|article.id),
               data = matglmm, REML=F) #does not converge
#wsglm5 <- glm(percentws_diff ~ just.dist.l + edge_age_years + matrix_type.f, #best
#              family = gaussian, data = matglmm) #needs 2 or more levels, not enough data
wsglm6 <- lmer(percentws_diff ~ just.dist.l * matrix_type.f + (1|article.id),
             data = matglmm, REML=F) 
wsglm7 <- glm(percentws_diff ~ just.dist.l * matrix_type.f,
              family = gaussian, data = matglmm) #best
wsglm8 <- lmer(percentws_diff ~ just.dist.l + edge_age_years + (1|article.id),
               data = matglmm, REML=F) 
wsglm9 <- glm(percentws_diff ~ just.dist.l, 
              data = matglmm)

AIC(wsglm,wsglm2,wsglm3,wsglm6,wsglm7,wsglm8,wsglm9)
AICc(wsglm,wsglm2,wsglm3,wsglm6,wsglm7,wsglm8,wsglm9)

bestws <- glm(percentws_diff ~ just.dist.l + edge_age_years, data = matglmm)

distlm4 <- glm(percentws_diff ~ just.dist.l,data=matglmm)

qqnorm(resid(distlm4))
qqline(resid(distlm4))



#lm of all variables ####
atlm <- lm(percent_diff~just.dist.l,data=matglmm)
atx <- -coef(atlm)[1]/coef(atlm)[2] #where crosses 0
actualat <- expm1(atx) #real number meters


rhlm <- lm(percentrh_diff~just.dist.l,data=matglmm)
rhx <- -coef(rhlm)[1]/coef(rhlm)[2] #where crosses 0
actualrh <- expm1(rhx) #real number meters

vpdlm <- lm(percentVPD_diff~just.dist.l,data=matglmm)
vpdx <- -coef(vpdlm)[1]/coef(vpdlm)[2] #where crosses 0
actualvpd <- expm1(vpdx) #real number meters

stlm <- lm(percentst_diff~just.dist.l,data=matglmm)
stx <- -coef(stlm)[1]/coef(stlm)[2] #where crosses 0
actualst <- expm1(stx) #real number meters

smlm <- lm(percentsm_diff~just.dist.l,data=matglmm)
smx <- -coef(smlm)[1]/coef(smlm)[2] #where crosses 0
actualsm <- expm1(smx) #real number meters

parlm <- lm(percentPAR_diff~just.dist.l,data=matglmm)
parx <- -coef(parlm)[1]/coef(parlm)[2] #where crosses 0
actualpar <- expm1(parx) #real number meters

wslm <- lm(percentws_diff~just.dist.l,data=matglmm)
wsx <- -coef(wslm)[1]/coef(wslm)[2] #where crosses 0
actualws <- expm1(wsx) #real number meters
#based on visual inspection, ws anywhere from 3 to 4.5
expm1(3)
expm1(4.5)

#mean of distance of x-intercept,except sm
all <- data.frame(c("actualat","actualrh","actualvpd","actualst","actualsm","actualpar","actualws"),
                  c(actualat,actualrh,actualvpd,actualst,actualsm,actualpar,actualws))
names(all) <- c("variable","xint")
all$xint <- round(all$xint)

mean(all$xint[-5])

#####skip to variances section


####how do significant columns other than dist interact with variables? viz ####

smmatrix <- ggplot(broadmat,aes(x = matrix_type,y=percentsm_diff))+
  geom_boxplot(aes(group=matrix_type))+ theme(axis.text.x = element_text(angle=90))+
  ggtitle("Soil moisture by matrix type")+
  xlab("Matrix type")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")
  





#############variances ####


#AT
AT <- read_csv("./Data/AT_seg.csv")

rem <- c(-30,-25,-20,-15)
AT <- AT[!AT$dist %in% rem,] #all -30,-20,-15 distances unnecessary
names(AT)[names(AT) == "SD_SE_CI_V"] <-"AT_var"
names(AT)[names(AT) == "SD_SE_CI_V_n"] <- "AT_var_n"

interiors <- AT %>% group_by(article.id) %>% summarize(dist = max(dist))
top <- AT %>% group_by(article.id,segment_n) %>% slice(which.max(dist))
zero <- AT %>% group_by(article.id, segment_n) %>% slice(which(dist==0))

sep <- merge(AT[,c(1,2,3,4,6,8,9,10,11,13,14,15)], top[,c(1,2,4,14)], by = c("article.id","segment_n"))
colnames(sep)[c(4,11,13,14)] <- c("just.dist","AT_var_n","max.dist","maxvar.n")

sep$atvardiff <- ifelse(ifelse(sep$just.dist == sep$max.dist, sep$maxvar.n,1) == 0.00, 
                        sep$atvardiff <- sep$maxvar.n - sep$AT_var_n, 
                        sep$atvardiff <- sep$AT_var_n - sep$maxvar.n)

#divide to make everything relative
sep$varpercent.at <- round((sep$atvardiff/sep$maxvar.n)*100)
sep$varpercent.at <- ifelse(is.na(sep$varpercent.at), NA, ifelse(is.infinite(sep$varpercent.at),0,ifelse(is.nan(sep$varpercent.at),0,sep$varpercent.at)))

short.at <- sep[,c(1,2,4,6,7,8,9,10,11,13,14,16)]
short.at$article.id <- as.factor(short.at$article.id)

#standard errors for later calculation of CI of interior space
atse <- sep[(sep$AT_var=="SE"&sep$just.dist==sep$max.dist),]



#PAR
PAR <- read_csv("./Data/PAR_seg.csv")

rem <- c(-30,-25,-20,-15)
PAR <- PAR[!PAR$dist %in% rem,] #all -30,-20,-15 distances unnecessary
names(PAR)[names(PAR) == "SD_SE_CI_V"] <-"PAR_var"
names(PAR)[names(PAR) == "SD_SE_CI_V_n"] <- "PAR_var_n"

interiors <- PAR %>% group_by(article.id) %>% summarize(dist = max(dist))
top <- PAR %>% group_by(article.id,segment_n) %>% slice(which.max(dist))
zero <- PAR %>% group_by(article.id, segment_n) %>% slice(which(dist==0))

sep <- merge(PAR[,c(1,2,3,4,5,9,10,11,12,14,15)], top[,c(1,2,5,15)], by = c("article.id","segment_n"))
colnames(sep)[c(5,11,12,13)] <- c("just.dist","PAR_var_n","max.dist","maxparvar.n")

sep$parvardiff <- ifelse(ifelse(sep$just.dist == sep$max.dist, sep$maxparvar.n,1) == 0.00, 
                        sep$parvardiff <- sep$maxparvar.n - sep$PAR_var_n, 
                        sep$parvardiff <- sep$PAR_var_n - sep$maxparvar.n)

#divide to make everything relative
sep$varpercent.par <- round((sep$parvardiff/sep$maxparvar.n)*100)
sep$varpercent.par <- ifelse(is.na(sep$varpercent.par), NA, ifelse(is.infinite(sep$varpercent.par),0,ifelse(is.nan(sep$varpercent.par),0,sep$varpercent.par)))

short.par <- sep[,c(1,2,5,6,7,8,9,10,11,15)]
short.par$article.id <- as.factor(short.par$article.id)

#standard errors for later calculation of CI of interior space
parse <- sep[(sep$PAR_var=="SE"&sep$just.dist==sep$max.dist),]


#RH

RH <- read_csv("./Data/RH_seg.csv")

rem <- c(-30,-25,-20,-15)
RH <- RH[!RH$dist %in% rem,] #all -30,-20,-15 distances unnecessary
names(RH)[names(RH) == "SD_SE_CI_V"] <-"RH_var"
names(RH)[names(RH) == "SD_SE_CI_V_n"] <- "RH_var_n"

interiors <- RH %>% group_by(article.id) %>% summarize(dist = max(dist))
top <- RH %>% group_by(article.id,segment_n) %>% slice(which.max(dist))
zero <- RH %>% group_by(article.id, segment_n) %>% slice(which(dist==0))

sep <- merge(RH[,c(1,2,3,4,6,8,9,10,11,13,14,15)], top[,c(1,2,4,14)], by = c("article.id","segment_n"))
colnames(sep)[c(4,11,13,14)] <- c("just.dist","RH_var_n","max.dist","maxrhvar.n")

sep$rhvardiff <- ifelse(ifelse(sep$just.dist == sep$max.dist, sep$maxrhvar.n,1) == 0.00, 
                         sep$rhvardiff <- sep$maxrhvar.n - sep$RH_var_n, 
                         sep$rhvardiff <- sep$RH_var_n - sep$maxrhvar.n)

#divide to make everything relative
sep$varpercent.rh <- round((sep$rhvardiff/sep$maxrhvar.n)*100)
sep$varpercent.rh <- ifelse(is.na(sep$varpercent.rh), NA, ifelse(is.infinite(sep$varpercent.rh),0,ifelse(is.nan(sep$varpercent.rh),0,sep$varpercent.rh)))


short.rh <- sep[,c(1,2,4,6,7,8,9,10,11,16)]
short.rh$article.id <- as.factor(short.rh$article.id)

#standard errors for later calculation of CI of interior space
rhse <- sep[(sep$RH_var=="SE"&sep$just.dist==sep$max.dist),]


#SM

SM <- read_csv("./Data/SM_seg.csv")

rem <- c(-30,-25,-20,-15)
SM <- SM[!SM$dist %in% rem,] #all -30,-20,-15 distances unnecessary
names(SM)[names(SM) == "SD_SE_CI_V"] <-"SM_var"
names(SM)[names(SM) == "SD_SE_CI_V_n"] <- "SM_var_n"

interiors <- SM %>% group_by(article.id) %>% summarize(dist = max(dist))
top <- SM %>% group_by(article.id,segment_n) %>% slice(which.max(dist))
zero <- SM %>% group_by(article.id, segment_n) %>% slice(which(dist==0))

sep <- merge(SM[,c(1,2,3,4,8,9,10,11,13,14,15)], top[,c(1,2,4,14)], by = c("article.id","segment_n"))
colnames(sep)[c(4,10,12,13)] <- c("just.dist","SM_var_n","max.dist","maxsmvar.n")

sep$smvardiff <- ifelse(ifelse(sep$just.dist == sep$max.dist, sep$maxsmvar.n,1) == 0.00, 
                         sep$smvardiff <- sep$maxsmvar.n - sep$SM_var_n, 
                         sep$smvardiff <- sep$SM_var_n - sep$maxsmvar.n)

#divide to make everything relative
sep$varpercent.sm <- round((sep$smvardiff/sep$maxsmvar.n)*100)
sep$varpercent.sm <- ifelse(is.na(sep$varpercent.sm), NA, ifelse(is.infinite(sep$varpercent.sm),0,ifelse(is.nan(sep$varpercent.sm),0,sep$varpercent.sm)))


short.sm <- sep[,c(1,2,4,5,6,7,8,9,10,15)]
short.sm$article.id <- as.factor(short.sm$article.id)

#standard errors for later calculation of CI of interior space
smse <- sep[(sep$SM_var=="SE"&sep$just.dist==sep$max.dist),]

#ST
ST <- read_csv("./Data/ST_seg.csv")

rem <- c(-30,-25,-20,-15)
ST <- ST[!ST$dist %in% rem,] #all -30,-20,-15 distances unnecessary
names(ST)[names(ST) == "SD_SE_CI_V"] <-"ST_var"
names(ST)[names(ST) == "SD_SE_CI_V_n"] <- "ST_var_n"

interiors <- ST %>% group_by(article.id) %>% summarize(dist = max(dist))
top <- ST %>% group_by(article.id,segment_n) %>% slice(which.max(dist))
zero <- ST %>% group_by(article.id, segment_n) %>% slice(which(dist==0))

sep <- merge(ST[,c(1,2,3,4,8,9,10,11,13,14,15)], top[,c(1,2,4,14)], by = c("article.id","segment_n"))
colnames(sep)[c(4,10,12,13)] <- c("just.dist","ST_var_n","max.dist","maxstvar.n")

sep$stvardiff <- ifelse(ifelse(sep$just.dist == sep$max.dist, sep$maxstvar.n,1) == 0.00, 
                        sep$stvardiff <- sep$maxstvar.n - sep$ST_var_n, 
                        sep$stvardiff <- sep$ST_var_n - sep$maxstvar.n)

#divide to make everything relative
sep$varpercent.st <- round((sep$stvardiff/sep$maxstvar.n)*100)
sep$varpercent.st <- ifelse(is.na(sep$varpercent.st), NA, ifelse(is.infinite(sep$varpercent.st),0,ifelse(is.nan(sep$varpercent.st),0,sep$varpercent.st)))

short.st <- sep[,c(1,2,4,5,6,7,8,9,10,15)]
short.st$article.id <- as.factor(short.st$article.id)

#standard errors for later calculation of CI of interior space
stse <- sep[(sep$ST_var=="SE"&sep$just.dist==sep$max.dist),]


#VPD
VPD <- read_csv("./Data/VPD_seg.csv")

rem <- c(-30,-25,-20,-15)
VPD <- VPD[!VPD$dist %in% rem,] #all -30,-20,-15 distances unnecessary
names(VPD)[names(VPD) == "SD_SE_CI_V"] <-"VPD_var"
names(VPD)[names(VPD) == "SD_SE_CI_V_n"] <- "VPD_var_n"

interiors <- VPD %>% group_by(article.id) %>% summarize(dist = max(dist))
top <- VPD %>% group_by(article.id,segment_n) %>% slice(which.max(dist))
zero <- VPD %>% group_by(article.id, segment_n) %>% slice(which(dist==0))

sep <- merge(VPD[,c(1,2,3,4,6,8,9,10,11,13,14,15)], top[,c(1,2,4,14)], by = c("article.id","segment_n"))
colnames(sep)[c(4,11,13,14)] <- c("just.dist","VPD_var_n","max.dist","maxvpdvar.n")

sep$vpdvardiff <- ifelse(ifelse(sep$just.dist == sep$max.dist, sep$maxvpdvar.n,1) == 0.00, 
                        sep$vpdvardiff <- sep$maxvpdvar.n - sep$VPD_var_n, 
                        sep$vpdvardiff <- sep$VPD_var_n - sep$maxvpdvar.n)

#divide to make everything relative
sep$varpercent.vpd <- round((sep$vpdvardiff/sep$maxvpdvar.n)*100)
sep$varpercent.vpd <- ifelse(is.na(sep$varpercent.vpd), NA, ifelse(is.infinite(sep$varpercent.vpd),0,ifelse(is.nan(sep$varpercent.vpd),0,sep$varpercent.vpd)))

short.vpd <- sep[,c(1,2,4,6,7,8,9,10,11,16)]
short.vpd$article.id <- as.factor(short.vpd$article.id)

#standard errors for later calculation of CI of interior space
vpdse <- sep[(sep$VPD_var=="SE"&sep$just.dist==sep$max.dist),]


#WS
WS <- read_csv("./Data/WS_seg.csv")

rem <- c(-30,-25,-20,-15)
WS <- WS[!WS$dist %in% rem,] #all -30,-20,-15 distances unnecessary
names(WS)[names(WS) == "SD_SE_CI_V"] <-"WS_var"
names(WS)[names(WS) == "SD_SE_CI_V_n"] <- "WS_var_n"

interiors <- WS %>% group_by(article.id) %>% summarize(dist = max(dist))
top <- WS %>% group_by(article.id,segment_n) %>% slice(which.max(dist))
zero <- WS %>% group_by(article.id, segment_n) %>% slice(which(dist==0))

sep <- merge(WS[,c(1,2,3,4,6,8,9,10,11,13,14,15)], top[,c(1,2,4,14)], by = c("article.id","segment_n"))
colnames(sep)[c(4,11,13,14)] <- c("just.dist","WS_var_n","max.dist","maxwsvar.n")

sep$wsvardiff <- ifelse(ifelse(sep$just.dist == sep$max.dist, sep$maxwsvar.n,1) == 0.00, 
                        sep$wsvardiff <- sep$maxwsvar.n - sep$WS_var_n, 
                        sep$wsvardiff <- sep$WS_var_n - sep$maxwsvar.n)

#divide to make everything relative
sep$varpercent.ws <- round((sep$wsvardiff/sep$maxwsvar.n)*100)
sep$varpercent.ws <- ifelse(is.na(sep$varpercent.ws), NA, ifelse(is.infinite(sep$varpercent.ws),0,ifelse(is.nan(sep$varpercent.ws),0,sep$varpercent.ws)))

short.ws <- sep[,c(1,2,4,6,7,8,9,10,11,16)]
short.ws$article.id <- as.factor(short.ws$article.id)

#standard errors for later calculation of CI of interior space
wsse <- sep[(sep$WS_var=="SE"&sep$just.dist==sep$max.dist),]



###combine variances
allvar1 <- merge(short.at, short.par[,c(1,2,3,8,9,10)], by = c("article.id","segment_n","just.dist"), all=TRUE)
allvar2 <- merge(allvar1, short.rh[,c(1,2,3,8,9,10)], by = c("article.id","segment_n","just.dist"), all=TRUE)
allvar3 <- merge(allvar2, short.sm[,c(1,2,3,8,9,10)], by = c("article.id","segment_n","just.dist"), all=TRUE)
allvar4 <- merge(allvar3, short.st[,c(1,2,3,8,9,10)], by = c("article.id","segment_n","just.dist"), all=TRUE)
allvar5 <- merge(allvar4, short.vpd[,c(1,2,3,8,9,10)], by = c("article.id","segment_n","just.dist"), all=TRUE)
allvar6 <- merge(allvar5, short.ws[,c(1,2,3,8,9,10)], by = c("article.id","segment_n","just.dist"), all=TRUE)

allvar6$idseg <- paste(allvar6[,1],allvar6[,2])

write.csv(allvar6,"./Outputs/allvariances.csv")



ggplot(allvar6,aes(x = just.dist)) +
  geom_smooth(aes(y=varpercent.at), color = "green",alpha=0) +
  geom_smooth(aes(y=varpercent.ws), color = "red",alpha=0) +
  geom_smooth(aes(y=varpercent.vpd), color = "blue",alpha=0) +
  geom_smooth(aes(y=varpercent.st), color = "cyan",alpha=0) +
  geom_smooth(aes(y=varpercent.sm), color = "orange",alpha=0) +
  geom_smooth(aes(y=varpercent.rh), color = "purple",alpha=0) +
  geom_smooth(aes(y=varpercent.par), color = "yellow",alpha=0) +
  coord_cartesian(ylim=c(-300,900),xlim=c(-10,250))+
  geom_line(aes(y=0),color="black")

ggplot(allvar6,aes(x=just.dist,y=varpercent.st, color = article.id, label = factor(article.id))) + geom_point() + geom_text()
ggplot(allvar6,aes(x=just.dist,y=varpercent.ws, color = article.id, label = factor(article.id))) + geom_point() + geom_text()


#individually

ga <- ggplot(allvar6,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Air temperature") +
  geom_point(aes(y=varpercent.at),color="lightslateblue",alpha=0.2) +
  geom_line(aes(y=varpercent.at,group=idseg),color="lightslateblue",alpha=0.2)+
  geom_smooth(aes(y=varpercent.at), color = "thistle4",alpha=0,size=1.5) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  ylim(-100,500)+
  coord_cartesian(xlim=c(-10,250))
  

gb <- ggplot(allvar6,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Relative humidity")+
  geom_point(aes(y=varpercent.rh),color="indianred3",alpha=0.2) +
  geom_line(aes(y=varpercent.rh,group=idseg),color="indianred3",alpha=0.2)+
  geom_smooth(aes(y=varpercent.rh), color = "thistle4",alpha=0,size=1.5) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  ylim(-100,200)+
  coord_cartesian(xlim=c(-10,250))

gc <- ggplot(allvar6,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("VPD")+
  geom_point(aes(y=varpercent.vpd),color="darkviolet",alpha=0.2) +
  geom_line(aes(y=varpercent.vpd,group=idseg),color="darkviolet",alpha=0.2)+
  geom_smooth(aes(y=varpercent.vpd), color = "thistle4",alpha=0,size=1.5) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  coord_cartesian(xlim=c(-10,250))

gd <- ggplot(allvar6,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Soil temperature")+
  geom_point(aes(y=varpercent.st),color="goldenrod2",alpha=0.2) +
  geom_line(aes(y=varpercent.st,group=idseg),color="goldenrod2",alpha=0.2)+
  geom_smooth(aes(y=varpercent.st), color = "thistle4",alpha=0,size=1.5) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  ylim(-50,300)+
  coord_cartesian(xlim=c(-10,250))

ge <- ggplot(allvar6,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Soil moisture")+
  geom_point(aes(y=varpercent.sm),color="turquoise3",alpha=0.2) +
  geom_line(aes(y=varpercent.sm,group=idseg),color="turquoise3",alpha=0.2)+
  geom_smooth(aes(y=varpercent.sm), color = "thistle4",alpha=0,size=1.5) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  ylim(-10,50)+
  coord_cartesian(xlim=c(-10,250))

gf <- ggplot(allvar6,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Light (PAR)")+
  geom_point(aes(y=varpercent.par),color="maroon2",alpha=0.2) +
  geom_line(aes(y=varpercent.par,group=idseg),color="maroon2",alpha=0.2)+
  geom_smooth(aes(y=varpercent.par), color = "thistle4",alpha=0,size=1.5) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  ylim(-50,700)+
  coord_cartesian(xlim=c(-10,250))

gg <- ggplot(allvar6,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Wind")+
  geom_point(aes(y=varpercent.ws),color="thistle4",alpha=0.2) +
  geom_line(aes(y=varpercent.ws,group=idseg),color="thistle4",alpha=0.2)+
  geom_smooth(aes(y=varpercent.ws), color = "thistle4",alpha=0,size=1.5) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  ylim(-100,600)+
  coord_cartesian(xlim=c(-10,250))

grid.arrange(ga,gb,gc,gd,ge,gf,gg,ncol=2,nrow=4)


#25 dominates trend of ST, to remove

minus <- 25
no25 <- allvar6[!allvar6$article.id %in% minus,]
ggplot(no25,aes(x = just.dist)) +
  geom_smooth(aes(y=varpercent.at), color = "green",alpha=0) +
  geom_smooth(aes(y=varpercent.ws), color = "red",alpha=0) +
  geom_smooth(aes(y=varpercent.vpd), color = "blue",alpha=0) +
  geom_smooth(aes(y=varpercent.st), color = "cyan",alpha=0) +
  geom_smooth(aes(y=varpercent.sm), color = "orange",alpha=0) +
  geom_smooth(aes(y=varpercent.rh), color = "purple",alpha=0) +
  geom_smooth(aes(y=varpercent.par), color = "yellow",alpha=0) +
  coord_cartesian(ylim=c(-300,900),xlim=c(-10,250))+
  geom_line(aes(y=0),color="black")




#shorten for simplicity
allvar6 <- read_csv("Outputs/allvariances.csv", 
                         col_types = cols(X1 = col_skip()))
allvar7 <- allvar6[,-c(9,12,15,18,21,24,27)]

#papers with a variance type
variances <- gather(allvar7, key = "variable", value = "percentchange", c(9,11,13,15,17,19,21))

variances2 <- gather(variances, key = "from", value = "type", c(8:14))

w <- variances2 %>% group_by(article.id)
w2 <- summarize(w,types=first(type))
w3 <- data.frame(table(unlist(w2$types)))

#calculate CI for interior point
# use Hedge's g for varying sample sizes, SDs
# compute Hedge's g for all studies

#find percentage of SE - SE = sqrt(v^2)
atse$perat <- atse$maxvar.n/atse$air_temp
rhse$perrh <- rhse$maxrhvar.n/rhse$rel_humid
vpdse$pervpd <- vpdse$maxvpdvar.n/vpdse$VPD
stse$perst <- stse$maxstvar.n/stse$soil_temp
smse$persm <- smse$maxsmvar.n/smse$soil_moist
parse$perpar <- ifelse(!is.na(parse$PAR), parse$maxparvar.n/parse$PAR ,parse$maxparvar.n/parse$log_value)
wsse$perws <- wsse$maxwsvar.n/wsse$wind_speed
wsse <- wsse[is.finite(wsse$perws),]

#all, 95% CI measure
atCI <- round(mean(atse$perat,na.rm=TRUE)*1.96,2)
rhCI <- round(mean(rhse$perrh,na.rm=TRUE)*1.96,2)
stCI <- round(mean(stse$perst,na.rm=TRUE)*1.96,2)
smCI <- round(mean(smse$persm,na.rm=TRUE)*1.96,2)
parCI <- round(mean(parse$perpar,na.rm=TRUE)*1.96,2)
vpdCI <- round(mean(vpdse$pervpd,na.rm=TRUE)*1.96,2)
wsCI <- round(mean(wsse$perws,na.rm=TRUE)*1.96,2)


#where regression of abiotic effects passes CI -- edge effects no longer distinguishable
#find by predicting for 0,5,10,20,40,60,80,100,120,150,200
cidists <- data.frame(just.dist.l = log1p(c(0,5,10,20,40,60,80,100,120,150,200)))

atpred <- round(expm1(predict(atlm, newdata = cidists)),2)
rhpred <- round(expm1(predict(rhlm, newdata = cidists)),2)
vpdpred <- round(expm1(predict(vpdlm, newdata = cidists)),2)
stpred <- round(expm1(predict(stlm, newdata = cidists)),2)
smpred <- round(expm1(predict(smlm, newdata = cidists)),2)
parpred <- round(expm1(predict(parlm, newdata = cidists)),2)
wspred <- round(expm1(predict(wslm, newdata = cidists)),2)


#graph with CI
ha <- ggplot(combined5,aes(x = log1p(just.dist))) +
  theme_classic()+
  ggtitle("Air temperature")+
  geom_point(aes(y=log1p(percent_diff/100)),color="lightslateblue",alpha=0.2) +
  geom_smooth(aes(y=log1p(percent_diff/100)),method = "lm", color = "green4",alpha=0) +
  xlab("log(Distance from edge)")+
  ylab("log(% difference from interior point)")+
  geom_hline(aes(yintercept=log1p(atCI)),color="blue")+
  geom_line(aes(y=0),color="black")+
  ylim(0,0.5)+
  geom_vline(xintercept=0)

hb <- ggplot(combined5,aes(x = log1p(just.dist))) +
  theme_classic()+
  ggtitle("Relative humidity")+
  geom_point(aes(y=log1p(percentrh_diff/100)),color="indianred3",alpha=0.2) +
  geom_smooth(aes(y=log1p(percentrh_diff/100)),method = "lm", color = "green4",alpha=0) +
  xlab("log(Distance from edge)")+
  ylab("log(% difference from interior point)")+
  geom_hline(aes(yintercept=-log1p(rhCI)),color="blue")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0)

hc <- ggplot(combined5,aes(x = log1p(just.dist))) +
  theme_classic()+
  ggtitle("VPD")+
  geom_point(aes(y=log1p(percentVPD_diff/100)),color="darkviolet",alpha=0.2) +
  geom_smooth(aes(y=log1p(percentVPD_diff/100)),method = "lm", color = "green4",alpha=0) +
  xlab("log(Distance from edge)")+
  ylab("log(% difference from interior point)")+
  geom_hline(aes(yintercept=log1p(vpdCI)),color="blue")+
  geom_line(aes(y=0),color="black")

hd <- ggplot(combined5,aes(x = log1p(just.dist))) +
  theme_classic()+
  ggtitle("Soil temperature")+
  geom_point(aes(y=log1p(percentst_diff/100)),color="goldenrod2",alpha=0.2) +
  geom_smooth(aes(y=log1p(percentst_diff/100)),method = "lm", color = "green4",alpha=0) +
  xlab("log(Distance from edge)")+
  ylab("log(% difference from interior point)")+
  geom_hline(aes(yintercept=log1p(stCI)),color="blue")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) 

he <- ggplot(combined5,aes(x = log1p(just.dist))) +
  theme_classic()+
  ggtitle("Soil moisture")+
  geom_point(aes(y=log1p(percentsm_diff/100)),color="turquoise3",alpha=0.2) +
  geom_smooth(aes(y=log1p(percentsm_diff/100)),method = "lm", color = "green4",alpha=0) +
  xlab("log(Distance from edge)")+
  ylab("log(% difference from interior point)")+
  geom_hline(aes(yintercept=-log1p(smCI)),color="blue")+
  geom_line(aes(y=0),color="black")+
  ylim(-1,0.5)+
  geom_vline(xintercept=0) 

hf <- ggplot(combined5,aes(x = log1p(just.dist))) +
  theme_classic()+
  ggtitle("Light (PAR)")+
  geom_point(aes(y=log1p(percentPAR_diff/100)),color="maroon2",alpha=0.2) +
  geom_smooth(aes(y=log1p(percentPAR_diff/100)),method = "lm", color = "green4",alpha=0) +
  xlab("log(Distance from edge)")+
  ylab("log(% difference from interior point)")+
  geom_hline(aes(yintercept=log1p(parCI)),color="blue")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) 

hg <- ggplot(combined5,aes(x = log1p(just.dist))) +
  theme_classic()+
  ggtitle("Wind")+
  geom_point(aes(y=log1p(percentws_diff/100)),color="thistle4",alpha=0.2) +
  geom_smooth(aes(y=log1p(percentws_diff/100)),method = "lm", color = "green4",alpha=0) +
  xlab("log(Distance from edge)")+
  ylab("log(% difference from interior point)")+
  geom_hline(aes(yintercept=log1p(wsCI)),color="blue")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) 

grid.arrange(ha,hb,hc,hd,he,hf,hg,ncol=2,nrow=4)

#compare where 0 +/- CI measure crosses pred
#AT - CI 0.05, pred 0.05 at 0 m
#RH - CI -0.08, pred -0.07 at 0 m
#VPD - CI 0.27, pred 0.30 at 20 m and 0.20 at 40m
#ST - CI 0.06, pred 0.05 at 0 m
#SM - CI 0.17, pred does not approach
#PAR - CI 1.01, pred 1.07 at 0m and 0.44 at 5m
#WS - CI 0.68, pred 0.71 at 5m and 0.45 at 10m

newcidists <- data.frame(just.dist.l = log1p(c(3,7,25,30,35)))

vpdpred2 <- round(expm1(predict(vpdlm, newdata = newcidists)),2)
parpred2 <- round(expm1(predict(parlm, newdata = newcidists)),2)
wspred2 <- round(expm1(predict(wslm, newdata = newcidists)),2)

#VPD - CI 0.27, pred 0.30 at 20m and 0.27 at 25m
#PAR - CI 1.01, pred 1.07 at 0m and 0.56 at 3m
#WS - CI 0.68, pred 0.91 at 3m and 0.58 at 7m






###how many studies with data? ####
length(unique(matglmm$article.id))
length(unique(matglmm$article.id[!is.na(matglmm$just.dist)]))
length(unique(matglmm$article.id[!is.na(matglmm$matrix_type.f)]))
length(unique(matglmm$article.id[!is.na(matglmm$edge_orient.f)]))
length(unique(matglmm$article.id[!is.na(matglmm$edge_age_years)]))

length(unique(matglmm$article.id[!is.na(matglmm$percent_diff)]))
length(unique(matglmm$article.id[(!is.na(matglmm$percent_diff)) & (!is.na(matglmm$matrix_type.f))]))
length(unique(matglmm$article.id[(!is.na(matglmm$percent_diff)) & (!is.na(matglmm$edge_orient.f))]))
length(unique(matglmm$article.id[(!is.na(matglmm$percent_diff)) & (!is.na(matglmm$edge_age_years))]))

length(unique(matglmm$article.id[!is.na(matglmm$percentrh_diff)]))
length(unique(matglmm$article.id[(!is.na(matglmm$percentrh_diff)) & (!is.na(matglmm$matrix_type.f))]))
length(unique(matglmm$article.id[(!is.na(matglmm$percentrh_diff)) & (!is.na(matglmm$edge_orient.f))]))
length(unique(matglmm$article.id[(!is.na(matglmm$percentrh_diff)) & (!is.na(matglmm$edge_age_years))]))


length(unique(matglmm$article.id[!is.na(matglmm$percentst_diff)]))
length(unique(matglmm$article.id[(!is.na(matglmm$percentst_diff)) & (!is.na(matglmm$matrix_type.f))]))
length(unique(matglmm$article.id[(!is.na(matglmm$percentst_diff)) & (!is.na(matglmm$edge_orient.f))]))
length(unique(matglmm$article.id[(!is.na(matglmm$percentst_diff)) & (!is.na(matglmm$edge_age_years))]))


length(unique(matglmm$article.id[!is.na(matglmm$percentsm_diff)]))
length(unique(matglmm$article.id[(!is.na(matglmm$percentsm_diff)) & (!is.na(matglmm$matrix_type.f))]))
length(unique(matglmm$article.id[(!is.na(matglmm$percentsm_diff)) & (!is.na(matglmm$edge_orient.f))]))
length(unique(matglmm$article.id[(!is.na(matglmm$percentsm_diff)) & (!is.na(matglmm$edge_age_years))]))


length(unique(matglmm$article.id[!is.na(matglmm$percentPAR_diff)]))
length(unique(matglmm$article.id[(!is.na(matglmm$percentPAR_diff)) & (!is.na(matglmm$matrix_type.f))]))
length(unique(matglmm$article.id[(!is.na(matglmm$percentPAR_diff)) & (!is.na(matglmm$edge_orient.f))]))
length(unique(matglmm$article.id[(!is.na(matglmm$percentPAR_diff)) & (!is.na(matglmm$edge_age_years))]))


length(unique(matglmm$article.id[!is.na(matglmm$percentVPD_diff)]))
length(unique(matglmm$article.id[(!is.na(matglmm$percentVPD_diff)) & (!is.na(matglmm$matrix_type.f))]))
length(unique(matglmm$article.id[(!is.na(matglmm$percentVPD_diff)) & (!is.na(matglmm$edge_orient.f))]))
length(unique(matglmm$article.id[(!is.na(matglmm$percentVPD_diff)) & (!is.na(matglmm$edge_age_years))]))


length(unique(matglmm$article.id[!is.na(matglmm$percentws_diff)]))
length(unique(matglmm$article.id[(!is.na(matglmm$percentws_diff)) & (!is.na(matglmm$matrix_type.f))]))
length(unique(matglmm$article.id[(!is.na(matglmm$percentws_diff)) & (!is.na(matglmm$edge_orient.f))]))
length(unique(matglmm$article.id[(!is.na(matglmm$percentws_diff)) & (!is.na(matglmm$edge_age_years))]))



###making heatmaps with loess curves, Python####

distancecats <- c(-10,0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200)

### AT
loes <- loess(percent_diff ~ just.dist,data = vardata)
newpoints <- predict(loes, newdata = distancecats)

heatpointsAT <- do.call(rbind,Map(data.frame, distance = distancecats, AT = newpoints))

#export for python
write.csv(heatpointsAT,"./Outputs/heatpointsAT.csv",row.names = FALSE)



### RH
loes <- loess(percentrh_diff ~ just.dist,data = vardata)
newpoints <- predict(loes, newdata = distancecats)

heatpointsRH <- do.call(rbind,Map(data.frame, distance = distancecats, RH = newpoints))

#export for python
write.csv(heatpointsRH,"./Outputs/heatpointsRH.csv",row.names = FALSE)


### ST
loes <- loess(percentst_diff ~ just.dist,data = vardata)
newpoints <- predict(loes, newdata = distancecats)

heatpointsST <- do.call(rbind,Map(data.frame, distance = distancecats, ST = newpoints))

#export for python
write.csv(heatpointsST,"./Outputs/heatpointsST.csv",row.names = FALSE)

### SM
loes <- loess(percentsm_diff ~ just.dist,data = vardata)
newpoints <- predict(loes, newdata = distancecats)

heatpointsSM <- do.call(rbind,Map(data.frame, distance = distancecats, SM = newpoints))

#export for python
write.csv(heatpointsSM,"./Outputs/heatpointsSM.csv",row.names = FALSE)

### VPD
loes <- loess(percentVPD_diff ~ just.dist,data = vardata)
newpoints <- predict(loes, newdata = distancecats)

heatpointsVPD <- do.call(rbind,Map(data.frame, distance = distancecats, VPD = newpoints))
#export for python
write.csv(heatpointsVPD,"./Outputs/heatpointsVPD.csv",row.names = FALSE)

### PAR
loes <- loess(percentPAR_diff ~ just.dist,data = vardata)
newpoints <- predict(loes, newdata = distancecats)

heatpointsPAR <- do.call(rbind,Map(data.frame, distance = distancecats, PAR = newpoints))
#export for python
write.csv(heatpointsPAR,"./Outputs/heatpointsPAR.csv",row.names = FALSE)

### WS
loes <- loess(percentws_diff ~ just.dist,data = vardata)
newpoints <- predict(loes, newdata = distancecats)

heatpointsWS <- do.call(rbind,Map(data.frame, distance = distancecats, WS = newpoints))
#export for python
write.csv(heatpointsWS,"./Outputs/heatpointsWS.csv",row.names = FALSE)



#find actual abiota measurements ####
biomes <- mergedrefined8[,c(1,96)]

#AT
#merge with biomes
sepAT <- merge(sepAT,biomes,by="article.id", all = FALSE)
#avg int temp per biome
avgbiome <- sepAT %>% group_by(broad) %>% summarize(avg=round(mean(max.airtemp,na.rm=T),2),five=round(avg*0.05,2),ten=round(avg*0.1,2)) #no data for actual boreal temps, just changes
#at or less than 5% change
#no interior points
sepATdist <- sepAT[!(sepAT$just.dist == sepAT$max.dist),]
fivechangesAT <- sepATdist[abs(sepATdist$full_diff/sepATdist$max.airtemp) <= 0.05,]
fivechangesAT <- fivechangesAT[!is.na(fivechangesAT$article.id),]
#where only other.diff exists
fivechangesAT2 <- sepATdist[abs(sepATdist$other.diff/sepATdist$max.diff) <= 0.05,]
fivechangesAT2 <- fivechangesAT2[!is.na(fivechangesAT2$article.id),]
#per biome
ATbor <- rbind(fivechangesAT[fivechangesAT$broad=="boreal",], fivechangesAT2[fivechangesAT2$broad=="boreal",])
ATtemp <- rbind(fivechangesAT[fivechangesAT$broad=="temperate",],fivechangesAT2[fivechangesAT2$broad=="temperate",])
ATtrop <- rbind(fivechangesAT[fivechangesAT$broad=="tropical",],fivechangesAT2[fivechangesAT2$broad=="tropical",])


