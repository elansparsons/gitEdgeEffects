library(reshape2)
library(ggplot2)

#knit together qualitative & quantitative data

qualquan <- merge(vardata,mergedrefined8,by="article.id", all = TRUE)


###without 1 ha plots
#ids with 1ha plots:



#graph by broad region
quanonly <- qualquan[!is.na(qualquan$just.dist),]

tropics <- qualquan[quanonly$broad == "tropical",]

boreal <- qualquan[quanonly$broad == "boreal",]

temperate <- qualquan[quanonly$broad == "temperate",]


length(unique(tropics$article.id))
length(unique(temperate$article.id))
length(unique(boreal$article.id))

#tropics trends
ggplot(tropics,aes(x = just.dist)) +
  geom_smooth(aes(y=percent_diff), color = "green",alpha=0) +
  geom_smooth(aes(y=percentws_diff), color = "red",alpha=0) +
  geom_smooth(aes(y=percentVPD_diff), color = "blue",alpha=0) +
  geom_smooth(aes(y=percentst_diff), color = "cyan",alpha=0) +
  geom_smooth(aes(y=percentsm_diff), color = "orange",alpha=0) +
  geom_smooth(aes(y=percentrh_diff), color = "purple",alpha=0) +
  geom_line(aes(y=0),color="black")+
  coord_cartesian(xlim=c(-10,250))

ggplot(tropics,aes(x = just.dist)) +
  geom_smooth(aes(y=percentPAR_diff), color = "yellow",alpha=0) +
  geom_line(aes(y=0),color="black")+
  coord_cartesian(xlim=c(-10,250))

#temperate trends
ggplot(temperate,aes(x = just.dist)) +
  geom_smooth(aes(y=percent_diff), color = "green",alpha=0) +
  geom_smooth(aes(y=percentVPD_diff), color = "blue",alpha=0) +
  geom_smooth(aes(y=percentst_diff), color = "cyan",alpha=0) +
  geom_smooth(aes(y=percentsm_diff), color = "orange",alpha=0) +
  geom_smooth(aes(y=percentrh_diff), color = "purple",alpha=0) +
  geom_line(aes(y=0),color="black")+
  coord_cartesian(xlim=c(-10,250))

ggplot(temperate,aes(x = just.dist)) +
  geom_smooth(aes(y=percentPAR_diff), color = "yellow",alpha=0) +
  geom_smooth(aes(y=percentws_diff), color = "red",alpha=0) +
  geom_line(aes(y=0),color="black")+
  coord_cartesian(xlim=c(-10,250))

#boreal trends

ggplot(boreal,aes(x = just.dist)) +
  geom_smooth(aes(y=percent_diff), color = "green",alpha=0) +
  geom_smooth(aes(y=percentsm_diff), color = "orange",alpha=0) +
  geom_smooth(aes(y=percentrh_diff), color = "purple",alpha=0) +
  geom_point(aes(y=percentsm_diff),color="orange")+
  geom_line(aes(y=0),color="black")+
  coord_cartesian(xlim=c(-10,250),ylim=c(-30,30))

#temperate to compare to boreal
ggplot(temperate,aes(x = just.dist)) +
  geom_smooth(aes(y=percent_diff), color = "green",alpha=0) +
  geom_smooth(aes(y=percentsm_diff), color = "orange",alpha=0) +
  geom_smooth(aes(y=percentrh_diff), color = "purple",alpha=0) +
  geom_point(aes(y=percentsm_diff),color="orange")+
  geom_line(aes(y=0),color="black")+
  coord_cartesian(xlim=c(-10,250),ylim=c(-30,30))

#tropics to compare to boreal
ggplot(tropics,aes(x = just.dist)) +
  geom_smooth(aes(y=percent_diff), color = "green",alpha=0) +
  geom_smooth(aes(y=percentsm_diff), color = "orange",alpha=0) +
  geom_smooth(aes(y=percentrh_diff), color = "purple",alpha=0) +
  geom_point(aes(y=percentsm_diff),color="orange")+
  geom_line(aes(y=0),color="black")+
  coord_cartesian(xlim=c(-10,250),ylim=c(-30,30))


#GLM on distance, lat
#remove percentages on quanonly, log to remove neg
intermed <- log1p((quanonly[,c(6,16,19,22,25,28,31)]/100))
names(intermed) <- c("proat_diff","propar_diff","prorh_diff","prosm_diff","prost_diff","provpd_diff","prows_diff")
forlatglm <- cbind(intermed,quanonly)

atlat <- glm(proat_diff ~ just.dist + simple.lat, family = gaussian, data = forlatglm)

rhlat <- glm(prorh_diff ~ just.dist + simple.lat, family = gaussian, data = forlatglm)

vpdlat <- glm(provpd_diff ~ just.dist + simple.lat, family = gaussian, data = forlatglm)

stlat <- glm(prost_diff ~ just.dist + simple.lat, family = gaussian, data = forlatglm)

smlat <- glm(prosm_diff ~ just.dist + simple.lat, family = gaussian, data = forlatglm)

wslat <- glm(prows_diff ~ just.dist + simple.lat, family = gaussian, data = forlatglm)

parlat <- glm(propar_diff ~ just.dist + simple.lat, family = gaussian, data = forlatglm)