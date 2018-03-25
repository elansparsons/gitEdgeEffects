#Combined qualitative and quantitative data

library(reshape2) #Version 0.8.7
library(ggplot2) #Version 2.2.1
library(gridExtra) #Version 2.3
library(readr) #Version 1.1.1

#knit together qualitative & quantitative data
vardata <- read_csv("./Outputs/vardata.csv")
mergedrefined8 <- read_csv("./Data/mergedrefined8.csv")

qualquan <- merge(vardata,mergedrefined8,by="article.id", all = TRUE)



#graph by broad region ####
quanonly <- qualquan[!is.na(qualquan$just.dist),]

tropics <- quanonly[quanonly$broad == "tropical",]

boreal <- quanonly[quanonly$broad == "boreal",]

temperate <- quanonly[quanonly$broad == "temperate",]

#citations, with data ####
quanbroad <- unique(quanonly[,c(1,36,129)])
length(unique(quanbroad$article.id[quanbroad$broad=="tropical"]))
length(unique(quanbroad$article.id[quanbroad$broad=="temperate"]))
length(unique(quanbroad$article.id[quanbroad$broad=="boreal"]))

quan.cite <- quanbroad %>% group_by(broad) %>% summarize(sum(citations)) 




#tropics trends ####
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

ba <- ggplot(tropics,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Temperature and humidity")+
  geom_smooth(aes(y=percent_diff), color = "lightslateblue",alpha=0) +
  geom_smooth(aes(y=percentrh_diff), color = "indianred3",alpha=0) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  coord_cartesian(xlim=c(-10,250))

bb <- ggplot(tropics,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("VPD")+
  geom_point(aes(y=percentVPD_diff), color = "lightslateblue") +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  coord_cartesian(xlim=c(-10,250))

bc <- ggplot(tropics,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Soil")+
  geom_smooth(aes(y=percentsm_diff), color = "turquoise3",alpha=0) +
  geom_smooth(aes(y=percentst_diff), color = "goldenrod2",alpha=0) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  coord_cartesian(xlim=c(-10,250))

bd <- ggplot(tropics,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Light")+
  geom_smooth(aes(y=percentPAR_diff), color = "maroon2",alpha=0) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  coord_cartesian(xlim=c(-10,250))

be <- ggplot(tropics,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Wind")+
  geom_smooth(aes(y=percentws_diff), color = "thistle4",alpha=0) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  coord_cartesian(xlim=c(-10,250))

grid.arrange(ba,bc,bd,be,ncol=2,nrow=2)

#temperate trends ####
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

ca <- ggplot(temperate,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Temperature and humidity")+
  geom_smooth(aes(y=percent_diff), color = "lightslateblue",alpha=0) +
  geom_smooth(aes(y=percentrh_diff), color = "indianred3",alpha=0) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  coord_cartesian(xlim=c(-10,250))

cb <- ggplot(temperate,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("VPD")+
  geom_point(aes(y=percentVPD_diff), color = "lightslateblue") +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  coord_cartesian(xlim=c(-10,250))

cc <- ggplot(temperate,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Soil")+
  geom_smooth(aes(y=percentsm_diff), color = "turquoise3",alpha=0) +
  geom_smooth(aes(y=percentst_diff), color = "goldenrod2",alpha=0) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  coord_cartesian(xlim=c(-10,250))

cd <- ggplot(temperate,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Light")+
  geom_smooth(aes(y=percentPAR_diff), color = "maroon2",alpha=0) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  coord_cartesian(xlim=c(-10,250))

ce <- ggplot(temperate,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Wind")+
  geom_smooth(aes(y=percentws_diff), color = "thistle4",alpha=0) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  coord_cartesian(xlim=c(-10,250))

grid.arrange(ca,cc,cd,ce,ncol=2,nrow=2)

#boreal trends ####

ggplot(boreal,aes(x = just.dist)) +
  geom_smooth(aes(y=percent_diff), color = "green",alpha=0) +
  geom_smooth(aes(y=percentsm_diff), color = "orange",alpha=0) +
  geom_smooth(aes(y=percentrh_diff), color = "purple",alpha=0) +
  geom_point(aes(y=percentsm_diff),color="orange")+
  geom_line(aes(y=0),color="black")+
  coord_cartesian(xlim=c(-10,250),ylim=c(-30,30))

da <- ggplot(boreal,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Temperature and humidity")+
  geom_smooth(aes(y=percent_diff), color = "lightslateblue",alpha=0) +
  geom_smooth(aes(y=percentrh_diff), color = "indianred3",alpha=0) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  coord_cartesian(xlim=c(-10,250))

db <- ggplot(boreal,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("VPD")+
  geom_point(aes(y=percentVPD_diff), color = "lightslateblue") +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  coord_cartesian(xlim=c(-10,250))

dc <- ggplot(boreal,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Soil")+
  geom_smooth(aes(y=percentsm_diff), color = "turquoise3",alpha=0) +
  geom_smooth(aes(y=percentst_diff), color = "goldenrod2",alpha=0) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  coord_cartesian(xlim=c(-10,250))

dd <- ggplot(boreal,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Light")+
  geom_smooth(aes(y=percentPAR_diff), color = "maroon2",alpha=0) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  coord_cartesian(xlim=c(-10,250))

de <- ggplot(boreal,aes(x = just.dist)) +
  theme_classic()+
  ggtitle("Wind")+
  geom_smooth(aes(y=percentws_diff), color = "thistle4",alpha=0) +
  xlab("Distance from edge")+
  ylab("% difference from interior point")+
  geom_line(aes(y=0),color="black")+
  geom_vline(xintercept=0) +
  coord_cartesian(xlim=c(-10,250))

#no data for light and wind

grid.arrange(da,dc,dd,de,ncol=2,nrow=2)

#temperate to compare to boreal ####
ggplot(temperate,aes(x = just.dist)) +
  geom_smooth(aes(y=percent_diff), color = "green",alpha=0) +
  geom_smooth(aes(y=percentsm_diff), color = "orange",alpha=0) +
  geom_smooth(aes(y=percentrh_diff), color = "purple",alpha=0) +
  geom_point(aes(y=percentsm_diff),color="orange")+
  geom_line(aes(y=0),color="black")+
  coord_cartesian(xlim=c(-10,250),ylim=c(-30,30))

#tropics to compare to boreal ####
ggplot(tropics,aes(x = just.dist)) +
  geom_smooth(aes(y=percent_diff), color = "green",alpha=0) +
  geom_smooth(aes(y=percentsm_diff), color = "orange",alpha=0) +
  geom_smooth(aes(y=percentrh_diff), color = "purple",alpha=0) +
  geom_point(aes(y=percentsm_diff),color="orange")+
  geom_line(aes(y=0),color="black")+
  coord_cartesian(xlim=c(-10,250),ylim=c(-30,30))


#GLM on distance, lat ####
#remove percentages on quanonly, log to remove neg
intermed <- log1p((quanonly[,c(6,16,19,22,25,28,31)]/100))
names(intermed) <- c("proat_diff","propar_diff","prorh_diff","prosm_diff","prost_diff","provpd_diff","prows_diff")
forlatglm <- cbind(intermed,quanonly)

atlat <- glm(proat_diff ~ just.dist + simple.lat, family = gaussian, data = forlatglm) #better
atlat2 <- glm(proat_diff ~ simple.lat, family = gaussian, data = forlatglm)
atlat3 <- glm(proat_diff ~ just.dist * simple.lat, family = gaussian, data = forlatglm) #better

rhlat <- glm(prorh_diff ~ just.dist + simple.lat, family = gaussian, data = forlatglm) #better
rhlat2 <- glm(prorh_diff ~ simple.lat, family = gaussian, data = forlatglm)
rhlat3 <- glm(prorh_diff ~ just.dist * simple.lat, family = gaussian, data = forlatglm) #better

vpdlat <- glm(provpd_diff ~ just.dist + simple.lat, family = gaussian, data = forlatglm)
vpdlat2 <- glm(provpd_diff ~ simple.lat, family = gaussian, data = forlatglm)
vpdlat3 <- glm(provpd_diff ~ just.dist * simple.lat, family = gaussian, data = forlatglm) #best

stlat <- glm(prost_diff ~ just.dist + simple.lat, family = gaussian, data = forlatglm) #better
stlat2 <- glm(prost_diff ~ simple.lat, family = gaussian, data = forlatglm)
stlat3 <- glm(prost_diff ~ just.dist * simple.lat, family = gaussian, data = forlatglm) #better

smlat <- glm(prosm_diff ~ just.dist + simple.lat, family = gaussian, data = forlatglm)
smlat2 <- glm(prosm_diff ~ simple.lat, family = gaussian, data = forlatglm) #somewhat better
smlat3 <- glm(prosm_diff ~ just.dist * simple.lat, family = gaussian, data = forlatglm)

wslat <- glm(prows_diff ~ just.dist + simple.lat, family = gaussian, data = forlatglm) #somewhat better
wslat2 <- glm(prows_diff ~ simple.lat, family = gaussian, data = forlatglm)
wslat3 <- glm(prows_diff ~ just.dist * simple.lat, family = gaussian, data = forlatglm) #somewhat better

parlat <- glm(propar_diff ~ just.dist + simple.lat, family = gaussian, data = forlatglm) #better
parlat2 <- glm(propar_diff ~ simple.lat, family = gaussian, data = forlatglm)
parlat3 <- glm(propar_diff ~ just.dist * simple.lat, family = gaussian, data = forlatglm) #better

summary(lm(proat_diff ~ simple.lat,data=forlatglm))

#graph GLM ####
ggplot(forlatglm,aes(x = simple.lat)) +
  geom_smooth(aes(y=proat_diff),method="glm", color = "green",alpha=0) +
  geom_smooth(aes(y=prorh_diff),method="glm", color = "red",alpha=0) +
  geom_smooth(aes(y=provpd_diff),method="glm", color = "purple",alpha=0) +
  geom_point(aes(y=proat_diff),color="green")+
  geom_point(aes(y=prorh_diff),color="red")+
  geom_point(aes(y=provpd_diff),color="purple")