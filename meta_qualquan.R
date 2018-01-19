library(reshape2)

#knit together qualitative & quantitative data

qualquan <- merge(vardata2,begin,by="article.id", all = TRUE)

###without 1 ha plots
#ids with 1ha plots:



#graph by broad region
tropics <- qualquan[qualquan$broad == "tropical",]
boreal <- qualquan[qualquan$broad == "boreal",]
temperate <- qualquan[qualquan$broad == "temperate",]

ggplot(tropics,aes(x = just.dist)) +
  geom_smooth(aes(y=varpercent.at), color = "green",alpha=0) +
  geom_smooth(aes(y=varpercent.ws), color = "red",alpha=0) +
  geom_smooth(aes(y=varpercent.vpd), color = "blue",alpha=0) +
  geom_smooth(aes(y=varpercent.st), color = "cyan",alpha=0) +
  geom_smooth(aes(y=varpercent.sm), color = "orange",alpha=0) +
  geom_smooth(aes(y=varpercent.rh), color = "purple",alpha=0) +
  geom_smooth(aes(y=varpercent.par), color = "yellow",alpha=0)
