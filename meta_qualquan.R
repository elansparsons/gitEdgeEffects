library(reshape2)

#knit together qualitative & quantitative data

qualquan <- merge(begin,vardata,by="article.id", all = TRUE)

###without 1 ha plots

