library(dplyr)

#knit together qualitative & quantitative data


colnames(mergedrefined7) <- c("article.id","variable","data","qualnotes")
mergedrefined7 %>% spread()


###without 1 ha plots

