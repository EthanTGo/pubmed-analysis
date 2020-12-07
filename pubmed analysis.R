getwd()
setwd("/Users/ethango")

library(RISmed)

word = list('influenza', 'obesity', 'cancer', 'covid-19')
year = list(2016,2017,2018,2019,2020)
result.df <- expand.grid(word, year)
result.list <- lapply(apply(result.df, 1, identity), unlist)
years <- c()
keyword <- c()
count <- c()
for (i in 1:20){
  search_topic <- result.list[[i]][1]
  keyword <- append(keyword,search_topic)
  year <- as.integer(result.list[[i]][2])
  years <- append(years,year)
  search_query <- EUtilsSummary(search_topic, mindate=year, maxdate=year)
  count <- append(count,attr(search_query, 'count') )
  Sys.sleep(1)
}
dow <- data.frame(
  Year = years,
  KeyWord = keyword,
  Count = count
)
dow

res <- EUtilsSummary("influenza", type="esearch", db="pubmed", datetype='pdat', mindate=2000, maxdate=2015, retmax=500)
QueryCount(res)
t<-ArticleTitle(EUtilsGet(res))
head(t)


word = list('influenza', 'obesity', 'cancer', 'covid-19', 'mental health')
year = list(2016,2017,2018,2019,2020)
result.df <- expand.grid(word, year)
result.list <- lapply(apply(result.df, 1, identity), unlist)
years <- c()
keyword <- c()
count <- c()
for (i in 1:20){
  search_topic <- result.list[[i]][1]
  keyword <- append(keyword,search_topic)
  year <- as.integer(result.list[[i]][2])
  years <- append(years,year)
  search_query <- EUtilsSummary(search_topic, mindate=year, maxdate=year)
  count <- append(count,attr(search_query, 'count') )
  Sys.sleep(1)
}
dow <- data.frame(
  Year = years,
  KeyWord = keyword,
  Count = count
)
dow

#install.packages("ggplot2")
#install.packages("ggthemes")
#install.packages("extrafont")
#install.packages("plyr")
library("ggplot2")
library("ggthemes")
library("extrafont")
library("plyr")

p1 <- ggplot() + geom_area(aes(y = Count, x = Year, fill =
                                 KeyWord), data = dow, stat="identity") + ggtitle('Area Chart of different word from 2016 to 2020')
p2 <- p1 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
p2








library(qdap)
myFunc<-function(argument){
  articles1<-data.frame('Abstract'=AbstractText(fetch), 'Year'=YearPubmed(fetch))
  abstracts1<-articles1[which(articles1$Year==argument),]
  abstracts1<-data.frame(abstracts1)
  abstractsOnly<-as.character(abstracts1$Abstract)
  abstractsOnly<-paste(abstractsOnly, sep="", collapse="")
  abstractsOnly<-as.vector(abstractsOnly)
  abstractsOnly<-strip(abstractsOnly)
  stsp<-rm_stopwords(abstractsOnly, stopwords = qdapDictionaries::Top100Words)
  ord<-as.data.frame(table(stsp))
  ord<-ord[order(ord$Freq, decreasing=TRUE),]
  head(ord,20)
}


oTen<-myFunc(2010)
oEleven<-myFunc(2011)
oTwelve<-myFunc(2012)
oThirteen<-myFunc(2013)
oFourteen<-myFunc(2014)
oFifteen<-myFunc(2015)
oSixteen<-myFunc(2016)
oSeventeen<-myFunc(2017)
oEighteen<-myFunc(2018)
oNineteenn<-myFunc(2019)

all<-cbind(oSix, oSeven)
names(all)<-c("2006","freq","2007","freq")

all








