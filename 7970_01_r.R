#EASWARAN.T.H (7970)
library(lubridate)
library(readxl)
library(tidyr)
library(dplyr)
library(varhandle) #unfactor
library(readr) #read_lim
library(descr) #crosstab
library(imputeTS)#impute values
library(mltools)#create bins
library(assertr)
library(stringr)

############   Salesdata dataset   ############
sld <- read_excel("SaleData.xlsx")
sld <- data.frame(sld)

##   1. Find the least amount sale that was done for each item
function.1 <- function(sld)
{
  lamt <- tapply(sld$Sale_amt,sld$Item,FUN=min)
  return(lamt)
}
print(function.1(sld))

##   2. Compute the total sales for each year and region across all items 
function.2 <- function(sld)
{
  x1<-tapply(df$Sale_amt,df$Region,FUN=sum)
  x2<-tapply(df$Sale_amt,year(df$OrderDate),FUN=sum)
  return(list(ans.1,ans.2))
}
print(function.2(sld))

##   3. Create new column 'days_diff' with number of days difference 
#       between reference date passed and each order date 
function.3 <- function(sld,ref_date)
{
  sld$days_diff<-round(difftime(ref_date,sld$OrderDate))
  return(sld)  
}
ref_date=Sys.Date()  #example
print(function.3(sld,ref_date))

##   4.Create a dataframe with two columns: 'manager', 'list_of_salesmen'.
#      Column 'manager' will contain the unique managers present and 
#      column 'list_of_salesmen' will contain an array of all salesmen under 
#      each manager
function.4 <- function(sld)
{
  man<-split.data.frame(sld,f=sld$Manager)
  v<-NULL
  for(i in 1:length(man))
  {
    t<-rbind(names(man[i]),c(distinct(man[[i]],SalesMan)))
    v<-cbind(v,t)
  }
  v<-data.frame(v)
  return(v)
}
print(function.4(sld))

##    5. For all regions find number of salesman and total sales. 
#       Return as a dataframe with three columns Region, salesmen_count 
#       and total_sale
function.5<- function(sld)
{  
  reg<-sld %>% group_by(Region) %>% summarise(n_distinct(SalesMan))
  tot<-tapply(sld$Sale_amt,sld$Region,FUN=sum)
  reg<-cbind(reg,tot)
  colnames(reg)<-c("Region","salesman_count","total_sales")
  return(reg)
}
print(function.5(sld))

##    6. Create a dataframe with total sales as percentage for each manager. 
#       Dataframe to contain manager and percent_sales
function.6 <- function(sld)
{
  per<-c(rep(0,4))
  man1<-split.data.frame(sld,f=sld$Manager)
  sale_tot=sum(sld$Sale_amt)
  for(i in 1:length(man1))
  {   
    per[i]<-sum(man1[[i]]$Sale_amt)/sale_tot
  }
  ret<-data.frame(names(man1),per)
  print(ret)
}
print(function.6(sld))

###########   IMDB Dataset    #############
imd <- read.csv("imdb.csv")
imd <- data.frame(imd)

##   7. Get the imdb rating for fifth movie of dataframe
function.7 <- function(imd)
{
  return(imd[5,6])
}
print(function.7(imd))

##  8. Return titles of movies with shortest and longest run time
function.8 <- function(imd)
{
  return(arrange(imd,duration)[c(1,nrow(imd)),'title'])
}
print(function.8(imd))

##  9. Sort the data frame by in the order of when they where released 
#       and have higer ratings, 
#       Hint : release_date (earliest) and Imdb rating(highest to lowest)
function.9 <- function(imd)
{
  return(arrange(imd,year,desc(imdbRating)))
}
print(function.9(imd))

##  10. Subset the dataframe with movies having the following prameters. 
#        revenue more than 2 million 
#        spent less than 1 million 
#        duration between 30 mintues to 180 minute
function.10 <- function(imd)
{
  return(subset(imd,between(duration,30,180)))  #only 3rd subdivision 
}
print(function.10(imd))

##############   diamonds dataset   #################
dmds <- read.csv("diamonds.csv")
dmds <- data.frame(dmds)

##  11. Count the duplicate rows of diamonds DataFrame. 
function.11 <- function(dmds)
{
  dis_dmds<- distinct(dmds)
  ndup<- nrow(dmds)-nrow(dis_dmds)
  return(ndup)  
}
print(function.11(dmds))

##  12. Drop rows in case of missing values in carat and cut columns. 
function.12 <- function(dmds)
{
  drp_dmds <- dmds[!((dmds$carat=="")|(dmds$cut=="")),]
  ndrp_dmds <- nrow(dmds)-nrow(drp_dmds)
  return(drp_dmds)
}
print(function.12(dmds))

##  13. Subset the dataframe with only numeric columns.
function.13 <- function(dmds)
{
  nums_dmds <- dmds[,unlist(lapply(dmds,is.numeric))]
  return(nums_dmds)
}
print(function.13(dmds))

##  14. Compute volume as (x y z) when depth is greater than 60. 
##     In case of depth less than 60 default volume to 8. 
function.14 <- function(dmds)
{
  volf<-function(depth,x,y,z)
  {
    if(depth>60)
      vol=x*y*z
    else
      vol=8
  }
  return(mapply(volf,dmds$depth,dmds$x,dmds$y,dmds$z))
}
print(function.14(dmds))

##  15. Impute missing price values with mean
function.15<-function(dmds)
{
  dmds$price[is.na(dmds$price)]<-mean(dmds$price,na.rm=T)
  return (dmds)
}
print(function.15(dmds))

##******************Bonus questions*****************************************
B_imd<-read_delim("imdb.csv", delim=',', escape_double=FALSE, escape_backslash=TRUE)
B_dia<-read_delim("diamonds.csv", delim=',', escape_double=FALSE, escape_backslash=TRUE)
B_met=read_delim('movie_metadata.csv', delim=',', escape_double=FALSE, escape_backslash=TRUE)

#  Bonus Q1.
bfunc.1<-function(df)
{
  movie <- df %>% select(imdbRating,year,duration)
  movie$title_length <- str_length(df$wordsInTitle)
  quant <- quantile(df$duration, probs = c(0.25,0.5, 0.75),na.rm=T)
  movie["percentile"]=movie["duration"] %>% lapply(function(x) {if(x<quant[1]) 1 else(if(x<quant[2]) 2 else(if(x<quant[3]) 3 else 4))})
  movie_table <- ftable(with(movie,tapply(title_length,c(),count)))
  return(movie_table)
}
print(bfunc.1(B_imd))

#  Bonus Q2.
bfunc.2<-function(imd)
{ 
  x<-na_mean(imd)
  x$year=floor(x$year)
  x$len=nchar(x$wordsInTitle)
  x[is.na(x$wordsInTitle),"len"]=nchar(as.character(unlist(x[is.na(x$wordsInTitle),"title"])))
  x$percentile<-bin_data(x$duration,bins=4,binType = "quantile")
  d<-as.data.frame.matrix(table(x$year,x$percentile))
  colnames(d)<-c("num_videos_less_than25Percentile","num_videos_25_50Percentile ","num_videos_50_75Percentile","num_videos_greaterthan75Precentile")
  y<-x%>%group_by(year)%>%summarise(min=min(len),max=max(len))
  return(cbind(y,d))
}
print(bfunc.2(B_imd))

#  Bonus Q3.
bfunc.3<-function(dia)
{
  dia$z[df.3$z=="None"]<-NA
  dia$z<-as.numeric(dia$z)
  x<-na_mean(dia)
  x$volume <- ifelse(x$depth>60,(x$x)*(x$y)*(as.integer(x$z)), 8)
  x$quant<-as.numeric(bin_data(x$volume,bins=5,binType="quantile"))
  y<-crosstab(x$quant,x$cut,plot=FALSE,prop.c=TRUE)
  return(y)
}
print(bfunc.3(B_dia))

#  Bonus Q4.
bfunc.4<-function(df)
{
  df=na.omit(df)
  x<-df %>% group_by(title_year)%>% top_frac(0.1,gross) %>% group_by(title_year,genres) %>% summarise(avgimdb=mean(imdb_score),count_movies=n())
  return(x)
}
print(bfunc.4(B_met))

#  Bonus Q5.
bfunc.5<-function(imd)
{
  imd<-imd[!is.na(imd$duration),]
  imd$decile=as.numeric(bin_data(imd$duration,bins=10,binType="quantile"))
  a<-imd[,17:45]
  b<-a%>%group_by(decile)%>%summarise_all(sum)
  x<-imd%>%group_by(decile)%>%summarise(nominations=sum(nrOfNominations),wins=sum(nrOfWins),count=n())
  x$top_genres=top_genre=col_concat(t(as.data.frame(apply(b,1,function(x) head(names(b)[order(-x)],3)),stringsAsFactors = FALSE)),sep="|")
  return(x)
}
print(bfunc.5(B_imd))
