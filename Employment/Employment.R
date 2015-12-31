library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(maps)
library(bit64)
library(RColorBrewer)
library(choroplethr)

setwd("/Users/Jake/Projects/AnalyticsExamples/Employment")
data = read.csv('2012.annual.singlefile.csv', stringsAsFactors = F)

dim(data) #3556289      38

head(data)

aggLevelTitles = read.csv('agglevel_titles.csv', stringsAsFactors = F)
areaTitles = read.csv('area_titles.csv', stringsAsFactors = F)
industryTitles = read.csv('industry_titles.csv', stringsAsFactors = F)
ownershipTitles = read.csv('ownership_titles.csv', stringsAsFactors = F)
sizeTitles = read.csv('size_titles.csv', stringsAsFactors = F)

data = left_join(data, aggLevelTitles)
data = left_join(data, industryTitles)
data = left_join(data, ownershipTitles)
data = left_join(data, sizeTitles)

head(areaTitles)

county.fips$fips = str_pad(county.fips$fips, width=5, pad="0")
county.fips$polyname = as.character(county.fips$polyname)
county.fips$county = sapply(
  gsub('[a-z\ ]+,([a-z\ ]+)','\\1',county.fips$polyname),
  simpleCap)
county.fips = unique(county.fips)


state.fips$fips = str_pad(state.fips$fips, width=2, pad="0", 
                           side='left')
state.fips$state = as.character(state.fips$polyname)
state.fips$state = gsub("([a-z\ ]+):[a-z\ \\']+",'\\1',state.fips$state)
state.fips$state = sapply(state.fips$state, simpleCap)

mystatefips = unique(state.fips[,c('fips','abb','state')])
lower48 = setdiff(unique(state.fips$state),c('Hawaii','Alaska'))

myarea = merge(areaTitles, county.fips, by.x='area_fips',by.y='fips', all.x=T)
myarea$state_fips = substr(myarea$area_fips, 1,2)
myarea = merge(myarea, mystatefips,by.x='state_fips',by.y='fips', all.x=T)

data = left_join(data, myarea)
data = filter(data, state %in% lower48)

d.state = filter(data, agglvl_code==50)
d.state = select(d.state, state, avg_annual_pay, annual_avg_emplvl)

d.state$wage = cut(d.state$avg_annual_pay, 
                    quantile(d.state$avg_annual_pay, c(seq(0,.8, by=.2), .9, .95, .99, 1)))
d.state$empquantile = cut(d.state$annual_avg_emplvl, quantile(d.state$annual_avg_emplvl, c(seq(0,.8,by=.2),.9,.95,.99,1)))

x = quantile(d.state$avg_annual_pay, c(seq(0,.8,by=.2),.9, .95, .99, 1))
xx = paste(round(x/1000),'K',sep='')
Labs = paste(xx[-length(xx)],xx[-1],sep='-')
levels(d.state$wage) <- Labs

x = quantile(d.state$annual_avg_emplvl, 
              c(seq(0,.8,by=.2),.9, .95, .99, 1))
xx = ifelse(x>1000, paste(round(x/1000),'K',sep=''),
             round(x))
Labs = paste(xx[-length(xx)],xx[-1],sep='-')
levels(d.state$empquantile) = Labs

Discretize <- function(x, breaks=NULL){
  if(is.null(breaks)){
    breaks <- quantile(x, c(seq(0,.8,by=.2),.9, .95, .99, 1))
    if (sum(breaks==0)>1) { 
      temp <- which(breaks==0, arr.ind=TRUE)
      breaks <- breaks[max(temp):length(breaks)]
    }
  }
  x.discrete <- cut(x, breaks, include.lowest=TRUE)
  breaks.eng <- ifelse(breaks > 1000,
                       paste0(round(breaks/1000),'K'),
                       round(breaks))
  Labs <- paste(breaks.eng[-length(breaks.eng)], breaks.eng[-1],
                sep='-')
  levels(x.discrete) <- Labs
  return(x.discrete)
}

d.cty = filter(data, agglvl_code==70)%>%
  select(state,county,abb, avg_annual_pay, annual_avg_emplvl)%>%
  mutate(wage=Discretize(avg_annual_pay),
         empquantile=Discretize(annual_avg_emplvl))

state_df = map_data('state')
county_df = map_data('county')

transform_mapdata <- function(x){
  names(x)[5:6] <- c('state','county')
  for(u in c('state','county'){
    x[,u] <- sapply(x[,u],simpleCap)
  }
  return(x)
}
state_df <- transform_mapdata(state_df)
county_df <- transform_mapdata(county_df)

chor <- left_join(state_df, d.state, by='state')
ggplot(chor, aes(long,lat,group=group))+
  geom_polygon(aes(fill=wage))+geom_path(color='black',size=0.2)+ scale_fill_brewer(palette='PuRd') +
  theme(axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank())

ggsave(file="map.pdf")