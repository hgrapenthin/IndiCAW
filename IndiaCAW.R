rm(list=ls())
####### Source of Data: http://ncrb.nic.in/CD-CII2012/Additional_Tables_CII_2012/Additional%20table%202012/CAWomen-CH-2001-2012.xls and http://ncrb.nic.in/CD-CII2012/cii-2012/Table%205.2.pdf
########## Preprocessing - Modified column names, made a few replacements ----- & to and, Odisha to Orissa, Delhi UT to Delhi, DandN Haveli to Dadra and Nagar Haveli,
########## Puducherry to Pondicherry, AandN Islands to Andaman and Nicobar Islands, removed rows for total crimes in Union Territories and total crimes in states but retained the grand total for both,
########## Few of the above changes done based on previous work with googleVis geoCharts in the Indian context
############ Inserted a column for year
#R_LIBS="C:/Users/patil/Documents/R/win-library"

setwd("~/R/RProject/IndiaCAW")
library(rCharts)
library(reshape2)
library(plyr)
library(scales)
library(XML)

# Get info on population status and growth of Indian States
url <- "http://en.wikipedia.org/wiki/List_of_states_and_union_territories_of_India_by_population"

pop <- readHTMLTable(url,as.data.frame=TRUE,header=TRUE,which=1,stringsAsFactors=FALSE)
pop <- pop[-nrow(pop), c(2,3,5:10)]
names(pop) <- c("StateUT","Population","Decadal Growth","Rural Pop.","Urban Pop.","Area km2","Density /km2","Sex Ratio")
pop1 <- pop
# pop1$Population <- gsub("\\(\\d{1,2}.?,?\\d{2,3}%\\)$","", pop$Population)
# str(pop$Population)
# Remove brackets and enclosed info
pop1[,c(2,4,5,6,7)] <- 
sapply(c(2,4,5,6,7), function(x) gsub("\\(.*\\)","", pop[,x])) # "\\(\\d{1,2}.\\d{2,3}%\\)"
pop2 <- pop1
# Strip leading 19 numbers - no idea where they come from
pop2[,c(2,4,5,6,7)] <- 
sapply(c(2,4,5,6,7), function(x) gsub("\\d{19}","", pop1[,x])) # "\\(\\d{1,2}.\\d{2,3}%\\)"
# remove other non-digits
pop2[,c(2,4,5,6,7)] <- 
sapply(c(2,4,5,6,7), function(x) gsub("/?km2","", pop2[,x])) # "\\(\\d{1,2}.\\d{2,3}%\\)"
pop2[,3] <- sapply(3, function(x) gsub("%","", pop2[,x])) # "\\(\\d{1,2}.\\d{2,3}%\\)"
# remove thousand seperator 
pop2[,c(2:8)] <- sapply(c(2:8), function(x) gsub("," ,"", pop2[,x]))
# remove all invisible spaces and newlines
pop2[,c(2:8)] <- sapply(c(2:8), function(x) gsub("[^0-9.]","", pop2[,x]))
# all numbers to numeric:
pop2[,c(2:8)] <- sapply(c(2:8), function(x) as.numeric(pop2[,x]))
# Unification of State Names
pop2$StateUT[pop2$StateUT=="Puducherry"]="Pondicherry"
pop2$StateUT[pop2$StateUT=="Manipurβ"]="Manipur"
# Change "Odisha" to the old name Orissa for compatibility
pop2$StateUT[pop2$StateUT=="Odisha"]="Orissa"
# select the states with population > 60 Mio
pop2 <- subset(pop2, pop2$Population > 7000000)

pop3 <- pop2
# markieren der übernommenen "Population" mit der Jahreszahl 2010 - als Faktor
Year <- rep(2010,nrow(pop2))
pop3 <- cbind(pop2[,c(1,2)], Year) 
#save(pop2,file="population.csv")
## Calculate annual growth from decadal growth
dgr <- pop2[, "Decadal Growth"]/100     # decadal growth rate dgr
agr <- (1+dgr)^(1/10)-1                 # annual growth rate  agr
# Pop2001 <- pop3[,"Population"]/(1+dgr)*(1+agr)
# Pop2010 <- Pop2001*(1+agr)^9

### Start mit 2001
popula <- NULL
popula <- round(pop2[,"Population"]/(1+dgr)*(1+agr),0)
str(popula)
#Year <- rep(2001,nrow(pop2))
popula <- data.frame(pop2[,"StateUT"],popula)
names(popula) <- c("StateUT","2001")
# str(popula)

# and loop until 2012
poprun <- popula
# poprunn <- popula
Year <- 2001
i<-0
for (i in 1:11){
#  i<- 1
popnew <- round(poprun[,1+i]*(1+agr),0)
poprun <- cbind(poprun,popnew)
Year <- 2001+i
names(poprun)[2+i] <- Year
i <- i+1
}

# reshape to long format
popshape <- reshape(poprun,direction="long",
                    idvar="StateUT",
                    ids="Year", 
                    times=as.character(2001:2012),
                    varying=as.character(2001:2012),
                    v.names="Population")
# again cleaning up row.names
row.names(popshape) <- seq(len=nrow(popshape))
names(popshape) <- c("StateUT","Year","Population")

### Get data on literacy in Indian states
link <- "http://en.wikipedia.org/wiki/Demographics_of_India"
lit <- readHTMLTable(link,as.data.frame=TRUE,header=TRUE,which=11,stringsAsFactors=FALSE)
# remove sum line and State No column
lit <- lit[-nrow(lit),-1]
lit[,1][lit[,1]=="Puducherry"]="Pondicherry"
lit[,1][lit[,1]=="Odisha"]="Orissa"
# merge the 2 tables into one data frame. merge, not melt, not reshape
poplit <- merge(pop2,lit, by.x="StateUT", by.y="India/State/UT")

## and back to the original blog
CWFull=read.csv("CAWomen.csv")
# CWFullsub <- subset(CWFull, CWFull$StateUT==pop2$StateUT)
# get rid of "Total" last line
# CWFull <- CWFull[-nrow(CWFull),]
citiesCrime=read.csv("citiesCrime.csv")

# Change in total crimes over years for Different states, union territories, overall India
# Combine yearly crime date and yearly populations
TotPopmerge <- merge(CWFull,popshape, by.x=c("Year", "StateUT"), by.y=c("Year", "StateUT"))
# Reduce Population count to number of millions inhabitants for furthter calculation
TotPopmerge <- cbind(TotPopmerge,TotPopmerge$Population/1000000)
names(TotPopmerge)[names(TotPopmerge)=="TotPopmerge$Population/1e+06"] = "PopulationInMio"
# names(TotPopmerge)[16] <- 'PopulationInMio' # gives same result
# get relative (dimensionless) Crimes per Million Inhabitants
ToPoDi <- as.data.frame(TotPopmerge)
# Take out "ImportationOfGirlsFromForeignCountry", "CommissionOfSatiPreventionAct" due to very small numbers
ToPoDi <- ToPoDi[,c(1:8,10,11,14:16)]
# Calculate absolute numbers to relative
ToPoDi[,3:10] <- ToPoDi[,3:10]/ToPoDi[,13]

ToPoDimelt <- melt(ToPoDi, id.vars=c("Year","StateUT"),measure.vars=names(ToPoDi[,3:10]))


TotCrimesmelt=melt(CWFull,id=c("Year","StateUT"))

save(TotCrimesmelt,file="TotCrimesmelt.rda")
#Saved file used in shiny app "TotCrime" in the last section of blog post for getting insights on States and UTs.
# Let's look at the Total Crimes in India over years separately here

TCrimeplot=nPlot(value~Year, group="variable", data=TotCrimesmelt[which(TotCrimesmelt$StateUT=="TOTAL"),], type="lineWithFocusChart",
                 height=450,width=750)
TCrimeplot

# ToPoDiplot <- nPlot(value~Year, 
#  group="variable", 
#  data=ToPoDimelt[which(ToPoDimelt$StateUT=="Bihar"),], 
#  type="lineWithFocusChart",
#  height=450,width=750)
# which(ToPoDimelt$StateUT=="TOTAL"),
# ToPoDiplot
# Uttar Pradesh #Maharashtra

# TCrimeplot$save("TCrimeplot.html",cdn=T)
#TCrimeplot$publish("TCrimeplot",host='gist')

library(ggplot2)
# qplot(Year,value, data=ToPoDimelt, facets=StateUT~variable)
ptmp <- ggplot(ToPoDimelt, aes(x=Year, y=value))
ptmp <- ptmp + geom_line() + facet_grid(StateUT~variable)+ theme_bw()
ptmp
# Plot too crowded, so we look at rape only
TopoSub <- subset(ToPoDimelt,ToPoDimelt$variable=="Rape")
ptmp <- ggplot(TopoSub, aes(x=Year, y=value))
ptmp <- ptmp + geom_line() + facet_wrap(~StateUT) + theme_bw() + labs(title = "Rape in Major States of India")
ptmp
#dev.off()
ggsave(filename="IndiaRape.png")
