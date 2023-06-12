
library(plyr)
library(dplyr)
library(naniar) 
library(lattice)

## LOADING AND CLEANING DATA
load("census.RData")
dim(census)

## 1. How many states are represented among the 74020 census tracts?
n_distinct(census$State_name)

#### How many counties? 
n_distinct(census$County_name)

## 2. Checking for classes
###Class of column 8
class(census$Med_HHD_Inc_ACS_09_13)

### class of column 9
class(census$Med_House_value_ACS_09_13)

### checking for the number of missing values in column 8 
length(census$Med_House_value_ACS_09_13[census$Med_HHD_Inc_ACS_09_13==""])

#### checking for the number of missing values in column 9
length(census$Med_House_value_ACS_09_13[census$Med_House_value_ACS_09_13==""]) #there are 1804 missing values in this column

## 3 Converting the 2 columns in From Factors to Numbers
dee = as.numeric(gsub("[\\$,]", "", census$ Med_HHD_Inc_ACS_09_13))
han = as.numeric(gsub("[\\$,]", "", census$ Med_House_value_ACS_09_13)) summary(dee)

### checking for missing values after variable conversion
n_miss(dee)
n_miss(han)

## 4 counting the number of missing values in each row
num.na.row = apply(census, 1, function(x) sum(is.na(x)))

### obtaining the indexes of rows containing missing values
contains.na=num.na.row[num.na.row>0]

## 5 states with no missing values
miss.row=unique(census[rowSums(is.na(census))>0,2]) no.miss.row=unique(census[,2]) 
setdiff(no.miss.row,miss.row)

## 6 remove all rows with missing values
new_census = census[complete.cases(census), ]
nrow(new_census)
attach(new_census)

#how many states are in the new dataframe
n_distinct(State_name) 

# How many counties?
n_distinct(County_name)


#state that has been taken off

setdiff(unique(census$State_name),unique(State_name))


#### dimension of the data

nrow(new_census)


# EXPLORATORY STATISTICS

## 1. the percentages of the population that are less than 5 years old
pct_Pop_0_4_ACS_09_13 =100-rowSums(new_census[,12:16]) Tot_Pop_0_4 =Tot_Population_ACS_09_13*(pct_Pop_0_4_ACS_09_13/100) pop.state_0_4= tapply(Tot_Pop_0_4,State_name, sum)

### determining Which state has the highest number of 0-4 years
sort(pop.state_0_4)[length(pop.state_0_4)]

## 2. Percentage of 0-4 year old
kat <- tapply(Tot_Population_ACS_09_13, State_name, sum) faz<- tapply(Tot_Pop_0_4, State_name, sum)
pct.all.states <- (faz/kat)*100 sort(pct.all.states)[length(pct.all.states)]

## 3 Calculating the correlation between each of the numeric variables
corr=cor(new_census[,8:31])

##### highest correlation variables
max.cor<- which(cor==max(cor[which(cor!=1 , arr.ind = T)]), arr.ind = T) names(new_census[0,8:31])[max.cor]

##### highest correlation value
corr.sorted[1:2]

##### least correlation variables
min.cor<- which(cor==min(cor), arr.ind = T) names(new_census[,8:31])[min.cor]

##### least correlation value
corr.sorted[576]

## 4. Plot a histogram of Med_House_value_ACS_09_13, and label the axes appropriately.
hist(Med_House_value_ACS_09_13, main='Histogram of Median House Value', xlab='Median house value',
     col='steelblue')

# 5 Applying my.test() Function to  variables in columns 10 through 31 of the census data frame
my.test = function(var){
  group = census$Med_House_value_ACS_09_13 == 1000001
  p.val = t.test(var[group], var[!group])$p.value return(p.val)
}

##### finding the 2 smallest p values
sort(apply(new_census[,10:31], 2, my.test)[1:2])

# SAMPLING AND PLOTTING

## 1 Writing a Function plot.sample()
plot.sample=function(x,y,nsample,xlab,ylab){ x=sample(x,500)
y=sample(y,500)
if(length(x)==length(y)){ plot(x,y,xlab = xlab,ylab = ylab)
}else{stop("the lenght of x and y are unequal")}
}
plot.sample(Med_HHD_Inc_ACS_09_13, Med_House_value_ACS_09_13, xlab="Median HHD income", ylab="Median house value")

## 2. adding a trend line
add.lineartrend.info=function(x,y){ abline(lm(y~x),lwd=2,col="red") title (signif(cor(x,y),3))
}
plot.sample(Med_HHD_Inc_ACS_09_13, Med_House_value_ACS_09_13, xlab="Median HHD income", ylab="Median house value")
add.lineartrend.info(Med_HHD_Inc_ACS_09_13,Med_House_value_ACS_09_13)

## 3. plot all function
plot.all = function(dataset, nsample=500){ p = ncol(dataset)
orig.par = par()
# Set the margins, and plotting grid
par(mar=c(2,2,2,2)) par(mfrow=c(p,p))
# TODO: your plotting code goes here
for (x in 1:p){
  for (y in 1:p){
    if(x==y){ plot(c(0,10),c(0,10),type = "n") text(5,5,labels =
                                                      paste(names(dataset)[x]))
    }else{
      plot.sample(dataset[,x],dataset[,y],
                  xlab = names(dataset)[x],ylab = names(dataset)[y]) add.lineartrend.info(dataset[,x],dataset[,y])
    }
  }
}
par(mar=orig.par$mar) par(mfrow=orig.par$mfrow)
}
mydat = new_census[,c("Med_HHD_Inc_ACS_09_13",
                      "Med_House_value_ACS_09_13","pct_College_ACS_09_13", "Mail_Return_Rate_CEN_2010")]
plot.all(mydat)




