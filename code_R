setwd('C:/Users/Tania/Desktop/seminar')
options(na.action = "na.exclude")
library(sandwich)
library(lmtest)
library(car)
library(foreign)
library(stargazer)
library(vcd)

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=TRUE,
                      conf.interval=.95, .drop=TRUE) {
  #library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


options(scipen=999)
x <- read.csv("f520dat.csv", header = TRUE, stringsAsFactors = FALSE)

options(na.action = "na.exclude")

# remove people in non relevant age
new_data = x[x$birthy < 4,]
# View(new_data)

#remove people without year income
new_data = new_data[new_data$cens50 != '.',]
# View(new_data)

# combine Soviet Union
new_data$origin[new_data$origin == 4] <- 3
new_data$origin[new_data$origin == 5] <- 3
new_data$origin[new_data$origin == 6] <- 3

#change names of data columns
names(new_data)[names(new_data) == "cens20"] <- "educ"
names(new_data)[names(new_data) == "cens23"] <- "occup"

# remove NA's from diploma
new_data = new_data[new_data$diploma != '.',]

# remove from educ and diploma people without any diploma or with "other diploma"
new_data = new_data[new_data$diploma != 8,]
new_data = new_data[new_data$diploma != 9,]
new_data = new_data[new_data$educ != 8,]
new_data = new_data[new_data$educ != 9,]
# dip_abroad
new_data$dip_abroad = ifelse(new_data$diploma == new_data$educ,1,0)

# combine MA with Phd
new_data$educ[new_data$educ == 7] <- 6
table(new_data$origin,new_data$educ)

# combine all the groups without academic education
new_data$educ[new_data$educ == 2] <- 1
new_data$educ[new_data$educ == 3] <- 1
new_data$educ[new_data$educ == 4] <- 1

# remove Ethiopians
new_data = new_data[new_data$origin != 1,]

# names to the origin and education
new_data$educ[new_data$educ == 1] = "Low"
new_data$educ[new_data$educ == 5] = "BA"
new_data$educ[new_data$educ == 6] = "Ma&Phd"
new_data$origin[new_data$origin == 2] = "Asia&Africa"
new_data$origin[new_data$origin == 3] = "USSR"
new_data$origin[new_data$origin == 7] = "Euor&America"


table(new_data$origin,new_data$educ) #important - shows frequancy in each category

# replace NA's in Average gross monthly income as employee and independent (cens46,cens47) by 0
new_data$cens46 <- as.numeric(as.character(new_data$cens46))
new_data$cens46 = replace(new_data$cens46,is.na(new_data$cens46),0)
new_data$cens47 <- as.numeric(as.character(new_data$cens47))
new_data$cens47 = replace(new_data$cens47,is.na(new_data$cens47),0)



table(new_data$cens29)
table(new_data$cens30)
Data <- subset(new_data, select = c(2,4,5,6,7,193,202,203,223,219,220,250,196))
View(Data)


# remove people we dont know about the width of their employment
Data = Data[Data$cens29 != '.',]
Data = Data[Data$cens30 != '.',]
Data = Data[Data$cens30 < '2',]

# remove NA's from occup or people that their occupation unknown
Data = Data[Data$occup != '.',]
Data = Data[Data$occup != 'X',]

summary(Data$occup)
# combine the 5 catgory in occup to 4 catgory (small number of observations)
Data$occup[Data$occup == 5] <- 4
Data$occup[Data$occup == 4] <- 5 
#transfer poeple from the army to 4 catgory instead of 0
Data$occup[Data$occup == 0] <- 4 


# remove people that worked less than 10  months during 08'
Data$cens29 <- as.numeric(as.character(Data$cens29))
Data = Data[Data$cens29 > 10,]
table(Data$cens29)
table(Data$cens30)
table(Data$origin,Data$educ)

# new colum that sums the income for employee and independent 
Data$salary = Data$cens46 + Data$cens47

# Define the approximate range of salary
## Average salary for each degree
means<-by(Data$salary, Data$educ, mean)
print(means)
## Average salary for each type of occupation
means_occup<-by(Data$salary, Data$occup, mean)
print(means_occup)
Data$salary[Data$salary<3000] <- NA
Data$salary[Data$salary>75000] <- NA
options(na.action = "na.exclude")

# lSalary

Data$lsalary = log(Data$salary)
summary(Data$lsalary)
sd(Data$lsalary)


# occupation vs lsalary - see the graph, the gaps between categories aren't constant
require('ggplot2')
ggplot(Data,aes(x=occup,y=lsalary)) + 
  geom_boxplot(color = "red", fill = "orange", alpha=0.2)

# educ - education
Data$educ = as.factor(Data$educ)
Data$educ = relevel(Data$educ, ref = "Low")
summary(Data$educ)

# origin - country of birth
Data$origin = as.factor(Data$origin)
summary(Data$origin)

# dip_abroad
Data$dip_abroad = as.factor(Data$dip_abroad)
table(Data$dip_abroad)

# birthy
table(Data$birthy)
summary(Data$birthy)
#Data$birthy = as.factor(Data$birthy)


# sex
Data$sex = as.factor(Data$sex)
table(Data$sex)


# occup: occupation
#names
Data$occup[Data$occup == 1] = "Manager"
Data$occup[Data$occup == 2] = "Academic"
Data$occup[Data$occup == 3] = "Technic"
Data$occup[Data$occup == 4] = "Military"
Data$occup[Data$occup == 5] = "Office"
Data$occup[Data$occup == 6] = "Agriculture"
Data$occup[Data$occup == 9] = "Non-profession"
Data$occup = as.factor(Data$occup)



# intersection

head(Data)
# occupation vs lsalary
ggplot(Data,aes(x=occup,y=lsalary)) + 
  geom_boxplot(color = "red", fill = "orange", alpha=0.2)
# origin vs lsalary
ggplot(Data,aes(x=origin,y=lsalary)) + 
  geom_boxplot(color = "red", fill = "orange", alpha=0.2)
# dip_abroad vs l salary
ggplot(Data,aes(x=dip_abroad,y=lsalary)) + 
  geom_boxplot(color = "purple", fill = "blue", alpha=0.2)
#Correlation between occupation and education
Data1 <- subset(Data, select = c(6,13))
summary(Data1)
assocstats(table(Data1$educ, Data1$occup))

# Graphs : 
# salary, origin, educ

dataSum <- summarySE(Data, measurevar="salary", groupvars=c("origin","educ"))
# Error bars represent standard error of the mean
ggplot(dataSum, aes(fill=educ, y=salary, x=origin) ) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=salary-se, ymax=salary+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))

# salary, origin, dip_abroad 

dataSum <- summarySE(Data, measurevar="salary", groupvars=c("origin","dip_abroad","educ"))
# Error bars represent standard error of the mean
ggplot(dataSum, aes(fill=dip_abroad, y=salary, x=origin) ) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=salary-se, ymax=salary+se),
                width=.3,                    # Width of the error bars
                position=position_dodge(.9))+
  facet_grid(~educ)+
  theme_bw(base_size = 12)

# Distribution of education for each origin group
ggplot(Data, aes(x= educ,  group=origin)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "percentage", x="education", title = "Distribution of education levels for each origin")+
  facet_grid(~origin) +
  theme_bw(base_size = 12) +
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), legend.position = "none")

# occup & origin 
ggplot(Data, aes(x= occup,  group=origin)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5, size=2.5) +
  labs(y = "Percent", x="occupation",title = "Distribution of occupation for each origin") +
  facet_grid(~origin) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw(base_size = 12) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), legend.position = "none")


ggplot(Data[Data$educ == "BA",], aes(x= occup,  group=origin)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5, size=2.5) +
  labs(y = "Percent", x="occupation",title = "Distribution of occupation for each origin with BA") +
  facet_grid(~origin) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw(base_size = 12) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), legend.position = "none")



ggplot(Data[Data$educ == "Low",], aes(x= occup,  group=origin)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5, size=2.5) +
  labs(y = "Percent", fill="occupation",title = "Distribution of occupation for each origin with Low") +
  facet_grid(~origin) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw(base_size = 12) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), legend.position = "none")


ggplot(Data[Data$educ == "Ma&Phd",], aes(x= occup,  group=origin)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5, size=2.5) +
  labs(y = "Percent", x="occupation",title = "Distribution of occupation for each origin with Ma&PhD") +
  facet_grid(~origin) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw(base_size = 12) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), legend.position = "none")


dataSum <- summarySE(Data, measurevar="salary", groupvars=c("origin","occup"))
# Error bars represent standard error of the mean
ggplot(dataSum, aes(fill=occup, y=salary, x=origin) ) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=salary-se, ymax=salary+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))



### OLS Regressions:                           #######################################################
Data$origin <- relevel(Data$origin, ref ="USSR")
Data$educ = relevel(Data$educ, ref = "Low")
Data$occup = relevel(Data$occup, ref = "Non-profession")
#Data$yearsisr=factor(Data$yearsisr)


library('robustbase')
mod1 = lmrob(lsalary~sex+birthy+origin+yearsisr+educ+dip_abroad+origin*educ+educ*dip_abroad+origin*dip_abroad,data=Data)
summary(mod1)
bptest(mod1)

# F test for low education
# difference between USSR and Asia&Africa with dip_abroad = 1
check_1 = linearHypothesis(mod1, c("originAsia&Africa=0", "originAsia&Africa:dip_abroad1=0"))
# difference between USSR and Eour&America with dip_abroad = 1
check_2 = linearHypothesis(mod1, c("originEuor&America=0", "originEuor&America:dip_abroad1=0"))
# difference between Asia&Africa with dip_abroad = 0 to Asia&Africa with dip_abroad = 1
check_3 = linearHypothesis(mod1, c("dip_abroad1=0", "originAsia&Africa:dip_abroad1=0"))
# difference between Euor&America with dip_abroad = 0 to Euor&America with dip_abroad = 1
check_4 = linearHypothesis(mod1, c("dip_abroad1=0", "originEuor&America:dip_abroad1=0"))

# F test for BA education
# difference between USSR with dip_abroad = 1 to Asia&Africa with dip_abroad = 1
check_5 = linearHypothesis(mod1, c("originAsia&Africa:educBA=0", "originAsia&Africa:dip_abroad1=0"))
# difference between USSR with dip_abroad = 1 to Euor&America with dip_abroad = 1
check_6 = linearHypothesis(mod1, c("originEuor&America:educBA=0", "originEuor&America:dip_abroad1=0"))
# difference between USSR with dip_abroad = 0 to USSR with dip_abroad = 1
check_7 = linearHypothesis(mod1, c("dip_abroad1=0", "educBA:dip_abroad1=0"))
# difference between Asia&Afric with dip_abroad = 0 to Asia&Africa with dip_abroad = 1
check_8 = linearHypothesis(mod1, c("dip_abroad1=0", "educBA:dip_abroad1=0", "originAsia&Africa:dip_abroad1=0"))
# difference between Euor&America with dip_abroad = 0 to Euor&America with dip_abroad = 1
check_9 = linearHypothesis(mod1, c("dip_abroad1=0", "educBA:dip_abroad1=0", "originEuor&America:dip_abroad1=0"))

# F test for MA&phD education
# difference between USSR with dip_abroad = 1 to Asia&Africa with dip_abroad = 1
check_10 = linearHypothesis(mod1, c("originAsia&Africa:educMa&Phd=0", "originAsia&Africa:dip_abroad1=0"))
check_10
# difference between USSR with dip_abroad = 1 to Euor&America with dip_abroad = 1
check_11 = linearHypothesis(mod1, c("originEuor&America:educMa&Phd", "originEuor&America:dip_abroad1=0"))
check_11
# difference between USSR with dip_abroad = 0 to USSR with dip_abroad = 1
check_12 = linearHypothesis(mod1, c("dip_abroad1=0", "educMa&Phd:dip_abroad1=0"))
check_12
# difference between Asia&Afric with dip_abroad = 0 to Asia&Africa with dip_abroad = 1
check_13 = linearHypothesis(mod1, c("dip_abroad1=0", "educMa&Phd:dip_abroad1=0", "originAsia&Africa:dip_abroad1=0"))
check_13
# difference between Euor&America with dip_abroad = 0 to Euor&America with dip_abroad = 1
check_14 = linearHypothesis(mod1, c("dip_abroad1=0", "educMa&Phd:dip_abroad1=0", "originEuor&America:dip_abroad1=0"))
check_14

#model11 = lm(lsalary~sex+birthy+origin+yearsisr+educ+dip_abroad+occup+origin*educ+educ*dip_abroad+origin*dip_abroad+origin*occup,data=Data)
#summary(model11)
mod2 = lmrob(lsalary~sex+birthy+origin+yearsisr+educ+dip_abroad+occup+origin*dip_abroad+origin*occup,data=Data)
summary(mod2)


# F test for return on education

# difference between Asia&Africa with dip_abroad = 1 and Asia&Africa with dip_abroad = 0
test_1 = linearHypothesis(mod1, c( "originAsia&Africa:dip_abroad1=0", "dip_abroad1=0"))
test_1

# difference between Euor&America with dip_abroad = 0 to Euor&America with dip_abroad = 1
test_2 = linearHypothesis(mod1, c( "originEuor&America:dip_abroad1=0", "dip_abroad1=0"))
test_2


# F test for salary gaps by occupation between origins

## difference between Non-profession occupations:
# Repatriants from USSR vs Asia&Africa
test_0 = linearHypothesis(mod2, c( "originAsia&Africa=0"))
# Repatriants from USSR vs Euor&America
test_0e = linearHypothesis(mod2, c( "originEuor&America=0"))
# Repatriants from Euor&America vs Asia&Africa
test_0a = linearHypothesis(mod2, c( "originEuor&America=originAsia&Africa"))
test_0
test_0e
test_0a

## difference between Academic occupations:
# Repatriants from USSR vs Asia&Africa
test_1 = linearHypothesis(mod2, c( "originAsia&Africa=0", "originAsia&Africa:occupAcademic=0"))
# Repatriants from USSR vs Euor&America
test_1e = linearHypothesis(mod2, c( "originEuor&America=0", "originEuor&America:occupAcademic=0"))
# Repatriants from Euor&America vs Asia&Africa
test_1a = linearHypothesis(mod2, c( "originEuor&America=originAsia&Africa", "originEuor&America:occupAcademic=originAsia&Africa:occupAcademic"))
test_1
test_1e
test_1a


## difference between Agriculture occupations:
# Repatriants from USSR vs Asia&Africa
test_2 = linearHypothesis(mod2, c( "originAsia&Africa=0", "originAsia&Africa:occupAgriculture=0"))
# Repatriants from USSR vs Euor&America
test_2e = linearHypothesis(mod2, c( "originEuor&America=0", "originEuor&America:occupAgriculture=0"))
# Repatriants from Euor&America vs Asia&Africa
test_2a = linearHypothesis(mod2, c( "originEuor&America=originAsia&Africa", "originEuor&America:occupAgriculture=originAsia&Africa:occupAgriculture"))
test_2
test_2e
test_2a



## difference between Manager occupations:
# Repatriants from USSR vs Asia&Africa
test_3 = linearHypothesis(mod2, c( "originAsia&Africa=0", "originAsia&Africa:occupManager=0"))
# Repatriants from USSR vs Euor&America
test_3e = linearHypothesis(mod2, c( "originEuor&America=0", "originEuor&America:occupManager=0"))
# Repatriants from Euor&America vs Asia&Africa
test_3a = linearHypothesis(mod2, c( "originEuor&America=originAsia&Africa", "originEuor&America:occupManager=originAsia&Africa:occupManager"))
test_3
test_3e
test_3a


## difference between Military occupations:
# Repatriants from USSR vs Asia&Africa
test_4 = linearHypothesis(mod2, c( "originAsia&Africa=0", "originAsia&Africa:occupMilitary=0"))
# Repatriants from USSR vs Euor&America
test_4e = linearHypothesis(mod2, c( "originEuor&America=0", "originEuor&America:occupMilitary=0"))
# Repatriants from Euor&America vs Asia&Africa
test_4a = linearHypothesis(mod2, c( "originEuor&America=originAsia&Africa", "originEuor&America:occupMilitary=originAsia&Africa:occupMilitary"))
test_4
test_4e
test_4a

## difference between Office occupations:
# Repatriants from USSR vs Asia&Africa
test_5 = linearHypothesis(mod2, c( "originAsia&Africa=0", "originAsia&Africa:occupOffice=0"))
# Repatriants from USSR vs Euor&America
test_5e = linearHypothesis(mod2, c( "originEuor&America=0", "originEuor&America:occupOffice=0"))
# Repatriants from Euor&America vs Asia&Africa
test_5a = linearHypothesis(mod2, c( "originEuor&America=originAsia&Africa", "originEuor&America:occupOffice=originAsia&Africa:occupOffice"))
test_5
test_5e
test_5a

## difference between Technic occupations:
# Repatriants from USSR vs Asia&Africa
test_6 = linearHypothesis(mod2, c( "originAsia&Africa=0", "originAsia&Africa:occupTechnic=0"))
# Repatriants from USSR vs Euor&America
test_6e = linearHypothesis(mod2, c( "originEuor&America=0", "originEuor&America:occupTechnic=0"))
# Repatriants from Euor&America vs Asia&Africa
test_6a = linearHypothesis(mod2, c( "originEuor&America=originAsia&Africa", "originEuor&America:occupTechnic=originAsia&Africa:occupTechnic"))
test_6
test_6e
test_6a



