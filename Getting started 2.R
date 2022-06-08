############ 1. GETTING STARTED
#Import raw data CSV into R#
#I used file-> import data-> import text (readr)#
print("helloworld")

wholedataorig <- read_csv("D:/DEPED/Working data/INT0017_DEPED_ae.csv", 
                          col_types = cols(sex = col_number(), 
                                           activage = col_number(), aearrivaldate = col_date(format = "%Y-%m-%d")))

test <- read_csv("D:/DEPED/Working data/INT0017_DEPED_ae.csv", 
                          col_types = cols(sex = col_number(), 
                                           activage = col_number(), aearrivaldate = col_date(format = "%Y-%m-%d")),col_select = c("activage", "aearrivaldate", "sex", "lsoa11", "aelowacuity", "aedepttype", "patient_id") )


#experiment to see if I can only take specific columns

wholedataorig <- fread("D:/DEPED/Working data/INT0017_DEPED_ae.csv", select = c("activage", "aearrivaldate", "sex", "lsoa11", "aelowacuity", "aedepttype", "patient_id"))

#View the column names#
colnames(wholedata)

#Installing packages#
install.packages("magrittr")
install.packages("tidyverse")
install.packages("tibble")
install.packages("dplyr")
install.packages("data.table")
install.packages("writexl")
install.packages ("ggplot2")
install.packages ("ggpubr")
install.packages ("pastecs")
install.packages ("caret")
install.packages ("fasttime")


library (tibble)
library (magrittr)
library (dplyr)
library (tidyverse)
library (data.table)
library (writexl)
library (ggplot2)
library (ggpubr)
library (readxl)
library (pastecs)
library (caret)
library (fasttime)

################# Working with the CURED data
#create agegroup variable#
wholedata <- wholedataorig %>% mutate(agegroup = case_when(activage >= 70  & activage <= 119 ~ '5',
                                                       activage >= 50  & activage <= 69 ~ '4',
                                                       activage >= 30  & activage <= 49 ~ '3',
                                                       activage >= 18  & activage <= 29 ~ '2',
                                                       activage >= 0  & activage <= 17 ~ '1'))


wholedata <- wholedataorig %>% mutate(agegroup = case_when(activage >= 70  & activage <= 119 ~ '8',
                                                           activage >= 50  & activage <= 69 ~ '7',
                                                           activage >= 50  & activage <= 59 ~ '6',
                                                           activage >= 50  & activage <= 49 ~ '5',
                                                           activage >= 30  & activage <= 39 ~ '4',
                                                           activage >= 20  & activage <= 29 ~ '3',
                                                           activage >= 10  & activage <= 19 ~ '2',
                                                           activage >= 0  & activage <= 9 ~ '1'))

##to take out data before 2013
wholedata <- subset (wholedata, aearrivaldate >"2012-12-31")

#to look at date range
range(wholedataorig$aearrivaldate)
range(wholedata$aearrivaldate)

wholedata <- wholedata %>% mutate(attendyear = case_when(aearrivaldate >"2010-12-31"  & aearrivaldate <"2012-01-01"  ~ '2011',
                                                       aearrivaldate >"2011-12-31"  & aearrivaldate <"2013-01-01"  ~ '2012',
                                                       aearrivaldate >"2012-12-31"  & aearrivaldate <"2014-01-01"  ~ '2013',
                                                       aearrivaldate >"2013-12-31"  & aearrivaldate <"2015-01-01"  ~ '2014',
                                                       aearrivaldate >"2014-12-31"  & aearrivaldate <"2016-01-01"  ~ '2015',
                                                       aearrivaldate >"2015-12-31"  & aearrivaldate <"2017-01-01"  ~ '2016',
                                                       aearrivaldate >"2016-12-31"  & aearrivaldate <"2018-01-01"  ~ '2017'))
## to count how many attendances in each year, and then by lowacuity/year
wholedata %>% count (attendyear)
wholedata %>% count (aelowacuity, attendyear)


#clean-up sex#
# note sex = 1 = male, sex = 2 = female, https://datadictionary.nhs.uk/data_elements/PERSON_STATED_GENDER_CODE.html #
              #First duplicate the sex column and call it clesex#
wholedata$clesex = wholedata$sex

              #Then recode 0 and 9 to NA (missing)#
wholedata$clesex [wholedata$clesex == 0] <- NA
wholedata$clesex [wholedata$clesex == 9] <- NA

# count occurences by agegroup and sex and lsoa, and save them into new dataframe called ageclesexlsoacount#
agegroupclesexlsoacountyear <- wholedata %>% count (agegroup, clesex, lsoa11, attendyear)
agegroupclesexlsoacount <- wholedata %>% count (agegroup, clesex, lsoa11)
# agegroupclesexlsoacount <- subset (agegroupclesexlsoacount, aearrivaldate >"2013-01-01")
View (agegroupclesexlsoacount)

##stratify the data by year
stratyear <- wholedata %>% count (agegroup, clesex, attendyear)
write.csv(stratyear, "D:/DEPED/Working data/stratyear.csv")

# rearrange rows into columns so that they match the lsoa population data we will use later on #
attendage <- agegroupclesexlsoacount %>% pivot_wider (names_from = agegroup, values_from = n)

# then rename age into age1 etc #
attendage <- attendage %>% rename (sex = clesex)
attendage <- attendage %>% rename (age1 = "1") %>% rename (age2 = "2")%>% rename (age3 = "3")%>% rename (age4 = "4")%>% rename (age5 = "5")
attendage <- attendage %>% rowwise () %>% mutate (totatt = sum (c_across(age1:age5), na.rm = TRUE))

#for descriptive visualisation
# count occurences by age and sex, and save them into new dataframe called agesexlsoacount#
agegroupclesexcount <- wholedata %>% count (agegroup, clesex)
View (agegroupclesexcount)
wideagegroupsex<- agegroupclesexcount %>% pivot_wider(id_cols = agegroup, names_from =clesex, values_from = n)
write.csv(wideagegroupsex, "D:/DEPED/Working data/wideagegroupsex.csv")

########################## Create ED low acuity proportion for each LSOA

#counting number of attendances stratified by lsoa and low acuity score
lowaclsoa <- wholedata %>% count (aelowacuity, lsoa11)

#pivoting table so it is easier to work with
widelowaclsoa <- lowaclsoa %>% pivot_wider(id_cols= lsoa11, names_from = aelowacuity, values_from = n)

#renaming low acuity column names
names(widelowaclsoa) [names(widelowaclsoa) == '0'] <- 'no'
names(widelowaclsoa) [names(widelowaclsoa) == '1'] <- 'yes'

# calculating fraction of lowacuity attendances
widelowaclsoa <- widelowaclsoa %>% mutate(lowacfract = yes /no)

#finalising lsoa and low acuity fraction
fractlowaclsoa <- subset (widelowaclsoa, select = c ("lsoa11", "lowacfract"))


################ 2. Working with YH population data
#Download Yorkshire & Humber LSOA population data - I chose mid-2015
### https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/censusoutputareaestimatesintheyorkshireandthehumberregionofengland #
#in excel, for each sex, add in column 1=male, 2 = female, and change LSOA11CD heading to lsoa11#
#import each sheet to  R separately#
yhlsoamale <- read_excel("D:/DEPED/Working data/YH lsoa population.xls", sheet = "Mid-2015 Males")

yhlsoafemale <- read_excel("D:/DEPED/Working data/YH lsoa population.xls", 
                           sheet = "Mid-2015 Females")

#merge the male and female gen pops, then clean environment
yhlsoagenpop <- rbind(yhlsoamale, yhlsoafemale)

# count the number of people in age group for the yorkshire and humber general pop
# drop the superfluous columns in yhagegroup (by keeping the ones I want)#
yhagegroup <- yhlsoagenpop %>%
  mutate(agegr1 = rowSums(.[4:21], na.rm=TRUE))  %>%
  mutate(agegr2 = rowSums(.[22:33], na.rm=TRUE)) %>%
  mutate(agegr3 = rowSums(.[34:53], na.rm=TRUE)) %>%
  mutate(agegr4 = rowSums(.[54:73], na.rm=TRUE)) %>%
  mutate(agegr5 = rowSums(.[74:94], na.rm=TRUE)) %>%
  mutate(totpop = rowSums(.[4:94], na.rm=TRUE))
yhagegroup = subset (yhagegroup, select = c( "sex", "lsoa11","agegr1", "agegr2", "agegr3", "agegr4", "agegr5", "totpop"))


## experimenting with 10 year age brackets
yhagegroup <- yhlsoagenpop %>%
  mutate(agegr1 = rowSums(.[4:13], na.rm=TRUE))  %>%
  mutate(agegr2 = rowSums(.[14:23], na.rm=TRUE)) %>%
  mutate(agegr3 = rowSums(.[24:33], na.rm=TRUE)) %>%
  mutate(agegr4 = rowSums(.[34:43], na.rm=TRUE)) %>%
  mutate(agegr5 = rowSums(.[44:54], na.rm=TRUE)) %>%
  mutate(agegr6 = rowSums(.[54:64], na.rm=TRUE)) %>%
  mutate(agegr7 = rowSums(.[64:74], na.rm=TRUE)) %>%
  mutate(agegr8 = rowSums(.[74:94], na.rm=TRUE)) %>%
  mutate(totpop = rowSums(.[4:94], na.rm=TRUE))
yhagegroup = subset (yhagegroup, select = c( "sex", "lsoa11","agegr1", "agegr2", "agegr3", "agegr4", "agegr5", "agegr6", "agegr7", "agegr8", "totpop"))
yhagegroup <- yhagegroup %>% group_by (sex, lsoa11) %>% summarise_all(funs(sum))


#add together multiple lsoa rows
yhagegroup <- yhagegroup %>% group_by (sex, lsoa11) %>% summarise_all(funs(sum))

#clean the environment#
rm (yhlsoagenpop)

###########Joining together cured + population data to calculate rates
#merge the ED attendance numbers and the lsoa populations#
popattend <- merge (attendage, yhagegroup, by=c("lsoa11", "sex"))


mapop <- popattend %>% filter (sex == 1)
fepop <- popattend %>% filter (sex == 2)
fepop <- fepop %>% rename (fage1 = "age1") %>%
  rename (fage2 = "age2") %>%
  rename (fage3 = "age3") %>%
  rename (fage4 = "age4") %>%
  rename (fage5 = "age5") %>%
  rename (fna = "NA") %>%
  rename (ftotatt = "totatt") %>%
  rename (fagegr1 = "agegr1") %>%
  rename (fagegr2 = "agegr2") %>%
  rename (fagegr3 = "agegr3") %>%
  rename (fagegr4 = "agegr4") %>%
  rename (fagegr5 = "agegr5") %>%
  rename (ftotpop = "totpop")
popattend <- merge (fepop, mapop, by=c("lsoa11"))

# replace na with 0, replace
popattend <- popattend %>% replace(is.na(.), 0)
popattend<-popattend[!(popattend$lsoa11=="0"),]

#calculate totals for each column <-- use this to calculate weights (remember to do YH pop rather than attendances!)
calctotals = subset (popattend, select = c( "age1", "age2","age3", "age4", "age5", "fage1", "fage2","fage3", "fage4", "fage5", "agegr1", "agegr2","agegr3", "agegr4", "agegr5", "fagegr1", "fagegr2","fagegr3", "fagegr4", "fagegr5"))
totals <- calctotals %>% replace(is.na(.), 0) %>%
  summarise_all(sum)
write.csv(totals, "D:/DEPED/Working data/poptotals.csv")



# calculate non-marginal totals. # Use this to calculate analytical sample and number of LSOAs - check nmcalctotals halfway through
marginallsoa11 <- read_csv("D:/DEPED/Working data/marginallsoa11.csv")
nmcalctotals <- anti_join(popattend, marginallsoa11, by=c("lsoa11"))
nmcalctotals = subset (nmcalctotals, select = c( "age1", "age2","age3", "age4", "age5", "fage1", "fage2","fage3", "fage4", "fage5", "agegr1", "agegr2","agegr3", "agegr4", "agegr5", "fagegr1", "fagegr2","fagegr3", "fagegr4", "fagegr5"))
nmcalctotals <- nmcalctotals %>% replace(is.na(.), 0) %>%
  summarise_all(sum)
write.csv(nmcalctotals, "D:/DEPED/Working data/nmpoptotals.csv")

#calculate total rates and population


#calculate the rates for each age group#
poprate <- popattend %>% mutate (rate1 = age1/agegr1*1000) %>%
  mutate (rate2 = age2/agegr2*1000)%>%
  mutate (rate3 = age3/agegr3*1000)%>%
  mutate (rate4 = age4/agegr4*1000)%>%
  mutate (rate5 = age5/agegr5*1000)%>%
  mutate (crurate = totatt/totpop*1000) %>%
  mutate (frate1 = fage1/fagegr1*1000) %>%
  mutate (frate2 = fage2/fagegr2*1000)%>%
  mutate (frate3 = fage3/fagegr3*1000)%>%
  mutate (frate4 = fage4/fagegr4*1000)%>%
  mutate (frate5 = fage5/fagegr5*1000)%>%
  mutate (fcrurate = ftotatt/ftotpop*1000)

poprate = subset (poprate, select = c( "lsoa11", "rate1",  "rate2",  "rate3" , "rate4",  "rate5", "crurate", "frate1",  "frate2",  "frate3" , "frate4",  "frate5", "fcrurate"))


# calculated the weighted rates and then add them together. I have calculated the weights for each strata on an excel, from 'poptotals' table.
weightedrates <- poprate %>% mutate (wema1 = rate1 * 0.108571699944496) %>%
  mutate (wema2 = rate2*0.0834678891457982) %>%
  mutate (wema3 = rate3*0.127520324358659)%>%
  mutate (wema4 = rate4*0.119833576226362)%>%
  mutate (wema5 = rate5*0.0537654974162316)%>%
  mutate (wemaall = crurate*0.493158987091546) %>%
  mutate (wefa1 = frate1*0.103955310156095) %>%
  mutate (wefa2 = frate2*0.0810946733707121)%>%
  mutate (wefa3 = frate3*0.129000871149948)%>%
  mutate (wefa4 = frate4*0.122730669227185)%>%
  mutate (wefa5 = frate5*0.0700594890045145)%>%
  mutate (wefaall = fcrurate*0.506841012908454)

weightedrates = subset (weightedrates, select = c( "lsoa11", "wema1", "wema2", "wema3", "wema4", "wema5", "wefa1", "wefa2", "wefa3", "wefa4", "wefa5", "wefaall", "wemaall"))

weightedrates <- weightedrates %>% mutate (werate = wema1 + wema2 + wema3 + wema4 +wema5 + wefa1 + wefa2 + wefa3 + wefa4 +wefa5)
#calculate annual rate by dividing by numbers of years in the data
weightedrates <- weightedrates %>% mutate (annwerate = werate/4.249)

ratelsoa = subset (weightedrates, select = c("lsoa11", "annwerate"))


#create non-marginal ratelsoa dataset
nmratelsoa <- anti_join(ratelsoa, marginallsoa11, by=c("lsoa11"))

#descriptive of rates
range (nmratelsoa$annwerate)
hist(nmratelsoa$annwerate, main = "Annual ED attendance rates frequency", col = "green", xlab = "Annual standardised ED attendance rates", ylab = "Number of LSOAs", breaks = 100)
summary(nmratelsoa)


### bringing in IMD and distance

#import the datasets
library(readxl)
distance <- read_excel("D:/DEPED/Working data/Routestotype1eds.xls", 
                               sheet = "Core data only", col_types = c("numeric", 
                                                                       "numeric", "text"))
View(distance)



imdrankslsoa <- read_excel("D:/DEPED/Working data/imdrankslsoa.xlsx", 
                             col_types = c("text", "numeric", "numeric", 
                                                    "numeric", "numeric"))
#remove unnecessary lsoas from imds and lowacuity fract
imdrankslsoa <- imdrankslsoa %>% semi_join (ratelsoa, by = "lsoa11")
fractlowaclsoa <- fractlowaclsoa %>% semi_join (ratelsoa, by = "lsoa11")


#merge the rates, distance and imd together
ratelsoa <- merge (ratelsoa, distance,  by=c("lsoa11"))
ratelsoa <- merge (ratelsoa, imdrankslsoa,  by=c("lsoa11"))
ratelsoa <- merge (ratelsoa, fractlowaclsoa,  by=c("lsoa11"))

#dichotomise rate to above mean
ratelsoa <- ratelsoa %>% mutate(highrate = case_when(annwerate > 311.787 ~ 1,
                                                           annwerate  <= 311.787 ~ 0))

ratelsoa <- ratelsoa %>% mutate(topquarate = case_when(annwerate > 385.837 ~ 1,
                                                     annwerate  <= 385.837 ~ 0))


ratelsoa <- ratelsoa %>% mutate(highnonurg = case_when(lowacfract > 0.16846 ~ 1,
                                                       lowacfract  <= 0.16846 ~ 0))
ratelsoa %>% count(topquarate)

#reverse the deprivation variables

ratelsoa <- ratelsoa %>% mutate (rimdrank=32844-imdrank) %>% mutate (rimddec=11-imddec) %>% mutate (rtime=46-traveltime)


rm(imdrankslsoa)
rm(distance)


## removing marginal lsoas

#import marginal lsoa list
marginallsoa11 <- read_csv("D:/DEPED/Working data/marginallsoa11.csv")

#create non-marginal ratelsoa dataset
nmratelsoa <- anti_join(ratelsoa, marginallsoa11, by=c("lsoa11"))

# repeat stats for non-marginal lsoas
summary(nmratelsoa)

#dichotomise rate to above mean
nmratelsoa <- nmratelsoa %>% mutate(highrate = case_when(annwerate > 296.507  ~ 1,
                                                     annwerate  <= 296.507  ~ 0))

nmratelsoa <- nmratelsoa %>% mutate(topquarate = case_when(annwerate > 380.757 ~ 1,
                                                       annwerate  <= 380.757 ~ 0))


nmratelsoa <- nmratelsoa %>% mutate(highnonurg = case_when(lowacfract > 0.1648 ~ 1,
                                                       lowacfract  <= 0.1648 ~ 0))


#STATS
summary(nmratelsoa)
desclsoa <- stat.desc(fractlowaclsoa)
desclsoa <- stat.desc(nmratelsoa)
write.csv(desclsoa, "D:/DEPED/Working data/desclsoa.csv")
t.test(nmratelsoa$annwerate, conf.level = 0.95)
t.test(nmratelsoa$lowacfract, conf.level = 0.95)
# Stats

## create a graph of IMD vs ED attendance rate

ggplot (ratelsoa, aes(x=imdrank, y = annwerate))+
  geom_point() +
  labs(
    x = "Index of Multiple Deprivation",
    y = "Annual ED attendance rate/1000"
  ) +
  theme_minimal()

## create a graph of travel time vs ED attendance rate
ggplot (ratelsoa, aes(x=traveltime, y = annwerate))+
  geom_point() +
  labs(
    x = "Travel Time",
    y = "Annual ED attendance rate/1000 population"
  ) +
  theme_minimal()


ggplot (ratelsoa, aes(x=lowacfract, y = annwerate))+
  geom_point() +
  labs(
    x = "Low acuity fraction",
    y = "Annual ED attendance rate/1000 population"
  ) +
  theme_minimal()

# graph with line on from here: https://statsandr.com/blog/multiple-linear-regression-made-simple/#simple-linear-regression-reminder
# i don't understand the last half of the code
ggplot(ratelsoa, aes(x = imdrank, y = annwerate)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.x = 3, label.y = 32) + # for regression equation
  stat_cor(aes(label = ..rr.label..), label.x = 3, label.y = 30) + # for R^2
  theme_minimal()

# this is a similar one but with no points
ggplot(ratelsoa, aes(x = imddec, y = annwerate)) +
  geom_smooth(method = "lm") +
  stat_regline_equation(label.x = 3, label.y = 32) + # for regression equation
  stat_cor(aes(label = ..rr.label..), label.x = 3, label.y = 30) + # for R^2
  theme_minimal()

# graph with colour for distance
ggplot(ratelsoa) +
  aes(x = imdrank, y = annwerate, colour = traveltime) +
  geom_point() +
  scale_color_gradient() +
  labs(
    y = "Annual ED attendance rate/1000",
    x = "imdrank",
    color = "Distance",
  ) +
  theme_minimal()

# graph with colour for distance and a regression line
ggplot(nmratelsoa) +
  geom_smooth(method = "lm") +
  aes(x = rimdrank, y = annwerate, colour = traveltime) +
  geom_point() +
  scale_color_gradient() +
  labs(
    y = "Annual ED attendance rate/1000 people",
    x = "Index of Multiple Deprivation",
    color = "Distance",
  ) +
  theme_minimal()

# build linear regression model
edimd <- lm (annwerate ~ imdrank, data = ratelsoa)
summary(edimd)

edimddec <- lm (annwerate ~ imddec, data = ratelsoa)
summary(edimddec)

edhealthdec <- lm (annwerate ~ healthdep, data = ratelsoa)
summary(edhealthdec)

edhealthrank <- lm (annwerate ~ healthrank, data = ratelsoa)
summary(edhealthdec)

edlowac <- lm (annwerate ~ lowacfract, data = ratelsoa)
summary(edlowac)
## finding - imdrank is the best fitting model, out of the models above

# build linear regression model adjusting for distance from ED
edimdtime <- lm (annwerate ~ imdrank * traveltime, data = ratelsoa)
summary(edimdtime)

edimdtimelowac <- lm (annwerate ~ imdrank + traveltime + lowacfract, data = nmratelsoa)
summary(edimdtimelowac)

ednotime <- lm (annwerate ~ imdrank * lowacfract, data = nmratelsoa)
summary(ednotime)

edimdtimelowac <- lm (annwerate ~ imdrank + traveltime * lowacfract + imdrank * traveltime, data = nmratelsoa)
summary(edimdtimelowac)

# best fitting model
edimdtimelowac <- lm (annwerate ~ imdrank * traveltime * lowacfract, data = nmratelsoa)
summary(edimdtimelowac)

edimddist <- lm (annwerate ~ imdrank + travelkm, data = ratelsoa)
summary(edimddist)
## travel time is a better model than travel distance
# https://statsandr.com/blog/multiple-linear-regression-made-simple/#simple-linear-regression-reminder

write.table(ratelsoa, "D:/DEPED/Working data/ratelsoa.csv")

##Build logistic regression model

loghighrate <- glm (highrate ~ imddec, data = ratelsoa, family = binomial)

nmratelsoa$rimddec = as.numeric(nmratelsoa$rimddec)

logithighrate <- glm (topquarate ~ rimddec + traveltime, data = nmratelsoa, family = binomial(link = logit))
summary (logithighrate)

confint(logithighrate)
str(ratelsoa)

logittime <- glm (topquarate ~ traveltime, data = nmratelsoa, family = binomial(link = logit))
summary (logittime)

confint(logittime, level = 0.95)
str(ratelsoa)



## time
logittime <- glm (topquarate ~ rtime, data = nmratelsoa, family = binomial(link = logit))
summary (logittime)

confint(logittime, level = 0.95)
str(ratelsoa)


loghighrate <- glm (topquarate ~ log(imddec), data = ratelsoa, family = binomial)
summary (loghighrate)


loghighratetime <- lm (topquarate ~ traveltime, data = ratelsoa, family = binomial)
summary (loghighratetime)

loghighratetime <- glm (topquarate ~ log(traveltime), data = ratelsoa, family = binomial)
summary (loghighratetime)

loghighratelowac <- glm (topquarate ~ lowacfract, data = ratelsoa, family = binomial)
summary (loghighratelowac)


loghighratelowac <- glm (topquarate ~ highnonurg, data = ratelsoa, family = binomial)
summary (loghighratelowac)

##Chi-squared
chiratelowac <- subset (ratelsoa, select = c ("topquarate", "highnonurg"))
test <- chisq.test (chiratelowac)
test

table(ratelsoa$topquarate, ratelsoa$highnonurge)
play <- chisq.test(table(ratelsoa$topquarate, ratelsoa$highnonurge))

## playing with models 14th may
logithighrate <- glm (topquarate ~ rimddec + rtime + highnonurg, data = nmratelsoa, family = binomial(link = logit))
summary (logithighrate)

confint(logithighrate)


