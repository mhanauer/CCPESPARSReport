---
title: "CCPE Final Report"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Spars report
```{r}
library(psych)
library(prettyR)
library(Hmisc)
library(lubridate)

setwd("S:/Indiana Research & Evaluation/CCPE/NewGPRAData")
base = read.csv("baseline.csv", header = TRUE, na.strings = c(-99, -98, -97))
dim(base)
base$baseline_completion_date = mdy(base$baseline_completion_date)
base = subset(base, baseline_completion_date >= "2019-10-01")
dim(base)
base = subset(base, baseline_completion_date <= "2019-12-31")
range(base$baseline_completion_date)
dim(base)


#Gender How do you describe yourself? 1 Male, 2 Female, 3 Transgender, 5 I do not identify as male, female, or transgender

describe.factor(base$gender)

#Ethnicity e_nonhispan 1 = nonhispanic, 0 = hispanic

describe.factor(base$e_nonhispan)

#RACE 
#What is your race? (One or more categories may be selected) 1 White, 2 Black or African American, 3 American Indian or Alaska Native, 4 Asian, 5 Native Hawaiian or other Pacific Islander; 1 = yes, 0 = no

#white
describe.factor(base$what_is_your_race_one_or_m___1)
#Black
describe.factor(base$what_is_your_race_one_or_m___2)
#American Indian
describe.factor(base$what_is_your_race_one_or_m___3)
#Asian
describe.factor(base$what_is_your_race_one_or_m___4)
#Native Hawaiian
describe.factor(base$what_is_your_race_one_or_m___5)

#find multiracial 

test_race = data.frame(White = base$what_is_your_race_one_or_m___1, Black = base$what_is_your_race_one_or_m___2, AmericanIndian = base$what_is_your_race_one_or_m___3, Asian = base$what_is_your_race_one_or_m___4, PacificIslander = base$what_is_your_race_one_or_m___5)

test_race$sum = rowSums(test_race)
View(test_race$sum)

#in sum column, 1 = one race, 2 = multiracial, 0 = unknown

describe.factor(test_race$sum)

View(test_race)


#Age what_is_your_date_of_birth should all be 18-24, check tracking sheet
describe.factor(base$what_is_your_date_of_birth)


#homeless hometype_n, 6 = homeless or in a shelter
describe.factor(base$hometype_n)

#tested for first time, Have you ever been informed of your HIV status based on the results of an HIV test? 1 = yes, 0 = no, first test = 0
describe.factor(base$hiv_results_n)

#informed of status = all in quarter are informed

#test results positive check tracking sheet

#referred to treatment for positive test check tracking sheet
```

#########################
Quarterly report
#########################


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}

library(lubridate)
library(tidyr)
library(tidyverse)
library(descr)
library(prettyR)

####CCPE New GPRA
setwd("S:/Indiana Research & Evaluation/CCPE/NewGPRAData")
base_Q1 = read.csv("Year5Q1_baseline.csv", header = TRUE, na.strings = c(-99, -98, -97))
base = read.csv("CCPE_base_01132020.csv", header = TRUE, na.strings = c(-99, -98, -97))
month3 = read.csv("CCPE_month3_01132020.csv", header = TRUE, na.strings = c(-99, -98, -97))

dim(base_Q1)
#112 new partipants this quarter, total of 1,380

#Cleaning data need to merge them

base$ID= base$participant_id
month3$ID = month3$redcap_survey_identifier
matchedDat = merge(base, month3, by = "ID", all.y = TRUE)
## Dim Check
dim(month3)
dim(matchedDat)
### ID Check
month3 = month3[order(month3$ID),]
matchedDat = matchedDat[order(matchedDat$ID),]
matchedDat$ID == month3$ID

```
rename participant ID in matchedDat
rename research ID in birthdate
```{r}
birthdate=data.frame(birthdate=base$what_is_your_date_of_birth, ID=base$participant_id)
birthdate_complete=na.omit(birthdate)
basedate= data.frame(basedate=base$baseline_completion_date, ID=base$participant_id)
basedate_complete=na.omit(basedate)
agedata=merge(birthdate_complete, basedate_complete, by="ID", all.x = TRUE)
agedata$birthdate = mdy(agedata$birthdate)
agedata$basedate = mdy(agedata$basedate)
library(eeptools)
agedata2=na.omit(agedata)
agedata2=subset(agedata2, birthdate<"2002-01-01")
agedata2$age = age_calc(dob=agedata2$birthdate, enddate=agedata2$basedate)
agedata2
mean(agedata2$age)/12
```

```{r}
#Goal Three Objective A: Increase knowledge about SA by 20%.

#know_sa Would you know where to go near where you live to see a health care professional regarding a drug or alcohol problem? Yes = 1 No = 0

matchedDat$know_SA.x= matchedDat$know_sa.x

matchedDat$know_SA.y= matchedDat$know_sa.y

G3OA = data.frame(matchedDat$know_sa.x, matchedDat$know_sa.y)
dim(G3OA)
G3OAComplete = na.omit(G3OA)
dim(G3OAComplete)
describe.factor(G3OAComplete$matchedDat.know_sa.x)
describe.factor(G3OAComplete$matchedDat.know_sa.y)

SABaseMean = round(mean(G3OAComplete$matchedDat.know_sa.x),3)
SAMonth3Mean = round(mean(G3OAComplete$matchedDat.know_sa.y),3)
G3OA_p_change = round((SAMonth3Mean - SABaseMean)/ SABaseMean,3)

G3OA_results = data.frame(N = dim(G3OAComplete)[1], SABaseMean, SAMonth3Mean, G3OA_p_change)
G3OA_results

```

```{r}

#Goal Three Objective B: Increase knowledge about HIV and VH by 20%.

#know_hiv: Would you know where to go near where you live to see a health care professional regarding HIV/AIDS or other sexually transmitted health issues?
matchedDat$know_hiv.x= matchedDat$know_hiv.x

matchedDat$know_hiv.y= matchedDat$know_hiv.y

G3OB = data.frame(matchedDat$know_hiv.x, matchedDat$know_hiv.y)
dim(G3OB)
G3OBComplete = na.omit(G3OB)
dim(G3OBComplete)
describe.factor(G3OBComplete$matchedDat.know_hiv.x)
describe.factor(G3OBComplete$matchedDat.know_hiv.y)

HIVBaseMean = round(mean(G3OBComplete$matchedDat.know_hiv.x),3)
HIVMonth3Mean = round(mean(G3OBComplete$matchedDat.know_hiv.y),3)
G3OB_p_change = round((HIVMonth3Mean - HIVBaseMean)/ HIVBaseMean,3)

G3OB_results = data.frame(N = dim(G3OBComplete)[1], HIVBaseMean, HIVMonth3Mean, G3OB_p_change)
G3OB_results
```


```{r}
#Objective C: Increase perception of SA harm risks/consequences by 20%.  

#unprotected: What level of risk do you think people have of harming themselves if they have sex (oral, vaginal, or anal) without a condom or dental dam?

unprotected_base = matchedDat$sex1.x

#unprotected_base<- ifelse(unprotected_base == "No risk", 1, ifelse(unprotected_base == "Slight risk", 2, ifelse(unprotected_base=="Moderate risk", 3, ifelse(unprotected_base =="Great risk", 4, ifelse(unprotected_base=="Don't know or can't say",NA,NA)))))

unprotected_month3 = matchedDat$sex1.y

#unprotected_month3<- ifelse(unprotected_month3 == "No risk", 1, ifelse(unprotected_month3 == "Slight risk", 2, ifelse(unprotected_month3=="Moderate risk", 3, ifelse(unprotected_month3 =="Great risk", 4, ifelse(unprotected_month3=="Don't know or can't say",NA, NA)))))

G3OC = data.frame(unprotected_base, unprotected_month3)
dim(G3OC)
G3OCComplete = na.omit(G3OC)
dim(G3OCComplete)
describe(G3OCComplete)
describe.factor(G3OCComplete$unprotected_base)
describe.factor(G3OCComplete$unprotected_month3)

unprotectedBaseMean = round(mean(G3OCComplete$unprotected_base),3)
unprotectedMonth3Mean = round(mean(G3OCComplete$unprotected_month3),3)
G3OC_p_change = round((unprotectedMonth3Mean - unprotectedBaseMean)/ unprotectedBaseMean,3)

G3OC_results = data.frame(N = dim(G3OCComplete)[1], unprotectedBaseMean, unprotectedMonth3Mean, G3OC_p_change)
G3OC_results
```


```{r}
#Objective D: Increase perception of HIV harm risks/consequences by 20%.   
#influence What level of risk do you think people have of harming themselves if they have sex while high on drugs or under the influence of alcohol?

influence_base = matchedDat$sex2.x

#influence_base<- ifelse(influence_base == "No risk", 1, ifelse(influence_base == "Slight risk", 2, ifelse(influence_base=="Moderate risk", 3, ifelse(influence_base =="Great risk", 4, ifelse(influence_base=="Don't know or can't say",NA,NA)))))

influence_month3 = matchedDat$sex2.y

#influence_month3<- ifelse(influence_month3 == "No risk", 1, ifelse(influence_month3 == "Slight risk", 2, ifelse(influence_month3=="Moderate risk", 3, ifelse(influence_month3 =="Great risk", 4, ifelse(influence_month3=="Don't know or can't say",NA, NA)))))

G3OD = data.frame(influence_base, influence_month3)
dim(G3OD)
G3ODComplete = na.omit(G3OD)
dim(G3ODComplete)
describe(G3ODComplete)
describe.factor(G3ODComplete$influence_base)
describe.factor(G3ODComplete$influence_month3)

influenceBaseMean = round(mean(G3ODComplete$influence_base),3)
influenceMonth3Mean = round(mean(G3ODComplete$influence_month3),3)
G3OD_p_change = round((influenceMonth3Mean - influenceBaseMean)/ influenceBaseMean,3)

G3OD_results = data.frame(N = dim(G3ODComplete)[1], influenceBaseMean, influenceMonth3Mean, G3OD_p_change)
G3OD_results
```

```{r}
#G3OH

#CIGARETTES
#cig.base=as.numeric(gsub("[ days]", "", matchedDat$Smoke.Cigarettes..x))
#cig.month3=as.numeric(gsub("[ days]", "", matchedDat$Smoke.Cigarettes..y))
cig.base= matchedDat$cig30d.x
cig.month3=matchedDat$cig30d.y
cig30day = data.frame(Base = cig.base, Month3 = cig.month3)
dim(cig30day)
cig30day_Complete = na.omit(cig30day)
dim(cig30day_Complete)

describe.factor(cig30day_Complete$Base)
describe.factor(cig30day_Complete$Month3)

cig30dayBaseMean = round(mean(cig30day_Complete$Base),3)
cig30dayMonth3Mean = round(mean(cig30day_Complete$Month3),3)
cig30day_p_change = round((cig30dayMonth3Mean - cig30dayBaseMean)/ cig30dayBaseMean,3)

cig30day_results = data.frame(N = dim(cig30day_Complete)[1], cig30dayBaseMean, cig30dayMonth3Mean, cig30day_p_change)
cig30day_results

#OTHER TOBACCO
#tob.base=as.numeric(gsub("[ days]", "", matchedDat$Use.other.tobacco.products..x))
#tob.month3=as.numeric(gsub("[ days]", "", matchedDat$Use.other.tobacco.products..y))

tob.base=matchedDat$tob30d.x
tob.month3=matchedDat$tob30d.y
tob30day = data.frame(Base = tob.base, Month3 = tob.month3)
tob30day_Complete = na.omit(tob30day)
dim(tob30day_Complete)

describe.factor(tob30day_Complete$Base)
describe.factor(tob30day_Complete$Month3)

tob30dayBaseMean = round(mean(tob30day_Complete$Base),3)
tob30dayMonth3Mean = round(mean(tob30day_Complete$Month3),3)
tob30day_p_change = round((tob30dayMonth3Mean - tob30dayBaseMean)/ tob30dayBaseMean,3)

tob30day_results = data.frame(N = dim(tob30day_Complete)[1], tob30dayBaseMean, tob30dayMonth3Mean, tob30day_p_change)
tob30day_results

#VAPE 
#vape.base=as.numeric(gsub("[ days]", "", matchedDat$Use.electronic.vapor.products..x))
#vape.month3=as.numeric(gsub("[ days]", "", matchedDat$Use.electronic.vapor.products..y))

vape.base=matchedDat$vap30d.x
vape.month3=matchedDat$vap30d.y
vap30day = data.frame(Base = vape.base, Month3 = vape.month3)
dim(vap30day)
vap30day_Complete = na.omit(vap30day)
dim(vap30day_Complete)

describe.factor(vap30day_Complete$Base)
describe.factor(vap30day_Complete$Month3)

vap30dayBaseMean = round(mean(vap30day_Complete$Base),3)
vap30dayMonth3Mean = round(mean(vap30day_Complete$Month3),3)
vap30day_p_change = round((vap30dayMonth3Mean - vap30dayBaseMean)/ vap30dayBaseMean,3)

vap30day_results = data.frame(N = dim(vap30day_Complete)[1], vap30dayBaseMean, vap30dayMonth3Mean, vap30day_p_change)
vap30day_results

#ALCOHOL
#alc.base=as.numeric(gsub("[ days]", "", matchedDat$Drink.Alcohol...any.use.at.all..x))
#alc.month3=as.numeric(gsub("[ days]", "", matchedDat$Drink.Alcohol...any.use.at.all..y))

alc.base=matchedDat$alc30d.x
alc.month3=matchedDat$alc30d.y
alc30day = data.frame(Base = alc.base, Month3 = alc.month3)
dim(alc30day)
alc30day_Complete = na.omit(alc30day)
dim(alc30day_Complete)

describe.factor(alc30day_Complete$Base)
describe.factor(alc30day_Complete$Month3)

alc30dayBaseMean = round(mean(alc30day_Complete$Base),3)
alc30dayMonth3Mean = round(mean(alc30day_Complete$Month3),3)
alc30day_p_change = round((alc30dayMonth3Mean - alc30dayBaseMean)/ alc30dayBaseMean,3)

alc30day_results = data.frame(N = dim(alc30day_Complete)[1], alc30dayBaseMean, alc30dayMonth3Mean, alc30day_p_change)
alc30day_results

#MARIJUANA
#mj.base=as.numeric(gsub("[ days]", "", matchedDat$Use.marijuana.or.hashish..x))
#mj.month3=as.numeric(gsub("[ days]", "", matchedDat$Use.marijuana.or.hashish..y))

mj.base=matchedDat$mj30d.x
mj.month3=matchedDat$mj30d.y
mar30day = data.frame(Base = mj.base, Month3 = mj.month3)
dim(mar30day)
mar30day_Complete = na.omit(mar30day)
dim(mar30day_Complete)

describe.factor(mar30day_Complete$Base)
describe.factor(mar30day_Complete$Month3)

mar30dayBaseMean = round(mean(mar30day_Complete$Base),3)
mar30dayMonth3Mean = round(mean(mar30day_Complete$Month3),3)
mar30day_p_change = round((mar30dayMonth3Mean - mar30dayBaseMean)/ mar30dayBaseMean,3)

mar30day_results = data.frame(N = dim(mar30day_Complete)[1], mar30dayBaseMean, mar30dayMonth3Mean, mar30day_p_change)
mar30day_results

#OPIOIDS
opi.base=as.numeric(gsub("[ days]", "", matchedDat$Use.prescription.opioid.drugs.without.orders.given.to.you.by.your.doctor..x))
opi.month3=as.numeric(gsub("[ days]", "", matchedDat$Use.prescription.opioid.drugs.without.orders.given.to.you.by.your.doctor..y))
opi30day = data.frame(Base = opi.base, Month3 = opi.month3)
dim(opi30day)
opi30day_Complete = na.omit(opi30day)
dim(opi30day_Complete)

describe.factor(opi30day_Complete$Base)
describe.factor(opi30day_Complete$Month3)

opi30dayBaseMean = round(mean(opi30day_Complete$Base),3)
opi30dayMonth3Mean = round(mean(opi30day_Complete$Month3),3)
opi30day_p_change = round((opi30dayMonth3Mean - opi30dayBaseMean)/ opi30dayBaseMean,3)

opi30day_results = data.frame(N = dim(opi30day_Complete)[1], opi30dayBaseMean, opi30dayMonth3Mean, opi30day_p_change)
opi30day_results

#OTHER PRESCRIPTION DRUGS
otherrx.base=as.numeric(gsub("[ days]", "", matchedDat$Use.other.prescription.drugs.without.orders.given.to.you.by.your.doctor...Please.exclude.prescription.opioid.drugs..))
otherrx.month3=as.numeric(gsub("[ days]", "", matchedDat$Use.other.prescription.drugs.without.orders.given.to.you.by.your.doctor...Please.exclude.prescription.opioid.drugs.))
otherRX = data.frame(Base = otherrx.base, Month3 = otherrx.month3)
dim(otherRX)
otherRX_Complete = na.omit(otherRX)
dim(otherRX_Complete)

describe.factor(otherRX_Complete$Base)
describe.factor(otherRX_Complete$Month3)

otherRXBaseMean = round(mean(otherRX_Complete$Base),3)
otherRXMonth3Mean = round(mean(otherRX_Complete$Month3),3)
otherRX_p_change = round((otherRXMonth3Mean - otherRXBaseMean)/ otherRXBaseMean,3)

otherRX_results = data.frame(N = dim(otherRX_Complete)[1], otherRXBaseMean, otherRXMonth3Mean, otherRX_p_change)
otherRX_results

#NON-PRESCIRPTION OPIOIDS
nonrxopi.base=as.numeric(gsub("[ days]", "", matchedDat$Use.non.prescription.opioid.drugs..x))
nonrxopi.month3=as.numeric(gsub("[ days]", "", matchedDat$Use.non.prescription.opioid.drugs..y))
nonRXopi = data.frame(Base = nonrxopi.base, Month3 = nonrxopi.month3)
dim(nonRXopi)
nonRXopi_Complete = na.omit(nonRXopi)
dim(nonRXopi_Complete)

describe.factor(nonRXopi_Complete$Base)
describe.factor(nonRXopi_Complete$Month3)

nonRXopiBaseMean = round(mean(nonRXopi_Complete$Base),3)
nonRXopiMonth3Mean = round(mean(nonRXopi_Complete$Month3),3)
nonRXopi_p_change = round((nonRXopiMonth3Mean - nonRXopiBaseMean)/ nonRXopiBaseMean,3)

nonRXopi_results = data.frame(N = dim(nonRXopi_Complete)[1], nonRXopiBaseMean, nonRXopiMonth3Mean, nonRXopi_p_change)
nonRXopi_results

#ILLICIT DRUGS
illicit.base=as.numeric(gsub("[ days]", "", matchedDat$Use.any.other.illegal.drugs....please.exclude.marijuana.hashish.and.non.prescription.opioid.drugs.))
illicit.month3=as.numeric(gsub("[ days]", "", matchedDat$Use.any.other.illegal.drugs...Please.exclude.marijuana.hashish.and.non.prescription.opioid.drugs.))
illicit = data.frame(Base = illicit.base, Month3 = illicit.month3)
dim(illicit)
illicit_Complete = na.omit(illicit)
dim(illicit_Complete)

describe.factor(illicit_Complete$Base)
describe.factor(illicit_Complete$Month3)

illicitBaseMean = round(mean(illicit_Complete$Base),3)
illicitMonth3Mean = round(mean(illicit_Complete$Month3),3)
illicit_p_change = round((illicitMonth3Mean - illicitBaseMean)/ illicitBaseMean,3)

illicit_results = data.frame(N = dim(illicit_Complete)[1], illicitBaseMean, illicitMonth3Mean, illicit_p_change)
illicit_results

#OVERALL

drug_use_base=data.frame(cig.base, tob.base, vape.base, alc.base, mj.base, opi.base, otherrx.base, nonrxopi.base, illicit.base)

drug_use_base_complete=na.omit(drug_use_base)
dim(drug_use_base_complete)

drug_use_base_complete$total_days=(drug_use_base_complete$cig.base + drug_use_base_complete$tob.base+ drug_use_base_complete$vape.base+ drug_use_base_complete$alc.base+ drug_use_base_complete$mj.base+ drug_use_base_complete$opi.base+ drug_use_base_complete$otherrx.base+ drug_use_base_complete$nonrxopi.base+ drug_use_base_complete$illicit.base)

yes_base = drug_use_base_complete$total_days>=1
describe.factor(yes_base)

drug_use_month3=data.frame(cig.month3, tob.month3, vape.month3, alc.month3, mj.month3, opi.month3, otherrx.month3, nonrxopi.month3, illicit.month3)

drug_use_month3_complete=na.omit(drug_use_month3)
dim(drug_use_month3_complete)

drug_use_month3_complete$total_days=(drug_use_month3_complete$cig.month3 + drug_use_month3_complete$tob.month3+ drug_use_month3_complete$vape.month3+ drug_use_month3_complete$alc.month3+ drug_use_month3_complete$mj.month3+ drug_use_month3_complete$opi.month3+ drug_use_month3_complete$otherrx.month3+ drug_use_month3_complete$nonrxopi.month3+ drug_use_month3_complete$illicit.month3)

dim(drug_use_month3_complete)

yes_month3 = drug_use_month3_complete$total_days>=1
describe.factor(yes_month3)

sum_drugs_base= rowSums(drug_use_base_complete)
sum_drugs_month3= rowSums(drug_use_month3_complete)
mean(sum_drugs_base)
mean(sum_drugs_month3)
dim(drug_use_base_complete)
mean(drug_use_base_complete$total_days)
mean(drug_use_month3_complete$total_days)
``` 

```{r}
#Goal 3, Obj M: Decrease STD incidence

std.base=matchedDat$std30.x
describe.factor(std.base)
std.month3=matchedDat$std30.y
describe.factor(std.month3)
```


```{r}
#Goal 3, Obj K: Increase youth and young adult empowerment by 20%.

#CNTRL_REFUSECNDM, 4 = Strongly agree, 3 = Agree, 2 = Disagree, 1 = Strongly disagree, 98 = No Response/Invalid Data

refuse.base=matchedDat$i_could_refuse_if_someone.x
#refuse.base<-ifelse(refuse.base=="Strongly disagree", 1, ifelse(refuse.base == "Disagree", 2, ifelse(refuse.base=="Agree", 3, ifelse(refuse.base=="Strongly agree", 4, ifelse(refuse.base=="No Repsonse/",NA,NA)))))

refuse.month3=matchedDat$i_could_refuse_if_someone.y
#refuse.month3 <- ifelse(refuse.month3 == "Strongly disagree", 1, ifelse(refuse.month3 == "Disagree", 2, ifelse(refuse.month3=="Agree", 3, ifelse(refuse.month3=="Strongly agree", 4, ifelse(refuse.month3=="No Repsonse/",NA,NA)))))

refuse = data.frame(Base = refuse.base, Month3 = refuse.month3)
dim(refuse)
refuse_Complete = na.omit(refuse)
dim(refuse_Complete)

describe.factor(refuse_Complete$Base)
describe.factor(refuse_Complete$Month3)

refuseBaseMean = round(mean(refuse_Complete$Base),3)
refuseMonth3Mean = round(mean(refuse_Complete$Month3),3)
refuse_p_change = round((refuseMonth3Mean - refuseBaseMean)/ refuseBaseMean,3)

refuse_results = data.frame(N = dim(refuse_Complete)[1], refuseBaseMean, refuseMonth3Mean, refuse_p_change)
refuse_results
```
       
```{r}
#Goal 3 Objective J Reduce drug/alcohol related suspensions and/or crime by 70%. 

#matchedDat$crime.x<- ifelse(matchedDat$Have.you.been.convicted.of.a.crime.that.was.related.to.your.drug.or.alcohol.use.in.the.past.30.days..x=="Yes", 1, ifelse(matchedDat$Have.you.been.convicted.of.a.crime.that.was.related.to.your.drug.or.alcohol.use.in.the.past.30.days..x=="No", 0, NA))

#matchedDat$crime.y<- ifelse(matchedDat$Have.you.been.convicted.of.a.crime.that.was.related.to.your.drug.or.alcohol.use.in.the.past.30.days..y=="Yes", 1, ifelse(matchedDat$Have.you.been.convicted.of.a.crime.that.was.related.to.your.drug.or.alcohol.use.in.the.past.30.days..y=="No", 0, NA))

crime.x=matchedDat$in_the_past_30_days_how_ma.x
crime.y=matchedDat$in_the_past_30_days_how_ma.y
crime= data.frame(Base = crime.x, Month3=crime.y)
dim(crime)
crime_Complete = na.omit(crime)
dim(crime_Complete)

describe.factor(crime_Complete$Base)
describe.factor(crime_Complete$Month3)

crimeBaseMean = round(mean(crime_Complete$Base),3)
crimeMonth3Mean = round(mean(crime_Complete$Month3),3)
crime_p_change = round((crimeMonth3Mean - crimeBaseMean)/ crimeBaseMean,3)

crime_results = data.frame(N = dim(crime_Complete)[1], crimeBaseMean, crimeMonth3Mean, crime_p_change)
crime_results
```

```{r}
#Goal 5 Objective B: Track, assess, and reduce sub-population disparities via data-driven quality improvement process.


#Racial demogrpahics where 1 = white, 2 = black, 3 = Native American, 4 = Asian, 5 = Pacific Islander
describe.factor(base$what_is_your_race_one_or_m___1)
#248 white, 117 non-white
248/(248+117)
describe.factor(base$what_is_your_race_one_or_m___2)
#89 black, 276 not black
89/(89+276)
describe.factor(base$what_is_your_race_one_or_m___3)
#5 NA, 360 not NA
5/365
describe.factor(base$what_is_your_race_one_or_m___4)
#32 Asian, 333 not Asian
32/365
describe.factor(base$what_is_your_race_one_or_m___5)
#none
describe.factor(base$e_nonhispan)
#51 Yes, 309 No, 5 blank
51/365

#Gender, 1=male, 2=female, 3=transgender 5=I do not indentify as male, female or trangender
describe.factor(base$gender)

#Education
describe.factor(base$are_you_currently_attendin)
```

```{r}
#Goal 5 Objective L: Decrease the percentage of participants who engage in unprotected sex by 20%.

unprotected= data.frame(maleBase = matchedDat$a_male.x, maleMonth3 = matchedDat$a_male.y, femaleBase=matchedDat$a_female.x , femaleMonth3=matchedDat$a_female.y, transBase=matchedDat$a_transgender_individual.x, transMonth3=matchedDat$a_transgender_individual.y, monogBase=matchedDat$a_significant_other_in_a_m.x, monogMonth3=matchedDat$a_significant_other_in_a_m.y, multBase=matchedDat$multiple_partners.x, multMonth3=matchedDat$multiple_partners.y, hivBase=matchedDat$an_hiv_positive_person.x, hivMonth3=matchedDat$an_hiv_positive_person.y, hepBase=matchedDat$a_hepatitis_positive_perso.x, hepMonth3=matchedDat$a_hepatitis_positive_perso.y, drugsBase=matchedDat$a_person_who_injects_drugs.x, drugsMonth3=matchedDat$a_person_who_injects_drugs.y, msmBase=matchedDat$a_man_who_has_sex_with_men.x, msmMonth3=matchedDat$a_man_who_has_sex_with_men.y)
dim(unprotected)
unprotected_Complete = na.omit(unprotected)
dim(unprotected_Complete)

unprotected_Complete$people_base=(unprotected_Complete$maleBase==1|unprotected_Complete$femaleBase==1|unprotected_Complete$transBase==1|unprotected_Complete$monogBase==1|unprotected_Complete$hivBase==1|unprotected_Complete$hepBase==1|unprotected_Complete$drugsBase==1|unprotected_Complete$multBase==1)

Base=sum(unprotected_Complete$people_base)
Base
Base/91

unprotected_Complete$people_Month3=(unprotected_Complete$maleMonth3==1|unprotected_Complete$femaleMonth3==1|unprotected_Complete$transMonth3==1|unprotected_Complete$monogMonth3==1|unprotected_Complete$hivMonth3==1|unprotected_Complete$hepMonth3==1|unprotected_Complete$drugsMonth3==1|unprotected_Complete$multMonth3==1)

Month3=sum(unprotected_Complete$people_Month3)
Month3
Month3/91

unprotected_p_change=((Month3-Base)/Base)
unprotected_p_change

```

```

