---
title: "CCPE Reports"
author: "Elaina"
date: "July 3, 2019"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
knitr::opts_chunk$set(echo = TRUE)


library(psych)
library(prettyR)
####CCPE New GPRA
setwd("S:/Indiana Research & Evaluation/CCPE/NewGPRAData")
base = read.csv("baseline.csv", header = TRUE, na.strings = c(-99, -98, -97))
month3 = read.csv("month3.csv", header = TRUE, na.strings = c(-99, -98, -97))

#85 new partipants this quarter, total of 1,015


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
birthdate=data.frame(birthdate=matchedDat$what_is_your_date_of_birth.x, participant_ID=matchedDat$participant_id.x)
birthdate_complete=na.omit(birthdate)
basedate$participant_ID=basedate$Research_ID
basedate_complete=na.omit(basedate)
agedata=merge(birthdate_complete, basedate_complete, by="participant_ID", all.x = TRUE)
agedata$birthdate = mdy(agedata$birthdate)
agedata$Baseline_Date = mdy(agedata$Baseline_Date)
library(eeptools)
agedata2=subset(agedata, birthdate<"2005-01-01")
agedata2$age = age_calc(dob=agedata2$birthdate, enddate=agedata2$Baseline_Date)
agedata2
mean(agedata2$age)/12
```

#Goal Three Objective A: Increase knowledge about SA by 20%.

#know_sa Would you know where to go near where you live to see a health care professional regarding a drug or alcohol problem? Yes = 1 No = 0

G3OA = data.frame(SABase = matchedDat$know_sa.x, SAMonth3 = matchedDat$know_sa.y)
dim(G3OA)
G3OAComplete = na.omit(G3OA)
dim(G3OAComplete)
describe(G3OAComplete)
describe.factor(G3OAComplete$SABase)
describe.factor(G3OAComplete$SAMonth3)

SABaseMean = round(mean(G3OAComplete$SABase),3)
SAMonth3Mean = round(mean(G3OAComplete$SAMonth3),3)
G3OA_p_change = round((SAMonth3Mean - SABaseMean)/ SABaseMean,3)

G3OA_results = data.frame(N = dim(G3OAComplete)[1], SABaseMean, SAMonth3Mean, G3OA_p_change)
G3OA_results

```

```{r}

253/310

#Goal Three Objective B: Increase knowledge about HIV and VH by 20%.

#know_hiv: Would you know where to go near where you live to see a health care professional regarding HIV/AIDS or other sexually transmitted health issues?
#Demographics

G3OB = data.frame(HIVBase = matchedDat$know_hiv.x, HIVMonth3 = matchedDat$know_hiv.y)
dim(G3OB)
G3OBComplete = na.omit(G3OB)
dim(G3OBComplete)
describe(G3OBComplete)
describe.factor(G3OBComplete$HIVBase)
describe.factor(G3OBComplete$HIVMonth3)

HIVBaseMean = round(mean(G3OBComplete$HIVBase),3)
HIVMonth3Mean = round(mean(G3OBComplete$HIVMonth3),3)
G3OB_p_change = round((HIVMonth3Mean - HIVBaseMean)/ HIVBaseMean,3)

G3OB_results = data.frame(N = dim(G3OBComplete)[1], HIVBaseMean, HIVMonth3Mean, G3OB_p_change)
G3OB_results
```


```{r}
#Objective C: Increase perception of SA harm risks/consequences by 20%.  

#sex1:What level of risk do you think people have of harming themselves if they have sex (oral, vaginal, or anal) without a condom or dental dam?

G3OC = data.frame(sex1Base = matchedDat$sex1.x, sex1Month3= matchedDat$sex1.y)
dim(G3OC)
G3OCComplete = na.omit(G3OC)
dim(G3OCComplete)
describe(G3OCComplete)
describe.factor(G3OCComplete$sex1Base)
describe.factor(G3OCComplete$sex1Month3)

sex1BaseMean = round(mean(G3OCComplete$sex1Base),3)
sex1Month3Mean = round(mean(G3OCComplete$sex1Month3),3)
G3OC_p_change = round((sex1Month3Mean - sex1BaseMean)/ sex1BaseMean,3)

G3OC_results = data.frame(N = dim(G3OCComplete)[1], sex1BaseMean, sex1Month3Mean, G3OC_p_change)
G3OC_results

wilcox.test(G3OCComplete$sex1Base, G3OCComplete$sex1Month3, paired=TRUE)

#sex2 What level of risk do you think people have of harming themselves if they have sex while high on drugs or under the influence of alcohol?
  
G3OC2 = data.frame(sex2Base = matchedDat$sex2.x, sex2Month3 = matchedDat$sex2.y)
dim(G3OC2)
G3OC2Complete = na.omit(G3OC2)
dim(G3OC2Complete)
describe(G3OC2Complete)
describe.factor(G3OC2Complete$sex2Base)
describe.factor(G3OCComplete$sex2Month3)

sex2BaseMean = round(mean(G3OC2Complete$sex2Base),3)
sex2Month3Mean = round(mean(G3OC2Complete$sex2Month3),3)
G3OC2_p_change = round((sex2Month3Mean - sex2BaseMean)/ sex2BaseMean,3)

G3OC2_results = data.frame(N = dim(G3OC2Complete)[1], sex2BaseMean, sex2Month3Mean, G3OC2_p_change)
G3OC2_results

t.test(G3OC2Complete$sex2Base, G3OC2Complete$sex2Month3, paired=TRUE)
```

```{r}
#G3OH

#CIGARETTES
cig30day = data.frame(cig30dayBase = matchedDat$cig30d.x, cig30dayMonth3 = matchedDat$cig30d.y)
dim(cig30day)
cig30day_Complete = na.omit(cig30day)
dim(cig30day_Complete)

describe.factor(cig30day_Complete$cig30dayBase)
describe.factor(cig30day_Complete$cig30dayMonth3)

cig30dayBaseMean = round(mean(cig30day_Complete$cig30dayBase),3)
cig30dayMonth3Mean = round(mean(cig30day_Complete$cig30dayMonth3),3)
cig30day_p_change = round((cig30dayMonth3Mean - cig30dayBaseMean)/ cig30dayBaseMean,3)

cig30day_results = data.frame(N = dim(cig30day_Complete)[1], cig30dayBaseMean, cig30dayMonth3Mean, cig30day_p_change)
cig30day_results

#OTHER TOBACCO
tob30day = data.frame(tob30dayBase = matchedDat$tob30d.x, tob30dayMonth3 = matchedDat$tob30d.y)
dim(tob30day)
tob30day_Complete = na.omit(tob30day)
dim(tob30day_Complete)

describe.factor(tob30day_Complete$tob30dayBase)
describe.factor(tob30day_Complete$tob30dayMonth3)

tob30dayBaseMean = round(mean(tob30day_Complete$tob30dayBase),3)
tob30dayMonth3Mean = round(mean(tob30day_Complete$tob30dayMonth3),3)
tob30day_p_change = round((tob30dayMonth3Mean - tob30dayBaseMean)/ tob30dayBaseMean,3)

tob30day_results = data.frame(N = dim(tob30day_Complete)[1], tob30dayBaseMean, tob30dayMonth3Mean, tob30day_p_change)
tob30day_results

#VAPE 
vap30day = data.frame(vap30dayBase = matchedDat$vap30d.x, vap30dayMonth3 = matchedDat$vap30d.y)
dim(vap30day)
vap30day_Complete = na.omit(vap30day)
dim(vap30day_Complete)

describe.factor(vap30day_Complete$vap30dayBase)
describe.factor(vap30day_Complete$vap30dayMonth3)

vap30dayBaseMean = round(mean(vap30day_Complete$vap30dayBase),3)
vap30dayMonth3Mean = round(mean(vap30day_Complete$vap30dayMonth3),3)
vap30day_p_change = round((vap30dayMonth3Mean - vap30dayBaseMean)/ vap30dayBaseMean,3)

vap30day_results = data.frame(N = dim(vap30day_Complete)[1], vap30dayBaseMean, vap30dayMonth3Mean, vap30day_p_change)
vap30day_results

#ALCOHOL
alc30day = data.frame(alc30dayBase = matchedDat$alc30d.x, alc30dayMonth3 = matchedDat$alc30d.y)
dim(alc30day)
alc30day_Complete = na.omit(alc30day)
dim(alc30day_Complete)

describe.factor(alc30day_Complete$alc30dayBase)
describe.factor(alc30day_Complete$alc30dayMonth3)

alc30dayBaseMean = round(mean(alc30day_Complete$alc30dayBase),3)
alc30dayMonth3Mean = round(mean(alc30day_Complete$alc30dayMonth3),3)
alc30day_p_change = round((alc30dayMonth3Mean - alc30dayBaseMean)/ alc30dayBaseMean,3)

alc30day_results = data.frame(N = dim(alc30day_Complete)[1], alc30dayBaseMean, alc30dayMonth3Mean, alc30day_p_change)
alc30day_results

#MARIJUANA
mar30day = data.frame(mar30dayBase = matchedDat$mj30d.x, mar30dayMonth3 = matchedDat$mj30d.y)
dim(mar30day)
mar30day_Complete = na.omit(mar30day)
dim(mar30day_Complete)

describe.factor(mar30day_Complete$mar30dayBase)
describe.factor(mar30day_Complete$mar30dayMonth3)

mar30dayBaseMean = round(mean(mar30day_Complete$mar30dayBase),3)
mar30dayMonth3Mean = round(mean(mar30day_Complete$mar30dayMonth3),3)
mar30day_p_change = round((mar30dayMonth3Mean - mar30dayBaseMean)/ mar30dayBaseMean,3)

mar30day_results = data.frame(N = dim(mar30day_Complete)[1], mar30dayBaseMean, mar30dayMonth3Mean, mar30day_p_change)
mar30day_results

#OPIOIDS
opi30day = data.frame(opi30dayBase = matchedDat$opi30.x, opi30dayMonth3 = matchedDat$opi30.y)
dim(opi30day)
opi30day_Complete = na.omit(opi30day)
dim(opi30day_Complete)

describe.factor(opi30day_Complete$opi30dayBase)
describe.factor(opi30day_Complete$opi30dayMonth3)

opi30dayBaseMean = round(mean(opi30day_Complete$opi30dayBase),3)
opi30dayMonth3Mean = round(mean(opi30day_Complete$opi30dayMonth3),3)
opi30day_p_change = round((opi30dayMonth3Mean - opi30dayBaseMean)/ opi30dayBaseMean,3)

opi30day_results = data.frame(N = dim(opi30day_Complete)[1], opi30dayBaseMean, opi30dayMonth3Mean, opi30day_p_change)
opi30day_results

#OTHER PRESCRIPTION DRUGS
otherRX = data.frame(otherRXBase = matchedDat$otherrx30.x, otherRXMonth3 = matchedDat$otherrx30.y)
dim(otherRX)
otherRX_Complete = na.omit(otherRX)
dim(otherRX_Complete)

describe.factor(otherRX_Complete$otherRXBase)
describe.factor(otherRX_Complete$otherRXMonth3)

otherRXBaseMean = round(mean(otherRX_Complete$otherRXBase),3)
otherRXMonth3Mean = round(mean(otherRX_Complete$otherRXMonth3),3)
otherRX_p_change = round((otherRXMonth3Mean - otherRXBaseMean)/ otherRXBaseMean,3)

otherRX_results = data.frame(N = dim(otherRX_Complete)[1], otherRXBaseMean, otherRXMonth3Mean, otherRX_p_change)
otherRX_results

#NON-PRESCIRPTION OPIAIODS
nonRXopi = data.frame(nonRXopiBase = matchedDat$opi32.x, nonRXopiMonth3 = matchedDat$opi32.y)
dim(nonRXopi)
nonRXopi_Complete = na.omit(nonRXopi)
dim(nonRXopi_Complete)

describe.factor(nonRXopi_Complete$nonRXopiBase)
describe.factor(nonRXopi_Complete$nonRXopiMonth3)

nonRXopiBaseMean = round(mean(nonRXopi_Complete$nonRXopiBase),3)
nonRXopiMonth3Mean = round(mean(nonRXopi_Complete$nonRXopiMonth3),3)
nonRXopi_p_change = round((nonRXopiMonth3Mean - nonRXopiBaseMean)/ nonRXopiBaseMean,3)

nonRXopi_results = data.frame(N = dim(nonRXopi_Complete)[1], nonRXopiBaseMean, nonRXopiMonth3Mean, nonRXopi_p_change)
nonRXopi_results

#ILLICIT DRUGS
illicit = data.frame(illicitBase = matchedDat$ill30d.x, illicitMonth3 = matchedDat$ill30d.y)
dim(illicit)
illicit_Complete = na.omit(illicit)
dim(illicit_Complete)

describe.factor(illicit_Complete$illicitBase)
describe.factor(illicit_Complete$illicitMonth3)

illicitBaseMean = round(mean(illicit_Complete$illicitBase),3)
illicitMonth3Mean = round(mean(illicit_Complete$illicitMonth3),3)
illicit_p_change = round((illicitMonth3Mean - illicitBaseMean)/ illicitBaseMean,3)

illicit_results = data.frame(N = dim(illicit_Complete)[1], illicitBaseMean, illicitMonth3Mean, illicit_p_change)
illicit_results

#OVERALL

drug_use_base=data.frame(matchedDat$cig30d.x, matchedDat$tob30d.x, matchedDat$alc30d.x, matchedDat$mj30d.x, matchedDat$opi30.x, matchedDat$otherrx30.x, matchedDat$opi32.x, matchedDat$ill30d.x)

drug_use_base_complete=na.omit(drug_use_base)
dim(drug_use_base_complete)

drug_use_base_complete$total_days=(drug_use_base_complete$matchedDat.cig30d.x+drug_use_base_complete$matchedDat.tob30d.x+ drug_use_base_complete$matchedDat.alc30d.x+drug_use_base_complete$matchedDat.mj30d.x+drug_use_base_complete$matchedDat.opi30.x+ drug_use_base_complete$matchedDat.opi32.x+ drug_use_base_complete$matchedDat.otherrx30.x+ drug_use_base_complete$matchedDat.ill30d.x)

yes_base = drug_use_base_complete$total_days>=1
describe.factor(yes_base)

drug_use_month3=data.frame(matchedDat$cig30d.y, matchedDat$tob30d.y, matchedDat$alc30d.y, matchedDat$mj30d.y, matchedDat$opi30.y, matchedDat$otherrx30.y, matchedDat$opi32.y, matchedDat$ill30d.y)

drug_use_month3_complete=na.omit(drug_use_month3)
dim(drug_use_month3_complete)

drug_use_month3_complete$total_days=(drug_use_month3_complete$matchedDat.cig30d.y+drug_use_month3_complete$matchedDat.tob30d.y+ drug_use_month3_complete$matchedDat.alc30d.y+drug_use_month3_complete$matchedDat.mj30d.y+drug_use_month3_complete$matchedDat.opi30.y+ drug_use_month3_complete$matchedDat.opi32.y+ drug_use_month3_complete$matchedDat.otherrx30.y+ drug_use_month3_complete$matchedDat.ill30d.y)

dim(drug_use_month3_complete)
yes_month3 = drug_use_month3_complete$total_days>=1
describe.factor(yes_month3)

mean(drug_use_base_complete$total_days)
mean(drug_use_month3_complete$total_days)
```       
       
```{r}
#Goal 3 Objective J Reduce drug/alcohol related suspensions and/or crime by 70%. 

crime= data.frame(crimeBase = matchedDat$crime.x, crimeMonth3 = matchedDat$crime.y)
dim(crime)
crime_Complete = na.omit(crime)
dim(crime_Complete)

describe.factor(crime_Complete$crimeBase)
describe.factor(crime_Complete$crimeMonth3)

crimeBaseMean = round(mean(crime_Complete$crimeBase),3)
crimeMonth3Mean = round(mean(crime_Complete$crimeMonth3),3)
crime_p_change = round((crimeMonth3Mean - crimeBaseMean)/ crimeBaseMean,3)

crime_results = data.frame(N = dim(crime_Complete)[1], crimeBaseMean, crimeMonth3Mean, crime_p_change)
crime_results
```

```{r}
#Goal 5 Objective B: Track, assess, and reduce sub-population disparities via data-driven quality improvement process.

#Racial demogrpahics where 1 = white, 2 = black, 3 = Native American, 4 = Asian, 5 = Pacific Islander
describe.factor(base$what_is_your_race_one_or_m___1)
describe.factor(base$what_is_your_race_one_or_m___2)
describe.factor(base$what_is_your_race_one_or_m___3)
describe.factor(base$what_is_your_race_one_or_m___4)
describe.factor(base$what_is_your_race_one_or_m___5)

describe.factor(base$e_nonhispan)

#Gender, 1=male, 2=female, 3=transgender 5=I do not indentify as male, female or trangender
describe.factor(base$gender)

#Education
describe.factor(base$are_you_currently_attendin)
```

```{r}
#Goal 5 Objective L: Decrease the percentage of participants who engage in unprotected sex by 20%.

unprotected= data.frame(maleBase = matchedDat$a_male.x, maleMonth3 = matchedDat$a_male.y, femaleBase=matchedDat$a_female.x , femaleMonth3=matchedDat$a_female.y , transBase=matchedDat$a_transgender_individual.x, transMonth3=matchedDat$a_transgender_individual.y, monogBase=matchedDat$a_significant_other_in_a_m.x, monogMonth3=matchedDat$a_significant_other_in_a_m.y, multBase=matchedDat$multiple_partners.x, multMonth3=matchedDat$multiple_partners.y, hivBase=matchedDat$an_hiv_positive_person.x, hivMonth3=matchedDat$an_hiv_positive_person.y, hepBase=matchedDat$a_hepatitis_positive_perso.x, hepMonth3=matchedDat$a_hepatitis_positive_perso.y, drugsBase=matchedDat$a_person_who_injects_drugs.x, drugsMonth3=matchedDat$a_person_who_injects_drugs.y, msmBase=matchedDat$a_man_who_has_sex_with_men.x, msmMonth3=matchedDat$a_man_who_has_sex_with_men.y)
dim(unprotected)
unprotected_Complete = na.omit(unprotected)
dim(unprotected_Complete)

unprotected_Complete$people_base=(unprotected_Complete$maleBase==1|unprotected_Complete$femaleBase==1|unprotected_Complete$transBase==1|unprotected_Complete$monogBase==1|unprotected_Complete$hivBase==1|unprotected_Complete$hepBase==1|unprotected_Complete$drugsBase==1|unprotected_Complete$multBase==1)

Base=sum(unprotected_Complete$people_base)
Base

unprotected_Complete$people_Month3=(unprotected_Complete$maleMonth3==1|unprotected_Complete$femaleMonth3==1|unprotected_Complete$transMonth3==1|unprotected_Complete$monogMonth3==1|unprotected_Complete$hivMonth3==1|unprotected_Complete$hepMonth3==1|unprotected_Complete$drugsMonth3==1|unprotected_Complete$multMonth3==1)

Month3=sum(unprotected_Complete$people_Month3)
Month3

unprotected_p_change=((Month3-Base)/Base)
unprotected_p_change

percent_Base=(length(unprotected_Complete)-Base)/Base
percent_Base
percent_Month3=(length(unprotected_Complete)-Month3)/Month3
percent_Month3
```

```
