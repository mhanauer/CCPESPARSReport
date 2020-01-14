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
