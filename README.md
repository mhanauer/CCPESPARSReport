---
title: "CCPE Final Report"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Library some packages
```{r}
library(psych)
library(prettyR)
```

So far this reporting period, how many people received an HIV test using funds from this grant?
We only collect data on those who get HIV tested so everyone in the baseline
Need to subset for data in the future ok with data set only those starting in April included.
```{r}
#setwd("S:/Indiana Research & Evaluation/CCPE/NewGPRAData")
#base = read.csv("baseline.csv", header = TRUE, na.strings = c(-99, -98, -97))
dim(base)
```
Demographics
How do you describe yourself? 
1, Male
2, Female
3, Transgender
5, I do not identify as male, female, or transgender
```{r}
describe.factor(matchedDat$gender.x)
```
Demographics

what_is_your_race_one_or_m___2.x
What is your race? (One or more categories may be selected)
1, White
2, Black or African American
3, American Indian or Alaska Native
4, Asian
5, Native Hawaiian or other Pacific Islander

gender
How do you describe yourself? 
1, Male
2, Female
3, Transgender
5, I do not identify as male, female, or transgender

sex_pr
Which of the following do you consider yourself to be?
1, Straight/heterosexual
2, Gay/Lesbian
3, Bisexual
4, Other
5, Prefer not to say

```{r}
describe.factor(matchedDat$what_is_your_race_one_or_m___2.x)
describe.factor(matchedDat$gender.x)
describe.factor(matchedDat$sex_pr.x)
```

