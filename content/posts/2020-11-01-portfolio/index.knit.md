---
title: Secondary School Student Performance 
author: Brendan
date: '2020-11-01'
slug: studentperf
draft: false
categories: []
tags: []
hero: /images/site/school.jpg
bibliography: ["references.bib"]
biblio-style: "apalike"
link-citations: true
---


# Introduction

I am writing this blog as part of an assignment. My Details:

 |
------------- | -------------
Student Name: | B Kent
Student Number: | cxxxxxxxx
Programme Code: | TU060
Version R: | R version 4.0.3 (2020-10-10)
R packages: | 


# Getting started

We explore a student exam performance data set. This is a data set about secondary school academical achievements in Portugal. The data contains two set of results, one set for results in Maths and one set for results in Portuguese. These are merged together to form one data set which I will endeavor to break down in order to learn about the basic properties of statistics, describing the Frequency and probabilities, hypothesis testing, Normality, Missing data, Relationships, Component Analysis and more. 

This data set is from a paper by P.Cortez and A. Silva entitled "Using  Data  Mining  to  Predict  Secondary  School  Student  Performance". [@cortez2008using]




We need to get the data

```r
library("tidyr")
library("finalfit") # for ff_glimpse(studentdf)
library("gridExtra") # for plots in a grid
library("ggplot2") # For creating histograms and plots
#download.file(url="https://archive.ics.uci.edu/ml/machine-learning-databases/00320/student.zip", destfile="student.zip")
#unzip("student.zip",exdir = "studentdf")
list.files("studentdf")
```

```
## [1] "student-mat.csv" "student-merge.R" "student-por.csv" "student.txt"
```

data set description: [https://archive.ics.uci.edu/ml/datasets/student+performance#](https://archive.ics.uci.edu/ml/datasets/student+performance#) or [here](/blog/description/)

read in the data

```r
matdf=read.table("studentdf/student-mat.csv",sep=";",header=TRUE, stringsAsFactors=TRUE)
pordf=read.table("studentdf/student-por.csv",sep=";",header=TRUE, stringsAsFactors=TRUE)

studentdf=merge(x=matdf,y=pordf,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
colnames(studentdf) <- tolower(colnames(studentdf))
nrow(studentdf)
```

```
[1] 382
```

So we have 382 records in our data set. 

Let's see what variables we have in each record.

```r
colnames(studentdf)
```

```
##  [1] "school"       "sex"          "age"          "address"      "famsize"     
##  [6] "pstatus"      "medu"         "fedu"         "mjob"         "fjob"        
## [11] "reason"       "nursery"      "internet"     "guardian.x"   "traveltime.x"
## [16] "studytime.x"  "failures.x"   "schoolsup.x"  "famsup.x"     "paid.x"      
## [21] "activities.x" "higher.x"     "romantic.x"   "famrel.x"     "freetime.x"  
## [26] "goout.x"      "dalc.x"       "walc.x"       "health.x"     "absences.x"  
## [31] "g1.x"         "g2.x"         "g3.x"         "guardian.y"   "traveltime.y"
## [36] "studytime.y"  "failures.y"   "schoolsup.y"  "famsup.y"     "paid.y"      
## [41] "activities.y" "higher.y"     "romantic.y"   "famrel.y"     "freetime.y"  
## [46] "goout.y"      "dalc.y"       "walc.y"       "health.y"     "absences.y"  
## [51] "g1.y"         "g2.y"         "g3.y"
```


The data set measures the grades for each students achievements at three time intervals, g1, g2 and g3. So grades at g1 and g2 can been used to predict g3, and from the paper, it can be seen that there is a strong correlation between g3 and g2/g1.

let's us pick a random student:































































