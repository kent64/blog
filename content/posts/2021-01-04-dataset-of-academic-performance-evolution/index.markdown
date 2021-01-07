---
title: "Dataset of academic performance evolution"
author: "Brendan"
date: '2021-01-06'
slug: academicperf
draft: no
categories: []
tags: []
hero: /images/site/library.jpg
bibliography: biblo.bib
biblio-style: apalike
link-citations: yes
---

| Student Name   | Brendan Kent                             |
| -------------- | ---------------------------------------- |
| Student Number | C08861692                                |
| Class          | Prob. and Statistical Inference MATH9102 |
| Course         | TU060                                    |

<style type="text/css">
.toc .nav-link {
  font-size: 16px;
  line-height: 1;
  padding: 0;
  padding-left: 1;
  transition: all ease-out 0.3s;
  /* color: #1c2d41; */
}

li.nav-item {
    font-size: 16px;
}

pre {
    background-color: #f2f2f2;
    border-style: ridge;
}

</style>

# Research Question

<blockquote class="blockquote">

What is the relationship between a student’s overall average score for their professional evaluation in the final year of their professional career
in Engineering and their results obtained in the final year of high
school using two generic high school tests amongst students of Engineering in Columbia. The two tests are Mathematics (MAT\_S11) and Critical Reading (CR\_S11). Consequently can these predictors be used to predict a student’s overall average score in Engineering by linear regression and which predictors are the most influential.

</blockquote>

# Dataset

The dataset contains the results in national assessments for secondary and university
education in engineering students and contains academic, social, economic information for 12,411 students.
The data was collected as part of the Master’s Degree in Engineering project of the Technological University of Bolívar (UTB) titled Academic Efficiency Analysis in Engineering students

A full descriptor is available at:
[https://www.sciencedirect.com/science/article/pii/S2352340920304315\#utbl0001]()

The dataset is available for download at [https://data.mendeley.com/datasets/83tcx8psxv/1]()

``` r
df <- read_excel("data_academic_performance.xlsx", sheet = "SABER11_SABERPRO")
```

    ## New names:
    ## * `` -> ...10

``` r
df <- df %>% select(-"...10") # deselect this column
```

## Exploring variables in research question

The variable looks clean, no missing records as we can see below. The Saber 11 results are all marked to a maximum of 100 which some students have achieved.

``` r
# ff_glimpse(df)
df %>%
  select(MAT_S11, CR_S11, CC_S11, BIO_S11, ENG_S11, G_SC) %>%
  ff_glimpse()
```

    ## $Continuous
    ##           label var_type     n missing_n missing_percent  mean   sd  min
    ## MAT_S11 MAT_S11    <dbl> 12411         0             0.0  64.3 11.9 26.0
    ## CR_S11   CR_S11    <dbl> 12411         0             0.0  60.8 10.0 24.0
    ## CC_S11   CC_S11    <dbl> 12411         0             0.0  60.7 10.1  0.0
    ## BIO_S11 BIO_S11    <dbl> 12411         0             0.0  64.0 11.2 11.0
    ## ENG_S11 ENG_S11    <dbl> 12411         0             0.0  61.8 14.3 26.0
    ## G_SC       G_SC    <dbl> 12411         0             0.0 162.7 23.1 37.0
    ##         quartile_25 median quartile_75   max
    ## MAT_S11        56.0   64.0        72.0 100.0
    ## CR_S11         54.0   61.0        67.0 100.0
    ## CC_S11         54.0   60.0        67.0 100.0
    ## BIO_S11        56.0   64.0        71.0 100.0
    ## ENG_S11        50.0   59.0        72.0 100.0
    ## G_SC          147.0  163.0       179.0 247.0
    ## 
    ## $Categorical
    ## # A tibble: 12,411 x 0

  - the “level of Measurement” of each of these variables is Ratio, because the value 0 has a meaning.

Since I prefer dealing with stardardized score, I will convert all these variables to z-scores.

``` r
df <- df %>% mutate(scale_MAT_S11 = scale(MAT_S11),
         scale_CR_S11 = scale(CR_S11),
         scale_CC_S11 = scale(CC_S11),
         scale_BIO_S11 = scale(BIO_S11),
         scale_ENG_S11 = scale(ENG_S11),
         scale_G_SC = scale(G_SC))
```

Next let’s see the distribution of each:

<div class="figure" style="text-align: center">

<img src="{{< relref "posts/2021-01-04-dataset-of-academic-performance-evolution/index.markdown" >}}index_files/figure-html/unnamed-chunk-5-1.png" alt="Five test results and overall grade in Engineering" width="672" />

<p class="caption">

Figure 1: Five test results and overall grade in Engineering

</p>

</div>

Assessing them for normality:

### Mathematics (MAT\_S11)

    ## skew (g1) 
    ##  18.17215 
    ## Excess Kur (g2) 
    ##        2.954064

Both skew and kurtosis are high. The cut off is +/- 1.96, anything above this is considered significant. Let’s check the outliers:

    ## Percentage greater than 1.96 SDs: 4.447667 % 
    ## Percentage greater than 3.29 SDs: 0 %

Zero outliers outside the 3.29 SD of mean and less than 5% were outside of 1.96 SD of mean, so we can treat this variable as normal as since the sample size is so large (12411 records).

Report assessment of normality

<blockquote class="blockquote">

Mathematics (MAT\_S11) scores was assessed for normality. Visual inspection of the histogram and QQ-Plot (see Figure 1) identified some issues with skewness and kurtosis. The standardised score for kurtosis (2.95) was considered unacceptable using the criteria proposed by West, Finch and Curran (1996), also the standardised score for skewness (18.17) was outside the acceptable range. However 100% of standardised scores for Mathematics (MAT\_S11) fall within the bounds of +/- 3.29, using the guidance of Field, Miles and Field (2013) the data can be considered to approximate a normal distribution (m=64.3, sd=11.9, n=12411).

</blockquote>

### Critical Reading (CR\_S11)

    ## skew (g1) 
    ##   9.74404 
    ## Excess Kur (g2) 
    ##        10.86751

Both skew and kurtosis are high. The cut off is +/- 1.96, anything above this is considered significant. Let’s check the outliers:

    ## Percentage greater than 1.96 SDs: 5.575699 % 
    ## Percentage greater than 3.29 SDs: 0.3786963 %

Almost zero outliers outside the 3.29 SD of mean and about 5% were outside of 1.96 SD of mean, so we can treat this variable as normal as the data set is larger than 80 (12411 records).

<blockquote class="blockquote">

Critical Reading (CR\_S11) scores was assessed for normality. Visual inspection of the histogram and QQ-Plot (see Figure 1) identified some issues with skewness and kurtosis. The standardised score for kurtosis (10.87) was considered unacceptable using the criteria proposed by West, Finch and Curran (1996), also the standardised score for skewness (9.74) was outside the acceptable range. However 99.6% of standardised scores for Critical Reading (CR\_S11) fall within the bounds of +/- 3.29, using the guidance of Field, Miles and Field (2013) the data can be considered to approximate a normal distribution (m=60.8, sd=10.0, n=12411).

</blockquote>

### Gender variable

WE need to access the GENDER variable. It is a variable of type “Nominal”. Let’s take a quick look at a visual:

``` r
df %>%
  ggplot(aes(x=GENDER, y=G_SC, fill=GENDER)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE, alpha=0.6) +
    geom_jitter(color="black", size=0.4, alpha=0.9) +
    ggtitle("A boxplot with GENDER and the Overall average score (G_SC)") +
    ylab("Overall average score (G_SC) ")
```

<img src="{{< relref "posts/2021-01-04-dataset-of-academic-performance-evolution/index.markdown" >}}index_files/figure-html/unnamed-chunk-10-1.png" width="672" />

stats for GENDER:

``` r
psych::describeBy(df$G_SC, df$GENDER, mat=TRUE)
```

    ##     item group1 vars    n     mean       sd median  trimmed     mad min max
    ## X11    1      F    1 5043 161.2782 22.33294    162 161.4994 23.7216  76 242
    ## X12    2      M    1 7368 163.6908 23.58262    164 163.9561 25.2042  37 247
    ##     range        skew    kurtosis        se
    ## X11   166 -0.07216008 -0.24717975 0.3144861
    ## X12   210 -0.12210492  0.01441897 0.2747370

### School variable

We need to access the SCHOOL\_NAT variable. It is a variable of type “Nominal”. Let’s take a quick look at a visual:

``` r
df %>%
  ggplot(aes(x=SCHOOL_NAT, y=G_SC, fill=SCHOOL_NAT)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE, alpha=0.6) +
    geom_jitter(color="black", size=0.4, alpha=0.9) +
    ggtitle("A boxplot with SCHOOL_NAT and the Overall average score (G_SC)") +
    ylab("Overall average score (G_SC) ")
```

<img src="{{< relref "posts/2021-01-04-dataset-of-academic-performance-evolution/index.markdown" >}}index_files/figure-html/unnamed-chunk-12-1.png" width="672" />

stats for SCHOOL\_NAT:

``` r
psych::describeBy(df$G_SC, df$SCHOOL_NAT, mat=TRUE)
```

    ##     item  group1 vars    n     mean       sd median  trimmed     mad min max
    ## X11    1 PRIVATE    1 6565 168.2158 22.82366    170 168.7379 23.7216  37 247
    ## X12    2  PUBLIC    1 5846 156.5281 21.83817    157 156.5667 22.2390  72 228
    ##     range        skew    kurtosis        se
    ## X11   210 -0.20406097  0.02424670 0.2816877
    ## X12   156 -0.05215938 -0.01912125 0.2856189

### Biology (BIO\_S11)

    ## skew (g1) 
    ##   13.7976 
    ## Excess Kur (g2) 
    ##        6.800432

Both skew and kurtosis are high. The cut off is +/- 1.96, anything above this is considered significant. Let’s check the outliers:

    ## Percentage greater than 1.96 SDs: 5.559584 % 
    ## Percentage greater than 3.29 SDs: 0.0322295 %

Almost zero outliers outside the 3.29 SD of mean and about 5% were outside of 1.96 SD of mean, so we can treat this variable as normal as the data set is larger than 80 (12411 records).

<blockquote class="blockquote">

Biology (BIO\_S11) scores was assessed for normality. Visual inspection of the histogram and QQ-Plot (see Figure 1) identified some issues with skewness and kurtosis. The standardised score for kurtosis (10.87) was considered unacceptable using the criteria proposed by West, Finch and Curran (1996), also the standardised score for skewness (9.74) was outside the acceptable range. However 99.6% of standardised scores for Biology (BIO\_S11) fall within the bounds of +/- 3.29, using the guidance of Field, Miles and Field (2013) the data can be considered to approximate a normal distribution (m=64.0, sd=11.2, n=12411).

</blockquote>

# Results

In this section, I will being to put together a model to predict G\_SC results.

## Statistical Evidence

Before proceeding with linear regression, there are a few assumptions:
1\. That there is a relationship, a linear one between the variables
2\. Homoscedasticity (we can do this visually or with a test)
3\. Independent observations
4\. The residual errors should follow a normal distribution (this is the space between the linear line model and the data point)

For 1. we can check that a relationship exists.

Correlation Scatter plot (MAT\_S11 and G\_SC):

``` r
df %>%
  ggplot(aes(x=scale_MAT_S11, y=scale_G_SC)) +
  geom_point() + 
  geom_smooth(method = "lm", colour = "Green", se = F) + 
  labs(x = "Mathematics (MAT_S11)", y = "Overall average score (G_SC)") 
```

    ## `geom_smooth()` using formula 'y ~ x'

<div class="figure" style="text-align: center">

<img src="{{< relref "posts/2021-01-04-dataset-of-academic-performance-evolution/index.markdown" >}}index_files/figure-html/unnamed-chunk-16-1.png" alt="Correlation Scatter plot (MAT_S11 and G_SC)" width="672" />

<p class="caption">

Figure 2: Correlation Scatter plot (MAT\_S11 and G\_SC)

</p>

</div>

Doing a Pearson Correlation since both variables are a) continuous b) paired c) independent d) homoscedasticity is present and importantly e) the variables have a normal distribution. Fromt eh plot above, homoscedasticity is ok and we know the data matches the other criteria.

``` r
#Pearson Correlation
cor.test(df$scale_MAT_S11, df$scale_G_SC, method='pearson')
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  df$scale_MAT_S11 and df$scale_G_SC
    ## t = 93.733, df = 12409, p-value < 0.00000000000000022
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.6334194 0.6540231
    ## sample estimates:
    ##       cor 
    ## 0.6438379

The correlation coefficient is a commonly used measure of the size of an effect: values of ±.1 represent a small effect, ±.3 is a medium effect and ±.5 is a large effect

To report Pearson coefficient here we say:

<blockquote class="blockquote">

12411 Mathematics (MAT\_S11)(M=64.3, SD=11.9) high school results and a college Engineering overall score(G\_SC) (M=162.7, SD=23.1) were investigated. A positive Pearson r correlation coefficient of 0.64 was revealed. There is strong correlation between Mathematics (MAT\_S11) results and the college Engineering overall score(G\_SC) with t(12409) = 93.733 and a p-value \< 0.001. The size of the effect is large.

</blockquote>

Now we know there is a strong relationship between the variables we can begin the model to use Mathematics (MAT\_S11) scores to predict Engineering overall scores(G\_SC).

### Simple Linear Regression

Let’s check and make sure the single predictor model is the same as the Pearson Correlation.

``` r
model1<-lm(df$scale_G_SC~df$scale_MAT_S11)
anova(model1)
```

    ## Analysis of Variance Table
    ## 
    ## Response: df$scale_G_SC
    ##                     Df Sum Sq Mean Sq F value                Pr(>F)    
    ## df$scale_MAT_S11     1 5144.3  5144.3  8785.8 < 0.00000000000000022 ***
    ## Residuals        12409 7265.7     0.6                                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(model1)
```

    ## 
    ## Call:
    ## lm(formula = df$scale_G_SC ~ df$scale_MAT_S11)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.2893 -0.4910  0.0195  0.5199  3.6112 
    ## 
    ## Coefficients:
    ##                              Estimate           Std. Error t value
    ## (Intercept)      0.000000000000002412 0.006868587579970870    0.00
    ## df$scale_MAT_S11 0.643837935031701836 0.006868864310398667   93.73
    ##                             Pr(>|t|)    
    ## (Intercept)                        1    
    ## df$scale_MAT_S11 <0.0000000000000002 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.7652 on 12409 degrees of freedom
    ## Multiple R-squared:  0.4145, Adjusted R-squared:  0.4145 
    ## F-statistic:  8786 on 1 and 12409 DF,  p-value: < 0.00000000000000022

Findings:

  - The p-value for ANOVA is exactly the same as the value we got for Pearson (p-value: \< 0.00000000000000022).
  - The ANOVA summary command produces the Multiple R-squared result which is 0.4145, so R = sqrt(0.4145) = 0.64 which is Pearson’s R.
  - The R squared number can also tell us how much of the variation in G\_SC results is explained by MAT\_S11. So that’s 41.5 % explained by High School Maths results.
  - The F-statistic result is (F(1, 12409) = 8786, p \< .001), which is statistically significant. We can conclude that this regression model is significantly better at predicting the G\_SC scores than if we used the mean value of G\_SC.

The final thing is to put together the equation of the line with the coefficient.

``` r
coef(model1)
```

    ##             (Intercept)        df$scale_MAT_S11 
    ## 0.000000000000002412467 0.643837935031701835698

The z-scale standardized equation is:

``` 
 G_SC = 0 + 0.64 * MAT_S11
```

## Mulpile Linear Regression

Now to add Critical Reading (CR\_S11) to out model. Going back to the assumption for linear regression we first need to check for a correlation.

Correlation Scatter plot (MAT\_S11 and G\_SC):

``` r
df %>%
  ggplot(aes(x=scale_CR_S11, y=scale_G_SC)) +
  geom_point() + 
  geom_smooth(method = "lm", colour = "Green", se = F) + 
  labs(x = "Critical Reading (CR_S11)", y = "Overall average score (G_SC)") 
```

    ## `geom_smooth()` using formula 'y ~ x'

<div class="figure" style="text-align: center">

<img src="{{< relref "posts/2021-01-04-dataset-of-academic-performance-evolution/index.markdown" >}}index_files/figure-html/unnamed-chunk-20-1.png" alt="Correlation Scatter plot (CR_S11 and G_SC)" width="672" />

<p class="caption">

Figure 3: Correlation Scatter plot (CR\_S11 and G\_SC)

</p>

</div>

``` r
#Pearson Correlation
cor.test(df$scale_CR_S11, df$scale_G_SC, method='pearson')
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  df$scale_CR_S11 and df$scale_G_SC
    ## t = 96.193, df = 12409, p-value < 0.00000000000000022
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.6433767 0.6635360
    ## sample estimates:
    ##       cor 
    ## 0.6535722

The correlation coefficient is a commonly used measure of the size of an effect: values of ±.1 represent a small effect, ±.3 is a medium effect and ±.5 is a large effect

The correlation between Critical Reading (CR\_S11) and overall score(G\_SC) is statistically significant.

To report Pearson coefficient here we say:

<blockquote class="blockquote">

12411 Critical Reading (CR\_S11)(M=60.8, SD=10.0) high school results and a college Engineering overall score(G\_SC) (M=162.7, SD=23.1) were investigated. A positive Pearson r correlation coefficient of 0.65 was revealed. There is strong correlation between Critical Reading (CR\_S11) results and the college Engineering overall score(G\_SC) with t(12409) = 96.193 and a p-value \< 0.001. The size of the effect is large.

</blockquote>

And now to add it to the first model that add just MAT\_S11 to create a extended multiple linear regression model.

A quick look at the simple linear regression model we created again along with adding Critical Reading (CR\_S11)

``` r
model2<-lm(df$scale_G_SC~df$scale_MAT_S11+df$scale_CR_S11)
stargazer::stargazer(model1, model2, type="text")
```

    ## 
    ## =============================================================================
    ##                                        Dependent variable:                   
    ##                     ---------------------------------------------------------
    ##                                            scale_G_SC                        
    ##                                 (1)                          (2)             
    ## -----------------------------------------------------------------------------
    ## scale_MAT_S11                 0.644***                     0.390***          
    ##                               (0.007)                      (0.008)           
    ##                                                                              
    ## scale_CR_S11                                               0.415***          
    ##                                                            (0.008)           
    ##                                                                              
    ## Constant                       0.000                        0.000            
    ##                               (0.007)                      (0.006)           
    ##                                                                              
    ## -----------------------------------------------------------------------------
    ## Observations                   12,411                       12,411           
    ## R2                             0.415                        0.523            
    ## Adjusted R2                    0.414                        0.523            
    ## Residual Std. Error      0.765 (df = 12409)           0.691 (df = 12408)     
    ## F Statistic         8,785.839*** (df = 1; 12409) 6,799.872*** (df = 2; 12408)
    ## =============================================================================
    ## Note:                                             *p<0.1; **p<0.05; ***p<0.01

Some outcomes from model 2

  - The Adjusted R squared has improved from 0.4145 to 0.523, meaning we can now explain more of variation in G\_SC by including CR\_S11 into our model
  - CR\_S11 is statically significant to p\<0.01.

One of the assumptions of a multiple linear regression model is that the residuals follow a normal distribution. We need to check this now on our model2. We can do this by finding any influential outliers by using cook’s distance.

<div class="figure" style="text-align: center">

<img src="{{< relref "posts/2021-01-04-dataset-of-academic-performance-evolution/index.markdown" >}}index_files/figure-html/unnamed-chunk-23-1.png" alt="Cook's D for model2" width="672" />

<p class="caption">

Figure 4: Cook’s D for model2

</p>

</div>

<div class="figure" style="text-align: center">

<img src="{{< relref "posts/2021-01-04-dataset-of-academic-performance-evolution/index.markdown" >}}index_files/figure-html/unnamed-chunk-24-1.png" alt="Cook's D for model2" width="672" />

<p class="caption">

Figure 5: Cook’s D for model2

</p>

</div>

  - According to (Stephanie [2018](#ref-stephanie_2018)), a general rule of thumb is that observations with a Cook’s D of more than 3 times the mean, is a possible outlier. In the figure above, in red we have the values which are greater than 4 times the mean Cook’s D.

Now to find rows related to influential observations, the ones above the line in the above figure:

``` r
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
cat("Number of influencers:", length(influential), "\n")
```

    ## Number of influencers: 595

``` r
cat ("20 influencers in MAT_S11",head(df[influential, ]$MAT_S11, n = 20),"\n")
```

    ## 20 influencers in MAT_S11 45 56 78 56 73 70 52 56 48 67 54 77 70 52 66 76 51 49 92 81

``` r
cat ("20 influencers in CR_S11",head(df[influential, ]$CR_S11, n = 20),"\n")
```

    ## 20 influencers in CR_S11 51 47 77 39 54 54 58 62 41 61 61 71 48 50 47 74 54 57 65 56

``` r
cat ("Percentage of influencers: ", (length(influential) / nrow(df)) * 100, "% \n")
```

    ## Percentage of influencers:  4.794134 %

The total number of influential observations is less than 5% of the total.

Bonferonni p-value for most extreme obs - Are there any cases where the outcome variable has an unusual variable for its predictor values?

``` r
car::outlierTest(model2)
```

    ##        rstudent        unadjusted p-value          Bonferroni p
    ## 1336  -8.963117 0.00000000000000000036042 0.0000000000000044731
    ## 7721  -5.540430 0.00000003078800000000000 0.0003821100000000000
    ## 3718  -5.249445 0.00000015508000000000001 0.0019246999999999999
    ## 204   -5.125972 0.00000030045999999999998 0.0037290000000000001
    ## 12376  5.097221 0.00000034975000000000002 0.0043407000000000003
    ## 1281   5.039005 0.00000047455000000000002 0.0058896000000000000
    ## 12069 -4.740949 0.00000215079999999999980 0.0266939999999999990

  - our model2 is telling us that these values above are unusual variables with very statistically significant results

<div class="figure" style="text-align: center">

<img src="{{< relref "posts/2021-01-04-dataset-of-academic-performance-evolution/index.markdown" >}}index_files/figure-html/unnamed-chunk-27-1.png" alt="leverage plots for model2" width="672" />

<p class="caption">

Figure 6: leverage plots for model2

</p>

</div>

<div class="figure" style="text-align: center">

<img src="{{< relref "posts/2021-01-04-dataset-of-academic-performance-evolution/index.markdown" >}}index_files/figure-html/unnamed-chunk-28-1.png" alt="Assess homocedasticity for model2" width="672" />

<p class="caption">

Figure 7: Assess homocedasticity for model2

</p>

</div>

  - We can see there is absolutely no heteroscedastity, we see a completely random, equal distribution of points throughout the range of X axis and a flat red line. There is no pattern in the residuals.

<div class="figure" style="text-align: center">

<img src="{{< relref "posts/2021-01-04-dataset-of-academic-performance-evolution/index.markdown" >}}index_files/figure-html/unnamed-chunk-29-1.png" alt="Histogram and density plot of the residuals for model2" width="672" />

<p class="caption">

Figure 8: Histogram and density plot of the residuals for model2

</p>

</div>

  - we have a good normal distribution of the residuals.

<!-- end list -->

``` r
residuals <- residuals(model2)
resids<- abs(residuals)
cat("Percentage greater than 1.96 SDs:", FSA::perc(as.numeric(resids), 1.96, "gt"),"% \n")
```

    ## Percentage greater than 1.96 SDs: 0.9910563 %

``` r
cat("Percentage greater than 3.29 SDs:",FSA::perc(as.numeric(resids), 3.29, "gt"), "%")
```

    ## Percentage greater than 3.29 SDs: 0.0483442 %

  - almost 0% outliers at 3.29 standards deviations from mean and for a data set this size. We can accept the distribution as normal.

<div class="figure" style="text-align: center">

<img src="{{< relref "posts/2021-01-04-dataset-of-academic-performance-evolution/index.markdown" >}}index_files/figure-html/unnamed-chunk-31-1.png" alt="QQ plot of the residuals for model2" width="672" />

<p class="caption">

Figure 9: QQ plot of the residuals for model2

</p>

</div>

    ## [1] 1336 7721

We now need to check for Collinearity in our model.

### Collinearity:

Calculate Collinearity:

``` r
vifmodel<-car::vif(model2)
vifmodel
```

    ## df$scale_MAT_S11  df$scale_CR_S11 
    ##         1.592284         1.592284

  - As a rule of thumb, a VIF score over 5 is a problem. A score over 10 should be remedied and you should consider dropping the problematic variable from the regression model

Calculate tolerance:

``` r
1/vifmodel
```

    ## df$scale_MAT_S11  df$scale_CR_S11 
    ##        0.6280287        0.6280287

  - If the VIF value is greater than 2.5 or the Tolerance is less than 0.4, then you have concerns over multicollinearity.

  - Collinearity occurs when two or more independent variables are giving the same information, one could be redundant.

  - To check collinearity, we examine the correlation matrix that compares the independent variables with each other.

  - If we get a correlation coefficient of above 0.8, then we may have collinearity.

<div class="figure" style="text-align: center">

<img src="{{< relref "posts/2021-01-04-dataset-of-academic-performance-evolution/index.markdown" >}}index_files/figure-html/unnamed-chunk-34-1.png" alt="correlation matrix for model2" width="672" />

<p class="caption">

Figure 10: correlation matrix for model2

</p>

</div>

  - the result from our correlation matrix for model2 show that these variables have a correlation less than 0.8, so we don’t have collinearity.

Reporting:

<blockquote class="blockquote">

Tests to see if the data met the assumption of collinearity indicated that multicollinearity was not a concern (MAT\_S11, Tolerance = .63, VIF = 1.6; CR\_S11, Tolerance = .63 VIF = 1.6).

</blockquote>

### Muliple Lienar regression checks

Association:

  - For MLR (multi-linear regressions), we must have some proof there is an actually real link between these variables used as predictors and the outcome variable. We can use test statistics as we have seen in the previous sections for this. We have shown this earlier.

Time Order:

  - making sure the predictor happens before the outcome variable, this high school data is definitely before the engineering results.

Non-spuriousness:

  - Try to reduce spurious relationships, relationships that are just a coincidence, don’t leave out key predictors, even if they are not significant, because they may interact with the predictors.

Outcome variable:

  - Must be continuous which G\_SC

### Reporting my model

<blockquote class="blockquote">

A multiple regression analysis was conducted to determine if a student’s high school Mathematics (MAT\_S11) score and high school Critical Reading (CR\_S11) score could predict a student’s Overall average score (G\_SC) in Engineering in college.

Examination of the histogram, normal Q-Q plots of standardized residuals and the scatterplot of the dependent variable, academic satisfaction, and standardized residuals showed that the some outliers existed. However, examination of the standardized residuals showed that none could be considered to have undue influence (95% within limits of -3.29 to plus 3.29 and less than 5% with a Cook’s distance three times the mean.

Examination for multicollinearity showed that the tolerance and variance influence factor measures were within acceptable levels (tolerance \>0.4, VIF \<2.5 ) as outlined in Tarling (2008). The scatterplot of standardized residuals showed that the data met the assumptions of homogeneity of variance and linearity. The data also meets the assumption of non-zero variances of the predictors.

</blockquote>

The z-scale standardized equation of model2 is:

``` r
coef(model2)
```

    ##             (Intercept)        df$scale_MAT_S11         df$scale_CR_S11 
    ## 0.000000000000001828726 0.390472346583577545331 0.415425295946089234711

``` 
 G_SC = 0 + 0.39 * MAT_S11 + 0.42 * CR_S11
```

  - explaining 52.3% of the variation in G\_SC

## Adding GENDER

Let’s check for a relationship between GENDER and G\_SC before adding it to our model.

I will do a simple t-test.

I will state the two hypothesis for this test:

H<sub>0</sub>: There is no difference in G\_SC results for a female in comparison to a male

H<sub>a</sub>: There is a difference in G\_SC results for a female in comparison to a male

We now first conduct Levene’s test for homogeneity of variance

``` r
car::leveneTest(G_SC ~ GENDER, data=df)
```

    ## Warning in leveneTest.default(y = y, group = group, ...): group coerced to
    ## factor.

    ## Levene's Test for Homogeneity of Variance (center = median)
    ##          Df F value   Pr(>F)    
    ## group     1  13.115 0.000294 ***
    ##       12409                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The statistical hypotheses for Levene’s Test are:
Null hypothesis (H<sub>0</sub>): the variances of the two groups are equal.
Alternative hypothesis (H<sub>a</sub>): the variances are different.

The variances are different from the Levene’s Test so we cannot use GENDER.

## Adding SCHOOL\_NAT

Let’s check for a relationship between SCHOOL\_NAT and G\_SC before adding it to our model.

I will do a simple t-test.

I will state the two hypothesis for this test:

H<sub>0</sub>: There is no difference in G\_SC results for a student in private school to one in public school

H<sub>a</sub>: There is a difference in G\_SC results for a student in private school to one in public school

We now first conduct Levene’s test for homogeneity of variance

``` r
car::leveneTest(G_SC ~ SCHOOL_NAT, data=df)
```

    ## Warning in leveneTest.default(y = y, group = group, ...): group coerced to
    ## factor.

    ## Levene's Test for Homogeneity of Variance (center = median)
    ##          Df F value    Pr(>F)    
    ## group     1  12.614 0.0003843 ***
    ##       12409                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The statistical hypotheses for Levene’s Test are:
Null hypothesis (H<sub>0</sub>): the variances of the two groups are equal.
Alternative hypothesis (H<sub>a</sub>): the variances are different.

The test result is significant and so we can reject the Null and these populations variances are different.

I will not add it to the model.

## Adding Biology (BIO\_S11)

Correlation Scatter plot (BIO\_S11 and G\_SC):

``` r
df %>%
  ggplot(aes(x=scale_BIO_S11, y=scale_G_SC)) +
  geom_point() + 
  geom_smooth(method = "lm", colour = "Green", se = F) + 
  labs(x = "Biology (BIO_S11)", y = "Overall average score (G_SC)") 
```

    ## `geom_smooth()` using formula 'y ~ x'

<div class="figure" style="text-align: center">

<img src="{{< relref "posts/2021-01-04-dataset-of-academic-performance-evolution/index.markdown" >}}index_files/figure-html/unnamed-chunk-38-1.png" alt="Correlation Scatter plot (BIO_S11 and G_SC)" width="672" />

<p class="caption">

Figure 11: Correlation Scatter plot (BIO\_S11 and G\_SC)

</p>

</div>

``` r
#Pearson Correlation
cor.test(df$scale_BIO_S11, df$scale_G_SC, method='pearson')
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  df$scale_BIO_S11 and df$scale_G_SC
    ## t = 99.627, df = 12409, p-value < 0.00000000000000022
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.6567441 0.6762966
    ## sample estimates:
    ##      cor 
    ## 0.666635

The correlation coefficient is a commonly used measure of the size of an effect: values of ±.1 represent a small effect, ±.3 is a medium effect and ±.5 is a large effect

The correlation between Biology (BIO\_S11) and overall score(G\_SC) is statistically significant.

To report Pearson coefficient here we say:

<blockquote class="blockquote">

12411 Biology (BIO\_S11)(M=64.0, SD=11.2) high school results and a college Engineering overall score(G\_SC) (M=162.7, SD=23.1) were investigated. A positive Pearson r correlation coefficient of 0.67 was revealed. There is strong correlation between Biology (BIO\_S11) results and the college Engineering overall score(G\_SC) with t(12409) = 99.62 and a p-value \< 0.001. The size of the effect is large.

</blockquote>

Creating a new model with Biology (BIO\_S11)

``` r
model3<-lm(df$scale_G_SC~df$scale_MAT_S11+df$scale_CR_S11+df$scale_BIO_S11)
stargazer::stargazer(model1, model2, model3, type="text")
```

    ## 
    ## ==========================================================================================================
    ##                                                      Dependent variable:                                  
    ##                     --------------------------------------------------------------------------------------
    ##                                                           scale_G_SC                                      
    ##                                 (1)                          (2)                          (3)             
    ## ----------------------------------------------------------------------------------------------------------
    ## scale_MAT_S11                 0.644***                     0.390***                     0.240***          
    ##                               (0.007)                      (0.008)                      (0.010)           
    ##                                                                                                           
    ## scale_CR_S11                                               0.415***                     0.333***          
    ##                                                            (0.008)                      (0.008)           
    ##                                                                                                           
    ## scale_BIO_S11                                                                           0.263***          
    ##                                                                                         (0.010)           
    ##                                                                                                           
    ## Constant                       0.000                        0.000                        0.000            
    ##                               (0.007)                      (0.006)                      (0.006)           
    ##                                                                                                           
    ## ----------------------------------------------------------------------------------------------------------
    ## Observations                   12,411                       12,411                       12,411           
    ## R2                             0.415                        0.523                        0.548            
    ## Adjusted R2                    0.414                        0.523                        0.547            
    ## Residual Std. Error      0.765 (df = 12409)           0.691 (df = 12408)           0.673 (df = 12407)     
    ## F Statistic         8,785.839*** (df = 1; 12409) 6,799.872*** (df = 2; 12408) 5,006.006*** (df = 3; 12407)
    ## ==========================================================================================================
    ## Note:                                                                          *p<0.1; **p<0.05; ***p<0.01

Some outcomes from model 3:

  - The Adjusted R squared has improved from 0.4145 to 0.523 and now to 0.547, meaning we can now explain more of variation in G\_SC by including BIO\_S11 into our model
  - BIO\_S11 is statically significant to p\<0.01.

One of the assumptions of a multiple linear regression model is that the residuals follow a normal distribution. We need to check this now on our model2. We can do this by finding any influential outliers by using cook’s distance.

<div class="figure" style="text-align: center">

<img src="{{< relref "posts/2021-01-04-dataset-of-academic-performance-evolution/index.markdown" >}}index_files/figure-html/unnamed-chunk-41-1.png" alt="Cook's D for model3" width="672" />

<p class="caption">

Figure 12: Cook’s D for model3

</p>

</div>

``` r
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
cat("Number of influencers:", length(influential), "\n")
```

    ## Number of influencers: 588

``` r
cat ("20 influencers in MAT_S11",head(df[influential, ]$MAT_S11, n = 20),"\n")
```

    ## 20 influencers in MAT_S11 71 58 71 51 40 58 80 70 57 60 51 77 56 65 53 69 87 73 77 71

``` r
cat ("20 influencers in CR_S11",head(df[influential, ]$CR_S11, n = 20),"\n")
```

    ## 20 influencers in CR_S11 74 67 95 50 54 64 51 52 56 54 67 54 62 81 42 55 67 69 64 78

``` r
cat ("Percentage of influencers: ", (length(influential) / nrow(df)) * 100, "% \n")
```

    ## Percentage of influencers:  4.737733 %

<div class="figure" style="text-align: center">

<img src="{{< relref "posts/2021-01-04-dataset-of-academic-performance-evolution/index.markdown" >}}index_files/figure-html/unnamed-chunk-43-1.png" alt="Cook's D for model3" width="672" />

<p class="caption">

Figure 13: Cook’s D for model3

</p>

</div>

<div class="figure" style="text-align: center">

<img src="{{< relref "posts/2021-01-04-dataset-of-academic-performance-evolution/index.markdown" >}}index_files/figure-html/unnamed-chunk-44-1.png" alt="Assess homocedasticity for model3" width="672" />

<p class="caption">

Figure 14: Assess homocedasticity for model3

</p>

</div>

  - We can see there is absolutely no heteroscedastity, we see a completely random, equal distribution of points throughout the range of X axis and a flat red line. There is no pattern in the residuals.

<div class="figure" style="text-align: center">

<img src="{{< relref "posts/2021-01-04-dataset-of-academic-performance-evolution/index.markdown" >}}index_files/figure-html/unnamed-chunk-45-1.png" alt="Histogram and density plot of the residuals for model3" width="672" />

<p class="caption">

Figure 15: Histogram and density plot of the residuals for model3

</p>

</div>

``` r
residuals <- residuals(model3)
resids<- abs(residuals)
cat("Percentage greater than 1.96 SDs:", FSA::perc(as.numeric(resids), 1.96, "gt"),"% \n")
```

    ## Percentage greater than 1.96 SDs: 0.8621384 %

``` r
cat("Percentage greater than 3.29 SDs:",FSA::perc(as.numeric(resids), 3.29, "gt"), "%")
```

    ## Percentage greater than 3.29 SDs: 0.0402868 %

  - almost 0% outliers at 3.29 standards deviations from mean and for a data set this size. We can accept the distribution as normal.

<div class="figure" style="text-align: center">

<img src="{{< relref "posts/2021-01-04-dataset-of-academic-performance-evolution/index.markdown" >}}index_files/figure-html/unnamed-chunk-47-1.png" alt="QQ plot of the residuals for model3" width="672" />

<p class="caption">

Figure 16: QQ plot of the residuals for model3

</p>

</div>

    ## [1]  1336 12376

We now need to check for Collinearity in our model3.

### Collinearity:

Calculate Collinearity:

``` r
vifmodel<-car::vif(model3)
vifmodel
```

    ## df$scale_MAT_S11  df$scale_CR_S11 df$scale_BIO_S11 
    ##         2.511959         1.866286         2.805998

  - As a rule of thumb, a VIF score over 5 is a problem. A score over 10 should be remedied and you should consider dropping the problematic variable from the regression model

Calculate tolerance:

``` r
1/vifmodel
```

    ## df$scale_MAT_S11  df$scale_CR_S11 df$scale_BIO_S11 
    ##        0.3980957        0.5358235        0.3563794

  - If the VIF value is greater than 2.5 or the Tolerance is less than 0.4, then you have concerns over multicollinearity.

  - Collinearity occurs when two or more independent variables are giving the same information, one could be redundant.

  - To check collinearity, we examine the correlation matrix that compares the independent variables with each other.

  - If we get a correlation coefficient of above 0.8, then we may have collinearity.

<div class="figure" style="text-align: center">

<img src="{{< relref "posts/2021-01-04-dataset-of-academic-performance-evolution/index.markdown" >}}index_files/figure-html/unnamed-chunk-50-1.png" alt="correlation matrix for model3" width="672" />

<p class="caption">

Figure 17: correlation matrix for model3

</p>

</div>

  - the result from our correlation matrix for model2 show that these variables have a correlation less than 0.8, so we don’t have collinearity.

Reporting:

<blockquote class="blockquote">

Tests to see if the data met the assumption of collinearity indicated that multicollinearity was a concern for BIO\_S11 results (MAT\_S11, Tolerance = .63, VIF = 1.6; CR\_S11, Tolerance = .63 VIF = 1.6; BIO\_S11, Tolerance = .35 VIF = 2.80).

</blockquote>

Therefore BIO\_S11 cannot be added to the model. We do not need any further tests.

# Discussion/Conclusion

# References

<div id="refs" class="references">

<div id="ref-stephanie_2018">

Stephanie. 2018. “Cook’s Distance / Cook’s d: Definition, Interpretation.” *Statistics How To*. <https://www.statisticshowto.com/cooks-distance/>.

</div>

</div>
