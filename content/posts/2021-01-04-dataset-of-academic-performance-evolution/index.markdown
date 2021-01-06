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
school using five generic high school tests amongst students of Engineering in Columbia. The five tests are Mathematics (MAT\_S11), Critical Reading (CR\_S11), Citizen Competencies (CC\_S11), Biology (BIO\_S11) and English (ENG\_S11). Consequently can these predictors be used to predict a student’s overall average score in Engineering by linear regression and which predictors are the most influential.

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

<img src="{{< relref "posts/2021-01-04-dataset-of-academic-performance-evolution/index.markdown" >}}index_files/figure-html/unnamed-chunk-10-1.png" alt="Correlation Scatter plot (MAT_S11 and G_SC)" width="672" />

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

### Mulpile Linear Regression

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

<img src="{{< relref "posts/2021-01-04-dataset-of-academic-performance-evolution/index.markdown" >}}index_files/figure-html/unnamed-chunk-14-1.png" alt="Correlation Scatter plot (CR_S11 and G_SC)" width="672" />

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

A quick look at the simple linear regression model we created again:

``` r
#Pearson Correlation
stargazer::stargazer(model1, type="text")
```

    ## 
    ## ================================================
    ##                         Dependent variable:     
    ##                     ----------------------------
    ##                              scale_G_SC         
    ## ------------------------------------------------
    ## scale_MAT_S11                 0.644***          
    ##                               (0.007)           
    ##                                                 
    ## Constant                       0.000            
    ##                               (0.007)           
    ##                                                 
    ## ------------------------------------------------
    ## Observations                   12,411           
    ## R2                             0.415            
    ## Adjusted R2                    0.414            
    ## Residual Std. Error      0.765 (df = 12409)     
    ## F Statistic         8,785.839*** (df = 1; 12409)
    ## ================================================
    ## Note:                *p<0.1; **p<0.05; ***p<0.01

Adding Critical Reading (CR\_S11)

``` r
model2<-lm(df$scale_G_SC~df$scale_MAT_S11+df$scale_CR_S11)
stargazer::stargazer(model2, type="text")
```

    ## 
    ## ================================================
    ##                         Dependent variable:     
    ##                     ----------------------------
    ##                              scale_G_SC         
    ## ------------------------------------------------
    ## scale_MAT_S11                 0.390***          
    ##                               (0.008)           
    ##                                                 
    ## scale_CR_S11                  0.415***          
    ##                               (0.008)           
    ##                                                 
    ## Constant                       0.000            
    ##                               (0.006)           
    ##                                                 
    ## ------------------------------------------------
    ## Observations                   12,411           
    ## R2                             0.523            
    ## Adjusted R2                    0.523            
    ## Residual Std. Error      0.691 (df = 12408)     
    ## F Statistic         6,799.872*** (df = 2; 12408)
    ## ================================================
    ## Note:                *p<0.1; **p<0.05; ***p<0.01

Some outcomes from model 2

  - The Adjusted R squared has improved from 0.4145 to 0.523, meaning we can now explain more of variation in G\_SC by including CR\_S11 into our model
  - CR\_S11 is statically significant to p\<0.01.

One of the assumptions of a multiple linear regression model is that the residuals follow a normal distribution. We need to check this now on our model2. We can do this by finding any influential outliers by using cook’s distance.

<div class="figure" style="text-align: center">

<img src="{{< relref "posts/2021-01-04-dataset-of-academic-performance-evolution/index.markdown" >}}index_files/figure-html/unnamed-chunk-18-1.png" alt="Cook's D for model2" width="672" />

<p class="caption">

Figure 4: Cook’s D for model2

</p>

</div>

<div class="figure" style="text-align: center">

<img src="{{< relref "posts/2021-01-04-dataset-of-academic-performance-evolution/index.markdown" >}}index_files/figure-html/unnamed-chunk-19-1.png" alt="Cook's D for model2" width="672" />

<p class="caption">

Figure 5: Cook’s D for model2

</p>

</div>

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

<img src="{{< relref "posts/2021-01-04-dataset-of-academic-performance-evolution/index.markdown" >}}index_files/figure-html/unnamed-chunk-22-1.png" alt="leverage plots for model2" width="672" />

<p class="caption">

Figure 6: leverage plots for model2

</p>

</div>

<div class="figure" style="text-align: center">

<img src="{{< relref "posts/2021-01-04-dataset-of-academic-performance-evolution/index.markdown" >}}index_files/figure-html/unnamed-chunk-23-1.png" alt="Assess homocedasticity for model2" width="672" />

<p class="caption">

Figure 7: Assess homocedasticity for model2

</p>

</div>

  - We can see there is absolutely no heteroscedastity, we see a completely random, equal distribution of points throughout the range of X axis and a flat red line. There is no pattern in the residuals.

<div class="figure" style="text-align: center">

<img src="{{< relref "posts/2021-01-04-dataset-of-academic-performance-evolution/index.markdown" >}}index_files/figure-html/unnamed-chunk-24-1.png" alt="Histogram and density plot of the residuals for model2" width="672" />

<p class="caption">

Figure 8: Histogram and density plot of the residuals for model2

</p>

</div>

# Discussion/Conclusion
