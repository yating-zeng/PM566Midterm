PM566_Midterm
================
Yating Zeng
2022-10-23

# Introduction

COVID-19 has been here for around 3 years, with vaccine widely used. It
would be likely that some of the people tend to not take the vaccine
than the others. Thus, in this project, the question of my interest is:
What is the association between age and two vaccination status (at least
one dose & completed a primary series) in California state? For this
project, I’ll use the dataset on Covid-19 vaccination from the Centers
for Disease Control and Prevention (CDC) website, which provided data
for select demographic characteristics (age, sex, and age by sex) of
people receiving COVID-19 vaccinations in the United States at the
national and jurisdictional levels, fitting my analysis interest well.
All the data were cumulative data, which were counted since the date it
started observing.

# Methods

## 1.Dataset

In this project, the dataset used was a public resource from CDC
website, named “COVID-19 Vaccination Age and Sex Trends in the United
States, National and Jurisdictional”. The link of the dataset is shown
below:
<https://data.cdc.gov/Vaccinations/COVID-19-Vaccination-Age-and-Sex-Trends-in-the-Uni/5i5k-6cmh>.
The CSV file of the data was then downloaded and read into R studio for
further analysis in this project.

``` r
library(readr)
```

    ## Warning: package 'readr' was built under R version 4.1.2

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.1.2

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.5     ✔ dplyr   1.0.9
    ## ✔ tibble  3.1.6     ✔ stringr 1.4.0
    ## ✔ tidyr   1.2.0     ✔ forcats 0.5.2
    ## ✔ purrr   0.3.4

    ## Warning: package 'tidyr' was built under R version 4.1.2

    ## Warning: package 'purrr' was built under R version 4.1.2

    ## Warning: package 'dplyr' was built under R version 4.1.2

    ## Warning: package 'forcats' was built under R version 4.1.2

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(dplyr)
library(stringr)
library(kableExtra)
```

    ## Warning: package 'kableExtra' was built under R version 4.1.2

    ## 
    ## Attaching package: 'kableExtra'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     group_rows

``` r
#read in the dataset

if (!file.exists("COVID19_Vaccination.csv")){
  library("RSocrata")
  vaccination <- read.socrata(
                 "https://data.cdc.gov/resource/n8mc-b4w4.json",
                  app_token = "KS8vICWuRMDR6QzLnGP7SVO1a",
                  email     = "yatingzeng18@gmail.com",
                  password  = "Ttzyt119089838--"
  )
}

vaccination <- read_csv("COVID19_Vaccination.csv")
```

    ## Rows: 1744080 Columns: 12
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): Date, Location, Demographic_Category
    ## dbl (9): census, Administered_Dose1, Series_Complete_Yes, Booster_Doses, Sec...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

## 2.Data cleaning, wrangling and EDA

After checking the summary of the content of the dataset, the dimensions
and the original properties for each variable were known. I filtered the
data to create a new dataset to keep only the information of California.
For simplifying the typing in analysis, 7 columns were renamed to be
shorter. Then the proportion of missing values of each column and column
“Demographic_Category” were checked. Considering that age and
vaccination status of primary dose series were the main factors towards
this analysis, the information about “Booster”, “Age_unknown” and all
the “Age\>65” levels of the “Demographic_Category” column, and and the
missing values of “dose1”(count of people take at least one dose) and
“census” (census statistics used for calculating the percentage of
vaccination) were removed.

Because the information this dataset was about the information strongly
rely on time series, and all the statistics were cumulative data, a new
variable “date” was created for further reorder the data by the time
recorded. Based on the category from “Demographic_Category” variable
(now named “cat”), the original dataset was split into 4 subset for
better analysis, which are 1. objects from both sex categorized only by
age level; 2. objects were all females categorized by age level; 3.
objects were all males categorized by age level; and 4. objects from
both sex categorized only by sex level.

Totally 8 summary tables and 8 summary figures (boxplots) were planed to
create by 2 vaccination status (“at least one dose” and “completed a
primary series”) and 4 categorical groups (“age”; “female_age”;
“male_age”; and “sex”), showing the minimum, 1st quantile, median, 3nd
quantile, maximum, and the number of recorded objects of “the percentage
of people” with the 2 kinds of vaccination status grouped by age, sex or
age groups stratified by sex. The reason to use data stratified by sex
was to remove the possible confounding effect from sex on the
association between vaccination status and age level. Then to find out
the association between the age and vaccination status, 8 grouped
scatterplots were planed to create, by the same approach mentioned in
the part of summary tables and figures.

``` r
#select only the data of CA
ca_vac <- vaccination[which(vaccination$Location == "CA"), ]
#str(ca_vac)

#reorder the dataset by Demographic_Category and then date
ca_vac <- ca_vac[order( ca_vac[,3], ca_vac[,1] ),]
```

    ## Warning in xtfrm.data.frame(x): cannot xtfrm data frames

    ## Warning in xtfrm.data.frame(x): cannot xtfrm data frames

``` r
#head(ca_vac)
#simplified the variable names
colnames(ca_vac)[3]  <- "cat"
colnames(ca_vac)[5]  <- "dose1"
colnames(ca_vac)[6]  <- "series"
colnames(ca_vac)[9]  <- "dose1_pct"
colnames(ca_vac)[10] <- "series_pct"
colnames(ca_vac)[11] <- "booster_pct"
colnames(ca_vac)[12] <- "secbooster_pct"
```

``` r
#checking the proportion of missing values
#(colMeans(is.na(ca_vac)))*100
```

``` r
vac <- ca_vac[which(ca_vac$cat != "Age_Unknown"), ] 
vac <- vac %>%
  filter(!is.na(vac$dose1),!is.na(vac$census))

#check the missing value again
#(colMeans(is.na(vac)))*100
```

``` r
#check about the "Demographic_Category", "Dose1_pct",and "series_pct"
#unique(vac$cat)
#summary(vac$dose1_pct)
#summary(vac$series_pct)
```

``` r
#create new variables about date
vac$Date  <- substr(vac$Date, 0, 10)
vac$year  <- substr(vac$Date, 7, 10)
vac$month <- substr(vac$Date, 0, 2)
vac$day   <- substr(vac$Date, 4, 5)
```

``` r
#sort the data by date 
vac1 <- vac[with(vac, order(year, month, day)), ]

#create a new "date" numeric variable with the time order acceptable for reoder dataset
vac1 <- mutate(vac1, date = paste(year, month, day))
vac1$date <- str_replace_all(vac1$date, fixed(" "), "")

vac1$year  <- as.numeric(vac1$year)
vac1$month <- as.numeric(vac1$month)
vac1$day   <- as.numeric(vac1$day)
```

``` r
#remove the some information of no interest: booster information; the data of level "ages 65+"
vac1 = subset(vac1, select = -c(Booster_Doses, Second_Booster, booster_pct, secbooster_pct) )
vac1 <- vac1 %>%
  filter(vac1$cat != "Ages_65+_yrs",
               vac1$cat != "Female_Ages_65+_yrs",
               vac1$cat != "Male_Ages_65+_yrs"
               )

#find that there is a unreasonable order for the level 5-11

#rename the level of 5-11 to 05-11
vac1$cat <- str_replace_all(vac1$cat, fixed("Female_Ages_5-11_yrs"), "Female_Ages_05-11_yrs")
vac1$cat <- str_replace_all(vac1$cat, fixed("Male_Ages_5-11_yrs"),   "Male_Ages_05-11_yrs")
vac1$cat <- str_replace_all(vac1$cat, fixed("Ages_5-11_yrs"),        "Ages_05-11_yrs")
vac1$cat <- str_replace_all(vac1$cat, fixed("Female_Ages_2-4_yrs"),  "Female_Ages_02-04_yrs")
vac1$cat <- str_replace_all(vac1$cat, fixed("Male_Ages_2-4_yrs"),    "Male_Ages_02-04_yrs")
vac1$cat <- str_replace_all(vac1$cat, fixed("Ages_2-4_yrs"),         "Ages_02-04_yrs")
```

``` r
#splitting the data by "cat" level into 4 subset: "age"; "Female_age"; "Male_age"; "sex"
vac1$CAT = substr(vac1$cat, 0, 1)
#build a subset for 
vac_age<- vac1 %>%
  filter(vac1$CAT == "A")
vac_Fage<- vac1 %>%
  filter(vac1$CAT == "F")
vac_Mage<- vac1 %>%
  filter(vac1$CAT == "M")
vac_sex<- vac1 %>%
  filter(vac1$CAT == "S")
```

# Results

## 1. Summary tables

From the summary table of the percent of people with at least one dose
grouped by age (Table 1), we could notice that excepting a little part
of the data, most part the data were showing a trend that the statistics
(the minimum, 1st quantile, median, 3nd quantile, maximum) of Percent of
people with at least one dose would be larger when the age level was
higher. And for the major part of observed objects, who were aged from
12-17 years old to 75+ years old, the final vaccination rate will went
up to around 90%, while for the low age-level(\<5 years) objects, the
vaccination rate were all under or around 10%, and for 5-11 years
objects the vaccination rate stayed in the middle, which is around 50%.
This trend were consistent with the results form all the other 2 summary
tables of Percent of people with at least one dose grouped by age
stratidied by sex (Table 2 & 3).

For the summary table of the percent of people completed a primary
series grouped by age (Table 5), we could find that there was still a
trend similar to the one above between the vaccination status and age
level, but what different was that the statistics were lower than the
one of people with at least one dose. For the major part of observed
objects, aged from 12-17 years old to 75+ years old, the final
vaccination rate will went up to 70%-90%, with group of 65-74 years age
would up to 95%. But for the low age-level(\<5 years) objects, the
vaccination rate were all under or around 5%, and for 5-11 years objects
the vaccination rate stayed in the middle, which is around 40%. This
trend were consistent with the results form all the other 2 summary
tables of Percent of people with at least one dose grouped by age
stratidied by sex (Table 6 & 7).

From the summary tables for the percentage of 2 vaccination status
grouped by sex (Table 4 & 8), we could find that the statistics of
female were always larger than males; and in the one for the percent of
people with at least one dose, the final vaccination would go up to
80+%, while for the one of the percent of people completed a primary
series, the rate could up to 70+%.

``` r
# summary table for dose1
vac_age_tab <- vac_age %>% group_by(cat) %>%
                   summarise(
                     min    = min(dose1_pct, na.rm = T),
                     q1     = quantile(dose1_pct, 0.25),
                     median = median(dose1_pct),
                     q3     = quantile(dose1_pct, 0.75),
                     max    = max(dose1_pct, na.rm = T), 
                     days_recorded    = sum(!is.na(dose1_pct)),
                     ) %>%  arrange(cat) %>% 
                                    kbl(caption = 
             "Table 1.Summary of Percent of people with at least one dose grouped by age") %>% 
                                      kable_styling()

vac_Fage_tab <- vac_Fage %>% group_by(cat) %>%
                   summarise(
                     min    = min(dose1_pct, na.rm = T),
                     q1     = quantile(dose1_pct, 0.25),
                     median = median(dose1_pct),
                     q3     = quantile(dose1_pct, 0.75),
                     max    = max(dose1_pct, na.rm = T), 
                     days_recorded    = sum(!is.na(dose1_pct))
                     ) %>%  arrange(cat) %>% 
                                    kbl(caption = 
            "Table 2.Summary of Percent of people with at least one dose in females grouped by age") %>% 
                                      kable_styling()

vac_Mage_tab <- vac_Mage %>% group_by(cat) %>%
                   summarise(
                     min           = min(dose1_pct, na.rm = T),
                     q1            = quantile(dose1_pct, 0.25),
                     median        = median(dose1_pct),
                     q3            = quantile(dose1_pct, 0.75),
                     max           = max(dose1_pct, na.rm = T), 
                     days_recorded  = sum(!is.na(dose1_pct))
                     
                     ) %>%  arrange(cat) %>% 
                                    kbl(caption = 
            "Table 3.Summary of Percent of people with at least one dose in males grouped by age") %>%
                                      kable_styling()

vac_sex_tab <- vac_sex %>% group_by(cat) %>%
                   summarise(
                     min    = min(dose1_pct, na.rm = T),
                     q1     = quantile(dose1_pct, 0.25),
                     median = median(dose1_pct),
                     q3     = quantile(dose1_pct, 0.75),
                     max    = max(dose1_pct, na.rm = T), 
                     days_recorded    = sum(!is.na(dose1_pct))
                     ) %>%  arrange(cat) %>% 
                                    kbl(caption = 
            "Table 4.Summary of Percent of people with at least one dose grouped by sex") %>% 
                                      kable_styling()

vac_age_tab
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>
Table 1.Summary of Percent of people with at least one dose grouped by
age
</caption>
<thead>
<tr>
<th style="text-align:left;">
cat
</th>
<th style="text-align:right;">
min
</th>
<th style="text-align:right;">
q1
</th>
<th style="text-align:right;">
median
</th>
<th style="text-align:right;">
q3
</th>
<th style="text-align:right;">
max
</th>
<th style="text-align:right;">
days_recorded
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Ages\_\<2yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3.725
</td>
<td style="text-align:right;">
5.20
</td>
<td style="text-align:right;">
6.200
</td>
<td style="text-align:right;">
7.1
</td>
<td style="text-align:right;">
122
</td>
</tr>
<tr>
<td style="text-align:left;">
Ages\_\<5yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
4.725
</td>
<td style="text-align:right;">
7.00
</td>
<td style="text-align:right;">
8.200
</td>
<td style="text-align:right;">
9.1
</td>
<td style="text-align:right;">
122
</td>
</tr>
<tr>
<td style="text-align:left;">
Ages_02-04_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
5.350
</td>
<td style="text-align:right;">
8.15
</td>
<td style="text-align:right;">
9.475
</td>
<td style="text-align:right;">
10.4
</td>
<td style="text-align:right;">
122
</td>
</tr>
<tr>
<td style="text-align:left;">
Ages_05-11_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.500
</td>
<td style="text-align:right;">
16.10
</td>
<td style="text-align:right;">
42.700
</td>
<td style="text-align:right;">
46.2
</td>
<td style="text-align:right;">
667
</td>
</tr>
<tr>
<td style="text-align:left;">
Ages_12-17_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
35.700
</td>
<td style="text-align:right;">
72.80
</td>
<td style="text-align:right;">
82.300
</td>
<td style="text-align:right;">
84.0
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Ages_18-24_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
54.100
</td>
<td style="text-align:right;">
77.65
</td>
<td style="text-align:right;">
88.325
</td>
<td style="text-align:right;">
90.7
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Ages_25-39_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
58.300
</td>
<td style="text-align:right;">
78.05
</td>
<td style="text-align:right;">
86.400
</td>
<td style="text-align:right;">
88.2
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Ages_25-49_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
62.300
</td>
<td style="text-align:right;">
81.05
</td>
<td style="text-align:right;">
89.000
</td>
<td style="text-align:right;">
90.6
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Ages_40-49_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
69.375
</td>
<td style="text-align:right;">
86.35
</td>
<td style="text-align:right;">
93.400
</td>
<td style="text-align:right;">
94.8
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Ages_50-64_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
78.200
</td>
<td style="text-align:right;">
91.35
</td>
<td style="text-align:right;">
95.000
</td>
<td style="text-align:right;">
95.0
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Ages_65-74_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
90.900
</td>
<td style="text-align:right;">
95.00
</td>
<td style="text-align:right;">
95.000
</td>
<td style="text-align:right;">
95.0
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Ages_75+\_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
85.700
</td>
<td style="text-align:right;">
93.35
</td>
<td style="text-align:right;">
95.000
</td>
<td style="text-align:right;">
95.0
</td>
<td style="text-align:right;">
676
</td>
</tr>
</tbody>
</table>

``` r
vac_Fage_tab
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>
Table 2.Summary of Percent of people with at least one dose in females
grouped by age
</caption>
<thead>
<tr>
<th style="text-align:left;">
cat
</th>
<th style="text-align:right;">
min
</th>
<th style="text-align:right;">
q1
</th>
<th style="text-align:right;">
median
</th>
<th style="text-align:right;">
q3
</th>
<th style="text-align:right;">
max
</th>
<th style="text-align:right;">
days_recorded
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Female_Ages\_\<2yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3.725
</td>
<td style="text-align:right;">
5.30
</td>
<td style="text-align:right;">
6.275
</td>
<td style="text-align:right;">
7.1
</td>
<td style="text-align:right;">
122
</td>
</tr>
<tr>
<td style="text-align:left;">
Female_Ages\_\<5yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
4.750
</td>
<td style="text-align:right;">
7.10
</td>
<td style="text-align:right;">
8.275
</td>
<td style="text-align:right;">
9.1
</td>
<td style="text-align:right;">
122
</td>
</tr>
<tr>
<td style="text-align:left;">
Female_Ages_02-04_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
5.425
</td>
<td style="text-align:right;">
8.20
</td>
<td style="text-align:right;">
9.575
</td>
<td style="text-align:right;">
10.5
</td>
<td style="text-align:right;">
122
</td>
</tr>
<tr>
<td style="text-align:left;">
Female_Ages_05-11_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.700
</td>
<td style="text-align:right;">
18.20
</td>
<td style="text-align:right;">
43.500
</td>
<td style="text-align:right;">
46.9
</td>
<td style="text-align:right;">
661
</td>
</tr>
<tr>
<td style="text-align:left;">
Female_Ages_12-17_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
37.400
</td>
<td style="text-align:right;">
75.15
</td>
<td style="text-align:right;">
84.800
</td>
<td style="text-align:right;">
86.6
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Female_Ages_18-24_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
57.600
</td>
<td style="text-align:right;">
80.35
</td>
<td style="text-align:right;">
91.300
</td>
<td style="text-align:right;">
93.7
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Female_Ages_25-39_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
60.600
</td>
<td style="text-align:right;">
80.25
</td>
<td style="text-align:right;">
88.700
</td>
<td style="text-align:right;">
90.5
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Female_Ages_25-49_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
64.600
</td>
<td style="text-align:right;">
83.05
</td>
<td style="text-align:right;">
90.900
</td>
<td style="text-align:right;">
92.5
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Female_Ages_40-49_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
71.500
</td>
<td style="text-align:right;">
87.90
</td>
<td style="text-align:right;">
94.700
</td>
<td style="text-align:right;">
95.0
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Female_Ages_50-64_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
79.000
</td>
<td style="text-align:right;">
91.45
</td>
<td style="text-align:right;">
95.000
</td>
<td style="text-align:right;">
95.0
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Female_Ages_65-74_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
90.100
</td>
<td style="text-align:right;">
95.00
</td>
<td style="text-align:right;">
95.000
</td>
<td style="text-align:right;">
95.0
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Female_Ages_75+\_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
83.800
</td>
<td style="text-align:right;">
91.25
</td>
<td style="text-align:right;">
95.000
</td>
<td style="text-align:right;">
95.0
</td>
<td style="text-align:right;">
676
</td>
</tr>
</tbody>
</table>

``` r
vac_Mage_tab
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>
Table 3.Summary of Percent of people with at least one dose in males
grouped by age
</caption>
<thead>
<tr>
<th style="text-align:left;">
cat
</th>
<th style="text-align:right;">
min
</th>
<th style="text-align:right;">
q1
</th>
<th style="text-align:right;">
median
</th>
<th style="text-align:right;">
q3
</th>
<th style="text-align:right;">
max
</th>
<th style="text-align:right;">
days_recorded
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Male_Ages\_\<2yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3.700
</td>
<td style="text-align:right;">
5.20
</td>
<td style="text-align:right;">
6.200
</td>
<td style="text-align:right;">
7.0
</td>
<td style="text-align:right;">
122
</td>
</tr>
<tr>
<td style="text-align:left;">
Male_Ages\_\<5yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
4.650
</td>
<td style="text-align:right;">
6.90
</td>
<td style="text-align:right;">
8.100
</td>
<td style="text-align:right;">
9.0
</td>
<td style="text-align:right;">
122
</td>
</tr>
<tr>
<td style="text-align:left;">
Male_Ages_02-04_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
5.250
</td>
<td style="text-align:right;">
8.05
</td>
<td style="text-align:right;">
9.375
</td>
<td style="text-align:right;">
10.2
</td>
<td style="text-align:right;">
122
</td>
</tr>
<tr>
<td style="text-align:left;">
Male_Ages_05-11_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.675
</td>
<td style="text-align:right;">
17.90
</td>
<td style="text-align:right;">
42.000
</td>
<td style="text-align:right;">
45.4
</td>
<td style="text-align:right;">
660
</td>
</tr>
<tr>
<td style="text-align:left;">
Male_Ages_12-17_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
33.975
</td>
<td style="text-align:right;">
70.35
</td>
<td style="text-align:right;">
79.600
</td>
<td style="text-align:right;">
81.3
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Male_Ages_18-24_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
50.375
</td>
<td style="text-align:right;">
74.55
</td>
<td style="text-align:right;">
84.900
</td>
<td style="text-align:right;">
87.2
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Male_Ages_25-39_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
55.475
</td>
<td style="text-align:right;">
75.15
</td>
<td style="text-align:right;">
83.300
</td>
<td style="text-align:right;">
85.0
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Male_Ages_25-49_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
59.300
</td>
<td style="text-align:right;">
78.25
</td>
<td style="text-align:right;">
86.000
</td>
<td style="text-align:right;">
87.6
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Male_Ages_40-49_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
66.300
</td>
<td style="text-align:right;">
83.75
</td>
<td style="text-align:right;">
91.000
</td>
<td style="text-align:right;">
92.3
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Male_Ages_50-64_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
76.575
</td>
<td style="text-align:right;">
90.15
</td>
<td style="text-align:right;">
95.000
</td>
<td style="text-align:right;">
95.0
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Male_Ages_65-74_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
91.100
</td>
<td style="text-align:right;">
95.00
</td>
<td style="text-align:right;">
95.000
</td>
<td style="text-align:right;">
95.0
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Male_Ages_75+\_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
87.700
</td>
<td style="text-align:right;">
95.00
</td>
<td style="text-align:right;">
95.000
</td>
<td style="text-align:right;">
95.0
</td>
<td style="text-align:right;">
676
</td>
</tr>
</tbody>
</table>

``` r
vac_sex_tab
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>
Table 4.Summary of Percent of people with at least one dose grouped by
sex
</caption>
<thead>
<tr>
<th style="text-align:left;">
cat
</th>
<th style="text-align:right;">
min
</th>
<th style="text-align:right;">
q1
</th>
<th style="text-align:right;">
median
</th>
<th style="text-align:right;">
q3
</th>
<th style="text-align:right;">
max
</th>
<th style="text-align:right;">
days_recorded
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Sex_Female
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
59.1
</td>
<td style="text-align:right;">
75.0
</td>
<td style="text-align:right;">
84.2
</td>
<td style="text-align:right;">
86.6
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Sex_Male
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
54.7
</td>
<td style="text-align:right;">
71.3
</td>
<td style="text-align:right;">
80.6
</td>
<td style="text-align:right;">
83.0
</td>
<td style="text-align:right;">
676
</td>
</tr>
</tbody>
</table>

``` r
# summary table for series dose
vac_age_tab2 <- vac_age %>% group_by(cat) %>%
                   summarise(
                     min    = min(series_pct, na.rm = T),
                     q1     = quantile(series_pct, 0.25, na.rm = T),
                     median = median(series_pct, na.rm = T),
                     q3     = quantile(series_pct, 0.75, na.rm = T),
                     max    = max(series_pct, na.rm = T), 
                     days_recorded    = sum(!is.na(series_pct))
                     ) %>%  arrange(cat) %>% 
                                    kbl(caption = 
            "Table 5.Summary of Percent of people completed a primary series grouped by age") %>% 
                                      kable_styling()

vac_Fage_tab2 <- vac_Fage %>% group_by(cat) %>%
                   summarise(
                     min    = min(series_pct, na.rm = T),
                     q1     = quantile(series_pct, 0.25, na.rm = T),
                     median = median(series_pct, na.rm = T),
                     q3     = quantile(series_pct, 0.75, na.rm = T),
                     max    = max(series_pct, na.rm = T), 
                     days_recorded    = sum(!is.na(series_pct))
                     ) %>%  arrange(cat) %>% 
                                    kbl(caption = 
         "Table 6.Summary of Percent of people completed a primary series in females grouped by age") %>% 
                                      kable_styling()

vac_Mage_tab2 <- vac_Mage %>% group_by(cat) %>%
                   summarise(
                     min    = min(series_pct, na.rm = T),
                     q1     = quantile(series_pct, 0.25, na.rm = T),
                     median = median(series_pct, na.rm = T),
                     q3     = quantile(series_pct, 0.75, na.rm = T),
                     max    = max(series_pct, na.rm = T), 
                     days_recorded    = sum(!is.na(series_pct))
                     ) %>%  arrange(cat) %>% 
                              kbl(caption = 
           "Table 7.Summary of Percent of people completed a primary series in males grouped by age") %>% 
                                  kable_styling()

vac_sex_tab2 <- vac_sex %>% group_by(cat) %>%
                   summarise(
                     min    = min(series_pct, na.rm = T),
                     q1     = quantile(series_pct, 0.25, na.rm = T),
                     median = median(series_pct, na.rm = T),
                     q3     = quantile(series_pct, 0.75, na.rm = T),
                     max    = max(series_pct, na.rm = T), 
                     days_recorded    = sum(!is.na(series_pct))
                     ) %>%  arrange(cat) %>% 
                               kable(caption = 
           "Table 8.Summary of Percent of people completed a primary series grouped by sex") %>%
                                  kable_styling()

vac_age_tab2
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>
Table 5.Summary of Percent of people completed a primary series grouped
by age
</caption>
<thead>
<tr>
<th style="text-align:left;">
cat
</th>
<th style="text-align:right;">
min
</th>
<th style="text-align:right;">
q1
</th>
<th style="text-align:right;">
median
</th>
<th style="text-align:right;">
q3
</th>
<th style="text-align:right;">
max
</th>
<th style="text-align:right;">
days_recorded
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Ages\_\<2yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.700
</td>
<td style="text-align:right;">
1.40
</td>
<td style="text-align:right;">
2.100
</td>
<td style="text-align:right;">
2.8
</td>
<td style="text-align:right;">
99
</td>
</tr>
<tr>
<td style="text-align:left;">
Ages\_\<5yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.400
</td>
<td style="text-align:right;">
2.00
</td>
<td style="text-align:right;">
3.200
</td>
<td style="text-align:right;">
4.4
</td>
<td style="text-align:right;">
116
</td>
</tr>
<tr>
<td style="text-align:left;">
Ages_02-04_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.725
</td>
<td style="text-align:right;">
2.50
</td>
<td style="text-align:right;">
3.975
</td>
<td style="text-align:right;">
5.4
</td>
<td style="text-align:right;">
114
</td>
</tr>
<tr>
<td style="text-align:left;">
Ages_05-11_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.400
</td>
<td style="text-align:right;">
11.25
</td>
<td style="text-align:right;">
36.100
</td>
<td style="text-align:right;">
39.2
</td>
<td style="text-align:right;">
630
</td>
</tr>
<tr>
<td style="text-align:left;">
Ages_12-17_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
20.950
</td>
<td style="text-align:right;">
64.20
</td>
<td style="text-align:right;">
73.200
</td>
<td style="text-align:right;">
74.7
</td>
<td style="text-align:right;">
667
</td>
</tr>
<tr>
<td style="text-align:left;">
Ages_18-24_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
43.200
</td>
<td style="text-align:right;">
67.85
</td>
<td style="text-align:right;">
74.900
</td>
<td style="text-align:right;">
76.6
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Ages_25-39_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
48.575
</td>
<td style="text-align:right;">
69.05
</td>
<td style="text-align:right;">
74.700
</td>
<td style="text-align:right;">
75.9
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Ages_25-49_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
52.275
</td>
<td style="text-align:right;">
72.15
</td>
<td style="text-align:right;">
77.500
</td>
<td style="text-align:right;">
78.6
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Ages_40-49_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
58.775
</td>
<td style="text-align:right;">
77.55
</td>
<td style="text-align:right;">
82.400
</td>
<td style="text-align:right;">
83.4
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Ages_50-64_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
67.900
</td>
<td style="text-align:right;">
82.45
</td>
<td style="text-align:right;">
87.000
</td>
<td style="text-align:right;">
88.6
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Ages_65-74_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
80.200
</td>
<td style="text-align:right;">
89.85
</td>
<td style="text-align:right;">
94.100
</td>
<td style="text-align:right;">
95.0
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Ages_75+\_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
75.500
</td>
<td style="text-align:right;">
83.05
</td>
<td style="text-align:right;">
86.725
</td>
<td style="text-align:right;">
88.8
</td>
<td style="text-align:right;">
676
</td>
</tr>
</tbody>
</table>

``` r
vac_Fage_tab2
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>
Table 6.Summary of Percent of people completed a primary series in
females grouped by age
</caption>
<thead>
<tr>
<th style="text-align:left;">
cat
</th>
<th style="text-align:right;">
min
</th>
<th style="text-align:right;">
q1
</th>
<th style="text-align:right;">
median
</th>
<th style="text-align:right;">
q3
</th>
<th style="text-align:right;">
max
</th>
<th style="text-align:right;">
days_recorded
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Female_Ages\_\<2yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.700
</td>
<td style="text-align:right;">
1.40
</td>
<td style="text-align:right;">
2.100
</td>
<td style="text-align:right;">
2.8
</td>
<td style="text-align:right;">
97
</td>
</tr>
<tr>
<td style="text-align:left;">
Female_Ages\_\<5yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.900
</td>
<td style="text-align:right;">
2.20
</td>
<td style="text-align:right;">
3.350
</td>
<td style="text-align:right;">
4.5
</td>
<td style="text-align:right;">
107
</td>
</tr>
<tr>
<td style="text-align:left;">
Female_Ages_02-04_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.600
</td>
<td style="text-align:right;">
2.90
</td>
<td style="text-align:right;">
4.400
</td>
<td style="text-align:right;">
5.5
</td>
<td style="text-align:right;">
100
</td>
</tr>
<tr>
<td style="text-align:left;">
Female_Ages_05-11_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.500
</td>
<td style="text-align:right;">
11.80
</td>
<td style="text-align:right;">
36.700
</td>
<td style="text-align:right;">
39.9
</td>
<td style="text-align:right;">
629
</td>
</tr>
<tr>
<td style="text-align:left;">
Female_Ages_12-17_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
34.525
</td>
<td style="text-align:right;">
67.05
</td>
<td style="text-align:right;">
75.875
</td>
<td style="text-align:right;">
77.3
</td>
<td style="text-align:right;">
650
</td>
</tr>
<tr>
<td style="text-align:left;">
Female_Ages_18-24_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
46.675
</td>
<td style="text-align:right;">
70.75
</td>
<td style="text-align:right;">
77.900
</td>
<td style="text-align:right;">
79.6
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Female_Ages_25-39_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
50.800
</td>
<td style="text-align:right;">
71.30
</td>
<td style="text-align:right;">
77.100
</td>
<td style="text-align:right;">
78.4
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Female_Ages_25-49_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
54.575
</td>
<td style="text-align:right;">
74.25
</td>
<td style="text-align:right;">
79.700
</td>
<td style="text-align:right;">
80.9
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Female_Ages_40-49_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
60.900
</td>
<td style="text-align:right;">
79.25
</td>
<td style="text-align:right;">
84.100
</td>
<td style="text-align:right;">
85.1
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Female_Ages_50-64_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
68.800
</td>
<td style="text-align:right;">
82.75
</td>
<td style="text-align:right;">
87.300
</td>
<td style="text-align:right;">
88.9
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Female_Ages_65-74_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
79.475
</td>
<td style="text-align:right;">
89.05
</td>
<td style="text-align:right;">
93.225
</td>
<td style="text-align:right;">
95.0
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Female_Ages_75+\_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
73.600
</td>
<td style="text-align:right;">
81.35
</td>
<td style="text-align:right;">
85.000
</td>
<td style="text-align:right;">
86.9
</td>
<td style="text-align:right;">
676
</td>
</tr>
</tbody>
</table>

``` r
vac_Mage_tab2
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>
Table 7.Summary of Percent of people completed a primary series in males
grouped by age
</caption>
<thead>
<tr>
<th style="text-align:left;">
cat
</th>
<th style="text-align:right;">
min
</th>
<th style="text-align:right;">
q1
</th>
<th style="text-align:right;">
median
</th>
<th style="text-align:right;">
q3
</th>
<th style="text-align:right;">
max
</th>
<th style="text-align:right;">
days_recorded
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Male_Ages\_\<2yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.700
</td>
<td style="text-align:right;">
1.40
</td>
<td style="text-align:right;">
2.100
</td>
<td style="text-align:right;">
2.8
</td>
<td style="text-align:right;">
98
</td>
</tr>
<tr>
<td style="text-align:left;">
Male_Ages\_\<5yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.525
</td>
<td style="text-align:right;">
2.00
</td>
<td style="text-align:right;">
3.175
</td>
<td style="text-align:right;">
4.3
</td>
<td style="text-align:right;">
114
</td>
</tr>
<tr>
<td style="text-align:left;">
Male_Ages_02-04_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.275
</td>
<td style="text-align:right;">
2.70
</td>
<td style="text-align:right;">
4.200
</td>
<td style="text-align:right;">
5.4
</td>
<td style="text-align:right;">
104
</td>
</tr>
<tr>
<td style="text-align:left;">
Male_Ages_05-11_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.400
</td>
<td style="text-align:right;">
15.10
</td>
<td style="text-align:right;">
35.500
</td>
<td style="text-align:right;">
38.5
</td>
<td style="text-align:right;">
615
</td>
</tr>
<tr>
<td style="text-align:left;">
Male_Ages_12-17_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
27.600
</td>
<td style="text-align:right;">
62.00
</td>
<td style="text-align:right;">
70.500
</td>
<td style="text-align:right;">
71.9
</td>
<td style="text-align:right;">
657
</td>
</tr>
<tr>
<td style="text-align:left;">
Male_Ages_18-24_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
39.575
</td>
<td style="text-align:right;">
64.65
</td>
<td style="text-align:right;">
71.600
</td>
<td style="text-align:right;">
73.2
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Male_Ages_25-39_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
45.900
</td>
<td style="text-align:right;">
66.30
</td>
<td style="text-align:right;">
71.825
</td>
<td style="text-align:right;">
72.9
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Male_Ages_25-49_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
49.500
</td>
<td style="text-align:right;">
69.45
</td>
<td style="text-align:right;">
74.700
</td>
<td style="text-align:right;">
75.8
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Male_Ages_40-49_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
55.975
</td>
<td style="text-align:right;">
75.05
</td>
<td style="text-align:right;">
80.000
</td>
<td style="text-align:right;">
81.0
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Male_Ages_50-64_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
66.300
</td>
<td style="text-align:right;">
81.40
</td>
<td style="text-align:right;">
86.000
</td>
<td style="text-align:right;">
87.6
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Male_Ages_65-74_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
80.600
</td>
<td style="text-align:right;">
90.25
</td>
<td style="text-align:right;">
94.525
</td>
<td style="text-align:right;">
95.0
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Male_Ages_75+\_yrs
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
77.675
</td>
<td style="text-align:right;">
84.95
</td>
<td style="text-align:right;">
88.800
</td>
<td style="text-align:right;">
91.1
</td>
<td style="text-align:right;">
676
</td>
</tr>
</tbody>
</table>

``` r
vac_sex_tab2
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>
Table 8.Summary of Percent of people completed a primary series grouped
by sex
</caption>
<thead>
<tr>
<th style="text-align:left;">
cat
</th>
<th style="text-align:right;">
min
</th>
<th style="text-align:right;">
q1
</th>
<th style="text-align:right;">
median
</th>
<th style="text-align:right;">
q3
</th>
<th style="text-align:right;">
max
</th>
<th style="text-align:right;">
days_recorded
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Sex_Female
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
49.3
</td>
<td style="text-align:right;">
66.20
</td>
<td style="text-align:right;">
73.925
</td>
<td style="text-align:right;">
75.8
</td>
<td style="text-align:right;">
676
</td>
</tr>
<tr>
<td style="text-align:left;">
Sex_Male
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
45.1
</td>
<td style="text-align:right;">
62.55
</td>
<td style="text-align:right;">
70.300
</td>
<td style="text-align:right;">
72.1
</td>
<td style="text-align:right;">
676
</td>
</tr>
</tbody>
</table>

## 2. Summary figures

All the summary figures (Figure 1-3 & 5-7) showed the similar trend
mentioned above relatively. Excepting these information we gained and
mentioned, we still could find that for the object age level from 5-11
years old to 65-74 years old, with the age level went up, the major part
of the statistics would with higher values, which means it might take
shorter time to have a relatively high vaccination rate for those people
with higher age level. And the two figures (Figure 4 & 8) for the
percent group by sex were still show that the females would have higher
vaccination rate, making it to be reasonable that we’d better use the
stratified data for analyze the association between age and vaccination
rate.

``` r
#summary graphs for dose1
vac_age %>%
    ggplot(aes(x=date, y=dose1_pct)) +
    geom_boxplot(mapping = aes(x = cat, y = dose1_pct, fill = cat)) +
    theme(axis.text.x  = element_text(angle = 60, hjust = 1),
          plot.caption = element_text(hjust=0.5, size=rel(1.2))) +
    labs(x = "Age group", y = "Percent of people with at least one dose", 
         caption = "Figure 1.Percent of people with at least one dose grouped by age") +
    guides(fill=guide_legend(title="Age group"))
```

![](README_files/figure-gfm/summary%20graphs%20for%20dose1-1.png)<!-- -->

``` r
vac_Fage %>%
    ggplot(aes(x=date, y=dose1_pct)) +
    geom_boxplot(mapping = aes(x = cat, y = dose1_pct, fill = cat)) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1),
          plot.caption = element_text(hjust=0.5, size=rel(1.2))) +
    labs(x = "Age group", y = "Percent of people with at least one dose", 
         caption = "Figure 2.Percent of people with at least one dose of females grouped by age") +
    guides(fill=guide_legend(title="Age group"))
```

![](README_files/figure-gfm/summary%20graphs%20for%20dose1-2.png)<!-- -->

``` r
vac_Mage %>%
    ggplot(aes(x=date, y=dose1_pct)) +
    geom_boxplot(mapping = aes(x = cat, y = dose1_pct, fill = cat)) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1),
          plot.caption = element_text(hjust=0.5, size=rel(1.2))) +
    labs(x = "Age group", y = "Percent of people with at least one dose", 
         caption = "Figure 3.Percent of people with at least one dose of males grouped by age") +
    guides(fill=guide_legend(title="Age group"))
```

![](README_files/figure-gfm/summary%20graphs%20for%20dose1-3.png)<!-- -->

``` r
vac_sex %>%
    ggplot(aes(x=date, y=dose1_pct)) +
    geom_boxplot(mapping = aes(x = cat, y = dose1_pct, fill = cat)) +
    theme(plot.caption = element_text(hjust=0.5, size=rel(1.2))) +
    labs(x = "Sex", y = "Percent of people with at least one dose", 
         caption = "Figure 4.Percent of people with at least one dose grouped by sex") +
    guides(fill=guide_legend(title="Sex group"))
```

![](README_files/figure-gfm/summary%20graphs%20for%20dose1-4.png)<!-- -->

``` r
#summary graphs for series doses
vac_age %>%
    ggplot(aes(x=date, y=series_pct)) +
    geom_boxplot(mapping = aes(x = cat, y = dose1_pct, fill = cat)) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1),
          plot.caption = element_text(hjust=0.5, size=rel(1.2))) +
    labs(x = "Age group", y = "Percent of people completed a primary series", 
         caption = "Figure 5.Percent of people completed a primary series grouped by age") +
    guides(fill=guide_legend(title="Age group"))
```

![](README_files/figure-gfm/summary%20graphs%20for%20series%20doses-1.png)<!-- -->

``` r
vac_Fage %>%
    ggplot(aes(x=date, y=series_pct)) +
    geom_boxplot(mapping = aes(x = cat, y = dose1_pct, fill = cat)) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1),
          plot.caption = element_text(hjust=0.5, size=rel(1.2))) +
    labs(x = "Age group", y = "Percent of people completed a primary series", 
         caption = "Figure 6.Percent of people completed a primary series of females grouped by age") +
    guides(fill=guide_legend(title="Age group"))
```

![](README_files/figure-gfm/summary%20graphs%20for%20series%20doses-2.png)<!-- -->

``` r
vac_Mage %>%
    ggplot(aes(x=date, y=series_pct)) +
    geom_boxplot(mapping = aes(x = cat, y = dose1_pct, fill = cat)) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1),
          plot.caption = element_text(hjust=0.5, size=rel(1.2))) +
    labs(x = "Age group", y = "Percent of people completed a primary series", 
         caption = "Figure 7.Percent of people completed a primary series of males grouped by age") +
    guides(fill=guide_legend(title="Age group"))
```

![](README_files/figure-gfm/summary%20graphs%20for%20series%20doses-3.png)<!-- -->

``` r
vac_sex %>%
    ggplot(aes(x=date, y=series_pct)) +
    geom_boxplot(mapping = aes(x = cat, y = dose1_pct, fill = cat)) +
    theme(plot.caption = element_text(hjust=0.5, size=rel(1.2))) +
    labs(x = "Sex", y = "Percent of people completed a primary seriese", 
         caption = "Figure 8.Percent of people completed a primary seriese grouped by sex") +
    guides(fill=guide_legend(title="Sex group"))
```

![](README_files/figure-gfm/summary%20graphs%20for%20series%20doses-4.png)<!-- -->

## 3. Visualization of the association

These 4 figures (Figure 9-12) all verified the trend that for both 2
kinds of vaccination status(take at least one dose & with completed
series) and both sex, the vaccination rate would be higher with the age
level being higher for the same time point, and the objects with higher
age might take shorter time to have a relatively high vaccination rate.
Which needs to be mentioned is that, this trend was also observed in the
low age-level group(\<5 years), but with obviously lower vaccination
rate than the major part of the sample objects.

``` r
#visualization for first dose
#vac_age %>%
#    ggplot(aes(x = date, y = dose1_pct)) + 
#    geom_point(mapping = aes(x = date, y = dose1_pct, color = cat)) +
#    scale_x_discrete(breaks=
#          c("20201213","20210601","20211201","20220601","20221019")) +
#    theme(plot.caption = element_text(hjust=0.5, size=rel(1.2))) +
#    labs(x = "Date(yyyymmdd)", y = "Percent of people with at least one dose", col="Age group",
#         caption = "Figure 9.2020-2022 Percent of people with at least one dose grouped by age") +
#    guides(fill=guide_legend(title="Age group"))
 
#vac_sex %>%
#    ggplot(aes(x = date, y = dose1_pct)) + 
#    geom_point(mapping = aes(x = date, y = dose1_pct, color = cat)) +
#    scale_x_discrete(breaks=
#          c("20201213","20210601","20211201","20220601","20221019")) +
#    theme(plot.caption = element_text(hjust=0.5, size=rel(1.2))) +
#    labs(x = "Date(yyyymmdd)", y = "Percent of people with at least one dose", col="Sex group", 
#         caption = "Figure 8.Percent of people completed a primary seriese grouped by sex") +
#    guides(fill=guide_legend(title="Sex group")) 

vac_Fage %>%
    ggplot(aes(x = date, y = dose1_pct)) + 
    geom_point(mapping = aes(x = date, y = dose1_pct, color = cat)) +
    scale_x_discrete(breaks=
          c("20201213","20210601","20211201","20220601","20221019")) +
    theme(plot.caption = element_text(hjust=0.5, size=rel(1.2))) +
    labs(x = "Date(yyyymmdd)", y = "Percent of people with at least one dose", col="Age group", caption = "Figure 9.2020-2022 Percent of people with at least one dose of females grouped by age") +
    guides(fill=guide_legend(title="Age group"))
```

![](README_files/figure-gfm/visualization%20for%20first%20dose-1.png)<!-- -->

``` r
vac_Mage %>%
    ggplot(aes(x=date, y=dose1_pct)) + 
    geom_point(mapping = aes(x = date, y = dose1_pct, color = cat)) +
    theme(plot.caption = element_text(hjust=0.5, size=rel(1.2))) +
    scale_x_discrete(breaks=
          c("20201213","20210601","20211201","20220601","20221019")) +
    labs(x = "Date(yyyymmdd)", y = "Percent of people with at least one dose", col="Age group", caption = "Figure 10.2020-2022 Percent of people with at least one dose of males grouped by age") +
    scale_fill_discrete(name = "Age group")
```

![](README_files/figure-gfm/visualization%20for%20first%20dose-2.png)<!-- -->

``` r
#visualization for series dose
#vac_age %>%
#    ggplot(aes(x=date, y=series_pct)) + 
#    geom_point(mapping = aes(x = date, y = dose1_pct, color = cat))  +
#    theme(plot.caption = element_text(hjust=0.5, size=rel(1.2))) +
#    scale_x_discrete(breaks=
#          c("20201213","20210601","20211201","20220601","20221019")) +
#    labs(x = "Date(yyyymmdd)", y = "Percent of people completed a primary series", col="Age group",
#         caption = "Figure 13.2020-2022 Percent of people completed a primary series grouped by age") +
#    guides(fill=guide_legend(title="Age group"))
 
#vac_sex %>%
#    ggplot(aes(x=date, y=series_pct)) + 
#    geom_point(mapping = aes(x = date, y = dose1_pct, color = cat))  +
#    theme(plot.caption = element_text(hjust=0.5, size=rel(1.2))) +
#    scale_x_discrete(breaks=
#          c("20201213","20210601","20211201","20220601","20221019")) +
#    labs(x = "Date(yyyymmdd)", y = "Percent of people completed a primary series", col="Sex group",
#         caption = "Figure 14.2020-2022 Percent of people completed a primary series grouped by sex") +
#    guides(fill=guide_legend(title="Sex group"))

vac_Fage %>%
    ggplot(aes(x=date, y=series_pct)) + 
    geom_point(mapping = aes(x = date, y = dose1_pct, color = cat))  +
    theme(plot.caption = element_text(hjust=0.5, size=rel(1.2))) +
    scale_x_discrete(breaks=
          c("20201213","20210601","20211201","20220601","20221019")) +
    labs(x = "Date(yyyymmdd)", y = "Percent of people completed a primary series", col="Age group",
         caption = "Figure 11.2020-2022 Percent of people completed a primary series of females grouped by age") +
    guides(fill=guide_legend(title="Age group"))
```

![](README_files/figure-gfm/visualization%20for%20series%20dose-1.png)<!-- -->

``` r
vac_Mage %>%
    ggplot(aes(x=date, y=series_pct)) + 
    geom_point(mapping = aes(x = date, y = dose1_pct, color = cat))  +
    theme(plot.caption = element_text(hjust=0.5, size=rel(1.2))) +
    scale_x_discrete(breaks=
          c("20201213","20210601","20211201","20220601","20221019")) +
    labs(x = "Date(yyyymmdd)", y = "Percent of people completed a primary series", col="Age group",
         caption = "Figure 12.2020-2022 Percent of people completed a primary series of males grouped by age") +
    guides(fill=guide_legend(title="Age group"))
```

![](README_files/figure-gfm/visualization%20for%20series%20dose-2.png)<!-- -->

# Conclusion

We could believe that there could be an association between age and the
two vaccination status (at least one dose & completed a primary series)
in California state.For both 2 kinds of vaccination status(take at least
one dose & with completed series) and both sex, the vaccination rate
would be higher with the age level being higher for the same time point,
and the objects with higher age might also take shorter time to have a
relatively high vaccination rate. And the final vaccination rate would
be higher with the age level being higher, but the rate for people with
age less than 5 years old would keep in a low level, even though they
follow the same trend mentioned above.
# PM566Midterm
