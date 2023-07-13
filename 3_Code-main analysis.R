## LONELINESS AND COGNITIVE FUNCTION IN OLDER ADULTS: LONGITUDINAL ANALYSIS IN 15 COUNTRIES- Supplemental material ##

#This file is a supplement to the paper "Loneliness and cognitive function in older adults: longitudinal analysis in 15 countries" by Cachón-Alonso et al. (2023). It contains the code used to conduct the main analysis included in the paper in following order:
#1. Data preparation and sample selection
#2. Modelling steps: unconstrained/grand means constrained/constrained model
#3. Multiple group RI-CLPM
#4. RI-CLPM adjusted for confounders
#Our statistical analysis were conducted with semTools (Jorgensen et al., 2022) and lavaan (Rosseel, 2012) using R (version 4.1.2) and following the coding steps proposed by Mulder and Hamaker (2021)  https://jeroendmulder.github.io/RI-CLPM/lavaan.html

#Data comes from waves 5 to 7/8 of the Survey of Health, Ageing and Retirement in Europe (SHARE). In our study, we included two measures of loneliness, four measures of cognitive function, and data on seven confounders. The final dataset included 55,662 adults (3 repeated measures). 
#Here, we show as an example the code for the models exploring the associations between verbal fluency and the three-item measure of loneliness. All other models followed the same steps.

## 1.Data preparation and sample selection
#####    Data preparation   #####
library(tidyverse)
load("/Users/lacachon/Desktop/R code and data/all_data1.RData") 

redo <- all_data1 %>% select(mergeid, id=newid, aika, country, age=age1, sex, mst=msd1, partner=partner1, education=edu1, 
                             diabetes= diab1, bp=bp1, stroke= stroke1,chd=chd1, weight=weightkg1, height= height1,
                             cancer=cancer1, resp=resp1, disability= disab1, nocompanion, leftout, isolated, lonely1=lonely, 
                             depsum, disease_sum, int_year=int_year1, yrbirth=yrbirth1,imrecall=k008, derecall= k016, 
                             fluency=k010, k108, k109, k110, k111, k112) %>% rowwise %>% 
  mutate(lonely3=nocompanion + leftout + isolated) %>%   
  mutate(fluency=replace(fluency, fluency> 45, 45)) %>% 
  ungroup

#creating the numeracy variable
redo$numeracy<-redo$k108+redo$k109+redo$k110+redo$k111+redo$k112 
redo <-select(redo, !starts_with("k1"))
summary(redo)

#adding employment status at baseline (additional analysis)
library(haven)
new<-read_dta("/Users/lacachon/Desktop/R code and data/laura_retirement.dta") #contains employment status variable
new<- new %>% select(mergeid, aika, jobst=ep005_) #job situation at baseline (original variable in SHARE = ep005_)
summary(new$jobst)
new$jobst<-as.numeric(new$jobst)

new$jobst<-recode(new$jobst, `-2` = 99, `-1` = 99, `1` = 1, `2` = 0, `3` = 0, `4` = 1, `5` = 0, `97` = 0) #job=1 if retired or permanently sick/disabled
new<-new %>% mutate(job=ifelse(jobst==99, NA, jobst))
new<-select(new, -jobst)

data <-left_join(redo, new, by =c("mergeid", "aika"))
summary(data)

#waves from 5 onwards & selection criteria
data<-data %>% filter(aika > 4)

### Sample selection procedure (wide format to have one row per participant) ###
wide0 <- data %>% select(id, time=aika, age, job, country, yrbirth, sex, mst, partner, education, depsum, disease_sum, lonely3, lonely1, fluency, imrecall, derecall, numeracy)
wide0 <- wide0 %>% pivot_wider(id_cols= c("id", "sex", "country", "yrbirth"), names_from = time, values_from = c("job", "age", "education", "mst", "partner", "depsum", "disease_sum", "lonely3", "lonely1", "fluency", "imrecall", "derecall", "numeracy")) 

#FIRST: country that participated in SHARE in wave 5
wide1<-filter(wide0, country=="Austria"| country =="Germany" | country =="Sweden"| country =="Netherlands"| country =="Spain"| country =="Italy"| country =="France"| country =="Denmark"| country =="Switzerland"| country =="Belgium" | country =="Israel"| country =="Czech Republic" | country =="Luxembourg"| country =="Slovenia"| country =="Estonia")

#SECOND: at least 50 years old at baseline
wide2<- wide1 %>% filter(yrbirth<1964) 

summary(wide2)

# select those with complete data for wave5 (countries with 0 obs in W5 already dropped out)
data1<- data %>% filter(yrbirth<1964) #first inclusion criteria: at least 50y at baseline
summary(data1)
wave5<-data1 %>% filter(aika == 5)
summary(wave5$age)
summary(wave5$age)
summary(wave5)
CFLonW5<-wave5 %>% select(id, aika, job, fluency, numeracy, imrecall, derecall, lonely1, lonely3, education, age, sex, partner, depsum, disease_sum, country, yrbirth) %>% na.omit()
summary(CFLonW5)

#Adding data from the next waves for people with complete data for wave 5
ids<-CFLonW5 %>% select(id)
fi_data<-left_join(ids, data1, by="id")
summary(fi_data)

wide_fi <- fi_data %>% select(id, time=aika, age, job, country, yrbirth, sex, mst, partner, education, depsum, disease_sum, lonely3, lonely1, fluency, imrecall, derecall, numeracy)
wide_fi <- wide_fi %>% pivot_wider(id_cols= c("id", "sex", "country", "yrbirth"), names_from = time, values_from = c("job", "age", "education", "mst", "partner", "depsum", "disease_sum", "lonely3", "lonely1", "fluency", "imrecall", "derecall", "numeracy")) 
summary(wide_fi)


# Replacing missing values in Wave 7 with data from Wave 8 
t78<-fi_data %>% filter(aika > 6)

t78 <- t78 %>% select(id, time=aika, age, job, country, yrbirth, sex, mst, partner, education, depsum, disease_sum, lonely3, lonely1, fluency, imrecall, derecall, numeracy)
wide_t78 <- t78 %>% pivot_wider(id_cols= c("id", "sex", "country", "yrbirth"), names_from = time, values_from = c("job", "age", "education", "mst", "partner", "depsum", "disease_sum", "lonely3", "lonely1", "fluency", "imrecall", "derecall", "numeracy")) 
summary(wide_t78)

# merging w7 and w8
merge_7.8<-function(x,y){x<-ifelse(is.na(x), y, x)}
wide_fi$mst_7<-merge_7.8(wide_fi$mst_7, wide_fi$mst_8)
wide_fi$partner_7<-merge_7.8(wide_fi$partner_7, wide_fi$partner_8)
wide_fi$disease_sum_7<-merge_7.8(wide_fi$disease_sum_7, wide_fi$disease_sum_8)
wide_fi$depsum_7<-merge_7.8(wide_fi$depsum_7, wide_fi$depsum_8)
wide_fi$job_7<-merge_7.8(wide_fi$job_7, wide_fi$job_8) 

wide_fi$lonely3_7<-merge_7.8(wide_fi$lonely3_7, wide_fi$lonely3_8)
wide_fi$lonely1_7<-merge_7.8(wide_fi$lonely1_7, wide_fi$lonely1_8)
wide_fi$fluency_7<-merge_7.8(wide_fi$fluency_7, wide_fi$fluency_8)
wide_fi$imrecall_7<-merge_7.8(wide_fi$imrecall_7, wide_fi$imrecall_8)
wide_fi$derecall_7<-merge_7.8(wide_fi$derecall_7, wide_fi$derecall_8)
wide_fi$numeracy_7<-merge_7.8(wide_fi$numeracy_7, wide_fi$numeracy_8)
wide_fi$education_7<-merge_7.8(wide_fi$education_7, wide_fi$education_8)

final.wide<-select(wide_fi, !ends_with("_8"))

## 2.Modelling steps: unconstrained vs. grand means constrained; unconstrained vs. constrained model ##

#Example dataset:
lon3.flu <- final.wide %>% select(x1 = lonely3_5, x2 = lonely3_6, x3 = lonely3_7,
                                  y1 = fluency_5, y2 = fluency_6, y3 = fluency_7) %>%
  select(matches('[xy][1-3]'))

#Define the unconstrained RI-CLPM:
unconstrained<- '
  # Create between components (random intercepts)
  RIx =~ 1*x1 + 1*x2 + 1*x3
  RIy =~ 1*y1 + 1*y2 + 1*y3 
  
  # Create within-person centered variables
  wx1 =~ 1*x1
  wx2 =~ 1*x2
  wx3 =~ 1*x3 

  wy1 =~ 1*y1
  wy2 =~ 1*y2
  wy3 =~ 1*y3
 
 # Estimate the lagged effects between the within-person centered variables.
  wx2 + wy2 ~ wx1 + wy1
  wx3 + wy3 ~ wx2 + wy2

 # Estimate the covariance between the within-person centered variables at the first wave. 
  wx1 ~~ wy1 # Covariance
  
  # Estimate the covariances between the residuals of the within-person centered variables.
  wx2 ~~ wy2
  wx3 ~~ wy3

  # Estimate the variance and covariance of the random intercepts. 
  RIx ~~ RIx
  RIy ~~ RIy
  RIx ~~ RIy

  # Estimate the (residual) variance of the within-person centered variables.
  wx1 ~~ wx1 # Variances
  wy1 ~~ wy1 
  wx2 ~~ wx2 # Residual variances
  wy2 ~~ wy2 
  wx3 ~~ wx3 
  wy3 ~~ wy3 
'

#Apply the unconstrained model to the data
unconst.lon3flu<- lavaan(unconstrained, data = lon3.flu, meanstructure = T, int.ov.free = T, missing="FIML", estimator="MLR") 
summary(unconst.lon3flu, standardized = T, fit.measures = T)


#Define the RI-CLPM with only grand means constrained:
onlymeansconstrained <- '
  # Create between components (random intercepts)
  RIx =~ 1*x1 + 1*x2 + 1*x3
  RIy =~ 1*y1 + 1*y2 + 1*y3 
  
  # Create within-person centered variables
  wx1 =~ 1*x1
  wx2 =~ 1*x2
  wx3 =~ 1*x3 

  wy1 =~ 1*y1
  wy2 =~ 1*y2
  wy3 =~ 1*y3
 
# Estimate the lagged effects between the within-person centered variables.
  wx2 + wy2 ~ wx1 + wy1
  wx3 + wy3 ~ wx2 + wy2

 # Estimate the covariance between the within-person centered variables at the first wave. 
  wx1 ~~ wy1 # Covariance
  
  # Estimate the covariances between the residuals of the within-person centered variables.
  wx2 ~~ wy2
  wx3 ~~ wy3

  # Estimate the variance and covariance of the random intercepts. 
  RIx ~~ RIx
  RIy ~~ RIy
  RIx ~~ RIy

  # Estimate the (residual) variance of the within-person centered variables.
  wx1 ~~ wx1 # Variances
  wy1 ~~ wy1 
  wx2 ~~ wx2 # Residual variances
  wy2 ~~ wy2 
  wx3 ~~ wx3 
  wy3 ~~ wy3 
 
 # Constrain the grand means over time. 
x1 + x2 + x3  ~ mx*1
y1 + y2 + y3  ~ my*1

'
#Apply the RI-CLPM with only means constrained to our data, and compare its fit with the RI-CLPM unconstrained 
means.lon3flu<- lavaan(onlymeansconstrained, data = lon3.flu, meanstructure = T, int.ov.free = T, missing="FIML", estimator="MLR") 
summary(means.lon3flu, standardized = T, fit.measures = T)
anova(unconst.lon3flu, means.lon3flu)

#Define the RI-CLPM with constrained grand means and lagged coefficients:
constrained<- '# Create between components (random intercepts)
RIx =~ 1*x1 + 1*x2 + 1*x3 
RIy =~ 1*y1 + 1*y2 + 1*y3 

# Create within-person centered variables
wx1 =~ 1*x1
wx2 =~ 1*x2
wx3 =~ 1*x3 
wy1 =~ 1*y1
wy2 =~ 1*y2
wy3 =~ 1*y3

# Estimate the lagged effects between the within-person centered variables (constrained).
wx2 ~ a*wx1 + b*wy1 
wy2 ~ c*wx1 + d*wy1
wx3 ~ a*wx2 + b*wy2
wy3 ~ c*wx2 + d*wy2

# Estimate the covariances between the residuals of the within-person centered variables (constrained).
wx2 ~~ cov*wy2
wx3 ~~ cov*wy3

# Estimate the covariance between the within-person centered variables at the first wave. 
wx1 ~~ wy1 # Covariance

# Estimate the variance and covariance of the random intercepts. 
RIx ~~ RIx
RIy ~~ RIy
RIx ~~ RIy

# Estimate the (residual) variance of the within-person centered variables (constrained).
wx1 ~~ wx1 # Variance
wy1 ~~ wy1 
wx2 ~~ vx*wx2 # Residual variance
wy2 ~~ vy*wy2 
wx3 ~~ vx*wx3 
wy3 ~~ vy*wy3 

# Constrain the grand means over time. 
x1 + x2 + x3  ~ mx*1
y1 + y2 + y3  ~ my*1
'
#Apply the RI-CLPM fully constrained and compare its fit with the RI-CLPM unconstrained 
# loneliness3+ fluency
const.lon3flu <- lavaan(constrained, data = lon3.flu, meanstructure = T, int.ov.free = T, missing="FIML", estimator="MLR") 
summary(const.lon3flu, standardized = T, fit.measures = T)
anova(const.lon3flu, uncont.lon3flu)


## 2. Multiple group RI-CLPM (Extension 2/constrained, Mulder and Hamaker, 2021) ##
#New variable for year of birth before/after 1949 at baseline (SHARE-wave5)
final.wide<- mutate(final.wide, G = yrbirth)
wideG<- final.wide %>% mutate(G=replace(G, G<1949, 1))
wide.G<- wideG %>% mutate(G=replace(G, G>1948, 0))

RICLPM.ext2unconst <- '
  RIx =~ 1*x1 + 1*x2 + 1*x3 
  RIy =~ 1*y1 + 1*y2 + 1*y3 
  
  wx1 =~ 1*x1
  wx2 =~ 1*x2
  wx3 =~ 1*x3 

  wy1 =~ 1*y1
  wy2 =~ 1*y2
  wy3 =~ 1*y3

  wx2 + wy2 ~ wx1 + wy1
  wx3 + wy3 ~ wx2 + wy2
 
  wx1 ~~ wy1 
  
  wx2 ~~ wy2
  wx3 ~~ wy3
  
  RIx ~~ RIx
  RIy ~~ RIy
  RIx ~~ RIy
  
  wx1 ~~ wx1 
  wy1 ~~ wy1 
  wx2 ~~ wx2
  wy2 ~~ wy2 
  wx3 ~~ wx3 
  wy3 ~~ wy3 

'

RICLPM.ext2const <- '
  RIx =~ 1*x1 + 1*x2 + 1*x3 
  RIy =~ 1*y1 + 1*y2 + 1*y3 
  
  wx1 =~ 1*x1
  wx2 =~ 1*x2
  wx3 =~ 1*x3 
 
  wy1 =~ 1*y1
  wy2 =~ 1*y2
  wy3 =~ 1*y3
  
  wx2 ~ c(a1, a1)*wx1 + c(b1, b1)*wy1
  wy2 ~ c(c1, c1)*wx1 + c(d1, d1)*wy1
  wx3 ~ c(a2, a2)*wx2 + c(b2, b2)*wy2
  wy3 ~ c(c2, c2)*wx2 + c(d2, d2)*wy2
  
  wx1 ~~ wy1 
  
  wx2 ~~ wy2
  wx3 ~~ wy3
 
  RIx ~~ RIx
  RIy ~~ RIy
  RIx ~~ RIy
  
  wx1 ~~ wx1 
  wy1 ~~ wy1 
  wx2 ~~ wx2 
  wy2 ~~ wy2 
  wx3 ~~ wx3 
  wy3 ~~ wy3 

'

#Example of dataset for the multigroup version of the RI-CLPM 
#lonely3+fluency
lon3.fluG <- wide.G %>% rename(x1 = lonely3_5, x2 = lonely3_6, x3 = lonely3_7,
                               y1 = fluency_5, y2 = fluency_6, y3 = fluency_7, G=G) %>%
  select(matches('[xy][1-3]'), G)


#Comparing a unconstrained and a constrained version of the RI-CLPM
RICLPM.GU.lon3flu <- lavaan(RICLPM.ext2unconst, data = lon3.fluG, group = "G", meanstructure = T, int.ov.free = T, missing="FIML", estimator="MLR")
RICLPM.GC.lon3flu <- lavaan(RICLPM.ext2const, data = lon3.fluG, group = "G", meanstructure = T, int.ov.free = T, missing="FIML", estimator="MLR")

summary(RICLPM.GU.lon3flu, standardized = T, fit.measures = T, ci=T)
summary(RICLPM.GC.lon3flu, standardized = T, fit.measures = T, ci=T)
anova(RICLPM.GU.lon3flu, RICLPM.GC.lon3flu) 


#Imposing the constrains was not tenable in 4 out of 8 models, so we conducted age-stratified analyses.
#We created two databases stratifying by year of birth, and applied the constrained RI-CLPM to each of them (2 age groups x 2 loneliness measures x 4 cognitive function measures).
wide_new.young<-wide_new %>% filter(yrbirth>1948) 
wide_new.old<-wide_new %>% filter(yrbirth<1949)

#loneliness3+ fluency (younger group)
lon3.y.flu <- wide_new.young %>% select(x1 = lonely3_5, x2 = lonely3_6, x3 = lonely3_7,
                                        y1 = fluency_5, y2 = fluency_6, y3 = fluency_7) %>%
  select(matches('[xy][1-3]'))

RICLPM5.y.lon3flu <- lavaan(constrained, data = lon3.y.flu, meanstructure = T, int.ov.free = T, missing="FIML", estimator="MLR") 
summary(RICLPM5.y.lon3flu, standardized = T, fit.measures = T, ci=T)

#loneliness3+ fluency (older group)
lon3.o.flu <- wide_new.old %>% select (x1 = lonely3_5, x2 = lonely3_6, x3 = lonely3_7,
                                       y1 = fluency_5, y2 = fluency_6, y3 = fluency_7) %>%
  select(matches('[xy][1-3]'))

RICLPM5.o.lon3flu <- lavaan(constrained, data = lon3.o.flu, meanstructure = T, int.ov.free = T, missing="FIML", estimator="MLR") 
summary(RICLPM5.o.lon3flu, standardized = T, fit.measures = T, ci=T)


#Additional sensitivity analysis: analogous multiple group RI-CLPM comparisons were conducted stratifying by employment status at baseline. 
wide.AG<- mutate(wide.AG, G = job_5) #G=1 retired/permanently sick; G=0 employed/unemployed/homemaker/others

## 3.RI-CLPM adjusted for confounders ##

#Grouping countries by geographical area and creating dummy variables 
x<-wide_new %>% mutate(country = recode(country, Austria="W", Germany="W", Sweden="N", Netherlands= "W", Denmark="N", France="W", Switzerland="W", Belgium="W", Luxembourg="W", Spain="S", Italy="S", "Czech Republic"="E", Slovenia="E", Estonia="E"))
x<-filter(x, country == "W" | country == "N" | country == "S" | country == "E" | country == "Israel")

library(fastDummies)
x<-dummy_cols(x, select_columns = "country")

z10wide_new.young<-x %>% filter(yrbirth>1948)
z10wide_new.old<-x %>% filter(yrbirth<1949)

#RI-CLPM adjusted for confounders (included as time-invariant predictors of the random intercepts. Extension 1/ Mulder and Hamaker, 2021)
RICLPM.z10<-
  '# Create between components (random intercepts)
RIx =~ 1*x1 + 1*x2 + 1*x3 
RIy =~ 1*y1 + 1*y2 + 1*y3 

# Create within-person centered variables
wx1 =~ 1*x1
wx2 =~ 1*x2
wx3 =~ 1*x3 
wy1 =~ 1*y1
wy2 =~ 1*y2
wy3 =~ 1*y3

# Regression of random intercepts on z1. 
RIx + RIy ~ z1 # Constrained over time. 
RIx + RIy ~ z2
RIx + RIy ~ z3
RIx + RIy ~ z4
RIx + RIy ~ z5
RIx + RIy ~ z6
RIx + RIy ~ z7
RIx + RIy ~ z8
RIx + RIy ~ z9
RIx + RIy ~z10

  # Estimate the lagged effects between the within-person centered variables (constrained).
wx2 ~ a*wx1 + b*wy1 
wy2 ~ c*wx1 + d*wy1
wx3 ~ a*wx2 + b*wy2
wy3 ~ c*wx2 + d*wy2

# Estimate the covariances between the residuals of the within-person centered variables (constrained).
wx2 ~~ cov*wy2
wx3 ~~ cov*wy3

# Estimate the covariance between the within-person centered variables at the first wave. 
wx1 ~~ wy1 # Covariance

# Estimate the variance and covariance of the random intercepts. 
RIx ~~ RIx
RIy ~~ RIy
RIx ~~ RIy

# Estimate the (residual) variance of the within-person centered variables (constrained).
wx1 ~~ wx1 # Variance
wy1 ~~ wy1 
wx2 ~~ vx*wx2 # Residual variance
wy2 ~~ vy*wy2 
wx3 ~~ vx*wx3 
wy3 ~~ vy*wy3 

# Constrain the grand means over time. 
x1 + x2 + x3  ~ mx*1
y1 + y2 + y3  ~ my*1
'

#Dataset: loneliness3+ fluency (young group as example)
z10.y.lon3.flu <- z10wide_new.young %>% select(x1 = lonely3_5, x2 = lonely3_6, x3 = lonely3_7,
                                               y1 = fluency_5, y2 = fluency_6, y3 = fluency_7, z1 = education_5, z2= sex, z3= age_5, z4= disease_sum_5, z5=depsum_5, z6=partner_5, z7=country_W, z8=country_N, z9=country_E,z10=country_S)

#Full Information Maximum Likelihood (main analysis)
z10.y.lon3flu<- lavaan(RICLPM.z10, data = z10.y.lon3.flu, meanstructure = T, int.ov.free = T, missing="FIML", estimator="MLR") #Method: MLR
summary(z10.y.lon3flu, standardized = T, fit.measures = T, ci=T)

#Multiple Imputation (sensitivity analysis)
set.seed(3)
z10.y.lon3flu_mi <- runMI(RICLPM.z10, data=z10.y.lon3.flu, m=5, miPackage = "mice", fun="lavaan", meanstructure=T) 
summary(z10.y.lon3flu_mi, standardized = T, fit.measures = T, ci=T)
