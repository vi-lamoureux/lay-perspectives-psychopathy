
#Reading In Data ----

library(readxl)
diss_data <- read_excel("~/Desktop/Desktop Folders/Dissertation/Data/Dissertation Full Dataset_R_Binary values recoded.xlsx")
View(diss_data)
summary(diss_data)

diss_data_corrtable <- read_excel("~/Desktop/Desktop Folders/Dissertation/Data/Dissertation Full Dataset_R_corrtable.xlsx")
View(diss_data_corrtable)

Diss_MISS_G_P_Data <- 
  read_excel("~/Desktop/Desktop Folders/Dissertation/Data/Dissertation Dataset_R_MISS_SCORES.xlsx")
View(Diss_MISS_G_P_Data)






#Creating correlation tables ----

library(corrtable)


#all data correlation table
corrtable:: save_correlation_matrix(diss_data_corrtable, 
                                    filename = "Diss_full_corrtable",
                                    digits = 2)


##H1 ----

#In terms of stigma, participant perspectives on mental illness will positively 
#correlate with participant perspectives on psychopathy. Specifically, 
#participant scores on each subscale of a measure of stigma toward mental 
#illness will be positively correlated with participant scores on each 
#corresponding subscale of a measure of stigma toward psychopathy.

#H1 correlation table
corrtable::correlation_matrix(Diss_MISS_G_P_Data, 
                              digits = 2,
                              use = "lower", 
                              replace_diagonal = TRUE)

corrtable:: save_correlation_matrix(Diss_MISS_G_P_Data, 
                                    filename = "Diss_MISS_GP_corrtable_2",
                                    digits = 2)

## p < .001 "***"
## p < .01, "** "
## p < .05, "*  "



#H2-H5 ----

#Means, SDs, and t-tests, all conducted in excel


#Regression Analyses (H6-H9) ----


##Logistic Regression (H6-H8) ----
##(regression with categorical/binary variables)
##more good info @https://stats.oarc.ucla.edu/r/dae/logit-regression/

#modelname <- glm(outcome ~ predictor + covariate1 + covariate2 + … + covariateN, 
#                 data=datamatrix, 
#                 family="binomial")

##print result
#summary(modelname)

##print CIs using profiled log-likelihood
#confint(modelname)

##print CIs using standard errors
#confint.default(modelname)

##print odds ratios
#exp(coef(modelname))

##print odds ratios and 95% CI
#exp(cbind(OR = coef(modelname), confint(modelname)))




### H6 ----

#Level of participant support for the insanity defense in general will predict 
#participant likelihood of support for the use of evidence of psychopathy in an 
#insanity defense.


# predictor = IDA_R_Total; outcome = PICP_4
# covariates = POLITICAL_IDEOLOGY; significantly correlated to both variables

# chi-square test first

H6_chisq <- chisq.test(table(diss_data$IDA_R_Total, 
                             diss_data$PICP_4))
H6_chisq

# binary logit reg

H6 <- glm(PICP_4 ~ IDA_R_Total + POLITICAL_IDEOLOGY,
            data = diss_data, 
            family="binomial")
summary(H6)
exp(coef(H6))
exp(confint(H6))
anova(H6, test = "Chisq")
NagelkerkeR2(H6)


### H7 ----

#Level of participant support for the use of capital punishment in general will 
#predict the degree to which participants feel psychopaths charged with capital 
#crimes are deserving of the death penalty.


# predictor = DEATH_QUAL; outcome = PICP_7
# covariates = POLITICAL_IDEOLOGY; significantly correlated to both variables

# chi-square test first

H7_chisq <- chisq.test(table(diss_data$DEATH_QUAL, 
                             diss_data$PICP_7))
H7_chisq

# multinomial logit reg

#Defining the comparison level

diss_data$PICP_7_relevel <- relevel(
  factor(diss_data$PICP_7), ref = "2")

#Fitting the model

H7 <- multinom(PICP_7_relevel ~ DEATH_QUAL + POLITICAL_IDEOLOGY, data = diss_data)
summary(H7)
exp(coef(H7))

z1 <- summary(H7)$coefficients/summary(H7)$standard.errors
z1
p <- (1 - pnorm(abs(z1), 0, 1)) * 2
p


### H8 ----

#H8: Participant beliefs regarding whether psychopathy is amenable to treatment 
#will predict likelihood of participant support for holding psychopaths 
#criminally responsible, likelihood of participant support for the use of 
#evidence of psychopathy in an insanity defense, and the degree to which 
#participants feel psychopaths charged with capital crimes are deserving of the 
#death penalty.

# three different analyses

# PICP_11 and PICP_12 are significantly different (p < .001) via t-test

# predictors = PICP_11 and PICP_12; outcome1 = PICP_5; outcome2 = PICP_4; outcome3 = PICP_7;
# covariates for analysis 1 = POLITICAL_IDEOLOGY; significantly correlated to PICP_5
# covariates for analysis 2 = POLITICAL_IDEOLOGY; significantly correlated to PICP_4
# covariates for analysis 3 = POLITICAL_IDEOLOGY; significantly correlated to PICP_7

# chi-square test first

# analysis 1 chi-square

H8a1_chisq <- chisq.test(table(diss_data$PICP_11, 
                             diss_data$PICP_5))
H8a1_chisq

H8a2_chisq <- chisq.test(table(diss_data$PICP_12, 
                               diss_data$PICP_5))
H8a2_chisq

# analysis 2 chi-square

H8b1_chisq <- chisq.test(table(diss_data$PICP_11, 
                               diss_data$PICP_4))
H8b1_chisq

H8b2_chisq <- chisq.test(table(diss_data$PICP_12, 
                               diss_data$PICP_4))
H8b2_chisq

# analysis 3 chi-square

H8c1_chisq <- chisq.test(table(diss_data$PICP_11, 
                               diss_data$PICP_7))
H8c1_chisq

H8c2_chisq <- chisq.test(table(diss_data$PICP_12, 
                               diss_data$PICP_7))
H8c2_chisq


# analysis 1 binary logit reg

H8a <- glm(PICP_5 ~ PICP_11 + PICP_12 + POLITICAL_IDEOLOGY, 
            data = diss_data, 
            family="binomial")
summary(H8a)
exp(coef(H8a))
exp(confint(H8a))
anova(H8a, test = "Chisq")
NagelkerkeR2(H8a)

H8a1 <- glm(PICP_5 ~ PICP_11 + POLITICAL_IDEOLOGY, 
           data = diss_data, 
           family="binomial")
summary(H8a1)
exp(coef(H8a1))
exp(confint(H8a1))
anova(H8a1, test = "Chisq")
NagelkerkeR2(H8a1)


H8a2 <- glm(PICP_5 ~ PICP_12 + POLITICAL_IDEOLOGY, 
                            data = diss_data, 
                            family="binomial")
summary(H8a2)
exp(coef(H8a2))
exp(confint(H8a2))
anova(H8a2, test = "Chisq")
NagelkerkeR2(H8a2)

# analysis 2 binary logit reg

H8b <- glm(PICP_4 ~ PICP_11 + PICP_12 + POLITICAL_IDEOLOGY, 
           data = diss_data, 
           family="binomial")
summary(H8b)
exp(coef(H8b))
exp(confint(H8b))
anova(H8b, test = "Chisq")
NagelkerkeR2(H8b)

H8b1 <- glm(PICP_4 ~ PICP_11 + POLITICAL_IDEOLOGY, 
           data = diss_data, 
           family="binomial")
summary(H8b1)
exp(coef(H8b1))
exp(confint(H8b1))
anova(H8b1, test = "Chisq")
NagelkerkeR2(H8b1)

H8b2 <- glm(PICP_4 ~ PICP_12 + POLITICAL_IDEOLOGY, 
           data = diss_data, 
           family="binomial")
summary(H8b2)
exp(coef(H8b2))
exp(confint(H8b2))
anova(H8b2, test = "Chisq")
NagelkerkeR2(H8b2)

# analysis 3 multinomial logit reg

#Defining the comparison level (already done for H6)

#diss_data$PICP_7_relevel <- relevel(factor(diss_data$PICP_7), ref = "2")

#Fitting the model

H8c <- multinom(PICP_7_relevel ~ PICP_11 + PICP_12 + POLITICAL_IDEOLOGY, data = diss_data)
summary(H8c)
exp(coef(H8c))

z1 <- summary(H8c)$coefficients/summary(H8c)$standard.errors
z1
p <- (1 - pnorm(abs(z1), 0, 1)) * 2
p

H8c1 <- multinom(PICP_7_relevel ~ PICP_11 + POLITICAL_IDEOLOGY, data = diss_data)
summary(H8c1)
exp(coef(H8c1))

z1 <- summary(H8c1)$coefficients/summary(H8c1)$standard.errors
z1
p <- (1 - pnorm(abs(z1), 0, 1)) * 2
p

H8c2 <- multinom(PICP_7_relevel ~ PICP_12 + POLITICAL_IDEOLOGY, data = diss_data)
summary(H8c2)
exp(coef(H8c2))

z1 <- summary(H8c2)$coefficients/summary(H8c2)$standard.errors
z1
p <- (1 - pnorm(abs(z1), 0, 1)) * 2
p

##Linear AND Logistic Regression (H9) ----

#model <- lm(outcome ~ predictor + covariate, data = datamatrix)
#summary(model)

### H9 ----

#H9: Level of participant agreeableness will predict level of stigma toward 
#mental illness, level of stigma toward psychopathy, likelihood of participant 
#support for holding psychopaths criminally responsible, and the degree to which 
#participants feel psychopaths charged with capital crimes are deserving of the 
#death penalty.

# 4 analyses
# ANALYSIS 1 = LINEAR
# ANALYSIS 2 = LINEAR
# ANALYSIS 3 = BINARY LOGISTIC
# ANALYSIS 4 = MULTINOMIAL LOGISTIC

# predictor = FFMRF_A; outcome1 = MISS_G_Total; outcome2 = MISS_P_Total; outcome3 = PICP_5; outcome4 = PICP_7
# covariates for analysis 1 = POLITICAL_IDEOLOGY; significantly correlated to subscales of MISS_P_Total
# covariates for analysis 2 = POLITICAL_IDEOLOGY; significantly correlated to subscales of MISS_P_Total 
# covariates for analysis 3 = POLITICAL_IDEOLOGY; significantly correlated to PICP_5
# covariates for analysis 4 = POLITICAL_IDEOLOGY; significantly correlated to PICP_7

# ANALYSIS 1: H9a


H9a <- lm(MISS_G_Total ~ FFMRF_A + POLITICAL_IDEOLOGY, data = diss_data)
summary(H9a)

# ANALYSIS 2: H9b


H9b <- lm(MISS_P_Total ~ FFMRF_A + POLITICAL_IDEOLOGY, 
          data = diss_data)
summary(H9b)

library(lm.beta)
lm.beta(H9a)


# ANALYSIS 3: H9c

# analysis 3 chi-square

H9c_chisq <- chisq.test(table(diss_data$FFMRF_A, 
                               diss_data$PICP_5))
H9c_chisq

# analysis 3 binary logit reg

H9c <- glm(PICP_5 ~ FFMRF_A + POLITICAL_IDEOLOGY, 
           data = diss_data, 
           family="binomial")
summary(H9c)
exp(coef(H9c))
exp(confint(H9c))
anova(H9c, test = "Chisq")
NagelkerkeR2(H9c)

# ANALYSIS 4: H9d

# analysis 4 chi-square

H9c_chisq <- chisq.test(table(diss_data$FFMRF_A, 
                              diss_data$PICP_7))
H9c_chisq

# analysis 4 multinomial logit reg

#Defining the comparison level (already done for H6)

#diss_data$PICP_7_relevel <- relevel(factor(diss_data$PICP_7), ref = "2")

#Fitting the model

H9d <- multinom(PICP_7_relevel ~ FFMRF_A + POLITICAL_IDEOLOGY, 
                data = diss_data)
summary(H9d)
exp(coef(H9d))

z1 <- summary(H9d)$coefficients/summary(H9d)$standard.errors
z1
p <- (1 - pnorm(abs(z1), 0, 1)) * 2
p









