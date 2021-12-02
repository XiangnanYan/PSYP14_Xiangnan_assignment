library(psych)
library(car) 
library(lmtest) 
library(sandwich) 
library(boot)
library(lmboot) 
library(tidyverse) 
library(lm.beta)
library(gridExtra)
#####part1####
home_sample_1=read.csv("https://tinyurl.com/yxm5rd89")

home_sample_1 = home_sample_1 %>% 	
  mutate(sex = factor(sex))	

home_sample_1 %>% 	
  summary()	

model1<-lm(pain~sex+age,data=home_sample_1)
model2<-lm(pain~sex+age+STAI_trait+pain_cat+cortisol_serum+cortisol_saliva+mindfulness,data=home_sample_1)

# Dealing with outliers	

# Identifying extreme cases	

home_sample_1 %>% 	
  mutate(rownum = row.names(home_sample_1)) %>% 	
  ggplot() +	
  aes(x = sex, y = pain, label = rownum) +	
  geom_label()	
home_sample_1 %>% 	
  mutate(rownum = row.names(home_sample_1)) %>% 	
  ggplot() +	
  aes(x = age, y = pain, label = rownum) +	
  geom_label()	
home_sample_1 %>% 	
  mutate(rownum = row.names(home_sample_1)) %>% 	
  ggplot() +	
  aes(x = STAI_trait, y = pain, label = rownum) +	
  geom_label()	
home_sample_1 %>% 	
  mutate(rownum = row.names(home_sample_1)) %>% 	
  ggplot() +	
  aes(x = pain_cat, y = pain, label = rownum) +	
  geom_label()
home_sample_1 %>% 	
  mutate(rownum = row.names(home_sample_1)) %>% 	
  ggplot() +	
  aes(x = cortisol_serum, y = pain, label = rownum) +	
  geom_label()	
home_sample_1 %>% 	
  mutate(rownum = row.names(home_sample_1)) %>% 	
  ggplot() +	
  aes(x = cortisol_saliva, y = pain, label = rownum) +	
  geom_label()	
home_sample_1 %>% 	
  mutate(rownum = row.names(home_sample_1)) %>% 	
  ggplot() +	
  aes(x = mindfulness, y = pain, label = rownum) +	
  geom_label()	

#residual - levarage plot and Cook's distance
model2 %>% 	
  plot(which = 5)	

model2 %>% 	
  plot(which = 4)	
#88,34,47 seem to be abnormal.

#assumption diagnose
#normality
#QQplot

model2 %>% 	
  plot(which = 2)	

# histogram	

residuals_model2 = enframe(residuals(model2))	
residuals_model2 %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()	

# skew and kurtosis	
describe(residuals(model2))	

#It is obvious that there are some outliers which make the violation of normality in model.For example,skew,kurtosis are outside the range of -1~1.
home_sample_1 %>% 	
  slice(c(34,47,88))	
#The level of pain in this case was recorded using a numerical rating scale from 0 to 10, while the participant of ID_88 reported 55 in pain level.
#The lowest score for STAI was 20, but the participant of ID_34 reported this score of less than 20, the score is only 4.2. 
#There are only a few extreme cases,so I will exclude cases34,47,88 according to Cook's distance and QQplot and other plots I have done before to build a new model.
#Make assumption of normality again to see if these problems can be solved and if the residuals of new model are normally distributed. 


#Delete 34,47,88 rows.
data1<-home_sample_1[-c(34,47,88),]

#build new model using new data.
model1_new<-lm(pain~sex+age,data=data1)
model2_new<-lm(pain~sex+age+STAI_trait+pain_cat+cortisol_serum+cortisol_saliva+mindfulness,data=data1)


#QQplot

model2_new %>% 	
  plot(which = 2)	

# histogram	

residuals_model2_new = enframe(residuals(model2_new))	
residuals_model2_new %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()	

# skew and kurtosis	
describe(residuals(model2_new))	
#The residual of the model basically satisfies normal distribution.skew,kurtosis are during -1~1.


#linerality
model2_new %>% 	
  residualPlots()	
# In this case, even though there is minor curvature visible on the plots, the tests are all non significant, so the linearity assumption seems to hold true for our model.	

# Homoscedasticty
model2_new %>% 	
  plot(which = 3)	

model2_new %>% 	
  ncvTest() # NCV test	

model2_new %>% 	
  bptest() # Breush-Pagan test	

# No multicollinearity

model2_new %>%
  vif()
#vif value of serum and saliva is high

data1 %>% 	
  select(pain,sex, age,STAI_trait,pain_cat,cortisol_serum,cortisol_saliva,mindfulness) %>% 	
  pairs.panels(col = "red", lm = T)	
#The correlation matrix clearly indicates that the correlation of cortisol_serum and cortisol_saliva is very high
#removing highly correlated predictors

model2_new_uncorrelated<-lm(pain~sex+age+STAI_trait+pain_cat+cortisol_serum+mindfulness,data=data1)
model2_new_uncorrelated%>%
  vif()


#check again

#assumption
#normality
#QQplot

model2_new_uncorrelated %>% 	
  plot(which = 2)	

residuals_model2_new_uncorrelated = enframe(residuals(model2_new_uncorrelated))	
residuals_model2_new_uncorrelated %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()	

# skew and kurtosis	
describe(residuals(model2_new_uncorrelated))	
#The residual of the model basically satisfies normal distribution.skew,kurtosis are during -1~1.


#linerality
model2_new_uncorrelated %>% 	
  residualPlots()	
# the tests are all non significant, so the linearity assumption seems to hold true for our model.	

# Homoscedasticty
model2_new_uncorrelated %>% 	
  plot(which = 3)	

model2_new_uncorrelated %>% 	
  ncvTest() # NCV test	

model2_new_uncorrelated %>% 	
  bptest() # Breush-Pagan test	

# No multicollinearity

model2_new_uncorrelated %>%
  vif()
#vif values are less than 3.

###compare
summary(model1_new)
confint(model1_new)
lm.beta(model1_new)

summary(model2_new_uncorrelated)
confint(model2_new_uncorrelated)
lm.beta(model2_new_uncorrelated)

AIC(model1_new)
AIC(model2_new_uncorrelated)

anova(model1_new,model2_new_uncorrelated)



################################################################################
#####part2####
home_sample_1=read.csv("https://tinyurl.com/yxm5rd89")

home_sample_1 = home_sample_1 %>% 	
  mutate(sex = factor(sex))	

data2<-home_sample_1[-c(34,47,88),]

initial_model<-lm(pain~sex+age+STAI_trait+pain_cat+cortisol_serum+mindfulness+weight+IQ+household_income,data=data2)

####assumption###
##normality
#QQplot

initial_model %>% 	
  plot(which = 2)	

#histogram	

residuals_initial_model = enframe(residuals(initial_model))	
residuals_initial_model %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()	

#skew and kurtosis	
describe(residuals(initial_model))


#linerality
initial_model %>% 	
  residualPlots()	

# Homoscedasticty
initial_model %>% 	
  plot(which = 3)	

initial_model %>% 	
  ncvTest() # NCV test	

initial_model %>% 	
  bptest() # Breush-Pagan test	

# No multicollinearity

initial_model %>%
  vif()

####backward model####
backward_model=step(initial_model,direction="backward")

####assumption###
##normality
#QQplot

backward_model %>% 	
  plot(which = 2)	

#histogram	

residuals_backward_model = enframe(residuals(backward_model))	
residuals_backward_model %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()	

#skew and kurtosis	
describe(residuals(backward_model))


#linerality
backward_model %>% 	
  residualPlots()	

# Homoscedasticty
backward_model %>% 	
  plot(which = 3)	

backward_model %>% 	
  ncvTest() # NCV test	

backward_model%>% 	
  bptest() # Breush-Pagan test	

# No multicollinearity

backward_model %>%
  vif()

##description
summary(backward_model)
confint(backward_model)
lm.beta(backward_model)

####theory_based_model####
theory_based_model<-lm(pain~sex+age+STAI_trait+pain_cat+cortisol_serum+mindfulness,data=data2)

#compare three models
AIC(initial_model)
AIC(backward_model)
AIC(theory_based_model)

anova(backward_model,theory_based_model)

##new data

home_sample_2=read.csv("http://tinyurl.com/87v6emky")

#calculate predicted values
pred_back<-predict(backward_model,home_sample_2)
pred_theory<-predict(theory_based_model,home_sample_2)

RSS_back=sum((home_sample_2[,"pain"]-pred_back)^2)
RSS_theory=sum((home_sample_2[,"pain"]-pred_theory)^2)

RSS_back
RSS_theory

##################################################################
######################part3#######################################
library(psych) 	
library(tidyverse) 	
library(cAIC4) 	
library(r2glmm)	
library(lme4) 
library(lmerTest)
library(MuMIn)
stdCoef.merMod <- function(object) {	
  sdy <- sd(getME(object,"y"))	
  sdx <- apply(getME(object,"X"), 2, sd)	
  sc <- fixef(object)*sdx/sdy	
  se.fixef <- coef(summary(object))[,"Std. Error"]	
  se <- se.fixef*sdx/sdy	
  return(data.frame(stdcoef=sc, stdse=se))	
}	



home_sample_3=read.csv("https://tinyurl.com/b385chpu")

home_sample_3=home_sample_3%>%
  mutate(hospital=factor(hospital))

home_sample_3 = home_sample_3 %>% 	
  mutate(sex = factor(sex))	

#build model
#random intercept model
model_rnd_int<-lmer(pain~sex+age+STAI_trait+pain_cat+cortisol_serum+mindfulness+(1|hospital),data=home_sample_3)
summary(model_rnd_int)
confint(model_rnd_int)
stdCoef.merMod(model_rnd_int)	

# marginal R squared with confidence intervals	
r2beta(model_rnd_int, method = "nsj", data = home_sample_3)	

# marginal and conditional R squared values	
r.squaredGLMM(model_rnd_int)	

##new data

home_sample_4=read.csv("https://tinyurl.com/4f8thztv")


#calculate predicted values
pred_model_rnd_int<-predict(model_rnd_int,home_sample_4,allow.new.levels=TRUE)

RSS_model_rnd_int=sum((home_sample_4[,"pain"]-pred_model_rnd_int)^2)
RSS_model_rnd_int

model_rnd_int_mean<-lm(pain~1,data=home_sample_4)
TSS_model_rnd_int=sum((home_sample_4[,"pain"]-predict(model_rnd_int_mean))^2)
TSS_model_rnd_int

R2= 1 - (RSS_model_rnd_int/TSS_model_rnd_int)
R2
#random slope model

#According to previous model,I choose cortisol_serum as the predictor
model_rnd_slope<-lmer(pain~cortisol_serum+(cortisol_serum|hospital),data=home_sample_3)

home_sample_3=home_sample_3 %>% 		
  mutate(	pred_slope = predict(model_rnd_slope))

home_sample_3 %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_serum, group = hospital)+		
  geom_point(aes(color = hospital), size = 4) +		
  geom_line(color='red', aes(y=pred_slope, x=cortisol_serum))+		
  facet_wrap( ~ hospital, ncol = 2)	

summary(model_rnd_slope)
stdCoef.merMod(model_rnd_slope)
r2beta(model_rnd_slope, method = "nsj", data = home_sample_3)	
r.squaredGLMM(model_rnd_slope)	


##finish


