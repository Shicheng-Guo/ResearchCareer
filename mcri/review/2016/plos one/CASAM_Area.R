#Read data file
setwd("/home/sguo/Downloads")
File<-read.csv2("DataForAnalyses.csv") #file_nameR

#Import cancer status [1 cancer, 0 control]
Affection<-as.numeric(as.character(File$Affection))


#Import SNP data
SNP<-as.numeric((File$rs10995190))

#> table(SNP)
#SNP
#  1   2   3   4 
# 51  22 274 715 

#Code the SNP fields as 0,1,2,3
SNP=abs(SNP-4)

#> table(SNP)
#SNP
#  0   1   2   3 
# 715 274  22  51



#Import Volpara and log transform it
Volpara<-log(as.numeric(as.character(File$Volpara)))

#Import CASAM-Area, already transformed using SQRT function
CASAM_Area<-as.numeric(as.character(File$Feature28_Region_3))

#Import CASAM_Vol
CASAM_Vol<-as.numeric(as.character(File$CASAM_Vol))

#Import age and bmi
bmi<-as.numeric(as.character(File$bmi))
age<-as.numeric(as.character(File$age))

#Import features (Aquisition parameters and statistical and textural features), features are in columns 17:520
Features<-((File[,15:518]))

#Import other covariates: menopause_status coded as [1 2 3], pre, peri and postmenopausal, respectively.
menopause_status<-(as.numeric(as.character(File$menopause_status)))

# HRT variable is coded as: 
# Never coded as   [0 0] 
# Past  coded as   [1 0] 
# Current coded as [0 1]

HRT<-File[,6:7]

# Parity variable is coded as: 
# Nulliparous coded as [0 0 0 0]
# Parity <=2 and age at first birth <=25 coded as [1 0 0 0] 
# Parity <=2 and age at first birth >25 coded as [0 1 0 0]
# Parity >2 and age at first birth <=25 coded as [0 0 1 0]
# Parity >2 and age at first birth >25 coded as [0 0 0 1]

Parity<-File[,8:11]


# Create a matrix to bin HRT variable
cat<-matrix(0,1062,2)
for (a in 1:1062) {
  cat[a,]<-as.numeric(HRT[a,])
}
HRT<-cat

# Create a matrix to bin Parity variable
cat<-matrix(0,1062,4)
for (a in 1:1062) {
  cat[a,]<-as.numeric(Parity[a,])
}
Parity<-cat




# Regenerating Table 2: fit logistic regression models (SNP association analyses)

# 1- For Volpara
mod <- glm(Volpara[Affection==0] ~ bmi[Affection==0]+age[Affection==0]+menopause_status[Affection==0]+HRT[Affection==0,]+Parity[Affection==0,]+SNP[Affection==0])
summary(mod)
confint(mod)
# SNP = -0.137725 (CI: -0.19076248 -0.084688312), p-value = 4.33e-07

# 2- For CASAM_Area
mod <- glm(CASAM_Area[Affection==0] ~ bmi[Affection==0]+age[Affection==0]+menopause_status[Affection==0]+HRT[Affection==0,]+Parity[Affection==0,]+SNP[Affection==0])

# SNP = -0.254048 (CI: -0.35284594 -0.15525058), p-value = 5.59e-07

# 3- For CASAM_Vol
mod <- glm(CASAM_Vol[Affection==0] ~ bmi[Affection==0]+age[Affection==0]+menopause_status[Affection==0]+HRT[Affection==0,]+Parity[Affection==0,]+SNP[Affection==0])
summary(mod)

# SNP = -0.113397 (CI: -0.15836824 -0.068424963), p-value = 9.15e-07



# Regenerating Table 3: fit logistic regression models 

# Association of Volpara after adjusting additionally for CASAM_Area
mod <- glm(Volpara[Affection==0] ~ bmi[Affection==0]+age[Affection==0]+menopause_status[Affection==0]+HRT[Affection==0,]+Parity[Affection==0,]+CASAM_Area[Affection==0]+SNP[Affection==0])
summary(mod)

# p-value = 0.0356

# Association of Volpara after adjusting additionally for CASAM_Vol
mod <- glm(Volpara[Affection==0] ~ bmi[Affection==0]+age[Affection==0]+menopause_status[Affection==0]+HRT[Affection==0,]+Parity[Affection==0,]+CASAM_Vol[Affection==0]+SNP[Affection==0])
summary(mod)
# p-value = 0.07859

# Association of CASAM_Area after adjusting additionally for Volpara
mod <- glm(CASAM_Area[Affection==0] ~ bmi[Affection==0]+age[Affection==0]+menopause_status[Affection==0]+HRT[Affection==0,]+Parity[Affection==0,]+Volpara[Affection==0]+SNP[Affection==0])
summary(mod)

# p-value = 0.0476

# Association of CASAM_Area after adjusting additionally for CASAM_Vol
mod <- glm(CASAM_Area[Affection==0] ~ bmi[Affection==0]+age[Affection==0]+menopause_status[Affection==0]+HRT[Affection==0,]+Parity[Affection==0,]+CASAM_Vol[Affection==0]+SNP[Affection==0])
summary(mod)
# p-value = 0.146576


# Association of CASAM_Vol after adjusting additionally for Volpara
mod <- glm(CASAM_Vol[Affection==0] ~ bmi[Affection==0]+age[Affection==0]+menopause_status[Affection==0]+HRT[Affection==0,]+Parity[Affection==0,]+Volpara[Affection==0]+SNP[Affection==0])
summary(mod)

# p-value = 0.198

# Association of CASAM_Vol after adjusting additionally for CASAM_Vol
mod <- glm(CASAM_Vol[Affection==0] ~ bmi[Affection==0]+age[Affection==0]+menopause_status[Affection==0]+HRT[Affection==0,]+Parity[Affection==0,]+CASAM_Area[Affection==0]+SNP[Affection==0])
summary(mod)
# p-value =  0.28183





# Regenerating Table 4: fit logistic regression models (case-control analyses)

# (a)partial adjustment (age and BMI)

mod <- glm(Affection ~ bmi+age+Volpara, family="binomial")
summary(mod)
confint(mod)
Volpara = 0.97813 (CI: 0.30024041  1.66027636), p-value = 0.00470

mod <- glm(Affection ~ bmi+age+CASAM_Area, family="binomial")
summary(mod)
confint(mod)
CASAM_Area = 0.48306 (CI: 0.11163300  0.86193488), p-value = 0.01153

mod <- glm(Affection ~ bmi+age+CASAM_Vol, family="binomial")
summary(mod)
confint(mod)
CASAM_Vol = 0.92615 (CI: 0.12436266  1.73047280), p-value = 0.023514


# (b)full adjustment (age, BMI and other covariates)

mod <- glm(Affection~ bmi+age+menopause_status+HRT+Parity+Volpara,family="binomial")
summary(mod)
confint(mod)
Volpara = 0.96124 (CI: 0.239019646  1.70552230), p-value = 0.0100

mod <- glm(Affection~ bmi+age+menopause_status+HRT+Parity+CASAM_Area,family="binomial")
summary(mod)
confint(mod)
CASAM_Area = 0.46698 (CI: 0.071296083  0.87902034), p-value = 0.023252

mod <- glm(Affection~ bmi+age+menopause_status+HRT+Parity+CASAM_Vol,family="binomial")
summary(mod)
confint(mod)
CASAM_Vol = 0.81333 (CI: -0.040684008  1.69169246), p-value = 0.064998


# Regenerating Table 5: fit logistic regression models


# (a) Partial adjustment


# P-values assessing the (residual) association between Volpara and case-control status adjusting additionally for other measures
# adjusting for CASAM_Area
mod<-glm(Affection ~age+bmi+Volpara,family="binomial")
d1<- mod$deviance
mod2<-glm(Affection~age+bmi+Volpara+CASAM_Area,family="binomial")
d2<- mod2$deviance
pvalue<-1-pchisq(d1-d2,1) #0.4790774


# adjusting for CASAM_Vol
mod<-glm(Affection ~age+bmi+Volpara,family="binomial")
d1<- mod$deviance
mod2<-glm(Affection~age+bmi+Volpara+CASAM_Vol,family="binomial")
d2<- mod2$deviance
pvalue<-1-pchisq(d1-d2,1) #0.8231509



.....



# (b) Full adjustment


# P-values assessing the (residual) association between Volpara and case-control status adjusting additionally for other measures
# adjusting for CASAM_Area
mod<-glm(Affection ~age+bmi+menopause_status+HRT+Parity+Volpara,family="binomial")
d1<- mod$deviance
mod2<-glm(Affection~age+bmi+menopause_status+HRT+Parity+Volpara+CASAM_Area,family="binomial")
d2<- mod2$deviance
pvalue<-1-pchisq(d1-d2,1) #0.5285616


# adjusting for CASAM_Vol
mod<-glm(Affection ~age+menopause_status+HRT+Parity+bmi+Volpara,family="binomial")
d1<- mod$deviance
mod2<-glm(Affection~age+bmi+menopause_status+HRT+Parity+Volpara+CASAM_Vol,family="binomial")
d2<- mod2$deviance
pvalue<-1-pchisq(d1-d2,1) #0.5613883


......


######## Looping through the features and generating the QQ plots

# Remember we imported the features at the start of the program

dim(Features)
# [1] 1062  504

# Import MASS library to be able to use boxcox transformation
library(MASS)



################Generate QQplot of figure 2 (a)
pv<-matrix(1,504,1)#Create a vector whose length is the number of features to store the p-values
for (a in 1:504) {
  
  #Read each feature, one at a time
  Feat<-as.numeric(as.character(Features[Affection==0,a]))
  
  #If the feature sum across all images is 0 then assign a pvalue of 1 (unsignificant), which is ignored in the plot
  if (sum(Feat,na.rm=TRUE)==0){
    pv[a]=1
  }else{
    
    #Normalise the feature values
    Feat=(Feat/max(Feat, na.rm=TRUE))
    #Right shift the values so that they are all positive
    #the function "boxcox" complains if any value is negative or 0
    Feat=(Feat-min(Feat, na.rm=TRUE))+0.00000000000001
    
    #Get the transformation's exponential value (boxcox transformation) to make the feature normally distributed
    pl<-boxcox(Feat  ~bmi[Affection==0]+age[Affection==0],lambda = seq(-7, 10, 1/10),plot=FALSE)
    pow<-pl$x[which(pl$y==max(pl$y))]
    
    #Transform the feature
    Feat=Feat^pow
    
    #fit logistic regression a model
    mod <- glm(Feat ~ bmi[Affection==0]+age[Affection==0]+menopause_status[Affection==0]+HRT[Affection==0,]+Parity[Affection==0,]+SNP[Affection==0])
    
    #Pick up the SNP pvalue
    pv[a]<-summary(mod)$coeff[44] }}  


#create a 2x2 grid to display the QQ plots
par(mfrow=c(2,2))
#Remove any unsignificant values
pv<-pv[pv != 1]

#Import Haplin library to be able to use the function pQQ
library(Haplin)

#Generate the QQplot
pQQ(pv, nlabs = 6, conf = 0.95,lim=c(0,8.5), main="(a)", mark = FALSE); abline(0,1)





################Generate QQplot of figure 2 (b)
pv<-matrix(1,504,1)#Create a vector whose length is the number of features to store the p-values
for (a in 1:504) {
  
  #Read each feature, one at a time
  Feat<-as.numeric(as.character(Features[Affection==0,a]))
  
  #If the feature sum across all images is 0 then assign a pvalue of 1 (unsignificant), which is ignored in the plot
  if (sum(Feat,na.rm=TRUE)==0){
    pv[a]=1
  }else{
    
    #Normalise the feature values
    Feat=(Feat/max(Feat, na.rm=TRUE))
    #Right shift the values so that they are all positive
    #the function "boxcox" complains if any value is negative or 0
    Feat=(Feat-min(Feat, na.rm=TRUE))+0.00000000000001
    
    #Get the transformation's exponential value to make the feature normally distributed
    pl<-boxcox(Feat  ~bmi[Affection==0]+age[Affection==0],lambda = seq(-7, 10, 1/10),plot=FALSE)
    pow<-pl$x[which(pl$y==max(pl$y))]
    
    #Transform the feature
    Feat=Feat^pow
    
    #fit logistic regression a model
    mod <- glm(Feat ~ bmi[Affection==0]+age[Affection==0]+menopause_status[Affection==0]+HRT[Affection==0,]+Parity[Affection==0,]+Volpara[Affection==0]+SNP[Affection==0])
    
    #Pick up the SNP pvalue
    pv[a]<-summary(mod)$coeff[48] }}  

#Remove any unsignificant values
pv<-pv[pv != 1]

pQQ(pv, nlabs = 6, conf = 0.95,lim=c(0,4), main="(b)", mark = FALSE); abline(0,1)


################Generate QQplot of figure 2 (c)
pv<-matrix(1,504,1)#Create a vector whose length is the number of features to store the p-values
for (a in 1:504) {
  
  #Read each feature, one at a time
  Feat<-as.numeric(as.character(Features[Affection==0,a]))
  
  #If the feature sum across all images is 0 then assign a pvalue of 1 (unsignificant), which is ignored in the plot
  if (sum(Feat,na.rm=TRUE)==0){
    pv[a]=1
  }else{
    
    #Normalise the feature values
    Feat=(Feat/max(Feat, na.rm=TRUE))
    #Right shift the values so that they are all positive
    #the function "boxcox" complains if any value is negative or 0
    Feat=(Feat-min(Feat, na.rm=TRUE))+0.00000000000001
    
    #Get the transformation's exponential value to make the feature normally distributed
    pl<-boxcox(Feat  ~bmi[Affection==0]+age[Affection==0],lambda = seq(-7, 10, 1/10),plot=FALSE)
    pow<-pl$x[which(pl$y==max(pl$y))]
    
    #Transform the feature
    Feat=Feat^pow
    
    #fit logistic regression a model
    mod <- glm(Feat ~ bmi[Affection==0]+age[Affection==0]+menopause_status[Affection==0]+HRT[Affection==0,]+Parity[Affection==0,]+CASAM_Vol[Affection==0]+SNP[Affection==0])
    
    #Pick up the SNP pvalue
    pv[a]<-summary(mod)$coeff[48] }}  


#Remove any unsignificant values
pv<-pv[pv != 1]

pQQ(pv, nlabs = 6, conf = 0.95,lim=c(0,4), main="(c)", mark = FALSE); abline(0,1)




################Generate QQplot of figure 2 (d)
pv<-matrix(1,504,1)#Create a vector whose length is the number of features to store the p-values
for (a in 1:504) {
  
  #Read each feature, one at a time
  Feat<-as.numeric(as.character(Features[Affection==0,a]))
  
  #If the feature sum across all images is 0 then assign a pvalue of 1 (unsignificant), which is ignored in the plot
  if (sum(Feat,na.rm=TRUE)==0){
    pv[a]=1
  }else{
    
    #Normalise the feature values
    Feat=(Feat/max(Feat, na.rm=TRUE))
    #Right shift the values so that they are all positive
    #the function "boxcox" complains if any value is negative or 0
    Feat=(Feat-min(Feat, na.rm=TRUE))+0.00000000000001
    
    #Get the transformation's exponential value to make the feature normally distributed
    pl<-boxcox(Feat  ~bmi[Affection==0]+age[Affection==0],lambda = seq(-7, 10, 1/10),plot=FALSE)
    pow<-pl$x[which(pl$y==max(pl$y))]
    
    #Transform the feature
    Feat=Feat^pow
    
    #fit logistic regression a model
    mod <- glm(Feat ~ bmi[Affection==0]+age[Affection==0]+menopause_status[Affection==0]+HRT[Affection==0,]+Parity[Affection==0,]+CASAM_Area[Affection==0]+SNP[Affection==0])
    
    #Pick up the SNP pvalue
    pv[a]<-summary(mod)$coeff[48] }}  


#Remove any unsignificant values
pv<-pv[pv != 1]

pQQ(pv, nlabs = 6, conf = 0.95,lim=c(0,4), main="(d)", mark = FALSE); abline(0,1)