M=GLM_data_freq[which(GLM_data_freq$CLAIM_COUNT==2),]
M
library(plyr)
Cat_var=list("CAR_capacity"=M[,2],"AGE_Of_VEHICLE"=M[,3],"Fuel_type"=M[,4],"NO_CLAIM_BONUS"=M[,5],"ZERO_DEPRECIATION_OF_PARTS"=M[,6],"INSURED_SUM"=M[,7],"VEHICLE_MAKE"=M[,13],"FUEL_AGE_NILDEP"=M[,15])
lapply(Cat_var,function(x)count(x))


count(GLM_data_freq$CLAIM_COUNT)

S=replace

library("ggplot2")
library("pscl")
library("boot")

install.packages("lmtest")



GLM_data_freq=read.csv(file="F://Project_doc//data file//GLM_data_freq.csv", sep=",",header=T)
GLM_data_severity=read.csv(file="F://Project_doc//data file//GLM_data_severity.csv", sep=",",header=T)


library("pscl")
library("boot")
#hurdle model with combination of variables with poisson dist

ctrl <- hurdle.control(method = "L-BFGS-B")
ctrl$reltol <- NULL
hrd_p2=hurdle(CLAIM_COUNT~CC_BAND_GROUP+VEHICLE_MAKE+FUEL_AGE_NILDEP,data=GLM_data_freq[,c(-1,-17,-18)],dist="poisson",zero.dist = "binomial",link="logit",control=ctrl)

#hurdle poisson model with offset
ctrl <- hurdle.control(method = "L-BFGS-B")
ctrl$reltol <- NULL
hrd_p4=hurdle(CLAIM_COUNT~CC_BAND_GROUP+VEHICLE_MAKE+FUEL_AGE_NILDEP,data=GLM_data_freq[,c(-1,-17,-18)],offset = log(NCB_ADJ_POLICY_COUNT),dist="poisson",zero.dist = "binomial",link="logit",control=ctrl)


#hurdle model with combination of variables with negative binomial
ctrl <- hurdle.control(method = "L-BFGS-B")
ctrl$reltol <- NULL
hrd_nb2=hurdle(CLAIM_COUNT~CC_BAND_GROUP+VEHICLE_MAKE+FUEL_AGE_NILDEP,data=GLM_data_freq[,c(-1,-17,-18)],dist="negbin",zero.dist = "binomial",link="logit",control=ctrl)

#hurdle model with combination of variables
ctrl <- hurdle.control(method = "L-BFGS-B")
ctrl$reltol <- NULL
hrd_nb4=hurdle(CLAIM_COUNT~CC_BAND_GROUP+VEHICLE_MAKE+FUEL_AGE_NILDEP,data=GLM_data_freq[,c(-1,-17,-18)],offset = log(NCB_ADJ_POLICY_COUNT),dist="negbin",zero.dist = "binomial",link="logit",control=ctrl)

t=table(GLM_data_freq$'FUEL_AGE_NILDEP',GLM_data_freq$VEHICLE_MAKE,GLM_data_freq$'CC_BAND_GROUP')
ftable(t)


Cat_var=list("CAR_capacity"=GLM_data_freq[,8],"AGE_floor2"=GLM_data_freq[,9],"Fuel_type"=GLM_data_freq[,10],"NO_CLAIM_BONUS"=GLM_data_freq[,11],"ZERO_DEPRECIATION_OF_PARTS"=GLM_data_freq[,12],"INSURED_SUM"=GLM_data_freq[,14],"VEHICLE_MAKE"=GLM_data_freq[,16])
barplot(table(GLM_data_freq$CC_BAND_GROUP),col=c("blue","red"))
