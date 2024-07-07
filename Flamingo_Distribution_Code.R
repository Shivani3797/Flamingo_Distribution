#GLM CODE R STUDIO
library(dismo)
library(ggplot2)
library(dplyr)
setwd("C://Users//Admin//Desktop//sdm_data")
flamingo_data=read.csv("flamingo_occur_final.csv")
env_data_current=stack("env_current.grd")
env_data_forecast=stack("env_forecast.grd")
plot(env_data_current)

flamingo_data_locations=select(flamingo_data,Lon,Lat)
flamingo_env=extract(env_data_current,flamingo_data_locations)
flamingo_data=cbind(flamingo_data,flamingo_env)

ggplot(flamingo_data,mapping=aes(x=tmin,y=precip,color=Present))+geom_point()

logistic_regr_model=glm(Present~ tmin+precip, family=binomial(link="logit"),
                        data=flamingo_data)
summary(logistic_regr_model)

presence_data=filter(flamingo_data,Present==1)
absence_data= filter(flamingo_data,Present==0)

evaluate=evaluate(presence_data,absence_data,logistic_regr_model)
plot(evaluate,"ROC")
predictions=predict(env_data_current,logistic_regr_model,
                    type="response")
plot (predictions,ext=extent(71,88,14,25))
points(presence_data[c("Lon","Lat")],pch="+",cex=2)
plot(predictions>0.5,ext=extent(71,88,14,25))
points(presence_data[c("Lon","Lat")],pch="+",cex=2)
tr=threshold(evaluate,stat='prevalence')
plot(predictions>tr,ext=extent(71,88,14,25))
points(presence_data[c("Lon","Lat")],pch="+",cex=2)
forecasts=predict(env_data_forecast,logistic_regr_model,type="response")
plot(forecasts,ext=extent(71,88,14,25))
plot(forecasts>tr,ext=extent(71,88,14,25))
plot(forecasts-predictions,ext=extent(71,88,14,25))