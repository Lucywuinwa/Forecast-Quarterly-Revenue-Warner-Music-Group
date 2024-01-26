#load libraries
library(tidyverse)

#set working directory (adjust this for your own computer)
setwd("C:/Users/Lucy Wu/Documents")

#Modeling a linear time series trend using regression

#read dataset into R
warner_music_df <- read.csv("warner_music.csv")
View(warner_music_df)


#create a time series plot showing quarterly net sales
ggplot(data = warner_music_df, mapping = aes(x = Quarter, y = Revenue)) +
  geom_line (group=1) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))
labs(title = "Warner Music Quarterly Net Sales ", 
     x = "Quarter", y = "Revenue")



#Create functions for the accuracy measures 
mae<-function(actual,pred){
  mae <- mean(abs(actual-pred), na.rm=TRUE)
  return (mae)
}

mse<-function(actual,pred){
  mse <- mean((actual-pred)^2, na.rm=TRUE)
  return (mse)
}

rmse<-function(actual,pred){
  rmse <- sqrt(mean((actual-pred)^2, na.rm=TRUE))
  return (rmse)
}  

mape<-function(actual,pred){
  mape <- mean(abs((actual - pred)/actual), na.rm=TRUE)*100
  return (mape)
}



#Add a column of consecutive numbers corresponding with each year
warner_music_df$Time <- 1:nrow(warner_music_df) 

#Create dummy variables corresponding to each quarter 
warner_music_df$Q1 <- ifelse(grepl("Q1",warner_music_df$Quarter), 1, 0)
warner_music_df$Q2 <- ifelse(grepl("Q2",warner_music_df$Quarter), 1, 0)
warner_music_df$Q3 <- ifelse(grepl("Q3",warner_music_df$Quarter), 1, 0)
warner_music_df$Q4 <- ifelse(grepl("Q4",warner_music_df$Quarter), 1, 0)

#Use regression with the time variable to generate a regression 
#equation for forecasting
wmreg<-lm(Revenue ~ Time, data = warner_music_df)
summary(wmreg)

#Create a vector of predicted values generated from the 
#regression above
wm_pred = predict(wmreg)

#calculate accuracy measures with vector of actual values and vector
#of predicted values as inputs
mae(warner_music_df$Revenue, wm_pred)
mse(warner_music_df$Revenue, wm_pred)
rmse(warner_music_df$Revenue, wm_pred)
mape(warner_music_df$Revenue, wm_pred)

#Use multiple regression with the time and quarters variables to generate 
#a regression equation for forecasting
wmreg2<-lm(Revenue ~ Time + Q2 + Q3 + Q4, data = warner_music_df)
summary(wmreg2)

#Create a vector of predicted values generated from the multiple 
#regression above
wm_pred2 = predict(wmreg2)

#calculate accuracy measures with vector of actual values and vector
#of predicted values as inputs
mae(warner_music_df$Revenue, wm_pred2)
mse(warner_music_df$Revenue, wm_pred2)
rmse(warner_music_df$Revenue, wm_pred2)
mape(warner_music_df$Revenue, wm_pred2)

#Predict Warner Music Net Sales for 2022 Q1, Q2, Q3, Q4

#Create an object with the time periods to use for the prediction
new <- data.frame(Time = c(26, 27, 28, 29), Q2 = c(0,1,0,0), Q3 = c(0,0,1,0), 
                  Q4 = c(0,0,0,1)) 
predict(wmreg2, newdata = new)



