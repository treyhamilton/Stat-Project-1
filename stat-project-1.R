library(MASS)
library(tidyverse)
library(readr)
library(viridis)
options(scipen = 999)

#Reading in dataframe
Data <- read_csv("diamonds4.csv")

#tidying data
#Refactoring categorical variables
Data <- Data %>%
  #note VVS1 diamonds rank higher on the clarity scale than VVS2
  mutate(clarity = clarity%>%fct_relevel(c("FL", "IF", "VVS1","VVS2","VS1","VS2", "SI1", "SI2"))) %>%
  mutate(color = color%>%fct_relevel(c("D", "E", "F","G","H","I", "J"))) %>%
  mutate(cut = cut%>%fct_relevel(c("Good", "Very Good", "Ideal","Astor Ideal")))

# reordering so that price is first
Data <- Data[, c(5, 1, 2, 3, 4)]

pairs(Data, lower.panel = NULL)

Data %>%
  ggplot(aes(x=carat, y = price, color = color)) +
  geom_point(alpha = 0.4) +
  labs(x="Carats", y="Price ($)", title = "Blue Nile Diamond Characteristics Influencing Price by Carat Size") +
  guides(color = guide_legend(title = "Color"), label.position = "right") +
    scale_color_viridis(discrete = TRUE, option = "A") +
  scale_fill_viridis(discrete = TRUE) +
   facet_wrap(~ cut + clarity) 


Data %>%
  ggplot(aes(x=carat, y = price, color = color)) +
  geom_point(alpha = 0.3) +
  labs(x="Carats", y="Price", 
       title = "Blue Nile Diamond Characteristics",
       subtitle = "Comparison between the various colors of diamonds
       against the size of diamond in carats")+
  facet_wrap(~color)

Data %>%
  ggplot(aes(x=color, y = carat, color = color)) +
  geom_point(alpha = 0.3) +
  labs(x="Diamond Colors", y="Carats", 
       title = "Blue Nile Diamond Characteristics",
       subtitle = "Comparison between the various colors of diamonds
       against the size of diamond in carats")

Data %>%
  ggplot(aes(x=cut, y=price)) +
  geom_point() +
  #geom_smooth(method="lm",se=F) +
  labs(x="Cut", y="Price ($)", title = "Blue Nile Diamonds: Cut and Price") 

Data %>%
  ggplot(aes(x=carat, y=price)) +
  geom_point() +
  geom_smooth(method="lm",se=F) +
  labs(x="Carats", y="Price ($)", title = "Blue Nile Diamonds: Factors Influencing Price") 

#Fitting the model
result<-lm(price~ carat, data=Data)
summary(result)

yhat<-result$fitted.values
res<-result$residuals
Data<-data.frame(Data,yhat,res)

ggplot(Data, aes(x=yhat,y=res))+
  geom_point()+
  geom_hline(yintercept=0, color="red")+
  labs(x="Fitted y", y="Residuals", title="Residual Plot")

boxcox(result, lambda = seq(.25,0.35,1/100))

ystar <- (Data$price)^(0.31)
Data<-data.frame(Data,ystar)

Data %>%
  ggplot(aes(x=carat, y=ystar)) +
  geom_point() +
  geom_smooth(method="lm",se=F) +
  labs(x="Carats", y="Price ($)*", title = "Blue Nile Diamonds: Factors Influencing Price") 

result.ystar<- lm(ystar~carat, data=Data)

#storing fitted y residuals
yhat2 <- result.ystar$fitted.values
res2 <- result.ystar$residuals
#add to data frame

Data <-data.frame(Data,yhat2,res2)

#residual plot
Data %>%
  ggplot(aes(x=yhat2,y=res2)) +
  geom_point() +
  geom_hline(yintercept=0,color="red")+
  labs(x="fitted y*", y = "residuals", title="residual plot")

boxcox(result.ystar)
#boxcox(result.ystar, lambda = seq(-1,5.5,1/10))

xstar <- sqrt(Data$carat)
Data<-data.frame(Data,xstar)

Data %>%
  ggplot(aes(x=xstar, y=ystar)) +
  geom_point() +
  geom_smooth(method="lm",se=F) +
  labs(x="Carats*", y="Price ($)*", title = "Blue Nile Diamonds: Factors Influencing Price (SLR Model)") 

result.ystar.xstar<- lm(ystar~xstar, data=Data)

#storing fitted y residuals
yhat3 <- result.ystar.xstar$fitted.values
res3 <- result.ystar.xstar$residuals
#add to data frame

Data <-data.frame(Data,yhat3,res3)

#residual plot
Data %>%
  ggplot(aes(x=yhat3,y=res3)) +
  geom_point() +
  geom_hline(yintercept=0,color="red")+
  labs(x="fitted y", y = "residuals", title="residual plot")

acf(res3,main="ACF Plot of Residuals with xstar and ystar")

qqnorm(res3)
qqline(res3,col="red")

summary(result.ystar.xstar)

