library(lubridate)
library(ggplot2)
library(dplyr)
library(cowplot)

df <- read.csv("RJ_state_weather_data_daily_2014_2018.csv",
               header=T,as.is = T)
head(df)

df$Date <- as.Date(df$Date)
is.Date(df$Date)

p1 <- ggplot(data = df,aes(x=Date,y=t2m)) + 
  geom_line(color="#d73027") +
  theme_light() +
  ylab(label = "Temperature - 2m (°C)") +
  scale_x_date(date_labels = "%Y",date_breaks = "1 year")
#p1

p2 <- ggplot(data = df,aes(x=Date,y=tp)) + 
  geom_line(color="#abd9e9") +
  theme_light()+
  ylab(label = "Total precipitation (m)") +
  scale_x_date(date_labels = "%Y",date_breaks = "1 year")

p3 <- ggplot(data = df,aes(x=Date,y=RH)) + 
  geom_line(color="#313695") +
  theme_light()+
  ylab(label = "Relative humidity (%)") +
  scale_x_date(date_labels = "%Y",date_breaks = "1 year")

p_merged<-plot_grid(p1,p2,p3,
                    labels=c("A","B","C"),
                    ncol=1)
p_merged