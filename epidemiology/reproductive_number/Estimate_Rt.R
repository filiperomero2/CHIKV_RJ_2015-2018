# Estimate Rt for CHIKV

# Load libraries
library(lubridate)
library(EpiEstim)
library(ggplot2)
library(incidence)
library(cowplot)

# Load incidence data
df <- read.csv("number_of_chikv_confirmed_cases_RJ.csv")
head(df)

# Convert dates
df$date <- dmy(df$date)
head(df)

# Subset the dataframe and keep only data from 2016
df <- df[c("date","value")]
df <- df[which(df$date > dmy(01012016)),]
df <- df[which(df$date < dmy(04102018)),]


## sensitivity options
si_options <- c(10,14,20)
time_options <- seq(from=3, to=8)


## gen time sensitivity
store<-list()
for(i in 1:length(si_options)){
  
mean_si <- si_options[i]  #14 # days
std_si <- sqrt(41) # days
mean_si <- mean_si/7 # weeks
std_si <- std_si/7 # weeks

T <- nrow(df)
t_start <- seq(2, T-7) # starting at 2 as conditional on the past observations
t_end <- t_start + 7   # 8 weeks windows for weekly incidence data


# Estimate Rt with EpiEstim
res <- estimate_R(df$value, 
                  method="parametric_si",
                  config = make_config(list(
                    t_start = t_start,
                    t_end = t_end,
                    mean_si = mean_si, 
                    std_si = std_si))
)
store[[i]]<-res$R
store[[i]]$value<-si_options[i]
store[[i]]$dates <- df$date[seq(from=2,to=length(res$dates)-7,by=1)]

print(i)
}

# plot
new<- rbind(store[[1]],store[[2]],store[[3]])
new$value<-as.factor(new$value)
head(new)

A<- ggplot(new)+geom_line(aes(x=dates, y=`Median(R)`,col=value))+
  geom_ribbon(aes(x=dates, 
                  ymin=`Quantile.0.025(R)`, 
                  ymax=`Quantile.0.975(R)`,fill=value),alpha=0.6)+
  theme_bw()+
  labs(x="Date", y="Rt", 
       col = "Mean \ngeneration \ntime (days)",
       fill = "Mean \ngeneration \ntime (days)")+
  scale_x_date(date_breaks = "year",
               minor_breaks = "month",
               limits = c(min(new$dates), max(new2$dates)))+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
A


## time window sensitivity
store2<-list()
for(i in 1:length(time_options)){
  
  mean_si <- 14 # days
  std_si <- sqrt(41) # days
  mean_si <- mean_si/7 # weeks
  std_si <- std_si/7 # weeks
  
  T <- nrow(df)
  minus<- time_options[i]-1
  t_start <- seq(2, T-minus) # starting at 2 as conditional on the past observations
  t_end <- t_start + minus   
  
  
  # Estimate Rt with EpiEstim
  res <- estimate_R(df$value, 
                    method="parametric_si",
                    config = make_config(list(
                      t_start = t_start,
                      t_end = t_end,
                      mean_si = mean_si, 
                      std_si = std_si))
  )
  store2[[i]]<-res$R
  store2[[i]]$time<-time_options[i]
  store2[[i]]$dates <- df$date[seq(from=2,to=length(res$dates)-minus,by=1)]
  
  print(i)
}

# plot
new2<- rbind(store2[[1]],store2[[2]],store2[[3]],
             store2[[4]],store2[[5]],store2[[6]])
new2$time<-as.factor(new2$time)
B<- ggplot(new2)+geom_line(aes(x=dates, y=`Median(R)`,col=time))+
  geom_ribbon(aes(x=dates, 
                  ymin=`Quantile.0.025(R)`, 
                  ymax=`Quantile.0.975(R)`,fill=time),alpha=0.6)+
  theme_bw()+
  labs(x="Date", y="Rt", 
       col = "Time \nwindow\n(weeks)",fill = "Time \nwindow\n(weeks)")+
  scale_x_date(date_breaks = "year",
               minor_breaks = "month")
B


## combine plots
plot_grid(A,B,labels="AUTO", ncol=1, align="v")


