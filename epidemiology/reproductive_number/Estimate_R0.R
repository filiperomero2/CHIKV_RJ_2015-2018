#Load packages
library(lubridate)
library(dplyr)
library(R0)
library(ggplot2)


df <- read.csv("number_of_chikv_confirmed_cases_RJ.csv",
               header=T,as.is=T)

dates <- dmy(df$date[which(df$year == 2016)][1:11])
values <- df$value[which(df$year == 2016)][1:11]


# adjust data
epid <- data.frame(cbind(dates,values))
names(epid) <- c("t","incid")
epid$incid <- as.numeric(epid$incid)
epid$t <- as.Date(epid$t)

# Run regression
tmp <- lm((log(incid)) ~ t, data=epid)

# get statistics
Rsquared = summary(tmp)$r.squared
r <- coefficients(tmp)[2]
conf.int = confint(tmp)[2,]

# Generation time distribution for CHIKV: https://www.pnas.org/doi/epdf/10.1073/pnas.1611391113
mean_si <- 14 # days
std_si <- sqrt(41) # days
mGT<-generation.time("gamma", c(mean_si, std_si))
R = as.numeric(R0:::R.from.r(r,mGT))
R.inf = as.numeric(R0:::R.from.r(conf.int[1],mGT))
R.sup = as.numeric(R0:::R.from.r(conf.int[2],mGT))
paste(R,R.inf,R.sup)
gt_14_r0 <- c(14,R,R.inf,R.sup)

# Sensitivity 10 days
mean_si <- 10 # days
std_si <- sqrt(41) # days
mGT<-generation.time("gamma", c(mean_si, std_si))
R = as.numeric(R0:::R.from.r(r,mGT))
R.inf = as.numeric(R0:::R.from.r(conf.int[1],mGT))
R.sup = as.numeric(R0:::R.from.r(conf.int[2],mGT))
paste(R,R.inf,R.sup)
gt_10_r0 <- c(10,R,R.inf,R.sup)

# Sensitivity 20 days
mean_si <- 20 # days
std_si <- sqrt(41) # days
mGT<-generation.time("gamma", c(mean_si, std_si))
R = as.numeric(R0:::R.from.r(r,mGT))
R.inf = as.numeric(R0:::R.from.r(conf.int[1],mGT))
R.sup = as.numeric(R0:::R.from.r(conf.int[2],mGT))
paste(R,R.inf,R.sup)
gt_20_r0 <- c(20,R,R.inf,R.sup)

results <- data.frame(rbind(gt_10_r0,gt_14_r0,gt_20_r0))
names(results) <- c("Generation_time","Mean","Min","Max")

results$Generation_time <- factor(results$Generation_time)

ggplot(data = results,aes(x=Generation_time,y=Mean)) +
  geom_point() + 
  geom_errorbar(aes(ymin=Min, ymax=Max), width=0) +
  theme_bw() +
  xlab("Generation time distribution mean") +
  ylab("Basic reproductive number")
