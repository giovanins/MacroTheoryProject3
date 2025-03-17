df <- read.csv("Macro Hw3 data.csv")

library(tseries)
library(urca)
source(file="intord.R")
library(dynlm)
library(forecast)
library(lmtest)
library(dplyr)
library(zoo)
library(TTR)


tdata <- ts(df, start = c(1991), frequency = 4)


Australia <- subset(tdata, tdata[,1] ==1)
Germany <- subset(tdata, tdata[,1] ==2)
US <- subset(tdata, tdata[,1] ==3)



CPI_Australia <- Australia[,3]
CPI_Germany <- Germany[,3]
CPI_US <- US[,3]

GDP_Australia <- Australia[,4]
GDP_Germany <- Germany[,4]
GDP_US <- US[,4]

IR_Australia <- Australia[,5]
IR_Germany <- Germany[,5]
IR_US <- US[,5]

UNEM_Australia <- Australia[,6]
UNEM_Germany <- Germany[,6]
UNEM_US <- US[,6]

###1

diff_CPI_Australia <- diff(CPI_Australia)
diff_CPI_Germany <- diff(CPI_Germany)
diff_CPI_US <- diff(CPI_US)


df0t=ur.df(diff_CPI_Australia,type="trend",selectlags="BIC")
summary(df0t)

df0t1=ur.df(diff_CPI_Germany,type="trend",selectlags="BIC")
summary(df0t1)

df0t2=ur.df(diff_CPI_US,type="trend",selectlags="BIC")
summary(df0t2)


diff_GDP_Australia <- diff(GDP_Australia)
diff_GDP_Germany <- diff(GDP_Germany)
diff_GDP_US <- diff(GDP_US)


df0t3=ur.df(diff_GDP_Australia,type="trend",selectlags="BIC")
summary(df0t3)

df0t4=ur.df(diff_GDP_Germany,type="trend",selectlags="BIC")
summary(df0t4)

df0t5=ur.df(diff_GDP_US,type="trend",selectlags="BIC")
summary(df0t5)



diff_IR_Australia <- diff(IR_Australia)
diff_IR_Germany <- diff(IR_Germany)
diff_IR_US <- diff(IR_US)


df0t6=ur.df(diff_IR_Australia,type="trend",selectlags="BIC")
summary(df0t6)

df0t7=ur.df(diff_IR_Germany,type="trend",selectlags="BIC")
summary(df0t7)

df0t8=ur.df(diff_IR_US,type="trend",selectlags="BIC")
summary(df0t8)


diff_UNEM_Australia <- diff(UNEM_Australia)
diff_UNEM_Germany <- diff(UNEM_Germany)
diff_UNEM_US <- diff(UNEM_US)


df0t9=ur.df(diff_UNEM_Australia,type="trend",selectlags="BIC")
summary(df0t9)

df0t10=ur.df(diff_UNEM_Germany,type="trend",selectlags="BIC")
summary(df0t10)

df0t11=ur.df(diff_UNEM_US,type="trend",selectlags="BIC")
summary(df0t11)



std_CPI_Australia <- sd(diff_CPI_Australia)
std_CPI_Germany <- sd(diff_CPI_Germany)
std_CPI_US <- sd(diff_CPI_US)


std_GDP_Australia <- sd(diff_GDP_Australia)
std_GDP_Germany <- sd(diff_GDP_Germany)
std_GDP_US <- sd(diff_GDP_US)


std_IR_Australia <- sd(diff_IR_Australia)
std_IR_Germany <- sd(diff_IR_Germany)
std_IR_US <- sd(diff_IR_US)


std_UNEM_Australia <- sd(diff_UNEM_Australia)
std_UNEM_Germany <- sd(diff_UNEM_Germany)
std_UNEM_US <- sd(diff_UNEM_US)


Australia_data <- data.frame(diff_CPI_Australia,diff_GDP_Australia,diff_IR_Australia,diff_UNEM_Australia)

cor_matrix_Australia <- cor(Australia_data, use = "complete.obs")
print(cor_matrix_Australia)


Germany_data <- data.frame(diff_CPI_Germany,diff_GDP_Germany,diff_IR_Germany,diff_UNEM_Germany)

cor_matrix_Germany <- cor(Germany_data, use = "complete.obs")
print(cor_matrix_Germany)


US_data <- data.frame(diff_CPI_US,diff_GDP_US,diff_IR_US,diff_UNEM_US)

cor_matrix_US <- cor(US_data, use = "complete.obs")
print(cor_matrix_US)


####2





####3



