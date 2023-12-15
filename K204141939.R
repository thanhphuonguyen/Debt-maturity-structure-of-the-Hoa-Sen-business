###Nguy???n Thanh Phuong Uyên - K204141939

###################################################      Create Datasets      ############################################################ 

# Ignore the warning messages
options(warn=-1)

# Import library
library(tidyverse)
library(readxl)
library(readr)
library(ggplot2)   # for plotting
library(forcats)   # for handling factors
library(scales)    # for axis scale formatting
library(xts)       # xts conclude index and core data (DataFrame have a timeseries columns)
library(tseries)   # adf.test
library(lmtest)    # coeftest
library(stats)     # Box.test
library(forecast)
library(PerformanceAnalytics)

# Import data from excel file.
df = read_excel("C:\\Users\\Admin\\Documents\\K204141939.xlsx")
View(df)

# Checking Na values.
sum(is.na(df))

covid_period = c(df$Time[which(df$Time == 'Q1/2020'):which(df$Time == 'Q4/2021')])

# Calculate control varibles

data = df %>% transmute(
  Quarter = Time,
  Debt_maturity_structure = round(Long_term_debt / Total_debt, 6),
  Leverage = round(Total_debt / Total_Assets, 6),
  Liquidity = round(Short_term_assets / Short_term_debt, 6),
  Grow_opportunity = round((Liabilities + Market_value_of_capital) / Total_Assets, 6), 
  Size = log(Total_Assets),
  Covid19 = ifelse(Time %in% covid_period, 1, 0)
)

# Checking Na values.
sum(is.na(data))

#######################################################    3. Descriptive statistics    ################################################################

#### Period all quarter
summary(data)
View(data)

# Descriptive statistics data
all_statis = data %>% summarise(variables = c('Debt_maturity_structure','Leverage', 'Liquidity','Size','Grow_opportunity'),
                                obs = nrow(data),
                                min = c(min(Debt_maturity_structure),min(Leverage),min(Liquidity),min(Size),min(Grow_opportunity)),
                                mean = c(mean(Debt_maturity_structure),mean(Leverage),mean(Liquidity),mean(Size),mean(Grow_opportunity)),
                                median = c(median(Debt_maturity_structure),median(Leverage),median(Liquidity),median(Size),median(Grow_opportunity)),
                                std = c(sd(Debt_maturity_structure),sd(Leverage),sd(Liquidity),sd(Size),sd(Grow_opportunity)),
                                max = c(max(Debt_maturity_structure),max(Leverage), max(Liquidity), max(Size), max(Grow_opportunity))
)
all_statis = data.frame(all_statis)
all_statis


#### Period Q1/2009 - Q4/2019
before= data %>% slice(1:which(Quarter == 'Q4/2019'))

# Descriptive statistics before Q1/2020
before_statis = before %>% summarise(variables = c('Debt_maturity_structure','Leverage', 'Liquidity','Size','Grow_opportunity'),
                                     obs = nrow(before),
                                     min = c(min(Debt_maturity_structure),min(Leverage),min(Liquidity),min(Size),min(Grow_opportunity)),
                                     mean = c(mean(Debt_maturity_structure),mean(Leverage),mean(Liquidity),mean(Size),mean(Grow_opportunity)),
                                     median = c(median(Debt_maturity_structure),median(Leverage),median(Liquidity),median(Size),median(Grow_opportunity)),
                                     std = c(sd(Debt_maturity_structure),sd(Leverage),sd(Liquidity),sd(Size),sd(Grow_opportunity)),
                                     max = c(max(Debt_maturity_structure),max(Leverage), max(Liquidity), max(Size), max(Grow_opportunity))
)
before_statis = data.frame(before_statis)
before_statis

#### Period Q1/2020 - now
after = data %>% slice(which(data$Quarter == 'Q1/2020'): nrow(data))

# Descriptive statistics after Q1/2020
after_statis = after %>% summarise(variables = c('Debt_maturity_structure','Leverage', 'Liquidity','Size','Grow_opportunity'),
                                   obs = nrow(after),
                                   min = c(min(Debt_maturity_structure),min(Leverage),min(Liquidity),min(Size),min(Grow_opportunity)),
                                   mean = c(mean(Debt_maturity_structure),mean(Leverage),mean(Liquidity),mean(Size),mean(Grow_opportunity)),
                                   median = c(median(Debt_maturity_structure),median(Leverage),median(Liquidity),median(Size),median(Grow_opportunity)),
                                   std = c(sd(Debt_maturity_structure),sd(Leverage),sd(Liquidity),sd(Size),sd(Grow_opportunity)),
                                   max = c(max(Debt_maturity_structure),max(Leverage), max(Liquidity), max(Size), max(Grow_opportunity))
)
after_statis = data.frame(after_statis)
after_statis


###########################################################     4. Visualization     #####################################################################  
graphics.off()

### Entire quarter
# Histogram of Debt_maturity_structure
data %>% ggplot(aes(Debt_maturity_structure)) +
  geom_histogram(binwidth = 0.03,fill = 'coral', color = 'white') + 
  labs(title = "Histogram of Debt_maturity_structure",
       x = "Debt_maturity_structure",
       y = "Count")+
  scale_x_continuous(labels = comma)+
  scale_y_continuous()


# Box plot of Debt_maturity_structure  
boxplot(data$Debt_maturity_structure,
        main = "Box plot of MSG Debt_maturity_structure",
        col = "green",
        border = "brown",
        xlab= "Debt_maturity_structure",
        horizontal = TRUE
)

### Before COVID-19
# Histogram of Debt_maturity_structure
before %>% ggplot(aes(Debt_maturity_structure)) +
  geom_histogram(binwidth = 0.03,fill = 'coral', color = 'white') + 
  labs(title = "Histogram of Debt_maturity_structure",
       x = "Debt_maturity_structure",
       y = "Count")+
  scale_x_continuous(labels = comma)+
  scale_y_continuous()


# Box plot of Debt_maturity_structure  
boxplot(before$Debt_maturity_structure,
        main = "Box plot of MSG Debt_maturity_structure",
        col = "green",
        border = "brown",
        xlab= "Debt_maturity_structure",
        horizontal = TRUE
)

### After COVID-19
# Histogram of Debt_maturity_structure
after %>% ggplot(aes(Debt_maturity_structure)) +
  geom_histogram(binwidth = 0.03,fill = 'coral', color = 'white') + 
  labs(title = "Histogram of Debt_maturity_structure",
       x = "Debt_maturity_structure",
       y = "Count")+
  scale_x_continuous(labels = comma)+
  scale_y_continuous()


# Box plot of Debt_maturity_structure  
boxplot(after$Debt_maturity_structure,
        main = "Box plot of MSG Debt_maturity_structure",
        col = "green",
        border = "brown",
        xlab= "Debt_maturity_structure",
        horizontal = TRUE
)

###########################    5. Perform multiple regression   ################## 


# Scatter for checking the linear characteristic in the data sets.
par(mfrow=c(1,3))
plot(Debt_maturity_structure ~ Leverage, data=data, col = "orange", lwd = 2)
plot(Debt_maturity_structure ~ Liquidity, data=data, col = "chartreuse4",lwd = 2)
plot(Debt_maturity_structure ~ Size, data=data, col = "red",lwd = 2)
plot(Debt_maturity_structure ~ Grow_opportunity, data=data, col = "blue",lwd = 2)


##### 5.1. Multiple Linear Regression with the usual individual variables (model 1) 

Debt_maturity_structure.lm<-lm(Debt_maturity_structure ~  Leverage + Liquidity + Size + Grow_opportunity, data = data)
summary(Debt_maturity_structure.lm)

# Visualization 
par(mfrow=c(2,2))
plot(Debt_maturity_structure.lm)

# Check normality
shapiro.test(resid(Debt_maturity_structure.lm)) # Null hypothesis is normality
# Install the lmtest package (if not already installed)
install.packages("lmtest")
# Load the lmtest package
library(lmtest)
# Check homoskedasticity
bptest(Debt_maturity_structure.lm) #Null hypothesis is homoskedasticity


##### 5.2. Multiple Linear Regression with the usual individual variables and the interaction 
# Between Covid-19 dummy variable (model 2)

data$Covid19 = factor(data$Covid19)
Debt_maturity_structure.lm.covid<-lm(Debt_maturity_structure ~  Leverage + Size + Liquidity + Grow_opportunity + Covid19 , data = data)
summary(Debt_maturity_structure.lm.covid)

# Visualization 
par(mfrow=c(2,2))
plot(Debt_maturity_structure.lm.covid)

# Check normality
shapiro.test(resid(Debt_maturity_structure.lm.covid))# Null hypothesis is normality

# Check homoskedasticity
bptest(Debt_maturity_structure.lm.covid) #Null hypothesis is homoskedasticity

##### 5.3. Prediction
pred.model = predict(Debt_maturity_structure.lm, data[,3:6])

pred.table =data.frame(Quanter = data$Quarter,
                       Debt_maturity_structure.Real = data$Debt_maturity_structure,
                       Debt_maturity_structure.Pred = round(pred.model,6))
pred.table

# Install and load the forecast package
install.packages("forecast")
library(forecast)

# Assuming you have valid predictions in pred.table$Debt_maturity_structure.Pred
# Assuming you have corresponding actual values in pred.table$Debt_maturity_structure.Real
# Calculate accuracy measures
accuracy(pred.table$Debt_maturity_structure.Pred, pred.table$Debt_maturity_structure.Real)

##############################################       6. ARIMA MODEL        ##############################################
# Checking Na values.
sum(is.na(data))

# Check class
class(data$Quarter)

# Convert "character" to "Date" data
date.time = seq(as.Date("2009/12/01"), by = "quarter", length.out = nrow(data)) 
date.time

# Install and load the xts package
install.packages("xts")
library(xts)

# Create a xts Dataframe
HSG.Debt_maturity_structure = xts(data[,2],date.time)

View(HSG.Debt_maturity_structure)

par(mfrow=c(1,1))
plot(HSG.Debt_maturity_structure$Debt_maturity_structure)

##### Debt_maturity_structure stationary check 

# ACF/PACF
par(mfrow=c(1,2))
acf(HSG.Debt_maturity_structure,main='ACF for Debt_maturity_structure',lag.max = 24)
pacf(HSG.Debt_maturity_structure$Debt_maturity_structure,main='PACF for Debt_maturity_structure',lag.max = 24)

# Install and load the tseries package
install.packages("tseries")
library(tseries)

# ADF test and KPSS test
adf.test(HSG.Debt_maturity_structure$Debt_maturity_structure) 
kpss.test(HSG.Debt_maturity_structure$Debt_maturity_structure)


# Caculate the percent change of Debt_maturity_structure.
HSG.Debt_maturity_structure$diff = diff(HSG.Debt_maturity_structure$Debt_maturity_structure)
lag.Debt_maturity_structure = HSG.Debt_maturity_structure$Debt_maturity_structure[-nrow(data)]

index(lag.Debt_maturity_structure) = seq(as.Date("2010/03/01"), by = "quarter", length.out = nrow(data)-1) 
HSG.Debt_maturity_structure$lag = lag.Debt_maturity_structure

HSG.Debt_maturity_structure = na.omit(HSG.Debt_maturity_structure)
HSG.Debt_maturity_structure$pct.change = HSG.Debt_maturity_structure$diff/lag.Debt_maturity_structure

##### The percent change of cash holding stationary check 

# ACF/PACF
par(mfrow=c(1,2))
acf(HSG.Debt_maturity_structure$pct.change,main='ACF for percent change of Debt_maturity_structure',lag.max = 24)
pacf(HSG.Debt_maturity_structure$pct.change,main='PACF for percent change of Debt_maturity_structure',lag.max = 24)

# ADF test and KPSS test
adf.test(HSG.Debt_maturity_structure$pct.change) 
kpss.test(HSG.Debt_maturity_structure$pct.change)

# Building Auto-ARIMA model
# Install and load the forecast package
install.packages("forecast")
library(forecast)

# Build Auto-ARIMA model
auto <- auto.arima(HSG.Debt_maturity_structure$pct.change, seasonal = FALSE, trace = TRUE, ic = "aic")
auto=auto.arima(HSG.Debt_maturity_structure$pct.change,seasonal=F,trace = T, ic='aic')
auto
coeftest(auto)

# Prediction
term = 4
fcastauto=forecast(auto,h=term)
fcastauto # predicted values for 4 terms (Q1/2022 - Q4/2021)

# Visualization the prediction
par(mfrow=c(1,1))
plot(fcastauto)

#train set
accuracy(fcastauto)

######################################################  End ########################################################### 


