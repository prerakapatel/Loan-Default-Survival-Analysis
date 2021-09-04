library(survival)
library(survminer)
library(readxl)
library(data.table)
#install.packages("visreg")
#install.packages("casebase")
library(visreg)
library(splines)
library(casebase)
require(survival)
#install.packages("epiR")
library(epiR)

MortgageData <- read_excel("D:/MGMT 672 ABA/Projects/MortgageData.xlsx")


MortgageData$start_date_year<- format(MortgageData$start_date, format = "%Y")
MortgageData$start_date <- as.Date(MortgageData$start_date)
MortgageData$Initial_date <- fifelse(MortgageData$vintage == MortgageData$start_date_year,MortgageData$start_date, as.Date(paste0(as.character(MortgageData$start_date_year),'-01-01')))



MortgageData$Length <- difftime(MortgageData$end_date, MortgageData$Initial_date, units = "weeks")/4
View(MortgageData)

df <- MortgageData

ggplot(df, aes(x=cred_score)) + 
  geom_histogram()

ggplot(df, aes(x=DBT_RATIO)) + 
  geom_histogram()

df$cred_level <- cut(df$cred_score, breaks = c(0,599,699,769,800,1000), labels = c("poor","fair","good","very good","exceptional"))
table(df$cred_level)
#df$cred_score <- as.integer(bin_data(df$cred_score, bins=c(0,599,679,739,789,1000)))
df$debt_level <- cut(df$DBT_RATIO, breaks = c(0,0.25,0.6,2), labels = c("Low Risk","Medium Risk","High Risk"))
table(df$debt_level)

# Survival plot based on credit level
fit.credit <- survfit(Surv(Length,event==1)~ cred_level,data=df)
plot(fit.credit,col=1:5, lty=1:5, main="cred_level",xlab="Time in Years",ylab = "Default Probality")
llabel<-gsub("x=","",names(fit.credit$strata))
legend("bottom",legend=llabel,col=1:5,lty=1:5,bty='n')

ggsurvplot(fit.credit, fun = function(y) 1-y, xlab="Time in Months", ylab="Default Probability")

survdiff(Surv(Length,(event==1))~cred_level,data=df, na.action = na.omit, rho =0)

# Survival plot based on Debt level
fit.debt <- survfit(Surv(Length,event==1)~ debt_level,data=df)
plot(fit.debt,col=1:5, lty=1:5, main="Debt Level",xlab="Time in Years",ylab = "Default Probality")
llabel<-gsub("x=","",names(fit.debt$strata))
legend("top",legend=llabel,col=1:5,lty=1:5,bty='n')

ggsurvplot(fit.debt, fun = function(y) 1-y, xlab="Time in Months",ylab = "Default Probality")

survdiff(Surv(Length,(event==1))~debt_level,data=df, na.action = na.omit, rho =0)

fit.cox<-coxph(Surv(Length,event==1)~ cred_level+debt_level,data=df)
fit.cox
test2.ph <- cox.zph(fit.cox)
test2.ph


df_r <- na.omit(df)
class(df_r$Length)

plot(fit.credit,col=1:5, lty=1:5, fun="cumhaz",  xlab="Months", ylab="Cumulative Hazard")
llabel<-gsub("x=","",names(fit.credit$strata))
legend("top",legend=llabel,col=1:5,lty=1:5,bty='n')

##Hazard Plots
hazard_credit <- epi.insthaz(fit.credit, conf.level = 0.95)

ggplot() +
  theme_bw() +
geom_step(data = hazard_credit, aes(x = time, y = hest), colour = "grey") + 
 facet_grid(strata ~ .) +
geom_smooth(data = hazard_credit, aes(x = time, y = hest), method = "loess", 
              colour = "black", size = 0.75, linetype = "solid", 
              se = FALSE, span = 0.20) +
 geom_smooth(data = hazard_credit, aes(x = time, y = hlow), method = "loess", 
              colour = "black", size = 0.5, linetype = "dashed", 
              se = FALSE, span = 0.20) +
  geom_smooth(data = hazard_credit, aes(x = time, y = hupp), method = "loess", 
             colour = "blue", size = 0.5, linetype = "dashed", 
             se = FALSE, span = 0.20) +
  scale_x_continuous(limits = c(0,100), name = "Time (Months)") +
  scale_y_continuous(limits = c(0,0.1), name = "Daily probability of event")


hazard_credit %>%
  ggplot(aes(x= time, y= hest), method = "loess") +
  facet_grid(strata ~ .) +
  geom_line() + ylab("h(t)") + xlab("Time(Months)")
