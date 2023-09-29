###### Data Analysis ###
###### The duration of political imprisonment: Evidence from China ###
###### Author: Christoph Valentin Steinert ###

# Load packages
library(tidyverse)
library(dplyr)
library(lubridate)
library(Hmisc)
library(readstata13)
library(MatchIt)
library(stringr)
library(texreg)
library(devtools)
library(sjPlot)
library(survival)
library(survminer)
library(broom)
library(cem)
library(stargazer)
library(cobalt)

# Load data
rm(list = ls())
# setwd("") set your working directory
load(file = "final_pprisdat.RData")
load(file = "complete_pprisdat.RData")

# Check detention status
table(final_pprisdat$currently_detained) # 1280 right-censored
table(final_pprisdat$released) # 1005 released
table(final_pprisdat$escaped) # 1 escaped
table(final_pprisdat$died_in_prison) # 14 died in prison

# Create survival-status variable
final_pprisdat$survstatus <- NA
final_pprisdat$survstatus[final_pprisdat$released == 1] <- 2
final_pprisdat$survstatus[final_pprisdat$currently_detained == 1] <- 1

# Exclude prisoners that a) died in prison, b) escaped, or c) were executed
final_pprisdat$detentiontime_months[final_pprisdat$died_in_prison == 1] <- NA
final_pprisdat$detentiontime_months[final_pprisdat$escaped == 1] <- NA
final_pprisdat$detentiontime_months[final_pprisdat$executed == 1] <- NA

# Exclude coding errors
final_pprisdat$detentiontime_months[final_pprisdat$detentiontime_months < 0] <- NA

# Create year-variable for yearly fixed-effects
final_pprisdat$Year <- format(final_pprisdat$date_of_detention_coarse_unrounded, "%Y")

# Create sub-dataset of only released individuals
onlyreleased <- filter(final_pprisdat, currently_detained == 0)

# Create sub-dataset without missings on the key explanatory variables
## no missings of criminalized action
notmiss_ca <- final_pprisdat %>% 
  filter(!is.na(crim_act))
## no missings of ethnic group
notmiss_ethnicity <- final_pprisdat %>% 
  filter(!is.na(ethnic_group))
## no missing on both
notmiss <- final_pprisdat %>% 
  filter(!is.na(ethnic_group) & !is.na(crim_act))

# Check average length by criminal statutes
#### See Table 15 in the appendix ####
byallegation <- tapply(onlyreleased$detentiontime_months, INDEX = onlyreleased$charge_statute, mean)

# Randomly select ~20% of cases for inter-coder reliability check
randomsample <- notmiss[sample(nrow(notmiss), replace = F, size = 186), ]
randomsample <- dplyr::select(randomsample, CECC_record_number, crim_act)
### write.csv(randomsample, "randomsample_intercodercheck.csv")

# Create survival objects for different datasets
surv_object <- Surv(final_pprisdat$detentiontime_months, final_pprisdat$survstatus)
surv_object2 <- Surv(notmiss_ca$detentiontime_months, notmiss_ca$survstatus)
surv_object3 <- Surv(notmiss_ethnicity$detentiontime_months, notmiss_ethnicity$survstatus)
surv_object4 <- Surv(notmiss$detentiontime_months, notmiss$survstatus)

# Distribution of dependent variable
#### See Figure 4 in appendix ####
#pdf(file = "density_plot.pdf")
notmiss$main_name <- as.character(notmiss$main.name)
plot(density(notmiss$detentiontime_months, na.rm = T), main = "", bty="n", 
     xlim = c(0, 340),
     xlab="Length of political imprisonment in months")
for(i in 1:3){
  abline(v=notmiss$detentiontime_months[order(notmiss$detentiontime_months, decreasing=TRUE)][i])
  text(x=notmiss$detentiontime_months[order(notmiss$detentiontime_months, decreasing=TRUE)][1], y = 0.0012,  
       labels=notmiss$main_name[order(notmiss$detention_to_release_month, decreasing=TRUE)][1], pos=1, cex=0.8)
  text(x=notmiss$detentiontime_months[order(notmiss$detentiontime_months, decreasing=TRUE)][2], y = 0.0017,  
       labels=notmiss$main_name[order(notmiss$detention_to_release_month, decreasing=TRUE)][2], pos=1, cex=0.8)
  text(x=notmiss$detentiontime_months[order(notmiss$detentiontime_months, decreasing=TRUE)][3], y = 0.0022,  
       labels=notmiss$main_name[order(notmiss$detention_to_release_month, decreasing=TRUE)][3], pos=1, cex=0.8)
}
#dev.off()

# Plot the distribution of ethnic groups
p1 <- ggplot(data = final_pprisdat) + 
  geom_bar(mapping = aes(x = ethnicity), width = 0.5) +
  xlab("") + ylab("# of known Chinese political prisoners") +
    theme_pubclean()
p_dt <- layer_data(p1)
p1 + annotate(geom = "text", label = p_dt$count, x = p_dt$x, y = p_dt$y + 50)

# Plot the distribution of religious groups
p1 <- ggplot(data = final_pprisdat) + 
  geom_bar(mapping = aes(x = confession), width = 0.5) +
  xlab("") + ylab("# of known Chinese political prisoners") +
  theme_pubclean()
p_dt <- layer_data(p1)
p1 + annotate(geom = "text", label = p_dt$count, x = p_dt$x, y = p_dt$y + 50)

# Plot the distribution of criminalized actions
level_order <- factor(final_pprisdat$crim_act, level = c("LCAP_MIN", "HCAP_MIN", "LCAP_MAX", "HCAP_MAX"))
#### See Figure 3 in the article ####
##pdf(file = "parsimon_threat.pdf", width=10, height=5)
p1 <-ggplot(data = final_pprisdat, aes(x = level_order, width = 0.3)) + 
  geom_bar(width = 0.7) + theme_pubclean() + ylab("# of known Chinese political prisoners") +
  xlab("") + scale_x_discrete(labels = c("Non-mobilizing reformers\n (LCAP_MIN)", "Mobilizing reformers\n (HCAP_MIN)", "Non-mobilizing dissidents\n (LCAP_MAX)", "Mobilizing dissidents\n (HCAP_MAX)", "NA")) +
  scale_y_continuous(breaks = c(0, 100, 200, 300,
                                           400, 500, 600, 700, 800, 900, 1000, 1100)) + theme(axis.ticks.x = element_blank())
p_dt <- layer_data(p1)
p1 + annotate(geom = "text", label = p_dt$count, x = p_dt$x, y = p_dt$y + 30)
## dev.off()

# Distribution of detention lengths across criminalized actions (exclude right-censored cases)
level_order <- factor(onlyreleased$crim_act, level = c("LCAP_MIN", "HCAP_MIN", "LCAP_MAX", "HCAP_MAX"))
#### See Figure 4 in the article ####
## pdf(file = "parsimon_threat_avlength.pdf", width=10, height=5)
ggplot(onlyreleased, aes(x = level_order, y = detentiontime_months)) +
  stat_summary(geom = "pointrange", size = 1, color = "red",
               fun.data = "mean_se", fun.args = list(mult = 1.96)) +
  scale_y_continuous(breaks = c(10, 20, 30, 40,
                                50, 60, 70, 80, 90, 100, 110)) +
  scale_x_discrete(labels = c("Non-mobilizing reformers\n (LCAP_MIN)", "Mobilizing reformers\n (HCAP_MIN)", "Non-mobilizing dissidents\n (LCAP_MAX)", "Mobilizing dissidents\n (HCAP_MAX)", "NA")) +
  labs(y = "Average number of months in detention", x = "") +
  theme_pubclean() + theme(axis.ticks = element_blank()) 
## dev.off()

# Detentions and releases over time
detentionmonths <- final_pprisdat %>%
  count(date_of_detention_coarse)
releasemonths <- final_pprisdat %>% 
  count(date_of_release_coarse)
timeseries <- read.csv("complete_datevector.csv")
timeseries$date <- as.Date(timeseries$date, "%d/%m/%Y")
releasemonths <- rename(releasemonths, released = n)
releasemonths <- rename(releasemonths, date = date_of_release_coarse)
detentionmonths <- rename(detentionmonths, detained = n)
detentionmonths <- rename(detentionmonths, date = date_of_detention_coarse)
timeseries_det <- merge(timeseries, detentionmonths, by = "date", all.x = T, all.y = T)
timeseries_complete <- merge(timeseries_det, releasemonths, by = "date", all.x = T, all.y = T)
timeseries_complete$testnum <- NULL
timeseries_complete$detained[is.na(timeseries_complete$detained)] <- 0
timeseries_complete$released[is.na(timeseries_complete$released)] <- 0
ggplot(data = timeseries_complete, aes(date)) +
  geom_line(aes(y = detained, color = "detained")) +
  geom_line(aes(y = released, color = "released")) +
  ylim(0, 50) + theme_minimal() + xlab("Date") + ylab("Number of known political prisoners")
timeseries_shorter <- filter(timeseries_complete, date > "2000-01-01")

# Plot detentions and releases over time
#### See Figure 5 in the appendix ####
#pdf(file = "detention_releases_timetrend.pdf")
ggplot(data = timeseries_shorter, aes(date)) +
  geom_line(aes(y = detained, color = "detained")) +
  geom_line(aes(y = released, color = "released")) +
  ylim(0, 50) + theme_minimal() + xlab("Year") + ylab("Number of known political prisoners") +
  theme(legend.title = element_blank()) 
#dev.off()

# Kaplan-Meier plot
plot(survfit(Surv(detentiontime_months, survstatus) ~ 1, data = final_pprisdat), 
     xlim = c(0, 300), xlab = "months t in detention", 
     ylab = "S(t): probability of remaining detained",
     )
#### See Figure 1 in the article ####
## pdf(file = "kaplan-meier-plot.pdf")
t <- ggsurvplot(
  fit = survfit(Surv(detentiontime_months, survstatus) ~ 1, data = final_pprisdat),
  legend = "none",
  xlim = c(0, 200),
  xlab = "months t in detention", 
  ylab = "S(t): probability of remaining detained", risk.table = T, 
  tables.height = 0.11,
  break.x.by = 25,
  cumcensor = T,
  tables.theme = theme_cleantable(),
  ncensor.plot.height = 0.11) 
t$table <- t$table
print(t)
## dev.off()

# Estimate median and mean detention time
final_pprisdat$onlyrel <- ifelse(final_pprisdat$survstatus == 2, final_pprisdat$detentiontime_months, NA)
mean(final_pprisdat$onlyrel, na.rm = T)
median(final_pprisdat$onlyrel, na.rm = T)
max(final_pprisdat$onlyrel, na.rm = T)

# Descriptive statistics table
modelvars <- dplyr::select(notmiss, detentiontime_months, HCAP_MAX, HCAP_MIN, LCAP_MAX, LCAP_MIN,
                              Male, Ethnic_Tibetan, Ethnic_Uyghur, Other_ethnic_minorities, Muslim, 
                              Christian, FalunGong, Buddhist, Other_religious_groups, Journalist,
                              Worker, Lawyer, Entrepreneur, Student, Professor, Unemployed, 
                              Charged, Sex, Recidivists, Focalevent_detention, Mass_amnesty)
#### See Table 1 in the appendix ####
stargazer(modelvars)

# Crosstab criminalized actions over ethnic/ religious groups
#### See Table 2 in the appendix ####
crosstab1 <- table(Uyghur = notmiss$Ethnic_Uyghur, HCAP_MAX = notmiss$HCAP_MAX)
crosstab2 <- table(Uyghur = notmiss$Ethnic_Uyghur, HCAP_MIN = notmiss$HCAP_MIN)
crosstab3 <- table(Uyghur = notmiss$Ethnic_Uyghur, LCAP_MIN = notmiss$LCAP_MIN)
crosstab4 <- table(Uyghur = notmiss$Ethnic_Uyghur, LCAP_MAX = notmiss$LCAP_MAX)
crosstab1a <- table(Tibetan = notmiss$Ethnic_Tibetan, HCAP_MAX = notmiss$HCAP_MAX)
crosstab2a <- table(Tibetan = notmiss$Ethnic_Tibetan, HCAP_MIN = notmiss$HCAP_MIN)
crosstab3a <- table(Tibetan = notmiss$Ethnic_Tibetan, LCAP_MIN = notmiss$LCAP_MIN)
crosstab4a <- table(Tibetan = notmiss$Ethnic_Tibetan, LCAP_MAX = notmiss$LCAP_MAX)
crosstab1b <- table(Otherethnicmin = notmiss$Other_ethnic_minorities, HCAP_MAX = notmiss$HCAP_MAX)
crosstab2b <- table(Otherethnicmin = notmiss$Other_ethnic_minorities, HCAP_MIN = notmiss$HCAP_MIN)
crosstab3b <- table(Otherethnicmin = notmiss$Other_ethnic_minorities, LCAP_MIN = notmiss$LCAP_MIN)
crosstab4b <- table(Otherethnicmin = notmiss$Other_ethnic_minorities, LCAP_MAX = notmiss$LCAP_MAX)
crosstab1c <- table(Muslim = notmiss$Muslim, HCAP_MAX = notmiss$HCAP_MAX)
crosstab2c <- table(Muslim = notmiss$Muslim, HCAP_MIN = notmiss$HCAP_MIN)
crosstab3c <- table(Muslim = notmiss$Muslim, LCAP_MIN = notmiss$LCAP_MIN)
crosstab4c <- table(Muslim = notmiss$Muslim, LCAP_MAX = notmiss$LCAP_MAX)
crosstab1d <- table(Buddhist = notmiss$Buddhist, HCAP_MAX = notmiss$HCAP_MAX)
crosstab2d <- table(Buddhist = notmiss$Buddhist, HCAP_MIN = notmiss$HCAP_MIN)
crosstab3d <- table(Buddhist = notmiss$Buddhist, LCAP_MIN = notmiss$LCAP_MIN)
crosstab4d <- table(Buddhist = notmiss$Buddhist, LCAP_MAX = notmiss$LCAP_MAX)
crosstab1e <- table(Christian = notmiss$Christian, HCAP_MAX = notmiss$HCAP_MAX)
crosstab2e <- table(Christian = notmiss$Christian, HCAP_MIN = notmiss$HCAP_MIN)
crosstab3e <- table(Christian = notmiss$Christian, LCAP_MIN = notmiss$LCAP_MIN)
crosstab4e <- table(Christian = notmiss$Christian, LCAP_MAX = notmiss$LCAP_MAX)
crosstab1f <- table(FalunGong = notmiss$FalunGong, HCAP_MAX = notmiss$HCAP_MAX)
crosstab2f <- table(FalunGong = notmiss$FalunGong, HCAP_MIN = notmiss$HCAP_MIN)
crosstab3f <- table(FalunGong = notmiss$FalunGong, LCAP_MIN = notmiss$LCAP_MIN)
crosstab4f <- table(FalunGong = notmiss$FalunGong, LCAP_MAX = notmiss$LCAP_MAX)
crosstab1g <- table(Otherrel = notmiss$Other_religious_groups, HCAP_MAX = notmiss$HCAP_MAX)
crosstab2g <- table(Otherrel = notmiss$Other_religious_groups, HCAP_MIN = notmiss$HCAP_MIN)
crosstab3g <- table(Otherrel = notmiss$Other_religious_groups, LCAP_MIN = notmiss$LCAP_MIN)
crosstab4g <- table(Otherrel = notmiss$Other_religious_groups, LCAP_MAX = notmiss$LCAP_MAX)
crosstab1h <- table(Charged = notmiss$Charged, HCAP_MAX = notmiss$HCAP_MAX)
crosstab2h <- table(Charged = notmiss$Charged, HCAP_MIN = notmiss$HCAP_MIN)
crosstab3h <- table(Charged = notmiss$Charged, LCAP_MIN = notmiss$LCAP_MIN)
crosstab4h <- table(Charged = notmiss$Charged, LCAP_MAX = notmiss$LCAP_MAX)
crosstab1i <- table(Sex = notmiss$Sex, HCAP_MAX = notmiss$HCAP_MAX)
crosstab2i <- table(Sex = notmiss$Sex, HCAP_MIN = notmiss$HCAP_MIN)
crosstab3i <- table(Sex = notmiss$Sex, LCAP_MIN = notmiss$LCAP_MIN)
crosstab4i <- table(Sex = notmiss$Sex, LCAP_MAX = notmiss$LCAP_MAX)
crosstab1j <- table(Worker = notmiss$Worker, HCAP_MAX = notmiss$HCAP_MAX)
crosstab2j <- table(Worker = notmiss$Worker, HCAP_MIN = notmiss$HCAP_MIN)
crosstab3j <- table(Worker = notmiss$Worker, LCAP_MIN = notmiss$LCAP_MIN)
crosstab4j <- table(Worker = notmiss$Worker, LCAP_MAX = notmiss$LCAP_MAX)
crosstab1k <- table(Student = notmiss$Student, HCAP_MAX = notmiss$HCAP_MAX)
crosstab2k <- table(Student = notmiss$Student, HCAP_MIN = notmiss$HCAP_MIN)
crosstab3k <- table(Student = notmiss$Student, LCAP_MIN = notmiss$LCAP_MIN)
crosstab4k <- table(Student = notmiss$Student, LCAP_MAX = notmiss$LCAP_MAX)
crosstab1l <- table(Lawyer = notmiss$Lawyer, HCAP_MAX = notmiss$HCAP_MAX)
crosstab2l <- table(Lawyer = notmiss$Lawyer, HCAP_MIN = notmiss$HCAP_MIN)
crosstab3l <- table(Lawyer = notmiss$Lawyer, LCAP_MIN = notmiss$LCAP_MIN)
crosstab4l <- table(Lawyer = notmiss$Lawyer, LCAP_MAX = notmiss$LCAP_MAX)
crosstab1m <- table(Han = notmiss$Ethnic_Han, HCAP_MAX = notmiss$HCAP_MAX)
crosstab2m <- table(Han = notmiss$Ethnic_Han, HCAP_MIN = notmiss$HCAP_MIN)
crosstab3m <- table(Han = notmiss$Ethnic_Han, LCAP_MIN = notmiss$LCAP_MIN)
crosstab4m <- table(Han = notmiss$Ethnic_Han, LCAP_MAX = notmiss$LCAP_MAX)

# Cox proportional hazard models

## Main specification, without fixed effects
m1 <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX + HCAP_MIN + LCAP_MAX +
              Ethnic_Tibetan + Ethnic_Uyghur + Other_ethnic_minorities + Muslim + 
              Christian + FalunGong + Buddhist + Other_religious_groups + Journalist +
              Worker + Lawyer + Entrepreneur + Student + Professor + Unemployed +
              Charged + Sex + Recidivists + Focalevent_detention, data = notmiss)
screenreg(m1)
#### See Figure 5 in the article ####
## pdf(file = "effectsPlot.pdf")
plot_model(m1, title = "") + theme_minimal() + geom_hline(yintercept = 1) + ylab("Hazard ratios of releases")
## dev.off()

## Main specification, with year fixed effects
m2 <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX + HCAP_MIN + LCAP_MAX +
              Ethnic_Tibetan + Ethnic_Uyghur + Other_ethnic_minorities + Muslim +
              Christian + FalunGong + Buddhist + Other_religious_groups + Journalist +
              Worker + Lawyer + Entrepreneur + Student + Professor + Unemployed +
              Charged + Sex + Recidivists + Focalevent_detention + factor(Year), data = notmiss)
screenreg(m2)

## Main specification, with province fixed effects
m3 <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX + HCAP_MIN + LCAP_MAX +
              Ethnic_Tibetan + Ethnic_Uyghur + Other_ethnic_minorities + Muslim +
              Christian + FalunGong + Buddhist + Other_religious_groups + Journalist +
              Worker + Lawyer + Entrepreneur + Student + Professor + Unemployed +
              Charged + Sex + Recidivists + Focalevent_detention + 
              factor(province_imprisoned_detained), data = notmiss)
screenreg(m3)

## No missings, with two-way fixed effects
m4 <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX + HCAP_MIN + LCAP_MAX +
              Ethnic_Tibetan + Ethnic_Uyghur + Other_ethnic_minorities + Muslim +
              Christian + FalunGong + Buddhist + Other_religious_groups + Journalist +
              Worker + Lawyer + Entrepreneur + Student + Professor + Unemployed + 
              Charged + Sex + Recidivists + Focalevent_detention + factor(Year) +
              factor(province_imprisoned_detained), data = notmiss)
summary(m4)
screenreg(m4)
#### See Table 3 in the appendix ####
stargazer(list(m1, m2, m3, m4), apply.coef = exp, p.auto = FALSE, title = "Survival analysis", omit="factor\\(Year", add.lines=list(c("Year fixed effects", rep("YES", 4))))


## Adjusted Survival curves based on model 4
##### Create Factor variables
notmiss <- notmiss %>% 
  mutate(EthnicUyghur = as.factor(ifelse(Ethnic_Uyghur== 1, "Ethnic Uyghurs", "Other ethnic groups")))
notmiss <- notmiss %>% 
  mutate(EthnicTibetan = as.factor(ifelse(Ethnic_Tibetan== 1, "Ethnic Tibetan", "Other ethnic groups")))
notmiss <- notmiss %>% 
  mutate(Charged_individuals = as.factor(ifelse(Charged == 1, "Charged", "Extrajudicially detained")))
notmiss <- notmiss %>% 
  mutate(HCAPMIN = as.factor(ifelse(HCAP_MIN== 1, "HCAP_MIN", "Others")))
##### Re-run Model 4
m4 <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX + HCAPMIN + LCAP_MAX +
              EthnicTibetan + EthnicUyghur + Other_ethnic_minorities + Muslim +
              Christian + FalunGong + Buddhist + Other_religious_groups + Journalist +
              Worker + Lawyer + Entrepreneur + Student + Professor + 
              Charged_individuals + Sex + Recidivists + Focalevent_detention + factor(Year) +
              factor(province_imprisoned_detained), data = notmiss)

#### See Figure 6 in the article ####
## pdf(file = "adjusted_uyghur.pdf")
ggadjustedcurves(m4, data = notmiss, variable = "EthnicUyghur") + theme_minimal() + 
  xlab("Months in detention") + ylab("Survival rate (in prison)") + theme(legend.title = element_blank()) +
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150,
                                175, 200)) + theme(
                                  legend.position = c(.25, .25),
                                  legend.justification = c("right", "top"),
                                  legend.box.just = "right",
                                  legend.margin = margin(6, 6, 6, 6)
                                )

## dev.off()                              

## Main specification, without fixed effects, missings of ethnicity-variable treated as Han
m5 <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX + HCAP_MIN + LCAP_MAX +
              Ethnic_Tibetan + Ethnic_Uyghur + Other_ethnic_minorities + Muslim +
              Christian + FalunGong + Buddhist + Other_religious_groups + Journalist +
              Worker + Lawyer + Entrepreneur + Student + Professor + Unemployed +
              Charged + Sex + Recidivists + Focalevent_detention, data = notmiss_ca)
screenreg(m5)
plot_model(m5, title = "") + theme_minimal() + geom_hline(yintercept = 1) + ylab("Hazard ratios of releases")

## Main specification, with year fixed effects, missings of ethnicity-variable treated as Han
m6 <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX + HCAP_MIN + LCAP_MAX +
              Ethnic_Tibetan + Ethnic_Uyghur + Other_ethnic_minorities + Muslim +
              Christian + FalunGong + Buddhist + Other_religious_groups + Journalist +
              Worker + Lawyer + Entrepreneur + Student + Professor + Unemployed +
              Charged + Sex + Recidivists + Focalevent_detention + factor(Year), data = notmiss_ca)
screenreg(m6)

## Main specification, with province fixed effects, missings of ethnicity-variable treated as Han
m7 <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX + HCAP_MIN + LCAP_MAX +
              Ethnic_Tibetan + Ethnic_Uyghur + Other_ethnic_minorities + Muslim +
              Christian + FalunGong + Buddhist + Other_religious_groups + Journalist +
              Worker + Lawyer + Entrepreneur + Student + Professor + 
              Charged + Sex + Recidivists + Focalevent_detention + 
              factor(province_imprisoned_detained), data = notmiss_ca)
screenreg(m7)

## No missings, with two-way fixed effects, missings of ethnicity-variable treated as Han
m8 <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX + HCAP_MIN + LCAP_MAX +
              Ethnic_Tibetan + Ethnic_Uyghur + Other_ethnic_minorities + Muslim +
              Christian + FalunGong + Buddhist + Other_religious_groups + Journalist +
              Worker + Lawyer + Entrepreneur + Student + Professor + 
              Charged + Sex + Recidivists + Focalevent_detention + factor(Year) +
              factor(province_imprisoned_detained), data = notmiss_ca)
screenreg(m8)

## Main specification (m1), including Age
m9 <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX + HCAP_MIN + LCAP_MAX +
              Ethnic_Tibetan + Ethnic_Uyghur + Other_ethnic_minorities + Muslim + Age +
              Christian + FalunGong + Buddhist + Other_religious_groups + Journalist +
              Worker + Lawyer + Entrepreneur + Student + Professor + Unemployed +
              Charged + Sex + Recidivists + Focalevent_detention, data = notmiss)
screenreg(m9)
plot_model(m9, title = "") + theme_minimal() + geom_hline(yintercept = 1) + ylab("Hazard ratios of releases")

## Main specification (m1), include Mass Amnesty
m10 <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX + HCAP_MIN + LCAP_MAX +
              Ethnic_Tibetan + Ethnic_Uyghur + Other_ethnic_minorities + Muslim + Age +
              Christian + FalunGong + Buddhist + Other_religious_groups + Journalist +
              Worker + Lawyer + Entrepreneur + Student + Professor + Unemployed +
              Charged + Sex + Recidivists + Focalevent_detention + Mass_amnesty, data = final_pprisdat)
screenreg(m10)
plot_model(m10, title = "") + theme_minimal() + geom_hline(yintercept = 1) + ylab("Hazard ratios of releases")

#### Vary the definition of Mass Amnesties
releasecount <- final_pprisdat %>% 
  group_by(actual_dat_released) %>% 
  count(actual_dat_released) %>% 
  arrange(desc(n))
#### At least 10 releases on a day 
final_pprisdat$mass_amnesty_a <- 0
final_pprisdat$mass_amnesty_a[final_pprisdat$actual_dat_released == "1995-10-06"] <- 1 
final_pprisdat$mass_amnesty_a[final_pprisdat$actual_dat_released == "1996-08-30"] <- 1 
final_pprisdat$mass_amnesty_a[final_pprisdat$actual_dat_released == "2013-11-12"] <- 1 
final_pprisdat$mass_amnesty_a[final_pprisdat$actual_dat_released == "2009-05-09"] <- 1 
m10_a <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX + HCAP_MIN + LCAP_MAX +
               Ethnic_Tibetan + Ethnic_Uyghur + Other_ethnic_minorities + Muslim + Age +
               Christian + FalunGong + Buddhist + Other_religious_groups + Journalist +
               Worker + Lawyer + Entrepreneur + Student + Professor + Unemployed +
               Charged + Sex + Recidivists + Focalevent_detention + mass_amnesty_a, data = final_pprisdat)
screenreg(m10_a)
plot_model(m10_a, title = "") + theme_minimal() + geom_hline(yintercept = 1) + ylab("Hazard ratios of releases")

#### At least 20 releases on a day 
final_pprisdat$mass_amnesty_b <- 0
final_pprisdat$mass_amnesty_b[final_pprisdat$actual_dat_released == "1995-10-06"] <- 1 
m10_b <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX + HCAP_MIN + LCAP_MAX +
                 Ethnic_Tibetan + Ethnic_Uyghur + Other_ethnic_minorities + Muslim + Age +
                 Christian + FalunGong + Buddhist + Other_religious_groups + Journalist +
                 Worker + Lawyer + Entrepreneur + Student + Professor + Unemployed +
                 Charged + Sex + Recidivists + Focalevent_detention + mass_amnesty_a, data = final_pprisdat)
screenreg(m10_b)
plot_model(m10_b, title = "") + theme_minimal() + geom_hline(yintercept = 1) + ylab("Hazard ratios of releases")

## Models without outliers
sortedvalues <- dplyr::arrange(notmiss, desc(detentiontime_months))
nooutlier <- filter(notmiss, detentiontime_months < 305)
m11 <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX + HCAP_MIN + LCAP_MAX +
              Ethnic_Tibetan + Ethnic_Uyghur + Other_ethnic_minorities + Muslim +
              Christian + FalunGong + Buddhist + Other_religious_groups + Journalist +
              Worker + Lawyer + Entrepreneur + Student + Professor + 
              Charged + Sex + Recidivists + Focalevent_detention + factor(Year) +
              factor(province_imprisoned_detained), data = nooutlier)
screenreg(m11)
#### See Table 5 in the appendix ####
stargazer(list(m11), apply.coef = exp, p.auto = FALSE, title = "Survival analysis after excluding three outliers", omit="factor\\(Year", add.lines=list(c("Year fixed effects", rep("YES", 1))))

# One-by-one inclusion of controls
one_by_one1 <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX + HCAP_MIN + LCAP_MAX +
              Journalist + Worker + Lawyer + Entrepreneur + Student + Professor + Unemployed + 
                 Recidivists + Focalevent_detention, data = notmiss)
screenreg(one_by_one1)
one_by_one2 <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX + HCAP_MIN + LCAP_MAX +
                       Journalist + Worker + Lawyer + Entrepreneur + Student + Professor + 
                       Charged + Unemployed + 
                       Recidivists + Focalevent_detention, data = notmiss)
screenreg(one_by_one2) 
one_by_one3 <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX + HCAP_MIN + LCAP_MAX +
                       Journalist + Worker + Lawyer + Entrepreneur + Student + Professor + 
                       Charged + Unemployed + Sex +
                       Recidivists + Focalevent_detention, data = notmiss)
screenreg(one_by_one3)  
one_by_one4 <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX + HCAP_MIN + LCAP_MAX +
                       Ethnic_Tibetan +
                       Journalist + Worker + Lawyer + Entrepreneur + Student + Professor + 
                       Charged + Unemployed + Sex +
                       Recidivists + Focalevent_detention, data = notmiss)
screenreg(one_by_one4)  
one_by_one5 <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX + HCAP_MIN + LCAP_MAX +
                       Ethnic_Tibetan + Ethnic_Uyghur + Other_ethnic_minorities +
                       Journalist + Worker + Lawyer + Entrepreneur + Student + Professor + 
                       Charged + Unemployed + Sex +
                       Recidivists + Focalevent_detention, data = notmiss)
screenreg(one_by_one5) 
one_by_one6 <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX + HCAP_MIN + LCAP_MAX +
                       Ethnic_Tibetan + Ethnic_Uyghur + Other_ethnic_minorities +
                       Muslim +
                       Journalist + Worker + Lawyer + Entrepreneur + Student + Professor + 
                       Charged + Unemployed + Sex +
                       Recidivists + Focalevent_detention, data = notmiss)
screenreg(one_by_one6)
one_by_one7 <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX + HCAP_MIN + LCAP_MAX +
                       Ethnic_Tibetan + Ethnic_Uyghur + Other_ethnic_minorities +
                       Muslim + Christian + FalunGong + Buddhist + Other_religious_groups + 
                       Journalist + Worker + Lawyer + Entrepreneur + Student + Professor + 
                       Charged + Unemployed + Sex +
                       Recidivists + Focalevent_detention, data = notmiss)
screenreg(one_by_one7)  
one_by_one8 <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX + HCAP_MIN + LCAP_MAX +
                       Ethnic_Tibetan + Ethnic_Uyghur + Other_ethnic_minorities +
                       Muslim + Christian + FalunGong + Buddhist + Other_religious_groups + 
                       Journalist + Worker + Lawyer + Entrepreneur + Student + Professor + 
                       Charged + Unemployed + Sex +
                       Recidivists + Focalevent_detention + factor(Year), data = notmiss)
screenreg(one_by_one8) 
one_by_one9 <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX + HCAP_MIN + LCAP_MAX +
                       Ethnic_Tibetan + Ethnic_Uyghur + Other_ethnic_minorities +
                       Muslim + Christian + FalunGong + Buddhist + Other_religious_groups + 
                       Journalist + Worker + Lawyer + Entrepreneur + Student + Professor + 
                       Charged + Unemployed + Sex +
                       Recidivists + Focalevent_detention + factor(Year) +
                       factor(province_imprisoned_detained), data = notmiss)
screenreg(one_by_one9) 

# Create dataframe without missings for matching (no missings on ethnicity-variable)
subvars <- dplyr::select(notmiss, c("survstatus", "Charged", "Male",  
                                           "detentiontime_months", "Ethnic_Uyghur", "Journalist",
                                           "Worker", "Lawyer", "Christian", "Unemployed",
                                           "Other_ethnic_minorities", "Other_religious_groups",
                                           "Professor", "Student", "Ethnic_Tibetan",
                                           "FalunGong", "HCAP_MAX", "Muslim", "Entrepreneur", 
                                           "LCAP_MIN", "HCAP_MIN", "Buddhist", "Focalevent_detention",
                                           "LCAP_MAX", "Recidivists", "province_imprisoned_detained",
                                           "Year"))
subvars <- subvars[!is.na(subvars$Ethnic_Uyghur) & !is.na(subvars$Charged)  & !is.na(subvars$Male) & 
                     !is.na(subvars$survstatus) & !is.na(subvars$detentiontime_months) &  
                     !is.na(subvars$Journalist) & !is.na(subvars$Worker) & !is.na(subvars$Lawyer) &
                     !is.na(subvars$Christian) & !is.na(subvars$Unemployed) & !is.na(subvars$Professor) &
                     !is.na(subvars$Other_ethnic_minorities) & !is.na(subvars$Student) & 
                     !is.na(subvars$Ethnic_Tibetan) & !is.na(subvars$FalunGong) & !is.na(subvars$LCAP_MIN) &
                     !is.na(subvars$HCAP_MAX) & !is.na(subvars$Muslim) & !is.na(subvars$Entrepreneur) &
                     !is.na(subvars$Other_religious_groups) & 
                     !is.na(subvars$Recidivists) & !is.na(subvars$HCAP_MIN) & 
                     !is.na(subvars$LCAP_MAX) & !is.na(subvars$province_imprisoned_detained) & 
                     !is.na(subvars$Year) & !is.na(subvars$Buddhist) & 
                     !is.na(subvars$Focalevent_detention), ]

subsubvars <- c("Charged", "Male", "Journalist", "LCAP_MAX", "Recidivists", "Lawyer",
                "Focalevent_detention", "HCAP_MAX", "LCAP_MIN", "Worker", "Professor",
                "Unemployed", "Student", "Entrepreneur")


# Pre-process data with coarsened exact matching
imbalance(group=subvars$Ethnic_Uyghur, data=subvars[subsubvars])
matched_data <- matchit(Ethnic_Uyghur ~ Charged + Male + LCAP_MAX,
                        data = subvars,
                        method = "cem")
matched_data

#### See Figure 3 in the appendix ####
## pdf(file = "balance_plot.pdf")
plot(matched_data, interactive = FALSE)
## dev.off()
## pdf(file = "love_plot.pdf")
love.plot(matched_data)
## dev.off()

# Create new data frame with matched data
matched <- match.data(matched_data)
imbalance(group=matched$Ethnic_Uyghur, data=matched[subsubvars])

# Re-run survival model with twoway fixed effects after matching on Ethnic Uyghur-dummy
m_match1 <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX + HCAP_MIN + LCAP_MAX +
                    Ethnic_Tibetan + Ethnic_Uyghur + Other_ethnic_minorities + Muslim +
                    Christian + FalunGong + Buddhist + Other_religious_groups + Journalist +
                    Worker + Lawyer + Entrepreneur + Student + Professor + 
                    Charged + Male + Recidivists + Focalevent_detention + factor(Year) +
                    factor(province_imprisoned_detained), data = matched, weights = weights)
screenreg(m_match1)
#### See Table 4 in the appendix ####
stargazer(list(m_match1), apply.coef = exp, p.auto = FALSE, title = "Survival analysis after CEM matching", omit="factor\\(Year", add.lines=list(c("Year fixed effects", rep("YES", 1))))

m_match1 <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX + HCAP_MIN + LCAP_MAX +
                    Ethnic_Tibetan + Ethnic_Uyghur + Other_ethnic_minorities + Muslim +
                    Christian + FalunGong + Buddhist + Other_religious_groups + Journalist +
                    Worker + Lawyer + Entrepreneur + Student + Professor + 
                    Charged + Male + Recidivists + Focalevent_detention, data = matched, weights = weights)
#pdf(file = "matched.pdf")
plot_model(m_match1, title = "") + theme_minimal() + geom_hline(yintercept = 1) + ylab("Hazard ratios of releases")
#dev.off()

# Pre-process data with coarsened exact matching differently (using LCAP_MIN instead)
matched_data <- matchit(Ethnic_Uyghur ~ Charged + Male + LCAP_MIN,
                        data = subvars,
                        method = "cem")
matched_data
plot(matched_data, interactive = FALSE)

# Create new data frame with matched data
matched <- match.data(matched_data)

# Re-run survival model with twoway fixed effects after matching
m_match2 <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX + HCAP_MIN + LCAP_MAX +
                    Ethnic_Tibetan + Ethnic_Uyghur + Other_ethnic_minorities + Muslim +
                    Christian + FalunGong + Buddhist + Other_religious_groups + Journalist +
                    Worker + Lawyer + Entrepreneur + Student + Professor + 
                    Charged + Male + Recidivists + Focalevent_detention + factor(Year) +
                    factor(province_imprisoned_detained), data = matched, weights = weights)
screenreg(m_match2)

# Create dataframe without missings for matching (treat NAs as reference group)
subvars <- dplyr::select(final_pprisdat, c("survstatus", "Charged", "Male",  
                                           "detentiontime_months", "Ethnic_Uyghur", "Journalist",
                                           "Worker", "Lawyer", "Christian", "Unemployed",
                                           "Other_ethnic_minorities", "Other_religious_groups",
                                           "Professor", "Student", "Ethnic_Tibetan",
                                           "FalunGong", "HCAP_MAX", "Muslim", "Entrepreneur", 
                                           "LCAP_MIN", "HCAP_MIN", "Buddhist", "Focalevent_detention",
                                           "LCAP_MAX", "Recidivists", "province_imprisoned_detained",
                                           "Year"))
subvars <- subvars[!is.na(subvars$Ethnic_Uyghur) & !is.na(subvars$Charged)  & !is.na(subvars$Male) & 
                     !is.na(subvars$survstatus) & !is.na(subvars$detentiontime_months) &  
                     !is.na(subvars$Journalist) & !is.na(subvars$Worker) & !is.na(subvars$Lawyer) &
                     !is.na(subvars$Christian) & !is.na(subvars$Unemployed) & !is.na(subvars$Professor) &
                     !is.na(subvars$Other_ethnic_minorities) & !is.na(subvars$Student) & 
                     !is.na(subvars$Ethnic_Tibetan) & !is.na(subvars$FalunGong) & !is.na(subvars$LCAP_MIN) &
                     !is.na(subvars$HCAP_MAX) & !is.na(subvars$Muslim) & !is.na(subvars$Entrepreneur) &
                     !is.na(subvars$Other_religious_groups) & 
                     !is.na(subvars$Recidivists) & !is.na(subvars$HCAP_MIN) & 
                     !is.na(subvars$LCAP_MAX) & !is.na(subvars$province_imprisoned_detained) & 
                     !is.na(subvars$Year) & !is.na(subvars$Buddhist) & 
                     !is.na(subvars$Focalevent_detention), ]

# Pre-process data with coarsened exact matching
matched_data <- matchit(Ethnic_Uyghur ~ Charged + Male + LCAP_MAX,
                        data = subvars,
                        method = "cem")
matched_data
plot(matched_data, interactive = FALSE)

# Create new data frame with matched data
matched <- match.data(matched_data)

# Re-run survival model with twoway fixed effects after matching
m_match3 <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX + HCAP_MIN + LCAP_MAX +
              LCAP_MIN + Ethnic_Tibetan + Ethnic_Uyghur + Other_ethnic_minorities + Muslim +
              Christian + FalunGong + Buddhist + Other_religious_groups + Journalist +
              Worker + Lawyer + Entrepreneur + Student + Professor + 
              Charged + Male + Recidivists + Focalevent_detention + factor(Year) +
              factor(province_imprisoned_detained), data = matched, weights = weights)
screenreg(m_match3)

# Pre-process data with coarsened exact matching differently
matched_data <- matchit(Ethnic_Uyghur ~ Charged + Male + LCAP_MIN,
                        data = subvars,
                        method = "cem")
matched_data
plot(matched_data, interactive = FALSE)

# Create new data frame with matched data
matched <- match.data(matched_data)

# Re-run survival model with twoway fixed effects after matching
m_match4 <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX + HCAP_MIN + LCAP_MAX +
                    LCAP_MIN + Ethnic_Tibetan + Ethnic_Uyghur + Other_ethnic_minorities + Muslim +
                    Christian + FalunGong + Buddhist + Other_religious_groups + Journalist +
                    Worker + Lawyer + Entrepreneur + Student + Professor + 
                    Charged + Male + Recidivists + Focalevent_detention + factor(Year) +
                    factor(province_imprisoned_detained), data = matched, weights = weights)
screenreg(m_match4)

# Subset analysis (only charged individuals)
onlycharged <- filter(notmiss, Charged == 1)
m12 <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX + HCAP_MIN + LCAP_MAX +
              Ethnic_Tibetan + Ethnic_Uyghur + Other_ethnic_minorities + Muslim +
              Christian + FalunGong + Buddhist + Other_religious_groups + Journalist +
              Worker + Lawyer + Entrepreneur + Student + Professor + 
              Sex + Recidivists + Focalevent_detention + 
              factor(province_imprisoned_detained), data = onlycharged)
screenreg(m12)

m12 <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX + HCAP_MIN + LCAP_MAX +
               Ethnic_Tibetan + Ethnic_Uyghur + Other_ethnic_minorities + Muslim +
               Christian + FalunGong + Buddhist + Other_religious_groups + Journalist +
               Worker + Lawyer + Entrepreneur + Student + Professor + 
               Sex + Recidivists + Focalevent_detention, data = onlycharged)
#### See Figure 2 in the appendix ####
#pdf(file = "onlycharged.pdf")
plot_model(m12, title = "") + theme_minimal() + geom_hline(yintercept = 1) + ylab("Hazard ratios of releases")
#dev.off()

# Subset analysis (only extrajudicially detained individuals)
extrajudicial <- filter(final_pprisdat, Charged == 0) ### too few cases in notmiss-data
m13 <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX + HCAP_MIN + LCAP_MAX +
               Ethnic_Tibetan + Ethnic_Uyghur + Other_ethnic_minorities + Muslim +
               Christian + FalunGong + Buddhist + Journalist +
               Worker + Lawyer + Entrepreneur + Student + Professor + 
               Sex + Recidivists + Focalevent_detention + factor(Year) +
               factor(province_imprisoned_detained), data = extrajudicial)
screenreg(m13)

m13 <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX + HCAP_MIN + LCAP_MAX +
               Ethnic_Tibetan + Ethnic_Uyghur + Other_ethnic_minorities + Muslim +
               Christian + FalunGong + Buddhist + Journalist +
               Worker + Lawyer + Entrepreneur + Student + Professor + 
               Sex + Recidivists + Focalevent_detention, data = extrajudicial)
#### See Figure 2 in the appendix ####
##pdf(file = "onlyextrajud.pdf")
plot_model(m13, title = "") + theme_minimal() + geom_hline(yintercept = 1) + ylab("Hazard ratios of releases")
##dev.off()

# Subset analysis for LCAP_MIN
lcapmin <- notmiss[notmiss$LCAP_MIN == 1, ]
m14 <- coxph(Surv(detentiontime_months, survstatus) ~ Ethnic_Tibetan + Ethnic_Uyghur + Other_ethnic_minorities + Muslim +
              Christian + FalunGong + Buddhist + Other_religious_groups + Journalist +
              Worker + Lawyer + Entrepreneur + Student + Professor + 
              Charged + Sex + Recidivists + Focalevent_detention +
              factor(province_imprisoned_detained), data = lcapmin)
screenreg(m14)

m14 <- coxph(Surv(detentiontime_months, survstatus) ~ Ethnic_Tibetan + Ethnic_Uyghur + Other_ethnic_minorities + Muslim +
               Christian + FalunGong + Buddhist + Other_religious_groups + Journalist +
               Worker + Lawyer + Entrepreneur + Student + Professor + 
               Charged + Sex, data = lcapmin)
screenreg(m14)
#### See Figure 1 in the appendix ####
## pdf(file = "onlylowcapmin.pdf")
plot_model(m14, title = "") + theme_minimal() + geom_hline(yintercept = 1) + ylab("Hazard ratios of releases")
## dev.off()

# Test independently maximalist demands and HCAP
notmiss$maximalist <- 0
notmiss$maximalist[notmiss$LCAP_MAX == 1] <- 1
notmiss$maximalist[notmiss$HCAP_MAX == 1] <- 1
notmiss$HCAP <- 0
notmiss$HCAP[notmiss$HCAP_MAX == 1] <- 1
notmiss$HCAP[notmiss$HCAP_MIN == 1] <- 1
m16 <- coxph(Surv(detentiontime_months, survstatus) ~ maximalist + 
              Ethnic_Tibetan + Ethnic_Uyghur + Other_ethnic_minorities + Muslim +
              Christian + FalunGong + Buddhist + Other_religious_groups + Journalist +
              Worker + Lawyer + Entrepreneur + Student + Professor + 
              Charged + Sex + Recidivists + Focalevent_detention + factor(Year) +
              factor(province_imprisoned_detained), data = notmiss)
screenreg(m16)
m16b <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP +
               Ethnic_Tibetan + Ethnic_Uyghur + Other_ethnic_minorities + Muslim +
               Christian + FalunGong + Buddhist + Other_religious_groups + Journalist +
               Worker + Lawyer + Entrepreneur + Student + Professor + 
               Charged + Sex + Recidivists + Focalevent_detention + factor(Year) +
             b  factor(province_imprisoned_detained), data = notmiss)
screenreg(m16)

# Are ethnic groups significantly over-represented in the criminalized action groups
check1 <- glm(HCAP_MIN ~ Ethnic_Uyghur + Ethnic_Tibetan + Other_ethnic_minorities, family=binomial(link="logit"),  
              data = notmiss)
screenreg(check1)
check2 <- glm(HCAP_MAX ~ Ethnic_Uyghur + Ethnic_Tibetan + Other_ethnic_minorities, family=binomial(link="logit"),  
              data = notmiss)
screenreg(check2)
check3 <- glm(LCAP_MAX ~ Ethnic_Uyghur + Ethnic_Tibetan + Other_ethnic_minorities, family=binomial(link="logit"),  
              data = notmiss)
screenreg(check3)
check4 <- glm(LCAP_MIN ~ Ethnic_Uyghur + Ethnic_Tibetan + Other_ethnic_minorities, family=binomial(link="logit"),  
              data = notmiss)
screenreg(check4)
#### See Table 11 in the appendix ####
stargazer(list(check1, check2, check3, check4), title = "Logistic regression")

# Impute missings with Amelia for ethnicity and criminalized actions
## Code as NA when information is not available
final_pprisdat$HCAP_MAX_nm <- ifelse(is.na(final_pprisdat$crim_act), 
                                     NA, final_pprisdat$HCAP_MAX)
final_pprisdat$LCAP_MAX_nm <- ifelse(is.na(final_pprisdat$crim_act), 
                                     NA, final_pprisdat$LCAP_MAX)
final_pprisdat$HCAP_MIN_nm <- ifelse(is.na(final_pprisdat$crim_act), 
                                     NA, final_pprisdat$HCAP_MIN)
final_pprisdat$LCAP_MIN_nm <- ifelse(is.na(final_pprisdat$crim_act), 
                                     NA, final_pprisdat$LCAP_MIN)
final_pprisdat$Ethnic_Uyghur_nm <- ifelse(is.na(final_pprisdat$ethnicity),
                                          NA, final_pprisdat$Ethnic_Uyghur)
final_pprisdat$Ethnic_Tibetan_nm <- ifelse(is.na(final_pprisdat$ethnicity),
                                          NA, final_pprisdat$Ethnic_Tibetan)
final_pprisdat$Other_ethnic_minorities_nm <- ifelse(is.na(final_pprisdat$ethnicity),
                                           NA, final_pprisdat$Other_ethnic_minorities)

final_pprisdat$Year <- as.numeric(final_pprisdat$Year)

# Run Amelia-Imputation
library(Amelia)
imputation_data <- dplyr::select(final_pprisdat, HCAP_MIN_nm, HCAP_MAX_nm, LCAP_MAX_nm, Ethnic_Uyghur_nm,
                                 Ethnic_Tibetan_nm, Other_ethnic_minorities_nm, rural, environ,
                                 relig, Democracy_related, Economy_related, Charged,
                                 PSB_detention, FalunGong, Muslim, Christian, Buddhist, Other_religious_groups,
                                 Unemployed, Journalist, occupation_artist, Student, Professor,
                                 Cleric, NGO_activist, occupation_farmer,
                                 Entrepreneur, Worker, Lawyer, occupation_doctor, Focalevent_detention,
                                 Male, Year, Recidivists, province_imprisoned_detained,
                                 CECC_record_number, detentiontime_months, survstatus)
imputation_data <- filter(imputation_data, !is.na(province_imprisoned_detained))
set.seed(987654321)
a.out <- Amelia::amelia(imputation_data, m = 5, idvars = c("CECC_record_number", "province_imprisoned_detained",
                                                           "detentiontime_months", "survstatus"))
# save(a.out, file = "imputed_ppris.RData")

# Create missingness map
keyvariables <- dplyr::select(final_pprisdat, HCAP_MIN_nm, HCAP_MAX_nm, LCAP_MAX_nm, Ethnic_Uyghur_nm,
                              Ethnic_Tibetan_nm, Male, Year, Charged)
keyvariables <- dplyr::rename(keyvariables, HCAP_MIN = HCAP_MIN_nm)
keyvariables <- dplyr::rename(keyvariables, HCAP_MAX = HCAP_MAX_nm)
keyvariables <- dplyr::rename(keyvariables, LCAP_MAX = LCAP_MAX_nm)
keyvariables <- dplyr::rename(keyvariables, Ethnic_Tibetan = Ethnic_Tibetan_nm)
keyvariables <- dplyr::rename(keyvariables, Ethnic_Uyghur = Ethnic_Uyghur_nm)
#### See Figure 7 in the appendix ####
##pdf(file = "missmap.pdf")
missmap(keyvariables, margins = c(5, 8))
##dev.off()

# Re-run main model with imputed values
m17 <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX_nm + HCAP_MIN_nm + LCAP_MAX_nm +
              Ethnic_Tibetan_nm + Ethnic_Uyghur_nm + Other_ethnic_minorities_nm + Muslim +
              Christian + FalunGong + Other_religious_groups + Journalist +
              Worker + Lawyer + Entrepreneur + Student + Professor + 
              Charged + Male + Recidivists + Focalevent_detention + factor(Year) +
              factor(province_imprisoned_detained), data = a.out$imputations$imp1)
screenreg(m17)
m17b <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX_nm + HCAP_MIN_nm + LCAP_MAX_nm +
               Ethnic_Tibetan_nm + Ethnic_Uyghur_nm + Other_ethnic_minorities_nm + Muslim +
               Christian + FalunGong + Other_religious_groups + Journalist +
               Worker + Lawyer + Entrepreneur + Student + Professor + 
               Charged + Male + Recidivists + Focalevent_detention + factor(Year) +
               factor(province_imprisoned_detained), data = a.out$imputations$imp2)
screenreg(m17b)
m17c <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX_nm + HCAP_MIN_nm + LCAP_MAX_nm +
                Ethnic_Tibetan_nm + Ethnic_Uyghur_nm + Other_ethnic_minorities_nm + Muslim +
                Christian + FalunGong + Other_religious_groups + Journalist +
                Worker + Lawyer + Entrepreneur + Student + Professor + 
                Charged + Male + Recidivists + Focalevent_detention + factor(Year) +
                factor(province_imprisoned_detained), data = a.out$imputations$imp3)
screenreg(m17c)
m17d <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX_nm + HCAP_MIN_nm + LCAP_MAX_nm +
                Ethnic_Tibetan_nm + Ethnic_Uyghur_nm + Other_ethnic_minorities_nm + Muslim +
                Christian + FalunGong + Other_religious_groups + Journalist +
                Worker + Lawyer + Entrepreneur + Student + Professor + 
                Charged + Male + Recidivists + Focalevent_detention + factor(Year) +
                factor(province_imprisoned_detained), data = a.out$imputations$imp4)
screenreg(m17d)
m17e <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX_nm + HCAP_MIN_nm + LCAP_MAX_nm +
                Ethnic_Tibetan_nm + Ethnic_Uyghur_nm + Other_ethnic_minorities_nm + Muslim +
                Christian + FalunGong + Other_religious_groups + Journalist +
                Worker + Lawyer + Entrepreneur + Student + Professor + 
                Charged + Male + Recidivists + Focalevent_detention + factor(Year) +
                factor(province_imprisoned_detained), data = a.out$imputations$imp4)
screenreg(m17e)
#### See Table 6 in the appendix ####
stargazer(list(m17e), apply.coef = exp, p.auto = FALSE, title = "Survival analysis", omit="factor\\(Year", add.lines=list(c("Year fixed effects", rep("YES", 1))))

# Change coding of recidivism 
### Create different identifiers
### 1) Name-ethnicity-religion
notmiss$prisoner_identifier <- paste(notmiss$main.name, "_", notmiss$ethnicity)
notmiss$prisoner_identifier_triple <- paste(notmiss$prisoner_identifier, "", notmiss$religion_coarse)
notmiss$prisoner_identifier_triple <- ifelse(str_detect(notmiss$prisoner_identifier_triple, "NA  unknown"), NA, notmiss$prisoner_identifier_triple)
notmiss$num <- ave(notmiss$tri_coarse, notmiss$prisoner_identifier_triple, FUN = seq_along)
notmiss$Recidivists2 <- ifelse(notmiss$num > 1, 1, 0)

### 2) Name-ethnicity-residence
notmiss$prisoner_identifier <- paste(notmiss$main.name, "_", notmiss$ethnicity)
notmiss$prisoner_identifier_triple <- paste(notmiss$prisoner_identifier, "", notmiss$residence_county)
notmiss$prisoner_identifier_triple <- ifelse(str_detect(notmiss$prisoner_identifier_triple, "NA  unknown"), NA, notmiss$prisoner_identifier_triple)
notmiss$num <- ave(notmiss$tri_coarse, notmiss$prisoner_identifier_triple, FUN = seq_along)
notmiss$Recidivists3 <- ifelse(notmiss$num > 1, 1, 0)

# Re-run main model (m4) with different codings of recidivism
m18 <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX + HCAP_MIN + LCAP_MAX +
                     Ethnic_Tibetan + Ethnic_Uyghur + Other_ethnic_minorities + Muslim +
                     Christian + FalunGong + Buddhist + Other_religious_groups + Journalist +
                     Worker + Lawyer + Entrepreneur + Student + Professor + Unemployed + 
                     Charged + Sex + Recidivists2 + Focalevent_detention + factor(Year) +
                     factor(province_imprisoned_detained), data = notmiss)
summary(m18)
screenreg(m18)

m19 <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX + HCAP_MIN + LCAP_MAX +
               Ethnic_Tibetan + Ethnic_Uyghur + Other_ethnic_minorities + Muslim +
               Christian + FalunGong + Buddhist + Other_religious_groups + Journalist +
               Worker + Lawyer + Entrepreneur + Student + Professor + Unemployed + 
               Charged + Sex + Recidivists3 + Focalevent_detention + factor(Year) +
               factor(province_imprisoned_detained), data = notmiss)
summary(m19)
screenreg(m19)

# Competing risk analysis (dying in prison as alternative outcome)
library(cmprsk)

### Create alternative duration variable
final_pprisdat$detentiontime_comp <- final_pprisdat$detentiontime_months

# Add prisoners that died from full CECC
cecc_died <- filter(cecc, died_in_prison == 1)
final_pprisdat <- full_join(final_pprisdat, cecc_died)

### calculate time until death in prison using "current_last_sentence_or_time_served_years"-variable
final_pprisdat$timeuntildeath <- ifelse(final_pprisdat$died_in_prison == 1,
                                        final_pprisdat$current_last_sentence_or_time_served_years * 12, NA)
final_pprisdat$timeuntildeath <- ifelse(final_pprisdat$died_in_prison == 1 & !is.na(final_pprisdat$current_last_sentence_or_time_served_months),
                                        final_pprisdat$current_last_sentence_or_time_served_months, final_pprisdat$timeuntildeath)
final_pprisdat$detentiontime_comp <- ifelse(!is.na(final_pprisdat$timeuntildeath), final_pprisdat$timeuntildeath, final_pprisdat$detentiontime_comp)

### Create event-variable (censored=0, died_in_prison=1, released=2)
final_pprisdat$event <- NA
final_pprisdat$event[final_pprisdat$currently_detained == 1] <- 0 
final_pprisdat$event[final_pprisdat$died_in_prison == 1] <- 1 
final_pprisdat$event[final_pprisdat$released == 1] <- 2
final_pprisdat$event <- as.factor(final_pprisdat$event) 

### Create data without missings that includes all dead prisoners
notmiss_dead <- final_pprisdat 
notmiss_dead$Ethnic_Tibetan <- ifelse(is.na(notmiss_dead$ethnicity) & notmiss_dead$died_in_prison != 1, NA,
                                     notmiss_dead$Ethnic_Tibetan)
notmiss_dead$Ethnic_Uyghur <- ifelse(is.na(notmiss_dead$ethnicity) & notmiss_dead$died_in_prison != 1, NA,
                                    notmiss_dead$Ethnic_Uyghur)
notmiss_dead$HCAP_MAX <- ifelse(is.na(notmiss_dead$crim_act) & notmiss_dead$died_in_prison != 1, NA,
                                     notmiss_dead$HCAP_MAX)
notmiss_dead$HCAP_MIN <- ifelse(is.na(notmiss_dead$crim_act) & notmiss_dead$died_in_prison != 1, NA,
                                notmiss_dead$HCAP_MIN)
notmiss_dead$LCAP_MAX <- ifelse(is.na(notmiss_dead$crim_act) & notmiss_dead$died_in_prison != 1, NA,
                                notmiss_dead$LCAP_MAX)

### Ethnic Uyghur as factor-variable
notmiss_dead$Ethnic_Uyghur <- as.factor(notmiss_dead$Ethnic_Uyghur)

### Change factor levels
levels(notmiss_dead$event)
levels(notmiss_dead$Ethnic_Uyghur)
library(plyr)
notmiss_dead$event <- revalue(notmiss_dead$event, c("0" = "detained", "1" = "died in prison", "2" = "released"))
notmiss_dead$Ethnic_Uyghur <- revalue(notmiss_dead$Ethnic_Uyghur, c("0" = "Other ethnic groups", "1" = "Ethnic Uyghurs"))

### Cumulative incidence functions
ci_fit <- cuminc(ftime = notmiss_dead$detentiontime_comp, fstatus = notmiss_dead$event)
ggcompetingrisks(ci_fit)
### Cumulative incidence function for Ethnic Uyghurs
ci_fit_uyghur <- survfit(Surv(detentiontime_comp, event, type = "mstate") ~ Ethnic_Uyghur, data = notmiss_dead)
ggcompetingrisks(ci_fit_uyghur, palette = "jco")
### Adjust label
notmiss_dead$u <- notmiss_dead$Ethnic_Uyghur
ci_fit_uyghur <- survfit(Surv(detentiontime_comp, event, type = "mstate") ~ u, data = notmiss_dead)
ci_fit_uyghur$states[ci_fit_uyghur$states == "(s0)"] <- "detained"
#### See Figure 8 in the article ####
#pdf(file = "ggcompetingriks.pdf")
ggcompetingrisks(ci_fit_uyghur, palette = "jco") + xlab("Time in months") +
  ggtitle("")
#dev.off()

### Change factor variables to numeric to run competing risk model
notmiss_dead$event <- revalue(notmiss_dead$event, c("detained" = "0", "died in prison" = "1", "released" = "2"))
notmiss_dead$Ethnic_Uyghur <- revalue(notmiss_dead$Ethnic_Uyghur, c("Other ethnic groups" = "0", "Ethnic Uyghurs" = "1"))
notmiss_dead$event <- as.numeric(as.character(notmiss_dead$event))
notmiss_dead$Ethnic_Uyghur <- as.numeric(as.character(notmiss_dead$Ethnic_Uyghur))

### Competing risk model
comprisk <- crr(notmiss_dead$detentiontime_comp, notmiss_dead$event, 
    cov1 = notmiss_dead[, c("Male", "Student", "Ethnic_Uyghur", "Ethnic_Tibetan", "Charged",
                           "HCAP_MAX", "Recidivists", "HCAP_MIN", "LCAP_MAX", "FalunGong",
                           "Journalist", "Worker", "Other_ethnic_minorities", "Professor",
                           "Buddhist", "Muslim", "Christian", "Other_religious_groups",
                           "Lawyer")])
print.crr(comprisk)
library(knitr)
#### See Table 7 in the appendix ####
broom::tidy(comprisk) %>% 
  knitr::kable()
glance(comprisk) %>% 
  knitr::kable(caption = "Table 2", digits = 3, exponentiate = TRUE)

# Check effects of ethnicity and religion without post-treatment covariates
noposttreatment <- coxph(Surv(detentiontime_months, survstatus) ~ Ethnic_Tibetan + Ethnic_Uyghur + Other_ethnic_minorities + Muslim +
              Christian + FalunGong + Buddhist + Other_religious_groups + 
              factor(Year) + factor(province_imprisoned_detained), data = notmiss)
screenreg(noposttreatment)
#### See Table 9 in the appendix ####
stargazer(noposttreatment, apply.coef = exp, p.auto = FALSE, title = "Survival analysis without post-treatment covariates", omit="factor\\(Year", add.lines=list(c("Year fixed effects", rep("YES", 1))))

# Test Cox Model Assumptions

### Test the proportional-hazards assumption for all models
test_ph <- cox.zph(m1)  
test_ph
ggcoxzph(test_ph)
test_ph <- cox.zph(m2)  
test_ph
ggcoxzph(test_ph)
test_ph <- cox.zph(m3)  
test_ph
ggcoxzph(test_ph)
test_ph <- cox.zph(m4)  
test_ph
ggcoxzph(test_ph)
test_ph <- cox.zph(m5)  
test_ph
ggcoxzph(test_ph)
test_ph <- cox.zph(m6)  
test_ph
ggcoxzph(test_ph)
test_ph <- cox.zph(m7)  
test_ph
ggcoxzph(test_ph)
test_ph <- cox.zph(m8)  
test_ph
ggcoxzph(test_ph)
test_ph <- cox.zph(m9)  
test_ph
ggcoxzph(test_ph)
test_ph <- cox.zph(m10)  
test_ph
ggcoxzph(test_ph)
test_ph <- cox.zph(m11)  
test_ph
ggcoxzph(test_ph)
test_ph <- cox.zph(m12)  
test_ph
ggcoxzph(test_ph)
test_ph <- cox.zph(m13)  
test_ph
ggcoxzph(test_ph)
test_ph <- cox.zph(m14)  
test_ph
ggcoxzph(test_ph)
test_ph <- cox.zph(m15)  
test_ph
ggcoxzph(test_ph)
test_ph <- cox.zph(m16)  
test_ph
ggcoxzph(test_ph)
test_ph <- cox.zph(m17)  
test_ph
ggcoxzph(test_ph)
test_ph <- cox.zph(m_match1)  
test_ph
ggcoxzph(test_ph)
test_ph <- cox.zph(m_match2)  
test_ph
ggcoxzph(test_ph)
test_ph <- cox.zph(m_match3)  
test_ph
ggcoxzph(test_ph)
test_ph <- cox.zph(m_match4)  
test_ph
ggcoxzph(test_ph)

# Testing influential observations
ggcoxdiagnostics(m1, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(m1, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(m2, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(m2, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(m3, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(m3, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(m4, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())
#### See Figure 6 in the appendix ####
# pdf(file = "deviance_residuals.pdf")
ggcoxdiagnostics(m4, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())
# dev.off()
ggcoxdiagnostics(m5, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(m5, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(m6, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(m6, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(m7, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(m7, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(m8, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(m8, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(m9, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(m9, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(m10, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(m10, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(m11, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(m11, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(m12, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(m12, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(m13, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(m13, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(m14, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(m14, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(m15, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(m15, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(m16, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(m16, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(m17, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(m17, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(m_match1, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(m_match1, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(m_match2, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(m_match2, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(m_match3, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(m_match3, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(m_match4, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(m_match4, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())

# Stratification to allow for non-proportionality of some covariates
m1 <- coxph(Surv(detentiontime_months, survstatus) ~ strata(Christian) + strata(Charged) +
              strata(LCAP_MAX)+ HCAP_MAX + HCAP_MIN +  
              Ethnic_Tibetan + Ethnic_Uyghur + Other_ethnic_minorities + Muslim + 
               + FalunGong + Buddhist + Other_religious_groups + Journalist +
              Worker + Lawyer + Entrepreneur + Student + Professor + Unemployed +
               Sex + Recidivists + Focalevent_detention, data = notmiss)
screenreg(m1)
test_ph <- cox.zph(m1)  
test_ph

# Stratification to allow for non-proportionality of some covariates
m2 <- coxph(Surv(detentiontime_months, survstatus) ~ strata(Charged) +
              strata(LCAP_MAX) + strata(factor(Year)) +
              HCAP_MAX + HCAP_MIN + 
              Ethnic_Tibetan + Ethnic_Uyghur + Other_ethnic_minorities + Muslim +
              Christian + FalunGong + Buddhist + Other_religious_groups + Journalist +
              Worker + Lawyer + Entrepreneur + Student + Professor + Unemployed +
              Sex + Recidivists + Focalevent_detention, data = notmiss)
screenreg(m2)
#### See Table 8 in the appendix ####
stargazer(m2, apply.coef = exp, p.auto = FALSE, title = "Survival analysis with stratified covariates", omit="factor\\(Year", add.lines=list(c("Year fixed effects", rep("YES", 1))))
test_ph <- cox.zph(m2)  
test_ph

# Stratification to allow for non-proportionality of some covariates
m_match1 <- coxph(Surv(detentiontime_months, survstatus) ~ HCAP_MAX + HCAP_MIN + LCAP_MAX +
                    Ethnic_Tibetan + Ethnic_Uyghur + Other_ethnic_minorities + strata(Muslim) +
                    Christian + FalunGong + Buddhist + Other_religious_groups + Journalist +
                    Worker + Lawyer + Entrepreneur + Student + Professor + 
                    strata(Charged) + Male + Recidivists  +
                    strata(factor(province_imprisoned_detained)), data = matched, weights = weights)
screenreg(m_match1)
test_ph <- cox.zph(m_match1)  
test_ph

# Prepare CECC dataset for logistic regression analysis
cecc$Other_ethnic_minorities <- 0
cecc$Other_ethnic_minorities[cecc$ethnic_group == "Ewenki"] <- 1
cecc$Other_ethnic_minorities[cecc$ethnic_group == "Hui"] <- 1
cecc$Other_ethnic_minorities[cecc$ethnic_group == "Kazak"] <- 1
cecc$Other_ethnic_minorities[cecc$ethnic_group == "Kirgiz"] <- 1
cecc$Other_ethnic_minorities[cecc$ethnic_group == "Korean"] <- 1
cecc$Other_ethnic_minorities[cecc$ethnic_group == "Manchu"] <- 1
cecc$Other_ethnic_minorities[cecc$ethnic_group == "Miao"] <- 1
cecc$Other_ethnic_minorities[cecc$ethnic_group == "Mongol"] <- 1
cecc$Other_ethnic_minorities[cecc$ethnic_group == "She"] <- 1
cecc$Other_ethnic_minorities[cecc$ethnic_group == "Tatar"] <- 1
cecc$Other_ethnic_minorities[cecc$ethnic_group == "Tujia"] <- 1
cecc$Other_ethnic_minorities[cecc$ethnic_group == "Uzbek"] <- 1
cecc$Other_ethnic_minorities[cecc$ethnic_group == "Yao"] <- 1
cecc$Other_ethnic_minorities[cecc$ethnic_group == "Yi"] <- 1
cecc$Other_religious_groups <- 0
cecc$Other_religious_groups[cecc$religion_coarse == "Mentu Hui"] <- 1
cecc$Other_religious_groups[cecc$religion_coarse == "Yi Guan Dao"] <- 1
cecc$Other_religious_groups[cecc$religion_coarse == "Eastern Lightning"] <- 1
cecc$Other_religious_groups[cecc$religion_coarse == "Lord God Religion"] <- 1
cecc$Other_religious_groups[cecc$religion_coarse == "Local Church "] <- 1
cecc$Other_religious_groups[cecc$religion_coarse == "Jehovah's Witness"] <- 1
cecc$Other_religious_groups[cecc$religion_coarse == "Full Scope Church"] <- 1

# Logistic regression models for deaths in prisons (with full dataset as too few Age-cases in reduced data)
logreg1 <- glm(died_in_prison ~ Ethnic_Tibetan + Ethnic_Uyghur + Age + Other_ethnic_minorities + Muslim +
            Christian + FalunGong + Buddhist + Journalist + Worker + Lawyer + Student + Professor +
            Charged + Sex + Recidivists, data = cecc, family = binomial(link = logit))
summary(logreg1)
#### See Table 10 in the appendix ####
stargazer(logreg1)
screenreg(logreg1)
#### See Figure 7 in the article ####
#pdf(file = "Logit-results.pdf")
plot_model(logreg1, title = "") +  geom_hline(yintercept = 1) + theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
#dev.off()




