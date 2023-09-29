###### CHRD validation ###
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
library(cem)
library(sampleSelection)
library(stargazer)

# Load data
rm(list = ls())
## setwd("") set your working directory here
chrd <- read.csv("CHRD_fullinfo.csv", na.strings = "")

# No missings on date variables
chrd_small <- filter(chrd, !is.na(rel.date))

# Prepare variables
chrd_small$det_dat <- as.Date(chrd_small$det.date, "%Y.%m.%d")
chrd_small$rel_dat <- as.Date(chrd_small$rel.date, "%Y.%m.%d")
chrd_small$duration <- chrd_small$rel_dat - chrd_small$det_dat

# Round to full days
chrd_small$duration <- ifelse(chrd_small$duration == 0, 1, chrd_small$duration)

# Histogram of duration variable to see distribution
hist(as.numeric(chrd_small$duration), border = "white", breaks = seq(from= 0, to = 144, by = 2),
     xlab = "Days in detention", main= "Distribution of detention times")

# Validation cases
table(chrd_small$cecc)

# Gender
chrd_small$female <- str_detect(chrd_small$gender, "female")
chrd_small$female <- as.numeric(chrd_small$female)

# Process detention type
chrd_small$tried <- str_detect(chrd_small$det.type, "Criminal detention")
chrd_small$tried <- as.numeric(chrd_small$tried)

# Recidivism
unique(chrd_small$Recidivism)
chrd_small$Recidivism <- ifelse(is.na(chrd_small$Recidivism), 0, chrd_small$Recidivism)

# Occupations
unique(chrd_small$Occupation)
chrd_small$Journ1 <- str_detect(chrd_small$Occupation, "Journalist")
chrd_small$Journ2 <- str_detect(chrd_small$Occupation, "Writer")
chrd_small$Journalists <- chrd_small$Journ1 + chrd_small$Journ2
chrd_small$Journ1 <- chrd_small$Journ2 <- NULL

# Religious groups
chrd_small$FalunGong <- str_detect(chrd_small$Religious.group, "Falun Gong")
chrd_small$FalunGong <- as.numeric(chrd_small$FalunGong)
chrd_small$Christian <- str_detect(chrd_small$Religious.group, "Christian")
chrd_small$Christian <- as.numeric(chrd_small$Christian)
chrd_small$religious_minorities <- 0
chrd_small$religious_minorities[chrd_small$FalunGong == 1] <- 1
chrd_small$religious_minorities[chrd_small$Christian == 1] <- 1

# Coding of criminalized actions
unique(chrd_small$Criminalized.action)
chrd_small$crim_act <- NA
chrd_small$crim_act[chrd_small$Criminalized.action == "Civil rights activist"] <- "LCAP_MIN"
chrd_small$crim_act[chrd_small$Criminalized.action == "Civil rights resistance"] <- "LCAP_MIN"
chrd_small$crim_act[chrd_small$Criminalized.action == "Civil rights activists"] <- "LCAP_MIN"
chrd_small$crim_act[chrd_small$Criminalized.action == "Civil rights leader"] <- "HCAP_MIN"
chrd_small$crim_act[chrd_small$Criminalized.action == "Active dissidents - travelled to UN"] <- "HCAP_MAX"
chrd_small$crim_act[chrd_small$Criminalized.action == "Religious belonging"] <- "LCAP_MIN"
chrd_small$crim_act[chrd_small$Criminalized.action == "Dissident activist"] <- "LCAP_MAX"
chrd_small$crim_act[chrd_small$Criminalized.action == "Interview of dissident"] <- "LCAP_MIN"
chrd_small$crim_act[chrd_small$Criminalized.action == "Dissident support"] <- "LCAP_MAX"
chrd_small$crim_act[chrd_small$Criminalized.action == "Active dissidents"] <- "LCAP_MAX"
chrd_small$crim_act[chrd_small$Criminalized.action == "Dissident in private"] <- "LCAP_MAX"
chrd_small$crim_act[chrd_small$Criminalized.action == "Civil right resistance"] <- "LCAP_MIN"
chrd_small$crim_act[chrd_small$Criminalized.action == "Civil rights activism"] <- "LCAP_MIN"
chrd_small$crim_act[chrd_small$Criminalized.action == "Civil rights mobilizer (helps petitioners)"] <- "HCAP_MIN"
chrd_small$crim_act[chrd_small$Criminalized.action == "Being naked"] <- "LCAP_MIN"
chrd_small$crim_act[chrd_small$Criminalized.action == "Active dissident"] <- "LCAP_MAX"
chrd_small$crim_act[chrd_small$Criminalized.action == "Dissident connections"] <- "LCAP_MAX"
chrd_small$crim_act[chrd_small$Criminalized.action == "Dissident leader"] <- "HCAP_MAX"
chrd_small$crim_act[chrd_small$Criminalized.action == "Journalism"] <- "HCAP_MIN"

# Create dummies for criminalized action
chrd_small$HCAP_MAX <- 0
chrd_small$HCAP_MAX[chrd_small$crim_act == "HCAP_MAX"] <- 1
chrd_small$HCAP_MIN <- 0
chrd_small$HCAP_MIN[chrd_small$crim_act == "HCAP_MIN"] <- 1
chrd_small$LCAP_MIN <- 0
chrd_small$LCAP_MIN[chrd_small$crim_act == "LCAP_MIN"] <- 1
### LCAP_MAX is the reference category

# Create bar plots
p1 <-ggplot(data = chrd_small, aes(x = crim_act, width = 0.3)) + 
  geom_bar() + theme_pubclean() + ylab("# of known Chinese political prisoners") +
  xlab("") + theme(axis.ticks.x = element_blank())
p_dt <- layer_data(p1)
p1 + annotate(geom = "text", label = p_dt$count, x = p_dt$x, y = p_dt$y + 2)

# Create survival-object
chrd_small$survstatus <- 0
surv_object <- Surv(chrd_small$duration, chrd_small$survstatus)

# Kaplan-Meier plots
plot(survfit(Surv(duration) ~ 1, data = chrd_small), 
     xlab = "days t in detention", 
     ylab = "S(t): probability of remaining detained",
)
t <- ggsurvplot(
  fit = survfit(Surv(duration) ~ 1, data = chrd_small),
  legend = "none",
  xlab = "days t in detention", 
  ylab = "S(t): probability of remaining detained", risk.table = T, ncensor.plot = TRUE,
  ncensor.plot.height = 0.25) 
t$plot + theme_minimal() + theme(legend.position = "none") + scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 
                                                                                           60, 70, 80, 90, 100, 110,
                                                                                           120, 130, 140))
                                                                                  

# Cox models of the duration of detention
m1 <- coxph(Surv(duration) ~ female + tried + Journalists + religious_minorities +
              Recidivism + HCAP_MAX + HCAP_MIN + LCAP_MIN, data = chrd_small)
screenreg(m1)

# Model the probability of inclusion
m1 <- glm(cecc ~ duration + religious_minorities + HCAP_MAX + HCAP_MIN + LCAP_MIN + tried + 
            female + Recidivism + Journalists, data = chrd_small, family = binomial(link = logit))
screenreg(m1)

m2 <- glm(cecc ~ duration * religious_minorities + HCAP_MAX + HCAP_MIN + LCAP_MIN + tried + 
            female + Recidivism + Journalists, 
           data = chrd_small, family = binomial(link = logit))
screenreg(m2)

m3 <- glm(cecc ~ duration * HCAP_MIN + religious_minorities + HCAP_MAX + LCAP_MIN + tried + 
            female + Recidivism + Journalists, 
          data = chrd_small, family = binomial(link = logit))
screenreg(m3)

m4 <- glm(cecc ~ duration * LCAP_MIN + religious_minorities + HCAP_MAX + HCAP_MIN + tried + 
            female + Recidivism + Journalists, 
          data = chrd_small, family = binomial(link = logit))
screenreg(m4)
models <- list(m1, m2, m3, m4)
#### See Table 13 in the appendix ####
stargazer(models, odd.ratio = T) 

# Check interaction of duration and criminalized actions
m3 <- glm(cecc ~ duration * crim_act + religious_minorities + tried + female + Recidivism  + Journalists, 
          data = chrd_small, family = binomial(link = logit))
screenreg(m3)

