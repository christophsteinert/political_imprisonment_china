###### Data Preparation ###
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

# Load data
rm(list = ls())
cecc <- read.csv("CECC_Version_23.07.2020.csv", na.strings = "")

# Prepare data
cecc <- rename(cecc, age = age.at.detention)
cecc <- rename(cecc, charge_statute = charge..statute.)
cecc <- rename(cecc, CECC_record_number = CECC.record.number)
cecc <- rename(cecc, detention_status = detention.status)
cecc <- rename(cecc, issue_category = issue.category)
cecc <- rename(cecc, ethnic_group = ethnic.group)
cecc <- rename(cecc, residence_province = residence.province)
cecc <- rename(cecc, residence_prefecture = residence.prefecture)
cecc <- rename(cecc, residence_county = residence.county)
cecc <- rename(cecc, date_of_detention = date.of.detention)
cecc <- rename(cecc, current_last_prison_detentioncenter = current..or.last..prison..detention.center..or.site)
cecc <- rename(cecc, current_last_sentence_or_time_served_years = current..or.last..sentence..or.time.served...years)
cecc <- rename(cecc, current_last_sentence_or_time_served_months = current..or.last..sentence..or.time.served...months)
cecc <- rename(cecc, current_last_sentence_or_time_served_weeks = current..or.last..sentence..or.time.served...weeks)
cecc <- rename(cecc, current_last_sentence_or_time_served_days = current..or.last..sentence..or.time.served...days)
cecc <- rename(cecc, province_imprisoned_detained = province.where.imprisoned..or.detained.)
cecc <- rename(cecc, county_imprisoned_detained = county.where.imprisoned..or.detained.)
cecc <- rename(cecc, prefecture_imprisoned_detained = prefecture.where.imprisoned..or.detained.)
cecc <- rename(cecc, legal_process = legal.process)
cecc <- rename(cecc, formal_arrest_date = formal.arrest.date)
cecc <- rename(cecc, trial_court = trial.court)
cecc <- rename(cecc, trial_date  = trial.date)
cecc <- rename(cecc, sentence_court = sentence.court)
cecc <- rename(cecc, sentence_date = sentence.date)
cecc <- rename(cecc, appeal_court = appeal.court)
cecc <- rename(cecc, appeal_date = appeal.date)
cecc <- rename(cecc, appeal_ruling_court = appeal.ruling.court)
cecc <- rename(cecc, appeal_ruling_date = appeal.ruling.date)
cecc <- rename(cecc, sentence_ends_per_PRC = sent..ends.per.PRC)
cecc <- rename(cecc, actual_date_released = actual.date.released)
cecc <- rename(cecc, short_summary = short.summary)
cecc$sentence_date <- as.character(cecc$sentence_date)
cecc$sentence_dat <- as.Date(cecc$sentence_date, "%Y/%m/%d")
cecc$trial_dat <- as.character(cecc$trial_date)
cecc$trial_dat <- as.Date(cecc$trial_date, "%Y/%m/%d")
cecc$formal_arrest_dat <- as.character(cecc$formal_arrest_date)
cecc$formal_arrest_dat <- as.Date(cecc$formal_arrest_date, "%Y/%m/%d")
cecc$detention_dat <- as.character(cecc$date_of_detention)
cecc$detention_dat <- as.Date(cecc$detention_dat, "%Y/%m/%d")
cecc$appeal_dat <- as.character(cecc$appeal_date)
cecc$appeal_dat <- as.Date(cecc$appeal_dat, "%Y/%m/%d")
cecc$appeal_ruling_dat <- as.character(cecc$appeal_ruling_date)
cecc$appeal_ruling_dat <- as.Date(cecc$appeal_ruling_dat, "%Y/%m/%d")
cecc$actual_dat_released <- as.character(cecc$actual_date_released)
cecc$actual_dat_released <- as.Date(cecc$actual_dat_released, "%Y/%m/%d")
cecc$legal_processes <- as.character(cecc$legal_process)

# Create separate variables for four different phases of legal process
cecc <- separate(cecc, col = legal_processes, into = c("phase1", "phase2", "phase3", "phase4"), sep = "/")
cecc$phase1_alleged <- gsub("\\?","", cecc$phase1)
cecc$phase2_alleged <- gsub("\\?","", cecc$phase2)
cecc$phase3_alleged <- gsub("\\?","", cecc$phase3)
cecc$phase4_alleged <- gsub("\\?","", cecc$phase4)

# Create separate variables for different charges
cecc$charge <- as.character(cecc$charge_statute)
cecc <- separate(cecc, col = charge, into = c("charge1", "charge2", "charge3", "charge4"), sep = "/")

# Get at time differences
cecc$detention_to_formalarrest <- cecc$formal_arrest_dat - cecc$detention_dat
cecc$detention_to_trial <- cecc$trial_dat - cecc$detention_dat
cecc$detention_to_sentence <- cecc$sentence_dat - cecc$detention_dat
cecc$detention_to_release <- cecc$actual_dat_released - cecc$detention_dat

# Create coarse charge-statute-variable that does not include subparagraphs
cecc$charge_statute_coarse <- gsub("\\([^\\)]+\\)", "", cecc$charge_statute)

# Issue categories: Create separate variables 
cecc$ethnic <- str_detect(cecc$issue_category, "eth")
cecc$ethnic[cecc$ethnic == T] <- 1
cecc$ethnic[cecc$ethnic == F] <- 0
cecc$environ <- str_detect(cecc$issue_category, "enviro")
cecc$environ[cecc$environ == T] <- 1
cecc$environ[cecc$environ == F] <- 0
cecc$relig <- str_detect(cecc$issue_category, "rel")
cecc$relig[cecc$relig == T] <- 1
cecc$relig[cecc$relig == F] <- 0
cecc$falung <- str_detect(cecc$issue_category, "FG")
cecc$falung[cecc$falung == T] <- 1
cecc$falung[cecc$falung == F] <- 0
cecc$illegal <- str_detect(cecc$issue_category, "rol")
cecc$illegal[cecc$illegal == T] <- 1
cecc$illegal[cecc$illegal == F] <- 0
cecc$rural <- str_detect(cecc$issue_category, "rur")
cecc$rural[cecc$rural == T] <- 1
cecc$rural[cecc$rural == F] <- 0
cecc$assist <- str_detect(cecc$issue_category, "assist")
cecc$assist[cecc$assist == T] <- 1
cecc$assist[cecc$assist == F] <- 0
cecc$democr1 <- str_detect(cecc$issue_category, "dem")
cecc$democr2 <- str_detect(cecc$issue_category, "civil")
cecc$democr3 <- str_detect(cecc$issue_category, "assoc")
cecc$democr4 <- str_detect(cecc$issue_category, "info")
cecc$democr5 <- str_detect(cecc$issue_category, "spch")
cecc$democr6 <- str_detect(cecc$issue_category, "6489")
cecc$democ_combine <- cecc$democr1 + cecc$democr2 + cecc$democr3 + cecc$democr4 + cecc$democr5 + cecc$democr6
cecc$democ <- 1
cecc$democ[cecc$democ_combine == 0] <- 0
cecc$democ_combine <- cecc$democr1 <- cecc$democr2 <- cecc$democr3 <- cecc$democr4 <- cecc$democr5 <- cecc$democr6 <- NULL 
cecc$econ1 <- str_detect(cecc$issue_category, "com")
cecc$econ2 <- str_detect(cecc$issue_category, "lab")
cecc$econ3 <- str_detect(cecc$issue_category, "prop")
cecc$econ_combin <- cecc$econ1 + cecc$econ2 + cecc$econ3
cecc$econ <- 1
cecc$econ[cecc$econ_combin == 0] <- 0
cecc$econ_combin <- cecc$econ1 <- cecc$econ2 <- cecc$econ3 <- NULL

# Legal Processes
### Phase 1

### Create coarse charge-variable that also includes alleged charges
cecc$chg_coarse <- str_detect(cecc$legal_process, "chg") # here chg and chg? included
cecc$chg_coarse[cecc$chg_coarse == T] <- 1
cecc$chg_coarse[cecc$chg_coarse == F] <- 0
cecc$chg_alleged <- str_detect(cecc$legal_process, "chg\\?")
cecc$chg_alleged[cecc$chg_alleged == T] <- 1
cecc$chg_alleged[cecc$chg_alleged == F] <- 0
cecc$chg <- cecc$chg_coarse - cecc$chg_alleged

### Create coarse PSB-variable that also includes alleged PSB
cecc$PSB_coarse <- str_detect(cecc$phase1, "PSB") # here PSB and PSB? included
cecc$PSB_coarse[cecc$PSB_coarse == T] <- 1
cecc$PSB_coarse[cecc$PSB_coarse == F] <- 0
cecc$PSB_alleged <- str_detect(cecc$phase1, "PSB\\?")
cecc$PSB_alleged[cecc$PSB_alleged == T] <- 1
cecc$PSB_alleged[cecc$PSB_alleged == F] <- 0
cecc$PSB <- cecc$PSB_coarse - cecc$PSB_alleged

### Create coarse PSB-house-variable that also includes alleged PSB-house
cecc$PSB_house_coarse <- str_detect(cecc$phase1, "PSB-house") # here PSB-house and PSB-house? included
cecc$PSB_house_coarse[cecc$PSB_house_coarse == T] <- 1
cecc$PSB_house_coarse[cecc$PSB_house_coarse == F] <- 0
cecc$PSB_house_alleged <- str_detect(cecc$phase1, "PSB_house\\?")
cecc$PSB_house_alleged[cecc$PSB_house_alleged == T] <- 1
cecc$PSB_house_alleged[cecc$PSB_house_alleged == F] <- 0
cecc$PSB_house <- cecc$PSB_house_coarse - cecc$PSB_house_alleged

### Create coarse admin-variable that also includes alleged admin
cecc$admin_RTL_coarse <- str_detect(cecc$phase1, "admin-RTL") # here admin_RTL and admin_RTL? included
cecc$admin_RTL_coarse[cecc$admin_RTL_coarse == T] <- 1
cecc$admin_RTL_coarse[cecc$admin_RTL_coarse == F] <- 0
cecc$admin_RTL_alleged <- str_detect(cecc$phase1, "admin-RTL\\?")
cecc$admin_RTL_alleged[cecc$admin_RTL_alleged == T] <- 1
cecc$admin_RTL_alleged[cecc$admin_RTL_alleged == F] <- 0
cecc$admin_RTL <- cecc$admin_RTL_coarse - cecc$admin_RTL_alleged

cecc$admin1 <- str_detect(cecc$phase1, "admin-RTL") 
cecc$admin2 <- str_detect(cecc$phase1, "admin-RTL-app") 
cecc$admin3 <- str_detect(cecc$phase1, "admin-ankang") 
cecc$admin4 <- str_detect(cecc$phase1, "admin-other") 
cecc$admin5 <- str_detect(cecc$phase1, "admin-other-app")
cecc$admin6 <- str_detect(cecc$phase1, "admin-psych") 
cecc$admin7 <- str_detect(cecc$phase1, "admin-RTL-trans") 
cecc$admin_combine <- cecc$admin1 + cecc$admin2 + cecc$admin3 + cecc$admin4 + cecc$admin5+ cecc$admin6 + cecc$admin7
cecc$admin_all <- 1
cecc$admin_all[cecc$admin_combine == 0] <- 0
cecc$admin1 <- cecc$admin2 <- cecc$admin3 <- cecc$admin4 <- cecc$admin5 <- cecc$admin6 <- cecc$admin7 <- NULL


### Phase 2 of legal process

### Create coarse trial-variable that also includes alleged trials
cecc$tri_coarse <- str_detect(cecc$phase2, "tri") # here tri and tri? included
cecc$tri_coarse[cecc$tri_coarse == T] <- 1
cecc$tri_coarse[cecc$tri_coarse == F] <- 0
cecc$tri_alleged <- str_detect(cecc$phase2, "tri\\?")
cecc$tri_alleged[cecc$tri_alleged == T] <- 1
cecc$tri_alleged[cecc$tri_alleged == F] <- 0
cecc$tri <- cecc$tri_coarse - cecc$tri_alleged

### Create coarse trial-closed-variable that also includes alleged closed trials
cecc$tri_close_coarse <- str_detect(cecc$phase2, "tri-close") # here tri_close and tri_close? included
cecc$tri_close_coarse[cecc$tri_close_coarse == T] <- 1
cecc$tri_close_coarse[cecc$tri_close_coarse == F] <- 0
cecc$tri_close_alleged <- str_detect(cecc$phase2, "tri-close\\?")
cecc$tri_close_alleged[cecc$tri_close_alleged == T] <- 1
cecc$tri_close_alleged[cecc$tri_close_alleged == F] <- 0
cecc$tri_close <- cecc$tri_close_coarse - cecc$tri_close_alleged

### Create coarse trial-open-variable that also includes alleged open trials
cecc$tri_open_coarse <- str_detect(cecc$phase2, "tri-open") # here tri_open and tri_open? included
cecc$tri_open_coarse[cecc$tri_open_coarse == T] <- 1
cecc$tri_open_coarse[cecc$tri_open_coarse == F] <- 0
cecc$tri_open_alleged <- str_detect(cecc$phase2, "tri-open\\?")
cecc$tri_open_alleged[cecc$tri_open_alleged == T] <- 1
cecc$tri_open_alleged[cecc$tri_open_alleged == F] <- 0
cecc$tri_open <- cecc$tri_open_coarse - cecc$tri_open_alleged

# Trial retributive
cecc$tri_retri <- str_detect(cecc$phase2, "tri-retri") 
cecc$tri_retri[cecc$tri_retri == T] <- 1
cecc$tri_retri[cecc$tri_retri == F] <- 0

### Create coarse released-variable that also includes alleged releases
cecc$rel2_coarse <- str_detect(cecc$phase2, "rel") # here rel and rel? included
cecc$rel2_coarse[cecc$rel2_coarse == T] <- 1
cecc$rel2_coarse[cecc$rel2_coarse == F] <- 0
cecc$rel2_alleged <- str_detect(cecc$phase2, "rel\\?")
cecc$rel2_alleged[cecc$rel2_alleged == T] <- 1
cecc$rel2_alleged[cecc$rel2_alleged == F] <- 0
cecc$rel2 <- cecc$rel2_coarse - cecc$rel2_alleged

# House arrest
cecc$sent_house <- str_detect(cecc$phase2, "sent-house") 
cecc$sent_house[cecc$sent_house == T] <- 1
cecc$sent_house[cecc$sent_house == F] <- 0

### Create coarse escape-variable that also includes alleged escapes
# escaped
cecc$esc2_coarse <- str_detect(cecc$phase2, "esc") # here esc and esc? included
cecc$esc2_coarse[cecc$esc2_coarse == T] <- 1
cecc$esc2_coarse[cecc$esc2_coarse == F] <- 0
cecc$esc2_alleged <- str_detect(cecc$phase2, "esc\\?")
cecc$esc2_alleged[cecc$esc2_alleged == T] <- 1
cecc$esc2_alleged[cecc$esc2_alleged == F] <- 0
cecc$esc2 <- cecc$esc2_coarse - cecc$esc2_alleged

# deceased
cecc$dec2_coarse <- str_detect(cecc$phase2, "dec") # here dec and dec? included
cecc$dec2_coarse[cecc$dec_coarse == T] <- 1
cecc$dec2_coarse[cecc$dec_coarse == F] <- 0
cecc$dec2_alleged <- str_detect(cecc$phase2, "dec\\?")
cecc$dec2_alleged[cecc$dec2_alleged == T] <- 1
cecc$dec2_alleged[cecc$dec2_alleged == F] <- 0
cecc$dec_2 <- cecc$dec2_coarse - cecc$dec2_alleged

### Phase 3 of legal process
# sentence
cecc$sent_coarse <- str_detect(cecc$phase3, "sent") # here sent and sent? included
cecc$sent_coarse[cecc$sent_coarse == T] <- 1
cecc$sent_coarse[cecc$sent_coarse == F] <- 0
cecc$sent_alleged <- str_detect(cecc$phase3, "sent\\?")
cecc$sent_alleged[cecc$sent_alleged == T] <- 1
cecc$sent_alleged[cecc$sent_alleged == F] <- 0
cecc$sent <- cecc$sent_coarse - cecc$sent_alleged

# Sentence closed
cecc$sent_close_coarse <- str_detect(cecc$phase3, "sent-close") # here sent_close and sent_close? included
cecc$sent_close_coarse[cecc$sent_close_coarse == T] <- 1
cecc$sent_close_coarse[cecc$sent_close_coarse == F] <- 0
cecc$sent_close_alleged <- str_detect(cecc$phase3, "sent-close\\?")
cecc$sent_close_alleged[cecc$sent_close_alleged == T] <- 1
cecc$sent_close_alleged[cecc$sent_close_alleged == F] <- 0
cecc$sent_close <- cecc$sent_close_coarse - cecc$sent_close_alleged

# released
cecc$rel3_coarse <- str_detect(cecc$phase3, "rel") # here rel3 and rel3? included
cecc$rel3_coarse[cecc$rel3_coarse == T] <- 1
cecc$rel3_coarse[cecc$rel3_coarse == F] <- 0
cecc$rel3_alleged <- str_detect(cecc$phase3, "rel\\?")
cecc$rel3_alleged[cecc$rel3_alleged == T] <- 1
cecc$rel3_alleged[cecc$rel3_alleged == F] <- 0
cecc$rel3 <- cecc$rel3_coarse - cecc$rel3_alleged

# deceased
cecc$dec3 <- str_detect(cecc$phase3, "dec") 
cecc$dec3[cecc$dec3 == T] <- 1
cecc$dec3[cecc$dec3 == F] <- 0

# escaped
cecc$esc3 <- str_detect(cecc$phase3, "esc") 
cecc$esc3[cecc$esc3 == T] <- 1
cecc$esc3[cecc$esc3 == F] <- 0

### Phase 4 of legal process
# released
cecc$rel4_coarse <- str_detect(cecc$phase4, "rel") # here rel4 and rel4? included
cecc$rel4_coarse[cecc$rel4_coarse == T] <- 1
cecc$rel4_coarse[cecc$rel4_coarse == F] <- 0
cecc$rel4_alleged <- str_detect(cecc$phase4, "rel\\?")
cecc$rel4_alleged[cecc$rel4_alleged == T] <- 1
cecc$rel4_alleged[cecc$rel4_alleged == F] <- 0
cecc$rel4 <- cecc$rel4_coarse - cecc$rel4_alleged

# deceased
cecc$dec4_coarse <- str_detect(cecc$phase4, "dec") # here dec4 and dec4? included
cecc$dec4_coarse[cecc$dec4_coarse == T] <- 1
cecc$dec4_coarse[cecc$dec4_coarse == F] <- 0
cecc$dec4_alleged <- str_detect(cecc$phase4, "dec\\?")
cecc$dec4_alleged[cecc$dec4_alleged == T] <- 1
cecc$dec4_alleged[cecc$dec4_alleged == F] <- 0
cecc$dec4 <- cecc$dec4_coarse - cecc$dec4_alleged

# escaped
cecc$esc4 <- str_detect(cecc$phase4, "esc") 
cecc$esc4[cecc$esc4 == T] <- 1
cecc$esc4[cecc$esc4 == F] <- 0

# executed
cecc$exe4 <- str_detect(cecc$phase4, "exe") 
cecc$exe4[cecc$exe4 == T] <- 1
cecc$exe4[cecc$exe4 == F] <- 0

# Recode detention status
cecc$detention_status <- as.character(cecc$detention_status)

# Create binary indicator for released prisoners
cecc$release1 <- str_detect(cecc$detention_status, "REL")
cecc$release2 <- str_detect(cecc$detention_status, "REL/dec")
cecc$release3 <- str_detect(cecc$detention_status, "REL\\?")
cecc$release4 <- str_detect(cecc$detention_status, "REL-PSB")
cecc$release <- cecc$release1 + cecc$release2 + cecc$release3 + cecc$release4
cecc$released <- 1
cecc$released[cecc$release == 0] <- 0

# Create binary indicator for prisoners that died in prison
cecc$dead1 <- str_detect(cecc$detention_status, "DEC")
cecc$dead2 <- str_detect(cecc$detention_status, "DEC\\?")
cecc$dead3 <- str_detect(cecc$detention_status, "DEC-ill")
cecc$dead <- cecc$dead1 + cecc$dead2 + cecc$dead3 
cecc$died_in_prison <- 1
cecc$died_in_prison[cecc$dead == 0] <- 0

# Create binary indicator for prisoners that escaped
cecc$escape1 <- str_detect(cecc$detention_status, "ESC")
cecc$escape2 <- str_detect(cecc$detention_status, "ESC\\?")
cecc$escape <- cecc$escape1 + cecc$escape2
cecc$escaped <- 1
cecc$escaped[cecc$escape == 0] <- 0

# Create binary indicator for prisoners that were executed
cecc$executed <- str_detect(cecc$detention_status, "DEC-exe")
cecc$executed[cecc$executed == T] <- 1
cecc$executed[cecc$executed == F] <- 0

# Create binary indicator for prisoners that are still detained
cecc$currently_detained1 <- str_detect(cecc$detention_status, "DET")
cecc$currently_detained2 <- str_detect(cecc$detention_status, "DET/bail")
cecc$currently_detained3 <- str_detect(cecc$detention_status, "DET/bail\\?")
cecc$currently_detained4 <- str_detect(cecc$detention_status, "DET/dth-r")
cecc$currently_detained5 <- str_detect(cecc$detention_status, "DET/life")
cecc$currently_detained6 <- str_detect(cecc$detention_status, "DET/life\\?")
cecc$currently_detained7 <- str_detect(cecc$detention_status, "DET/med")
cecc$currently_detained8 <- str_detect(cecc$detention_status, "DET/med\\?")
cecc$currently_detained9 <- str_detect(cecc$detention_status, "DET/surveil")
cecc$currently_detained10 <- str_detect(cecc$detention_status, "DET/suspend")
cecc$currently_detained11 <- str_detect(cecc$detention_status, "DET\\?")
cecc$currently_detained12 <- str_detect(cecc$detention_status, "DET\\?/dth-r")
cecc$currently_detained13 <- str_detect(cecc$detention_status, "DET\\?/life")
cecc$currently_detained14 <- str_detect(cecc$detention_status, "DET?/life\\?")
cecc$currently_detain <- cecc$currently_detained1 + cecc$currently_detained2 + 
  cecc$currently_detained3 + cecc$currently_detained4 + cecc$currently_detained5 + 
  cecc$currently_detained6 + cecc$currently_detained7 + cecc$currently_detained8 +
  cecc$currently_detained9 + cecc$currently_detained10 + cecc$currently_detained11 +
  cecc$currently_detained12 + cecc$currently_detained13 + cecc$currently_detained14
cecc$currently_detained <- 1
cecc$currently_detained[cecc$currently_detain == 0] <- 0

# Expand detention_to_release variable with right-censored cases
strDates <- c("07/23/2020")
date_infer <- as.Date(strDates, "%m/%d/%Y") 
cecc$infer_currentlengths <- date_infer  - cecc$detention_dat
sub <- cecc$infer_currentlengths[cecc$currently_detained == 1]
cecc$detention_to_release[cecc$currently_detained == 1] <- sub

# Create alternative detention_to_release variable in months (includes cases where days are missing)
cecc$date_of_detention <- as.character(cecc$date_of_detention)
cecc$actual_date_released <- as.character(cecc$actual_date_released)
# detention date
cecc$date_of_detention_coarse <- gsub("dd","01", cecc$date_of_detention)
cecc$date_of_detention_coarse <- as.Date(cecc$date_of_detention_coarse, "%Y/%m/%d")
cecc$date_of_detention_coarse_unrounded <- cecc$date_of_detention_coarse # to identify detentions during focal events
cecc$date_of_detention_coarse <- round_date(cecc$date_of_detention_coarse, unit = "month") 
cecc$date_of_detention_coarse <- as.Date(cecc$date_of_detention_coarse, "%Y/%m/%d") 
# release date
cecc$date_of_release_coarse <- gsub("dd", "01", cecc$actual_date_released)
cecc$date_of_release_coarse <- as.Date(cecc$actual_date_released, "%Y/%m/%d")
cecc$date_of_release_coarse <- round_date(cecc$date_of_release_coarse, unit = "month")
cecc$date_of_release_coarse <- as.Date(cecc$date_of_release_coarse, "%Y/%m/%d")
cecc$detention_to_release_months <- cecc$date_of_release_coarse - cecc$date_of_detention_coarse

# Expand monthly detention_to_release variable with right-censored cases
cecc$infer_currentlengths <- date_infer  - cecc$date_of_detention_coarse
sub <- cecc$infer_currentlengths[cecc$currently_detained == 1]
cecc$detention_to_release_months[cecc$currently_detained == 1] <- sub

# Problem: Less than 15 days are rounded to 0. Therefore, replace with values of unrounded variable (both variables are still on a daily level)
cecc$detention_to_release_months <- ifelse(cecc$detention_to_release_months == 0, cecc$detention_to_release,
                                           cecc$detention_to_release_months)

# Outcome variable on a daily level: recode to monthly level
cecc$detentiontime_months <- cecc$detention_to_release_months / 30
cecc$detentiontime_months <- ifelse(cecc$detentiontime_months > 0.5, round(cecc$detentiontime_months, digits = 0), cecc$detentiontime_months)

# Create dummy variables for different ethnic groups
cecc$ethnic_group <- as.character(cecc$ethnic_group)
table(cecc$ethnic_group)

## Indicator for ethnic Han
cecc$han1 <- str_detect(cecc$ethnic_group, "Han")
cecc$han2 <- str_detect(cecc$ethnic_group, "Han\\?")
cecc$hanconnect <- cecc$han1 + cecc$han2
cecc$ethnic_han <- 1
cecc$ethnic_han[cecc$hanconnect == 0] <- 0
cecc$ethnic_han[is.na(cecc$hanconnect)] <- 0
cecc$hanconnect <- NULL

## Indicator for ethnic Tibetan
cecc$ethnic_tibetan <- str_detect(cecc$ethnic_group, "Tibetan")
cecc$ethnic_tibetan[cecc$ethnic_tibetan == T] <- 1
cecc$ethnic_tibetan[cecc$ethnic_tibetan == F] <- 0
cecc$ethnic_tibetan[is.na(cecc$ethnic_tibetan)] <- 0

## Indicator for ethnic Uyghur
cecc$ethnic_uyghur <- str_detect(cecc$ethnic_group, "Uyghur")
cecc$ethnic_uyghur[cecc$ethnic_uyghur == T] <- 1
cecc$ethnic_uyghur[cecc$ethnic_uyghur == F] <- 0
cecc$ethnic_uyghur[is.na(cecc$ethnic_uyghur)] <- 0

## Indicator for ethnic Mongol
cecc$ethnic_mongol <- str_detect(cecc$ethnic_group, "Mongol")
cecc$ethnic_mongol[cecc$ethnic_mongol == T] <- 1
cecc$ethnic_mongol[cecc$ethnic_mongol == F] <- 0
cecc$ethnic_mongol[is.na(cecc$ethnic_mongol)] <- 0

# Create dummy variables for different religions
cecc$religion <- as.character(cecc$religion)
cecc$religion_coarse <- gsub("\\([^\\)]+\\)", "", cecc$religion)

### Indicator for Buddhists
cecc$budd1 <- str_detect(cecc$religion_coarse, "Buddhist ")
cecc$budd2 <- str_detect(cecc$religion_coarse, "Chinese Buddhist")
cecc$budd3 <- str_detect(cecc$religion_coarse, "Tibetan Buddhist")
cecc$budd4 <- str_detect(cecc$religion_coarse, "Tibetan Buddhist ")
cecc$buddconnect <- cecc$budd1 + cecc$budd2 + cecc$budd3 + cecc$budd4
cecc$religion_buddhist <- 1
cecc$religion_buddhist[cecc$buddconnect == 0] <- 0
cecc$religion_buddhist[is.na(cecc$buddconnect)] <- 0
table(cecc$religion_buddhist)
cecc$buddconnect <- NULL

### Indicator for Falun Gong
cecc$religion_falungong <- str_detect(cecc$religion_coarse, "Falun Gong")
cecc$religion_falungong[cecc$religion_falungong == T] <- 1
cecc$religion_falungong[cecc$religion_falungong == F] <- 0
cecc$religion_falungong[is.na(cecc$religion_falungong)] <- 0

### Indicator for Muslims
cecc$musl1 <- str_detect(cecc$religion_coarse, "Muslim")
cecc$musl2 <- str_detect(cecc$religion_coarse, "Muslim ")
cecc$musconnect <- cecc$musl1 + cecc$musl2
cecc$religion_muslim <- 1
cecc$religion_muslim[cecc$musconnect == 0] <- 0
cecc$religion_muslim[is.na(cecc$musconnect)] <- 0
cecc$musconnect <- NULL

### Indicator for Christians
cecc$christ1 <- str_detect(cecc$religion_coarse, "Catholic ")
cecc$christ2 <- str_detect(cecc$religion_coarse, "Christian ")
cecc$christ3 <- str_detect(cecc$religion_coarse, "Protestant ")
cecc$christconnect <- cecc$christ1 + cecc$christ2 + cecc$christ3
cecc$religion_christian <- 1
cecc$religion_christian[cecc$christconnect == 0] <- 0
cecc$religion_christian[is.na(cecc$christconnect)] <- 0
cecc$christconnect <- NULL

# Create dummy variables for different occupations
cecc$occupation <- as.character(cecc$occupation)
cecc$occupation_coarse <- gsub("\\([^\\)]+\\)", "", cecc$occupation)

### Create dummy variable for unemployed individuals
cecc$unemp1 <- str_detect(cecc$occupation_coarse, "unemployed")
cecc$unemp2 <- str_detect(cecc$occupation_coarse, "worker, unemployed")
cecc$unemp3 <- str_detect(cecc$occupation_coarse, "student, unemployed")
cecc$unempconnect <- cecc$unemp1 + cecc$unemp2 + cecc$unemp3
cecc$occupation_unemployed <- 1
cecc$occupation_unemployed[cecc$unempconnect == 0] <- 0
cecc$occupation_unemployed[is.na(cecc$unempconnect)] <- 0
cecc$unempconnect <- NULL

### Create dummy variable for journalists
cecc$journ1 <- str_detect(cecc$occupation_coarse, "writer, political")
cecc$journ2 <- str_detect(cecc$occupation_coarse, "writer, news")
cecc$journ3 <- str_detect(cecc$occupation_coarse, "writer, multiple styles or types")
cecc$journ4 <- str_detect(cecc$occupation_coarse, "writer, intellectual")
cecc$journ5 <- str_detect(cecc$occupation_coarse, "writer, essayist")
cecc$journ6 <- str_detect(cecc$occupation_coarse, "writer, commentator")
cecc$journ7 <- str_detect(cecc$occupation_coarse, "writer, blogger")
cecc$journ8 <- str_detect(cecc$occupation_coarse, "writer, advocacy")
cecc$journ9 <- str_detect(cecc$occupation_coarse, "publishing, magazine")
cecc$journ10 <- str_detect(cecc$occupation_coarse, "journalist, writer and presenter")
cecc$journ11 <- str_detect(cecc$occupation_coarse, "journalist, television")
cecc$journ12 <- str_detect(cecc$occupation_coarse, "journalist, reporter")
cecc$journ13 <- str_detect(cecc$occupation_coarse, "journalist, radio")
cecc$journ14 <- str_detect(cecc$occupation_coarse, "journalist, photojournalism")
cecc$journ15 <- str_detect(cecc$occupation_coarse, "journalist, newspaper rep. \\& ed.")
cecc$journ16 <- str_detect(cecc$occupation_coarse, "journalist, newspaper")
cecc$journ17 <- str_detect(cecc$occupation_coarse, "journalist, news agency")
cecc$journ18 <- str_detect(cecc$occupation_coarse, "journalist, magazine")
cecc$journ19 <- str_detect(cecc$occupation_coarse, "journalist, journalist")
cecc$journ20 <- str_detect(cecc$occupation_coarse, "journalist, Internet")
cecc$journ21 <- str_detect(cecc$occupation_coarse, "journalist, freelance")
cecc$journ22 <- str_detect(cecc$occupation_coarse, "journalist, editor")
cecc$journ23 <- str_detect(cecc$occupation_coarse, "journalist ")
cecc$journ24 <- str_detect(cecc$occupation_coarse, "editor, newspaper")
cecc$journ25 <- str_detect(cecc$occupation_coarse, "editor, news agency")
cecc$journ26 <- str_detect(cecc$occupation_coarse, "editor, magazine")
cecc$journ27 <- str_detect(cecc$occupation_coarse, "editor, Internet")
cecc$journ28 <- str_detect(cecc$occupation_coarse, "editor, books")
cecc$journ29 <- str_detect(cecc$occupation_coarse, "editor ")
cecc$journconnect <- cecc$journ1 + cecc$journ2 + cecc$journ3 + cecc$journ4 + cecc$journ5 + 
  cecc$journ6 + cecc$journ7 + cecc$journ8 + cecc$journ9 + cecc$journ10 + cecc$journ11 + 
  cecc$journ12 + cecc$journ13 + cecc$journ14 + cecc$journ15 + cecc$journ16 + cecc$journ17 + 
  cecc$journ18 + cecc$journ19 + cecc$journ20 + cecc$journ21 + cecc$journ22 + cecc$journ23 + 
  cecc$journ24 + cecc$journ25 + cecc$journ26 + cecc$journ27 + cecc$journ28 + cecc$journ29
cecc$occupation_journalist <- 1
cecc$occupation_journalist[cecc$journconnect == 0] <- 0
cecc$occupation_journalist[is.na(cecc$journconnect)] <- 0
cecc$journconnect <- NULL

# Create dummy variable for artists
cecc$art1 <- str_detect(cecc$occupation_coarse, "artist ")
cecc$art2 <- str_detect(cecc$occupation_coarse, "artist, musician")
cecc$art3 <- str_detect(cecc$occupation_coarse, "artist, composer")
cecc$art4 <- str_detect(cecc$occupation_coarse, "artist, musician ")
cecc$art5 <- str_detect(cecc$occupation_coarse, "artist, painter")
cecc$art6 <- str_detect(cecc$occupation_coarse, "artist, painter ")
cecc$art7 <- str_detect(cecc$occupation_coarse, "artist, performance ")
cecc$art8 <- str_detect(cecc$occupation_coarse, "artist, sculptor")
cecc$art9 <- str_detect(cecc$occupation_coarse, "artist, singer")
cecc$art10 <- str_detect(cecc$occupation_coarse, "performer, actor")
cecc$art11 <- str_detect(cecc$occupation_coarse, "performer, actor & singer")
cecc$art12 <- str_detect(cecc$occupation_coarse, "performer, comedian")
cecc$art13 <- str_detect(cecc$occupation_coarse, "performer, musician")
cecc$art14 <- str_detect(cecc$occupation_coarse, "performer, musician ")
cecc$art15 <- str_detect(cecc$occupation_coarse, "performer, song")
cecc$art16 <- str_detect(cecc$occupation_coarse, "performer, song ")
cecc$art17 <- str_detect(cecc$occupation_coarse, "performer, song & dance")
cecc$art18 <- str_detect(cecc$occupation_coarse, "performer, traditional")
cecc$art19 <- str_detect(cecc$occupation_coarse, "producer, audio-video")
cecc$artconnect <- cecc$art1 + cecc$art2 + cecc$art3 + cecc$art4 + cecc$art5 + 
  cecc$art6 + cecc$art7 + cecc$art8 + cecc$art9 + cecc$art10 + cecc$art11 + 
  cecc$art12 + cecc$art13 + cecc$art14 + cecc$art15 + cecc$art16 + cecc$art17 + 
  cecc$art18 + cecc$art19
cecc$occupation_artist <- 1
cecc$occupation_artist[cecc$artconnect == 0] <- 0
cecc$occupation_artist[is.na(cecc$artconnect)] <- 0
cecc$artconnect <- NULL

# Create dummy-variable for students
cecc$stud1 <- str_detect(cecc$occupation_coarse, "student, vocational")
cecc$stud2 <- str_detect(cecc$occupation_coarse, "student, university")
cecc$stud3 <- str_detect(cecc$occupation_coarse, "student, unemployed")
cecc$stud4 <- str_detect(cecc$occupation_coarse, "student, religion")
cecc$stud5 <- str_detect(cecc$occupation_coarse, "student, primary")
cecc$stud6 <- str_detect(cecc$occupation_coarse, "student, middle")
cecc$stud7 <- str_detect(cecc$occupation_coarse, "student, high")
cecc$stud8 <- str_detect(cecc$occupation_coarse, "student, graduate")
cecc$stud9 <- str_detect(cecc$occupation_coarse, "artist, student ")
cecc$stud10 <- str_detect(cecc$occupation_coarse, "monk and student")
cecc$studconnect <- cecc$stud1 + cecc$stud2 + cecc$stud3 + cecc$stud4 + cecc$stud5 + 
  cecc$stud6 + cecc$stud7 + cecc$stud8 + cecc$stud9 + cecc$stud10 
cecc$occupation_student <- 1
cecc$occupation_student[cecc$studconnect == 0] <- 0
cecc$occupation_student[is.na(cecc$studconnect)] <- 0
cecc$studconnect <- NULL

# Create dummy-variable for professors
cecc$prof1 <- str_detect(cecc$occupation_coarse, "teacher, university ")
cecc$prof2 <- str_detect(cecc$occupation_coarse, "teacher, university")
cecc$prof3 <- str_detect(cecc$occupation_coarse, "teacher, Tibetan language")
cecc$prof4 <- str_detect(cecc$occupation_coarse, "teacher, tertiary")
cecc$prof5 <- str_detect(cecc$occupation_coarse, "teacher, researcher")
cecc$prof6 <- str_detect(cecc$occupation_coarse, "teacher, private")
cecc$prof7 <- str_detect(cecc$occupation_coarse, "teacher, primary")
cecc$prof8 <- str_detect(cecc$occupation_coarse, "teacher, middle")
cecc$prof9 <- str_detect(cecc$occupation_coarse, "teacher, high")
cecc$prof10 <- str_detect(cecc$occupation_coarse, "teacher, headmaster")
cecc$prof11 <- str_detect(cecc$occupation_coarse, "teacher, English")
cecc$prof12 <- str_detect(cecc$occupation_coarse, "teacher ")
cecc$prof13 <- str_detect(cecc$occupation_coarse, "teacher")
cecc$prof14 <- str_detect(cecc$occupation_coarse, "professor, literature")
cecc$prof15 <- str_detect(cecc$occupation_coarse, "professor, law; PC deputy")
cecc$prof16 <- str_detect(cecc$occupation_coarse, "professor, law")
cecc$prof17 <- str_detect(cecc$occupation_coarse, "professor, electronics")
cecc$prof18 <- str_detect(cecc$occupation_coarse, "professor ")
cecc$prof19 <- str_detect(cecc$occupation_coarse, "lecturer, university")
cecc$profconnect <- cecc$prof1 + cecc$prof2 + cecc$prof3 + cecc$prof4 + cecc$prof5 + 
  cecc$prof6 + cecc$prof7 + cecc$prof8 + cecc$prof9 + cecc$prof10 + cecc$prof11 + 
  cecc$prof12 + cecc$prof13 + cecc$prof14 + cecc$prof15 + cecc$prof16 + cecc$prof17 + 
  cecc$prof18 + cecc$prof19
cecc$occupation_professor <- 1
cecc$occupation_professor[cecc$profconnect == 0] <- 0
cecc$occupation_professor[is.na(cecc$profconnect)] <- 0
cecc$profconnect <- NULL

# Create dummy-variable for monks 
cecc$reli1 <- str_detect(cecc$occupation_coarse, "monk")
cecc$reli2 <- str_detect(cecc$occupation_coarse, "nun")
cecc$reli3 <- str_detect(cecc$occupation_coarse, "imam")
cecc$reli4 <- str_detect(cecc$occupation_coarse, "religious center , head")
cecc$reli5 <- str_detect(cecc$occupation_coarse, "priest, Catholic")
cecc$reli6 <- str_detect(cecc$occupation_coarse, "priest, Catholic ")
cecc$reli7 <- str_detect(cecc$occupation_coarse, "pastor, assistant")
cecc$reli8 <- str_detect(cecc$occupation_coarse, "pastor")
cecc$reli9 <- str_detect(cecc$occupation_coarse, "bishop")
cecc$reli10 <- str_detect(cecc$occupation_coarse, "bishop ")
cecc$reliconnect <- cecc$reli1 + cecc$reli2 + cecc$reli3 + cecc$reli4 + cecc$reli5 + 
  cecc$reli6 + cecc$reli7 + cecc$reli8 + cecc$reli9 + cecc$reli10 
cecc$occupation_religion <- 1
cecc$occupation_religion[cecc$reliconnect == 0] <- 0
cecc$occupation_religion[is.na(cecc$reliconnect)] <- 0
cecc$reliconnect <- NULL

# Create dummy-variable from Communist party cadre
cecc$ccp1 <- str_detect(cecc$occupation_coarse, "CCP, cadre")
cecc$ccp2 <- str_detect(cecc$occupation_coarse, "CCP, cadre ")
cecc$ccp_connect <- cecc$ccp1 + cecc$ccp2
cecc$occupation_ccp_cadre <- 1
cecc$occupation_ccp_cadre[cecc$ccp_connect == 0] <- 0
cecc$occupation_ccp_cadre[is.na(cecc$ccp_connect)] <- 0
cecc$ccp_connect <- NULL

# Create dummy-variable for activists
cecc$activ1 <- str_detect(cecc$occupation_coarse, "activist")
cecc$activ2 <- str_detect(cecc$occupation_coarse, "NGO ")
cecc$activ3 <- str_detect(cecc$occupation_coarse, "NGO, development")
cecc$activ4 <- str_detect(cecc$occupation_coarse, "NGO, environmental")
cecc$activ5 <- str_detect(cecc$occupation_coarse, "NGO, health")
cecc$activ6 <- str_detect(cecc$occupation_coarse, "NGO, manager")
cecc$activ7 <- str_detect(cecc$occupation_coarse, "NGO, staff")
cecc$activ_connect <- cecc$activ1 + cecc$activ2 + cecc$activ3 + cecc$activ4 + cecc$activ5 + 
  cecc$activ6 + cecc$activ7 
cecc$occupation_ngo <- 1
cecc$occupation_ngo[cecc$activ_connect == 0] <- 0
cecc$occupation_ngo[is.na(cecc$activ_connect)] <- 0
cecc$activ_connect <- NULL

# Create dummy-variable for farmers
cecc$farm1 <- str_detect(cecc$occupation_coarse, "peasant ")
cecc$farm2 <- str_detect(cecc$occupation_coarse, "farmer")
cecc$farm3 <- str_detect(cecc$occupation_coarse, "farmer / herder")
cecc$farm4 <- str_detect(cecc$occupation_coarse, "farmer / trader")
cecc$farm5 <- str_detect(cecc$occupation_coarse, "farmer, environmentalist")
cecc$farm_connect <- cecc$farm1 + cecc$farm2 + cecc$farm3 + cecc$farm4 + cecc$farm5
cecc$occupation_farmer <- 1
cecc$occupation_farmer[cecc$farm_connect == 0] <- 0
cecc$occupation_farmer[is.na(cecc$farm_connect)] <- 0
cecc$farm_connect <- NULL

# Create dummy-variable for entrepreneurs 
cecc$entre1 <- str_detect(cecc$occupation_coarse, "business owner ")
cecc$entre2 <- str_detect(cecc$occupation_coarse, "entrepreneur")
cecc$entre_connect <- cecc$entre1 + cecc$entre2
cecc$occupation_entrepreneur <- 1
cecc$occupation_entrepreneur[cecc$entre_connect == 0] <- 0
cecc$occupation_entrepreneur[is.na(cecc$entre_connect)] <- 0
cecc$entre_connect <- NULL

# Create dummy-variable for workers
cecc$occupation_worker <- str_detect(cecc$occupation_coarse, "worker")
cecc$occupation_worker[cecc$occupation_worker == T] <- 1
cecc$occupation_worker[cecc$occupation_worker == F] <- 0
cecc$occupation_worker[is.na(cecc$occupation_worker)] <- 0

# Create dummy-variable for lawyers
cecc$lawy1 <- str_detect(cecc$occupation_coarse, "court, judge and lawyer")
cecc$lawy2 <- str_detect(cecc$occupation_coarse, "court, judge")
cecc$lawy3 <- str_detect(cecc$occupation_coarse, "judge")
cecc$lawy4 <- str_detect(cecc$occupation_coarse, "lawyer")
cecc$lawy5 <- str_detect(cecc$occupation_coarse, "lawyer ")
cecc$lawy6 <- str_detect(cecc$occupation_coarse, "lawyer, business")
cecc$lawy7 <- str_detect(cecc$occupation_coarse, "lawyer, civil")
cecc$lawy8 <- str_detect(cecc$occupation_coarse, "lawyer, defense")
cecc$lawy9 <- str_detect(cecc$occupation_coarse, "legal advocate")
cecc$lawy10 <- str_detect(cecc$occupation_coarse, "procuratorate, prosecutor ")
cecc$lawy11 <- str_detect(cecc$occupation_coarse, "procuratorate, retired")
cecc$lawy12 <- str_detect(cecc$occupation_coarse, "professor, law")
cecc$law_connect <- cecc$lawy1 + cecc$lawy2 + cecc$lawy3 + cecc$lawy4 + cecc$lawy5 + 
  cecc$lawy6 + cecc$lawy7 + cecc$lawy8 + cecc$lawy9 + cecc$lawy10 + cecc$lawy11 + 
  cecc$lawy12
cecc$occupation_lawyer <- 1
cecc$occupation_lawyer[cecc$law_connect == 0] <- 0
cecc$occupation_lawyer[is.na(cecc$law_connect)] <- 0
cecc$law_connect <- NULL

# Create dummy variables for doctors
cecc$doc1 <- str_detect(cecc$occupation_coarse, "veterinary doctor")
cecc$doc2 <- str_detect(cecc$occupation_coarse, "doctor, retired")
cecc$doc3 <- str_detect(cecc$occupation_coarse, "doctor, psychiatrist")
cecc$doc4 <- str_detect(cecc$occupation_coarse, "doctor ")
cecc$doc5 <- str_detect(cecc$occupation_coarse, "doctor")
cecc$doc6 <- str_detect(cecc$occupation_coarse, "dentist")
cecc$doc_connect <- cecc$doc1 + cecc$doc2 + cecc$doc3 + cecc$doc4 + cecc$doc5 + 
  cecc$doc6 
cecc$occupation_doctor <- 1
cecc$occupation_doctor[cecc$doc_connect == 0] <- 0
cecc$occupation_doctor[is.na(cecc$doc_connect)] <- 0
cecc$doc_connect <- NULL

### Include controls for detentions during focal events (see Truex 2019)
cecc$focalevent_detention <- 0
# Extract month-years of detention dates
cecc$focalevent_detention[cecc$date_of_detention_coarse_unrounded == "1999-06-01"] <- 1 # 10th Anniversary of Tiananmen Square Massacre
cecc$focalevent_detention[cecc$date_of_detention_coarse_unrounded == "1999-10-01"] <- 1 # 50th Anniversary of founding of PRC
cecc$focalevent_detention[cecc$date_of_detention_coarse_unrounded == "2002-11-01"] <- 1 # 16th Party Congress
cecc$focalevent_detention[cecc$date_of_detention_coarse_unrounded == "2003-03-01"] <- 1 # NPC elects Hu Jintao President of PRC
cecc$focalevent_detention[cecc$date_of_detention_coarse_unrounded == "2004-06-01"] <- 1 # 15th Anniversary of Tiananmen Square Massacre
cecc$focalevent_detention[cecc$date_of_detention_coarse_unrounded == "2007-10-01"] <- 1 # 17th Party Congress
cecc$focalevent_detention[cecc$date_of_detention_coarse_unrounded == "2008-08-01"] <- 1 # Beijing Olympic Games
cecc$focalevent_detention[cecc$date_of_detention_coarse_unrounded == "2009-06-01"] <- 1 # 20th Anniversary of Tiananmen Square Massacre
cecc$focalevent_detention[cecc$date_of_detention_coarse_unrounded == "2009-10-01"] <- 1 # 60th Anniversary of founding of PRC
cecc$focalevent_detention[cecc$date_of_detention_coarse_unrounded == "2010-12-01"] <- 1 # Dissident Liu Xiaobo awarded Novel Peace Prize
cecc$focalevent_detention[cecc$date_of_detention_coarse_unrounded == "2010-11-01"] <- 1 # 16th Asia Games (in China)
cecc$focalevent_detention[cecc$date_of_detention_coarse_unrounded == "2012-11-01"] <- 1 # 18th Party Congress
cecc$focalevent_detention[cecc$date_of_detention_coarse_unrounded == "2013-03-01"] <- 1 # NPC elects Xi Jinping President of PRC
cecc$focalevent_detention[cecc$date_of_detention_coarse_unrounded == "2014-06-01"] <- 1 # 25th Anniversary of Tiananmen Square Massacre
cecc$focalevent_detention[cecc$date_of_detention_coarse_unrounded == "2019-06-01"] <- 1 # 30th Anniversary of Tiananmen Square Massacre
cecc$focalevent_detention[cecc$date_of_detention_coarse_unrounded == "2017-10-01"] <- 1 # 19th Party Congress
cecc$focalevent_detention[cecc$date_of_detention_coarse_unrounded == "2018-03-01"] <- 1 # Two-term limit removed for Xi Jinping

# Create indicators for main charges
table(cecc$charge_statute_coarse)
cecc$sect_charge <- 0
cecc$sect_charge[cecc$charge_statute_coarse == "CL97-art300"] <- 1
cecc$disrupting_public_order <- 0
cecc$disrupting_public_order[cecc$charge_statute_coarse == "CL97-art293"] <- 1
cecc$gather_crowd <- 0 
cecc$gather_crowd[cecc$charge_statute_coarse == "CL97-art290"] <- 1
cecc$subverting_state_power <- 0
cecc$subverting_state_power[cecc$charge_statute_coarse == "CL97-art10"] <- 1
cecc$colluding_foreign_entity <- 0 
cecc$colluding_foreign_entity[cecc$charge_statute_coarse == "CL97-art102"] <- 1

# Check for mass amnesties/ general amnesties (definition at least 15 on a day)
table(cecc$actual_dat_released) ### mass releases: "1995-10-06" and "1996-08-30" (more than 15 pol. prisoners released on same day)
cecc$mass_amnesty <- 0
cecc$mass_amnesty[cecc$actual_dat_released == "1995-10-06"] <- 1 
cecc$mass_amnesty[cecc$actual_dat_released == "1996-08-30"] <- 1 
table(cecc$mass_amnesty)
releasecount <- cecc %>% 
  group_by(actual_dat_released) %>% 
  count(actual_dat_released) %>% 
  arrange(desc(n))

# Sex variable as female-dummy and male-dummy
cecc$Female <- NA
cecc$Female <- ifelse(cecc$sex == "F", 1, 0)
cecc$Male <- NA
cecc$Male <- ifelse(cecc$sex == "M", 1, 0)

# Check for repeated detentions of same individual (based on name-location-occupation identifier - narrow conceptualization of "recidivists")
cecc$prisoner_identifier <- paste(cecc$main.name, "_", cecc$residence_county)
cecc$prisoner_identifier_triple <- paste(cecc$prisoner_identifier, "", cecc$occupation)
cecc$prisoner_identifier_triple <- ifelse(str_detect(cecc$prisoner_identifier_triple, "NA  unknown"), NA, cecc$prisoner_identifier_triple)
cecc$num <- ave(cecc$tri_coarse, cecc$prisoner_identifier_triple, FUN = seq_along)
cecc$Recidivists <- ifelse(cecc$num > 1, 1, 0)
cecc$num <- cecc$prisoner_identifier <- cecc$prisoner_identifier_triple <- NULL

### Delete unnecessary variables
cecc$currently_detained1 <- cecc$currently_detained2 <- cecc$currently_detained3 <-
  cecc$currently_detained4 <- cecc$currently_detained5 <- cecc$currently_detained6 <-
  cecc$currently_detained7 <- cecc$currently_detained8 <- cecc$currently_detained9 <-
  cecc$currently_detained10 <- cecc$currently_detained11 <- cecc$currently_detained12 <- 
  cecc$currently_detained13 <- cecc$currently_detained14 <- cecc$currently_detain <-
  cecc$han1 <- cecc$han2 <- cecc$budd1 <- cecc$budd2 <- cecc$budd3 <- cecc$budd4 <- cecc$musl1 <- 
  cecc$musl2 <- cecc$christ1 <- cecc$christ2 <- cecc$christ3 <- cecc$unemp1 <- cecc$unemp2 <-
  cecc$unemp3 <- cecc$journ1 <- cecc$journ2 <- cecc$journ3 <- cecc$journ4 <- cecc$journ5 <-
  cecc$journ6 <- cecc$journ7 <- cecc$journ8 <- cecc$journ9 <- cecc$journ10 <- cecc$journ11 <-
  cecc$journ12 <- cecc$journ13 <- cecc$journ14 <- cecc$journ15 <- cecc$journ16 <- cecc$journ17 <- 
  cecc$journ18 <- cecc$journ19 <- cecc$journ20 <- cecc$journ21 <- cecc$journ22 <- cecc$journ22 <-
  cecc$journ23 <- cecc$journ24 <- cecc$journ25 <- cecc$journ26 <- cecc$journ26 <- cecc$journ27 <-
  cecc$journ28 <- cecc$journ29 <- cecc$art1 <- cecc$art2 <- cecc$art3 <- cecc$art4 <- cecc$art5 <-
  cecc$art6 <- cecc$art7 <- cecc$art8 <- cecc$art9 <- cecc$art10 <- cecc$art11 <- cecc$art12 <- 
  cecc$art13 <- cecc$art14 <- cecc$art15 <- cecc$art16 <- cecc$art17 <- cecc$art18 <- cecc$art19 <-
  cecc$stud1 <- cecc$stud2 <- cecc$stud3 <- cecc$stud4 <- cecc$stud5 <- cecc$stud6 <- cecc$stud7 <-
  cecc$stud8 <- cecc$stud9 <- cecc$stud10 <- cecc$prof1 <- cecc$prof2 <- cecc$prof3 <- cecc$prof4 <-
  cecc$prof5 <- cecc$prof6 <- cecc$prof7 <- cecc$prof8 <- cecc$prof9 <- cecc$prof10 <- cecc$prof11 <-
  cecc$prof12 <- cecc$prof13 <- cecc$prof14 <- cecc$prof15 <- cecc$prof16 <- cecc$prof17 <-
  cecc$prof18 <- cecc$prof19 <- cecc$reli1 <- cecc$reli2 <- cecc$reli3 <- cecc$reli4 <- cecc$reli5 <-
  cecc$reli6 <- cecc$reli7 <- cecc$reli8 <- cecc$reli9 <- cecc$reli10 <- cecc$ccp1 <- cecc$ccp2 <-
  cecc$activ1 <- cecc$activ2 <- cecc$activ3 <- cecc$activ4 <- cecc$activ5 <- cecc$activ6 <-
  cecc$activ7 <-  cecc$farm1 <- cecc$farm2 <- cecc$farm3 <- cecc$farm4 <- cecc$farm5 <- cecc$entre1 <-
  cecc$entre2 <- cecc$lawy1 <- cecc$lawy2 <- cecc$lawy3 <- cecc$lawy4 <- cecc$lawy5 <- cecc$lawy6 <-
  cecc$lawy7 <- cecc$lawy8 <- cecc$lawy9 <- cecc$lawy10 <- cecc$lawy11 <- cecc$lawy12 <-
  cecc$doc1 <- cecc$doc2 <- cecc$doc3 <- cecc$doc4 <- cecc$doc5 <- cecc$doc6 <- cecc$escape1 <-
  cecc$escape2 <- cecc$dead1 <- cecc$dead2 <- cecc$dead3 <- cecc$release1 <- cecc$release2 <- 
  cecc$release3 <- cecc$release4 <- NULL

# Rename variables
ppris_logistic <- filter(cecc, !is.na(age) & !is.na(sex))
cecc <- rename(cecc, Christian = religion_christian)
cecc <- rename(cecc, Unemployed = occupation_unemployed)
cecc <- rename(cecc, Professor = occupation_professor)
cecc <- rename(cecc, Cleric = occupation_religion)
cecc <- rename(cecc, PSB_detention = PSB_coarse)
cecc <- rename(cecc, NGO_activist = occupation_ngo)
cecc <- rename(cecc, Ethnic_Han = ethnic_han)
cecc <- rename(cecc, Charged = chg_coarse)
cecc <- rename(cecc, Sex = sex)
cecc <- rename(cecc, Journalist = occupation_journalist)
cecc <- rename(cecc, Worker = occupation_worker)
cecc <- rename(cecc, Lawyer = occupation_lawyer)
cecc <- rename(cecc, Entrepreneur = occupation_entrepreneur)
cecc <- rename(cecc, Student = occupation_student)
cecc <- rename(cecc, Ethnic_Tibetan = ethnic_tibetan)
cecc <- rename(cecc, Ethnic_Uyghur = ethnic_uyghur)
cecc <- rename(cecc, FalunGong = religion_falungong)
cecc <- rename(cecc, Buddhist = religion_buddhist)
cecc <- rename(cecc, Muslim = religion_muslim)
cecc <- rename(cecc, Democracy_related = democ)
cecc <- rename(cecc, Economy_related = econ)
cecc <- rename(cecc, Focalevent_detention = focalevent_detention)
cecc <- rename(cecc, Mass_amnesty = mass_amnesty)
cecc <- rename(cecc, Age = age)
ppris_logistic <- rename(ppris_logistic, Christian = religion_christian)
ppris_logistic <- rename(ppris_logistic, Unemployed = occupation_unemployed)
ppris_logistic <- rename(ppris_logistic, Professor = occupation_professor)
ppris_logistic <- rename(ppris_logistic, PSB_detention = PSB_coarse)
ppris_logistic <- rename(ppris_logistic, Cleric = occupation_religion)
ppris_logistic <- rename(ppris_logistic, NGO_activist = occupation_ngo)
ppris_logistic <- rename(ppris_logistic, Ethnic_Han = ethnic_han)
ppris_logistic <- rename(ppris_logistic, Charged = chg_coarse)
ppris_logistic <- rename(ppris_logistic, Sex = sex)
ppris_logistic <- rename(ppris_logistic, Journalist = occupation_journalist)
ppris_logistic <- rename(ppris_logistic, Worker = occupation_worker)
ppris_logistic <- rename(ppris_logistic, Lawyer = occupation_lawyer)
ppris_logistic <- rename(ppris_logistic, Entrepreneur = occupation_entrepreneur)
ppris_logistic <- rename(ppris_logistic, Student = occupation_student)
ppris_logistic <- rename(ppris_logistic, Ethnic_Tibetan = ethnic_tibetan)
ppris_logistic <- rename(ppris_logistic, Ethnic_Uyghur = ethnic_uyghur)
ppris_logistic <- rename(ppris_logistic, FalunGong = religion_falungong)
ppris_logistic <- rename(ppris_logistic, Buddhist = religion_buddhist)
ppris_logistic <- rename(ppris_logistic, Muslim = religion_muslim)
ppris_logistic <- rename(ppris_logistic, Democracy_related = democ)
ppris_logistic <- rename(ppris_logistic, Economy_related = econ)
ppris_logistic <- rename(ppris_logistic, Focalevent_detention = focalevent_detention)
ppris_logistic <- rename(ppris_logistic, Mass_amnesty = mass_amnesty)
ppris_logistic <- rename(ppris_logistic, Age = age)

# Subdataset for those with complete information on duration of pol. imprisonment
final_pprisdat <- filter(cecc, !is.na(detentiontime_months))

# Create dummy variable for other ethnic minorities
final_pprisdat$Other_ethnic_minorities <- 0
final_pprisdat$Other_ethnic_minorities[final_pprisdat$ethnic_group == "Ewenki"] <- 1
final_pprisdat$Other_ethnic_minorities[final_pprisdat$ethnic_group == "Hui"] <- 1
final_pprisdat$Other_ethnic_minorities[final_pprisdat$ethnic_group == "Kazak"] <- 1
final_pprisdat$Other_ethnic_minorities[final_pprisdat$ethnic_group == "Kirgiz"] <- 1
final_pprisdat$Other_ethnic_minorities[final_pprisdat$ethnic_group == "Korean"] <- 1
final_pprisdat$Other_ethnic_minorities[final_pprisdat$ethnic_group == "Manchu"] <- 1
final_pprisdat$Other_ethnic_minorities[final_pprisdat$ethnic_group == "Miao"] <- 1
final_pprisdat$Other_ethnic_minorities[final_pprisdat$ethnic_group == "Mongol"] <- 1
final_pprisdat$Other_ethnic_minorities[final_pprisdat$ethnic_group == "She"] <- 1
final_pprisdat$Other_ethnic_minorities[final_pprisdat$ethnic_group == "Tatar"] <- 1
final_pprisdat$Other_ethnic_minorities[final_pprisdat$ethnic_group == "Tujia"] <- 1
final_pprisdat$Other_ethnic_minorities[final_pprisdat$ethnic_group == "Uzbek"] <- 1
final_pprisdat$Other_ethnic_minorities[final_pprisdat$ethnic_group == "Yao"] <- 1
final_pprisdat$Other_ethnic_minorities[final_pprisdat$ethnic_group == "Yi"] <- 1

# Create dummy for other religious groups
table(final_pprisdat$religion_coarse)
final_pprisdat$Other_religious_groups <- 0
final_pprisdat$Other_religious_groups[final_pprisdat$religion_coarse == "Mentu Hui"] <- 1
final_pprisdat$Other_religious_groups[final_pprisdat$religion_coarse == "Yi Guan Dao"] <- 1
final_pprisdat$Other_religious_groups[final_pprisdat$religion_coarse == "Eastern Lightning"] <- 1
final_pprisdat$Other_religious_groups[final_pprisdat$religion_coarse == "Lord God Religion"] <- 1
final_pprisdat$Other_religious_groups[final_pprisdat$religion_coarse == "Local Church "] <- 1
final_pprisdat$Other_religious_groups[final_pprisdat$religion_coarse == "Jehovah's Witness"] <- 1
final_pprisdat$Other_religious_groups[final_pprisdat$religion_coarse == "Full Scope Church"] <- 1

# Create categorical ethnicity-variable
final_pprisdat$ethnicity <- NA
final_pprisdat$ethnicity[final_pprisdat$Ethnic_Han == 1] <- "Ethnic Han"
final_pprisdat$ethnicity[final_pprisdat$Ethnic_Tibetan == 1] <- "Ethnic Tibetan"
final_pprisdat$ethnicity[final_pprisdat$Ethnic_Uyghur == 1] <- "Ethnic Uyghur"
final_pprisdat$ethnicity[final_pprisdat$ethnic_mongol == 1] <- "Ethnic Mongol"
final_pprisdat$ethnicity[final_pprisdat$ethnic_group == "Kazak"] <- "Ethnic Kazak"
final_pprisdat$ethnicity[final_pprisdat$ethnic_group == "Ewenki"] <- "Others"
final_pprisdat$ethnicity[final_pprisdat$ethnic_group == "Hui"] <- "Others"
final_pprisdat$ethnicity[final_pprisdat$ethnic_group == "Kirgiz"] <- "Others"
final_pprisdat$ethnicity[final_pprisdat$ethnic_group == "Korean"] <- "Others"
final_pprisdat$ethnicity[final_pprisdat$ethnic_group == "Manchu"] <- "Others"
final_pprisdat$ethnicity[final_pprisdat$ethnic_group == "Miao"] <- "Others"
final_pprisdat$ethnicity[final_pprisdat$ethnic_group == "She"] <- "Others"
final_pprisdat$ethnicity[final_pprisdat$ethnic_group == "Tatar"] <- "Others"
final_pprisdat$ethnicity[final_pprisdat$ethnic_group == "Tujia"] <- "Others"
final_pprisdat$ethnicity[final_pprisdat$ethnic_group == "Uzbek"] <- "Others"
final_pprisdat$ethnicity[final_pprisdat$ethnic_group == "Yao"] <- "Others"
final_pprisdat$ethnicity[final_pprisdat$ethnic_group == "Yi"] <- "Others"
table(final_pprisdat$ethnicity)

# Create categorical religion-variable
final_pprisdat$confession <- NA
final_pprisdat$confession[final_pprisdat$Buddhist == 1] <- "Buddhist"
final_pprisdat$confession[final_pprisdat$Muslim == 1] <- "Muslim"
final_pprisdat$confession[final_pprisdat$Christian == 1] <- "Christian"
final_pprisdat$confession[final_pprisdat$religion_coarse == "Falun Gong"] <- "Falun Gong"
final_pprisdat$confession[final_pprisdat$religion_coarse == "Eastern Lightning"] <- "Others"
final_pprisdat$confession[final_pprisdat$religion_coarse == "Full Scope Church"] <- "Others"
final_pprisdat$confession[final_pprisdat$religion_coarse == "Jehovah's Witness"] <- "Others"
final_pprisdat$confession[final_pprisdat$religion_coarse == "Mentu Hui"] <- "Others"
final_pprisdat$confession[final_pprisdat$religion_coarse == "Local Church "] <- "Others"
final_pprisdat$confession[final_pprisdat$religion_coarse == "Yi Guan Dao"] <- "Others"
final_pprisdat$confession[final_pprisdat$religion_coarse == "Lord God Religion"] <- "Others"
final_pprisdat$confession[is.na(final_pprisdat$religion_coarse)] <- "No religion"

# Save dataset
### save(cecc, file = "complete_pprisdat.RData")
### save(final_pprisdat, file = "final_pprisdat.RData") 

# Note: After this data preparation, I manually coded the criminalized action-variable for all prisoners included in the "final_pprisdat" dataset