###### China Map ###
###### The duration of political imprisonment: Evidence from China ###
###### Author: Christoph Valentin Steinert ###

### Packages
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
library(magrittr)
library(hchinamap)
library(tmap)

# Load prisoner data
rm(list = ls())
setwd("~/PhD Political Imprisonments/China-Projekt")
load(file = "final_pprisdat.RData")

# Open mapping data

# install.packages('hchinamap', build_vignettes = TRUE)
# Demo dataset
dir <- tempdir()
download.file('https://czxb.github.io/br/chinadf.rda', file.path(dir, 'chinadf.rda'))
load(file.path(dir, 'chinadf.rda'), verbose = TRUE)

# Load China map (here example maps from package)
library(hchinamap)
china <- chinadf %>% 
  dplyr::filter(region == "China")
hchinamap(name = china$name, value = china$value,
          width = "100%", height = "400px",
          title = "Map of China", region = "China")

# Add translations in Pinyin
library('pinyin')
pydic(method = c("quanpin", "tone", "toneless"), multi = FALSE, only_first_letter = FALSE, dic = c("pinyin", "pinyin2"))
chinadf$pny <- py(char = chinadf$name, sep = "_", other_replace = NULL, dic = pydic(method = "toneless"))
chinadf$pny_completed <- gsub("_","", chinadf$pny)

# Include year of detention-variable
final_pprisdat$final_pprisdat_rn <- final_pprisdat$CECC_record_number
final_pprisdat <- separate(final_pprisdat, col = final_pprisdat_rn, into = c("year","idnumyear"), sep = "-")

# Exclude prisoners that died in prison, that escaped or that were executed
final_pprisdat$detentiontime_months[final_pprisdat$died_in_prison == 1] <- NA
final_pprisdat$detentiontime_months[final_pprisdat$escaped == 1] <- NA
final_pprisdat$detentiontime_months[final_pprisdat$executed == 1] <- NA

# Exclude coding errors
final_pprisdat$detentiontime_months[final_pprisdat$detentiontime_months < 0] <- NA

# Code right-censored observations as NA
final_pprisdat$detentiontime_months[final_pprisdat$currently_detained == 1] <- NA

# Average detention lengths by prefectures/ provinces
average_lengths_prov <- final_pprisdat %>% 
  group_by(province_imprisoned_detained) %>% 
  summarise(average_lengths_prov = mean(detentiontime_months, na.rm = T))
ungroup(final_pprisdat)
average_lengths_pref <- final_pprisdat %>% 
  group_by(prefecture_imprisoned_detained) %>% 
  summarise(average_lengths_pref = mean(detentiontime_months, na.rm = T))
ungroup(final_pprisdat)

# Prepare prefecture-data for merging
average_lengths_pref$prefecture_imprisoned_detained <- gsub("Shi","", average_lengths_pref$prefecture_imprisoned_detained)
average_lengths_pref$prefecture_imprisoned_detained <- gsub("\\([^\\)]+\\)", "", average_lengths_pref$prefecture_imprisoned_detained)
average_lengths_pref$prefecture_imprisoned_detained <- gsub(pattern = '([[:upper:]])', perl = TRUE, replacement = '\\L\\1', average_lengths_pref$prefecture_imprisoned_detained)
average_lengths_pref$prefecture_imprisoned_detained <- gsub("dist\\.", "", average_lengths_pref$prefecture_imprisoned_detained)
average_lengths_pref$prefecture_imprisoned_detained <- gsub("pref\\.", "", average_lengths_pref$prefecture_imprisoned_detained)
average_lengths_pref$prefecture_imprisoned_detained <- gsub("\\s", "", average_lengths_pref$prefecture_imprisoned_detained) 

# Data cleaning
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "akesu[aksu]"] <- "akesu"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "alashanleague"] <- "alashanmeng"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "ali[ngari]"] <- "ali"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "bayinguoleng[bayangol]mongolauto."] <- "bayinguoleng"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "beijingcty."] <- "beijing"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "changdu[chamdo]"] <- "changdou"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "chengdu"] <- "chengdou"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "chongqingcty."] <- "chongqing"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "chuxiongyiauto."] <- "chuxiongyicou"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "dalibaiauto."] <- "dalibaicou"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "diqing[dechen]tibetanauto."] <- "diqing"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "e'erduosi[ordos]"] <- "eerduoshi"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "gannan[kanlho]tibetanauto."] <- "ganna"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "ganzi[kardze]tibetanauto."] <- "ganzi"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "ha'erbin[harbin]"] <- "haerbin"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "haibei[tsojang]tibetanauto."] <- "haibei"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "haidong[tsoshar]"] <- "haidong"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "hainan[tsolho]tibetanauto."] <- "Hainan"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "haixi[tsonub]mongol&tibetanauto."] <- "haixi"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "hetian[hotan]"] <- "hetian"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "huai'an"] <- "huaian"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "huainan"] <- "huaina"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "huangnan[malho]tibetanauto."] <- "huangna"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "huludao"] <- "huhudao"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "ji'an"] <- "jian"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "jiamusi"] <- "jiamushi"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "kelamayi[qaramay]"] <- "kelamayi"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "kezilesu[kizilsu]kyrgyzauto."] <- "kezilesukeerkezi"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "lasa[lhasa]"] <- "lasa"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "liangshanyiauto."] <- "liangshanyicou"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "linzhi[kongpo]"] <- "linzhi"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "longnan"] <- "longna"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "loudi"] <- "loude"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "luliang"] <- "lvliang"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "ma'anshan"] <- "maanshan"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "nanchang"] <- "nachang"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "nanchong"] <- "nachong"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "nanjing"] <- "najing"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "nanning"] <- "naning"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "natong"] <- "natong"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "nanyang"] <- "nayang"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "naqu[nagchu]"] <- "naqu"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "neijiang"] <- "nayang"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "ningbo"] <- "ningbei"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "panzhihua"] <- "panqihua"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "pingdingshan"] <- "pingxiang"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "pu'er"] <- "puer"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "qiandongnanmiao&dongauto."] <- "qiandong"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "quanzhoumuni.urb.area"] <- "quanzhou"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "rikaze[gatse]"] <- "rikaze"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "sanming"] <- "sanmeng"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "shanghaicty."] <- "shanghai"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "shannan[lhokha]"] <- "shanna"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "zuishan"] <- "zhoushan"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "siping"] <- "sibeng"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "tai'an"] <- "taian"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "taiyuan"] <- "tayuan"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "tianjincty."] <- "tianjin"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "weinan"] <- "weina"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "wulumuqi[urumqi]"] <- "wulumuji"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "wuzhong"] <- "wuzhou"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "xi'an"] <- "xian"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "xianyang"] <- "xiangyang"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "xilinguole[xilingol]league"] <- "xilinguolemeng"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "xinyang"] <- "xinjiang"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "ya'an"] <- "yaan"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "yan'an"] <- "yanan"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "yanbiankoreanauto."] <- "yanbian"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "yushu[yulshul]tibetanauto."] <- "yushu"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "yuxi"] <- "yuqi"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "aba[ngaba]tibetan&qiangauto."] <- "abaqiangcou"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "benxi"] <- "benqi"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "boertalamongolauto."] <- "boerdala"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "ganzi[kardze]tibetanauto."] <- "ganzi"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "huangshi"] <- "huangshan"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "jinan"] <- "jina"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "nanping"] <- "naning"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "xining[ziling]"] <- "xining"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "akesu [aksu] "] <- "akesu"

# Indicate which data is province-level in the mapping data
chinadf$pny_completed[chinadf$name == "“ –Œ"] <- "beijing_prov" 
chinadf$pny_completed[chinadf$name == "“H™ž"] <- "tianjin_prov" 
chinadf$pny_completed[chinadf$name == "—Ç“ "] <- "hebei_prov" 
chinadf$pny_completed[chinadf$name == "’Ó”è"] <- "shanxi_prov" 
chinadf$pny_completed[chinadf$name == "???¥ß“«"] <- "liaoning_prov" 
chinadf$pny_completed[chinadf$name == "”k—®"] <- "jilin_prov" 
chinadf$pny_completed[chinadf$name == "¢8???”¿"] <- "heilongjiang_prov" 
chinadf$pny_completed[chinadf$name == "’·›¿"] <- "shanghai_prov" 
chinadf$pny_completed[chinadf$name == "”¿???"] <- "jiangsu_prov" 
chinadf$pny_completed[chinadf$name == "›À”¿"] <- "zhejiang_prov" 
chinadf$pny_completed[chinadf$name == "”™ªw"] <- "anhui_prov" 
chinadf$pny_completed[chinadf$name == "¥ž˜ù"] <- "fujian_prov" 
chinadf$pny_completed[chinadf$name == "”¿”è"] <- "jiangxi_prov" 
chinadf$pny_completed[chinadf$name == "’Ó???"] <- "shandong_prov" 
chinadf$pny_completed[chinadf$name == "—Ç˜±"] <- "hena_prov"
chinadf$pny_completed[chinadf$name == " ¸“ "] <- "hubei_prov" 
chinadf$pny_completed[chinadf$name == " ¸˜±"] <- "huna_prov" 
chinadf$pny_completed[chinadf$name == "„p???"] <- "guandong_prov" 
chinadf$pny_completed[chinadf$name == "„p”è"] <- "anxi_prov" 
chinadf$pny_completed[chinadf$name == "›¿˜±"] <- "haina_prov" 
chinadf$pny_completed[chinadf$name == "š???"] <- "chongqing_prov" 
chinadf$pny_completed[chinadf$name == "“½’Ô"] <- "sichuan_prov" 
chinadf$pny_completed[chinadf$name == "???”"] <- "guizhou_prov" 
chinadf$pny_completed[chinadf$name == "’è˜±"] <- "yunna_prov" 
chinadf$pny_completed[chinadf$name == "”è¬E"] <- "xicang_prov" 
chinadf$pny_completed[chinadf$name == "???”è"] <- "shanxi_prov" 
chinadf$pny_completed[chinadf$name == "“ë???"] <- "gansu_prov" 
chinadf$pny_completed[chinadf$name == "˜†›¿"] <- "jinghai_prov" 
chinadf$pny_completed[chinadf$name == "Ýoší"] <- "ningjia_prov"
chinadf$pny_completed[chinadf$name == "¢º¬Ð"] <- "xinjiang_prov" 
chinadf$pny_completed[chinadf$name == "“¹???"] <- "siwan_prov" 
chinadf$pny_completed[chinadf$name == "š ª"] <- "xianggang_prov" 
chinadf$pny_completed[chinadf$name == "©D???"] <- "aomen_prov" 
chinadf$pny_completed[chinadf$name == "˜±›¿??????"] <- "nahaizhudao_prov" 

# Add province-data to prefecture data 
average_lengths_prov <- rename(average_lengths_prov, average_lengths_pref  = average_lengths_prov)
average_lengths_prov <- rename(average_lengths_prov, prefecture_imprisoned_detained  = province_imprisoned_detained)
average_lengths_pref <- rbind(average_lengths_pref, average_lengths_prov)

# Change names of provinces for merging
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "Anhui Province"] <- "anhui_prov"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "Beijing Shi (prov.)"] <- "beijing_prov"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "Chongqing Shi (prov.)"] <- "chongqing_prov"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "Fujian Province"] <- "fujian_prov"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "Gansu Province"] <- "gansu_prov"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "Guangdong Province"] <- "guandong_prov"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "Guizhou Province"] <- "guizhou_prov"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "Hainan Province"] <- "haina_prov"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "Hebei Province"] <- "hebei_prov"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "Heilongjiang Province"] <- "heilongjiang_prov"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "Henan Province"] <- "hena_prov"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "Hubei Province"] <- "hubei_prov"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "Hunan Province"] <- "huna_prov"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "Jiangsu Province"] <- "jiangsu_prov"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "Jiangxi Province"] <- "jiangxi_prov"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "Jilin Province"] <- "jilin_prov"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "Liaoning Province"] <- "liaoning_prov"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "Ningxia Hui Auto. Region"] <- "ningjia_prov"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "Qinghai Province"] <- "jinghai_prov"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "Shaanxi Province"] <- "shanxi_prov"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "Shandong Province"] <- "shandong_prov"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "Shanghai Shi (prov.)"] <- "shanghai_prov"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "Shanxi Province"] <- "shanxi_prov"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "Sichuan Province"] <- "sichuan_prov"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "Tianjin Shi (prov.)"] <- "tianjin_prov"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "Yunnan Province"] <- "yunna_prov"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "Zhejiang Province"] <- "zhejiang_prov"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "Hong Kong [Xianggang] SAR"] <- "xianggang_prov"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "Tibet [Xizang] Auto. Region"] <- "xicang_prov"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "Guangxi Zhuang Auto. Region"] <- "anxi_prov"
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "Macao SAR"] <- "aomen_prov"
# average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "Inner Mongolia [Neimenggu] Auto. Region"] <- ???
average_lengths_pref$prefecture_imprisoned_detained[average_lengths_pref$prefecture_imprisoned_detained == "Xinjiang Uyghur Auto. Region"] <- "xinjiang_prov"

# Merging
chinadf <- rename(chinadf, prefecture_imprisoned_detained = pny_completed)
unmerged <- anti_join(average_lengths_pref, chinadf,  by="prefecture_imprisoned_detained")
chinamapping <- merge(chinadf, average_lengths_pref, by = "prefecture_imprisoned_detained", all.x = T, all.y = F)
chinamapping$n <- ifelse(is.na(chinamapping$n), 0, chinamapping$n)

# Present China map
#### See Figure 2 in the article ####
china <- chinamapping %>% 
  dplyr::filter(region == "China")
hchinamap(name = china$name, value = china$average_lengths_pref,
          legendVerticalAlign = "bottom",  
          min = 0, minColor = "rgb(153,255, 153)", maxColor = "rgb(255, 0, 0)",
          legendTitle = "Average length of political imprisonment in months", region = "China") 
dev.off()






