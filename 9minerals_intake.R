#删除mineral缺失的剩余41433，删除CKD有缺失的剩余25714，删除20岁以下剩余23061，删除孕妇22794，删除能量摄入极端剩余21944#
####打开文件，library R包####
install.packages("nhanesA")
install.packages("dplyr")
install.packages("plyr")
install.packages("survey")
install.packages("foreign")
install.packages("tidyverse")
install.packages("do")
install.packages("haven")
install.packages("arsenal")
install.packages("gtsummary")
install.packages("tidyverse")
install.packages("Hmisc")
install.packages("psych")
install.packages("ggplot2")
install.packages("epiDisplay")
install.packages("bkmr")
install.packages("bkmrhat")

library(nhanesA)
library(dplyr)
library(plyr)
library(survey)
library(foreign)
library(tidyverse)
library(do)
library(haven)
library(kidney.epi)
library(arsenal)
library(gtsummary)
library(tidyverse)
library(Hmisc)
library(psych)
library(ggplot2)
library(epiDisplay)
library(bkmr)
library(bkmrhat)
setwd("E:\\R  NHANES\\9mineral_intake")
mineral_mice<-read_xpt("mineral_mice11_2.xpt")
view(mineral_mice)
####baseline####
Baseline<-read_xpt("Baseline.xpt")
view(Baseline)
####9mineral_Dietary_intake####
#总摄入量
#DR1TCALC - 钙（毫克）;DR1TPHOS - 磷（毫克）;DR1TMAGN - 镁（毫克）;
#DR1TIRON - 铁（毫克）;DR1TZINC - 锌（毫克）;DR1TCOPP - 铜（毫克）
#DR1TSODI - 钠（毫克）;DR1TPOTA - 钾（毫克）;DR1TSELE - 硒（微克）
DR1TOT_G<-read.xport('DR1TOT_G.XPT')[,c("SEQN","DR1TCALC","DR1TPHOS","DR1TMAGN",
                                        "DR1TIRON","DR1TZINC","DR1TCOPP",
                                        "DR1TSODI","DR1TPOTA","DR1TSELE")]

DR2TOT_G<-read.xport('DR2TOT_G.XPT')[,c("SEQN","DR2TCALC","DR2TPHOS","DR2TMAGN",
                                        "DR2TIRON","DR2TZINC","DR2TCOPP",
                                        "DR2TSODI","DR2TPOTA","DR2TSELE")]

D_mineral<-plyr::join_all(list(DR1TOT_G,DR2TOT_G),by="SEQN")
view(D_mineral)
DEMO_G<-read_xpt("DEMO_G.xpt")
#算两天的摄入平均值#
D_mineral$Ca<-rowMeans(D_mineral[, c("DR1TCALC", "DR2TCALC")], na.rm = TRUE)
D_mineral$P<-rowMeans(D_mineral[, c("DR1TPHOS", "DR2TPHOS")], na.rm = TRUE)
D_mineral$Mg<-rowMeans(D_mineral[, c("DR1TMAGN", "DR2TMAGN")], na.rm = TRUE)
D_mineral$Fe<-rowMeans(D_mineral[, c("DR1TIRON", "DR2TIRON")], na.rm = TRUE)
D_mineral$Zn<-rowMeans(D_mineral[, c("DR1TZINC", "DR2TZINC")], na.rm = TRUE)
D_mineral$Cu<-rowMeans(D_mineral[, c("DR1TCOPP", "DR2TCOPP")], na.rm = TRUE)
D_mineral$Na<-rowMeans(D_mineral[, c("DR1TSODI", "DR2TSODI")], na.rm = TRUE)
D_mineral$K<-rowMeans(D_mineral[, c("DR1TPOTA", "DR2TPOTA")], na.rm = TRUE)
D_mineral$Se<-rowMeans(D_mineral[, c("DR1TSELE", "DR2TSELE")], na.rm = TRUE)
write_xpt(D_mineral,"D_mineral.xpt")
####9mineral_Supplement_intake####
#DS1TCALC - 钙（毫克）;DS1TPHOS - 磷（毫克）;DS1TMAGN - 镁（毫克）
#DS1TIRON - 铁（毫克）;DS1TZINC - 锌（毫克）;DS1TCOPP - 铜（毫克）
#DS1TSODI - 钠（毫克）;DS1TPOTA - 钾（毫克）;DS1TSELE - 硒（微克T
DS1TOT_G<-read.xport('DS1TOT_G.XPT')[,c("SEQN","DS1TCALC","DS1TPHOS","DS1TMAGN",
                                        "DS1TIRON","DS1TZINC","DS1TCOPP",
                                        "DS1TSODI","DS1TPOTA","DS1TSELE")]
DS2TOT_G<-read.xport('DS2TOT_G.XPT')[,c("SEQN","DS2TCALC","DS2TPHOS","DS2TMAGN",
                                        "DS2TIRON","DS2TZINC","DS2TCOPP",
                                        "DS2TSODI","DS2TPOTA","DS2TSELE")]
S_mineral<-plyr::join_all(list(DS1TOT_G,DS2TOT_G),by="SEQN")
view(S_mineral)
#算两天的摄入平均值#
S_mineral$Ca_S<-rowMeans(S_mineral[, c("DS1TCALC", "DS2TCALC")], na.rm = TRUE)
S_mineral$P_S<-rowMeans(S_mineral[, c("DS1TPHOS", "DS2TPHOS")], na.rm = TRUE)
S_mineral$Mg_S<-rowMeans(S_mineral[, c("DS1TMAGN", "DS2TMAGN")], na.rm = TRUE)
S_mineral$Fe_S<-rowMeans(S_mineral[, c("DS1TIRON", "DS2TIRON")], na.rm = TRUE)
S_mineral$Zn_S<-rowMeans(S_mineral[, c("DS1TZINC", "DS2TZINC")], na.rm = TRUE)
S_mineral$Cu_S<-rowMeans(S_mineral[, c("DS1TCOPP", "DS2TCOPP")], na.rm = TRUE)
S_mineral$Na_S<-rowMeans(S_mineral[, c("DS1TSODI", "DS2TSODI")], na.rm = TRUE)
S_mineral$K_S<-rowMeans(S_mineral[, c("DS1TPOTA", "DS2TPOTA")], na.rm = TRUE)
S_mineral$Se_S<-rowMeans(S_mineral[, c("DS1TSELE", "DS2TSELE")], na.rm = TRUE)
view(S_mineral)
write_xpt(S_mineral,"S_mineral.xpt")
mineral1<-D_mineral[,c("SEQN","Ca","P","Mg","Fe","Zn","Cu","Na","K","Se")]
view(mineral1)
mineral2<-S_mineral[,c("SEQN","Ca_S","P_S","Mg_S","Fe_S","Zn_S","Cu_S","Na_S","K_S","Se_S")]
view(mineral2)
mineral<-plyr::join_all(list(mineral1,mineral2),by="SEQN")
view(mineral)
# 使用rowSums计算A和B列的和，忽略空缺值
mineral$Ca_all<- rowSums(mineral[, c("Ca", "Ca_S")], na.rm = TRUE)
mineral$P_all<- rowSums(mineral[, c("P", "P_S")], na.rm = TRUE)
mineral$Mg_all<- rowSums(mineral[, c("Mg", "Mg_S")], na.rm = TRUE)
mineral$Fe_all<- rowSums(mineral[, c("Fe", "Fe_S")], na.rm = TRUE)
mineral$Zn_all<- rowSums(mineral[, c("Zn", "Zn_S")], na.rm = TRUE)
mineral$Cu_all<- rowSums(mineral[, c("Cu", "Cu_S")], na.rm = TRUE)
mineral$Na_all<- rowSums(mineral[, c("Na", "Na_S")], na.rm = TRUE)
mineral$K_all<- rowSums(mineral[, c("K", "K_S")], na.rm = TRUE)
mineral$Se_all<- rowSums(mineral[, c("Se", "Se_S")], na.rm = TRUE)
write_xpt(mineral,"mineral.xpt")

mineral_final<-mineral[,c("SEQN","Ca_all","P_all","Mg_all","Fe_all",
                          "Zn_all","Cu_all","Na_all","K_all","Se_all")]
view(mineral_final)
#删除空缺值
mineral_final<-na.omit(mineral_final)
any(is.na(mineral_final))
#使用rowSums删除除了"SEQN"之外所有值都为0的行
mineral_final_filtered <- mineral_final[rowSums(mineral_final[ , !names(mineral_final) %in% "SEQN"]) != 0, ]
View(mineral_final_filtered)
write_xpt(mineral_final_filtered,"mineral_final_filtered.xpt")
####合并数据####
mineral_data<-merge(Baseline,mineral_final_filtered, by = "SEQN", 
                all = FALSE)
#删除CKD有空缺的行
any(is.na(mineral_data$CKD))
mineral_data <- mineral_data[!is.na(mineral_data$CKD), ]
#删除年龄小于20岁的
mineral_data <- mineral_data[mineral_data$Age >= 20, ]
#删除孕妇
mineral_data <- mineral_data[mineral_data$Pregnancy != 1, ]
mineral_data<- subset(mineral_data, select = -Pregnancy)
#删除能量摄入极端值数据#
mineral_data<- mineral_data %>%
  filter(
    (Gender == 1 & Engergy >= 800 & Engergy <= 4200) |
      (Gender == 2 & Engergy >= 500 & Engergy <= 3500))
write_xpt(mineral_data,"mineral_data.xpt")
####删除minerals摄入量极端值####
mineral_mice<-read_xpt("mineral_mice11.xpt")
view(mineral_mice)
mineral_mice<- mineral_mice %>%filter(ln_Se <6.5 & ln_Se>3.3)
mineral_mice<- mineral_mice %>%filter(ln_K>6 & ln_K <10)
mineral_mice<- mineral_mice %>%filter(ln_Cu <2.5)
mineral_mice<- mineral_mice %>%filter(ln_Zn > 0.75)
mineral_mice<- mineral_mice %>%filter(ln_P > 5.8)
mineral_mice<- mineral_mice %>%filter(ln_Ca > 5)
mineral_mice<- mineral_mice %>%filter(ln_Mg > 4.3)
mineral_mice<- mineral_mice %>%filter(ln_Fe > 0.9)
view(mineral_mice)
write_xpt(mineral_mice,"mineral_mice11_2.xpt")
####初步检测与CKD的相关性####
view(mineral_data)
#连续变量or值#
logit.glm <- glm(CKD ~ Se_all,data = mineral_data)
summary(logit.glm)
coefficients <- coef(logit.glm)
odds_ratios <- exp(coefficients)
conf_interval <- confint(logit.glm)
or_conf_interval <- exp(conf_interval)
print(odds_ratios)
####填补数据####
library(mice)
library(randomForest)
mineral_data <- mice(mineral_data, method="rf")
mineral_mice <- complete(mineral_data)
view(mineral_mice)
write_xpt(mineral_mice,"mineral_mice.xpt")
#将教育合并#
table(mineral_mice$Education_level)
mineral_mice$Education_level[mineral_mice$Education_level == 7|mineral_mice$Education_level==9] <- 2
mineral_mice$Education2 <- ifelse(mineral_mice$Education_level == 1, 1,
                              ifelse(mineral_mice$Education_level %in% c(2, 3), 2, 3))
table(mineral_mice$Education2)
#RIP分级#
mineral_mice$RIP_level<- ifelse(mineral_mice$RIP <= 1.3, 1,
                            ifelse(mineral_mice$RIP <= 3.5, 2, 3))
table(mineral_mice$RIP_level)
#PA分级#
mineral_mice$PA_level <- ifelse(mineral_mice$PAALL < 600, 1,
                            ifelse(mineral_mice$PAALL < 4000, 2, 3))
table(mineral_mice$PA_level)
#将eGFR变成分类计算相关性,1为low_eGFR(<60)#
mineral_mice$low_eGFR<-ifelse(mineral_mice$ckdepi< 60 , 1, 0)
table(mineral_mice$low_eGFR)
#将ACR变成分类计算相关性,1为蛋白尿ACR≥30#
mineral_mice$pro_urea<-ifelse(mineral_mice$Acr>= 30 , 1, 0)
####计算四分位数####
quantiles<-quantile(mineral_mice$Ca_all)
table(quantiles)
mineral_mice$Ca_Quintiles<-cut(mineral_mice$Ca_all, breaks = quantiles,
                               labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE)

quantiles<-quantile(mineral_mice$P_all)
table(quantiles)
mineral_mice$P_Quintiles<-cut(mineral_mice$P_all, breaks = quantiles,
                              labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE)


quantiles<-quantile(mineral_mice$Mg_all)
table(quantiles)
mineral_mice$Mg_Quintiles<-cut(mineral_mice$Mg_all, breaks = quantiles,
                               labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE)


quantiles<-quantile(mineral_mice$Fe_all)
table(quantiles)
mineral_mice$Fe_Quintiles<-cut(mineral_mice$Fe_all, breaks = quantiles,
                               labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE)


quantiles<-quantile(mineral_mice$Zn_all)
table(quantiles)
mineral_mice$Zn_Quintiles<-cut(mineral_mice$Zn_all, breaks = quantiles,
                               labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE)


quantiles<-quantile(mineral_mice$Cu_all)
table(quantiles)
mineral_mice$Cu_Quintiles<-cut(mineral_mice$Cu_all, breaks = quantiles,
                               labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE)


quantiles<-quantile(mineral_mice$Na_all)
table(quantiles)
mineral_mice$Na_Quintiles<-cut(mineral_mice$Na_all, breaks = quantiles,
                               labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE)


quantiles<-quantile(mineral_mice$K_all)
table(quantiles)
mineral_mice$K_Quintiles<-cut(mineral_mice$K_all, breaks = quantiles,
                              labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE)


quantiles<-quantile(mineral_mice$Se_all)
table(quantiles)
mineral_mice$Se_Quintiles<-cut(mineral_mice$Se_all, breaks = quantiles,
                               labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE)

view(mineral_mice)
write_xpt(mineral_mice,"mineral_mice11_2.xpt")
####设定权重数据####
DR1TOT_G<-read.xport('DR1TOT_G.XPT')[,c("SEQN","WTDRD1","WTDR2D")]
DR2TOT_G<-read.xport('DR2TOT_G.XPT')[,c("SEQN","WTDRD1","WTDR2D")]
# 使用full_join()函数合并数据框，并以SENQ列为基础
library(dplyr)
WEIGHTS <- full_join(DR1TOT_G, DR2TOT_G, by = "SEQN")
View(WEIGHTS)
# 对于每个重复的列，使用coalesce合并
WEIGHTS <- WEIGHTS %>%
  dplyr::mutate(WTDRD1 = coalesce(WTDRD1.x, WTDRD1.y)) %>%
  dplyr::mutate(WTDR2D = coalesce(WTDR2D.x, WTDR2D.y)) %>%
  dplyr::select(-dplyr::ends_with(".x"), -dplyr::ends_with(".y"))
#删除权重有缺失值
WEIGHTS <- WEIGHTS %>% 
  dplyr::filter(!is.na(WTDR2D))
#合并权重
mineral_mice<-merge(mineral_mice,WEIGHTS, by = "SEQN", 
                    all = FALSE)
view(mineral_mice)
write_xpt(mineral_mice,"mineral_mice.xpt")

designdata <- svydesign(id=~SDMVPSU, strata=~SDMVSTRA,
                        weights=~WTDR2D, nest=TRUE,
                        data=mineral_mice)

dput(names(mineral_mice))

#### 正态性检验(样本超5000)，取ln值####
#"SEQN","Ca_all","P_all","Mg_all","Fe_all","Zn_all","Cu_all","Na_all","K_all","Se_all"
hist(mineral_mice$Age)
hist(mineral_mice$BMI)
hist(mineral_mice$Hba1c)
hist(mineral_mice$HGB)
hist(mineral_mice$TG)
hist(mineral_mice$LDL)
hist(mineral_mice$HDL)
hist(mineral_mice$TC)
hist(mineral_mice$Alb)
hist(mineral_mice$BUN)
hist(mineral_mice$sCr)
hist(mineral_mice$UA)
hist(mineral_mice$Acr)
hist(mineral_mice$ckdepi)
hist(mineral_mice$Engergy)


hist(mineral_mice$Ca_all)
hist(mineral_mice$P_all)
hist(mineral_mice$Mg_all)
hist(mineral_mice$Fe_all)
hist(mineral_mice$Zn_all)
hist(mineral_mice$Cu_all)
hist(mineral_mice$Na_all)
hist(mineral_mice$K_all)
hist(mineral_mice$Se_all)

mineral_mice2<-mineral_mice
mineral_mice2<-mineral_mice2%>% mutate(ln_Ca = log(Ca_all))
mineral_mice2<-mineral_mice2%>% mutate(ln_P = log(P_all))
mineral_mice2<-mineral_mice2%>% mutate(ln_Mg = log(Mg_all))
mineral_mice2<-mineral_mice2%>% mutate(ln_Fe = log(Fe_all))
mineral_mice2<-mineral_mice2%>% mutate(ln_Zn = log(Zn_all))
mineral_mice2<-mineral_mice2%>% mutate(ln_Cu = log(Cu_all))
mineral_mice2<-mineral_mice2%>% mutate(ln_Na = log(Na_all))
mineral_mice2<-mineral_mice2%>% mutate(ln_K = log(K_all))
mineral_mice2<-mineral_mice2%>% mutate(ln_Se = log(Se_all))
view(mineral_mice2)

hist(mineral_mice2$ln_Ca)
hist(mineral_mice2$ln_P)
hist(mineral_mice2$ln_Mg)
hist(mineral_mice2$ln_Fe)
hist(mineral_mice2$ln_Zn)
hist(mineral_mice2$ln_Cu)
hist(mineral_mice2$ln_Na)
hist(mineral_mice2$ln_K)
hist(mineral_mice2$ln_Se)
write_xpt(mineral_mice2,"mineral_mice2.xpt")
library(dplyr)
# 假设df是你的数据框，A是你想要计算自然对数的列
df <- df %>% mutate(NewColumn = log(A))


####设置参数####
allVars <- c("Gender", "Age", "Race", "Education2","RIP_level","Marital", "SMQ", 
             "PA_level","ALQ", "DM", "HBP", "Cancer", "Gout", "CVD",  "BMI", 
             "Hba1c", "HGB", "TG", "LDL", "HDL", "TC", "Alb", "BUN", "sCr", 
             "UA", "Acr", "ckdepi", "CKD", "Engergy",  "low_eGFR", "pro_urea",
            "Ca_all" ,"P_all","Mg_all","Fe_all","Zn_all","Cu_all","Na_all",
            "K_all","Se_all") #总变量
fvars <- c("Gender", "Race", "Education2","Marital", "SMQ","ALQ", "RIP_level","PA_level",
           "DM", "HBP", "Cancer", "Gout", "CKD","CVD", "low_eGFR", "pro_urea") #分类变量
bb <- c("TG","LDL","HDL","BUN","sCr","Acr","Ca_all","P_all","Mg_all","Fe_all",
        "Zn_all","Cu_all","Na_all","K_all","Se_all") #偏态分布

####不加权基线表####
library(haven)
library(Hmisc)
library(tableone)
library(survey)
Svytab1 <- CreateTableOne(vars=allVars, strata="CKD", data=mineral_mice,
                          factorVars=fvars,addOverall=T)
tab1_CKD <- print(Svytab1, factorVars=fvars, nonnormal=bb,
                  quote=T, showAllLevels=T, smd=T, missing=F)
write.csv(tab1_CKD,"tab1_CKD.csv")
####（不加权！）加权基线表####
Svytab2 <- svyCreateTableOne(vars=allVars, strata="CKD", data=designdata, factorVars=fvars,addOverall=T)
tab2_svy_CKD <- print(Svytab2, factorVars=fvars, nonnormal=bb,quote=T, showAllLevels=T, smd=T, missing=F)
write.csv(tab2_svy_CKD,"tab2_svy_CKD.csv")

####logistic回归####
library(survey)
# 将Quintiles列转换为虚拟变量（因子）,Q1为参照#
view(mineral_mice)
mineral_mice$Ca_Quintiles <- relevel(factor (mineral_mice$Ca_Quintiles), ref = "1")
mineral_mice$P_Quintiles <- relevel(factor (mineral_mice$P_Quintiles), ref = "1")
mineral_mice$Mg_Quintiles <- relevel(factor (mineral_mice$Mg_Quintiles), ref = "1")
mineral_mice$Fe_Quintiles <- relevel(factor (mineral_mice$Fe_Quintiles), ref = "1")
mineral_mice$Zn_Quintiles <- relevel(factor (mineral_mice$Zn_Quintiles), ref = "1")
mineral_mice$Cu_Quintiles <- relevel(factor (mineral_mice$Cu_Quintiles), ref = "1")
mineral_mice$Na_Quintiles <- relevel(factor (mineral_mice$Na_Quintiles), ref = "1")
mineral_mice$K_Quintiles <- relevel(factor (mineral_mice$K_Quintiles), ref = "1")
mineral_mice$Se_Quintiles <- relevel(factor (mineral_mice$Se_Quintiles), ref = "1")

dput(names(mineral_mice))
mineral_mice$Race <- as.factor(mineral_mice$Race)
mineral_mice$Gender <- as.factor(mineral_mice$Gender)
mineral_mice$Education_leve <- as.factor(mineral_mice$Education_level)
mineral_mice$Marital <- as.factor(mineral_mice$Marital)
mineral_mice$SMQ<- as.factor(mineral_mice$SMQ)
mineral_mice$AL<- as.factor(mineral_mice$ALQ)
mineral_mice$HBP<- as.factor(mineral_mice$HBP)
mineral_mice$DM<- as.factor(mineral_mice$DM)
mineral_mice$CVD<- as.factor(mineral_mice$CVD)

#(不需要) 将数值标准化#
library(survey)
dput(names(mineral_mice))

Se_sd <- sd(mineral_mice$Se_all)
mineral_mice$Se_standardized <- (mineral_mice$Se_all - mean(mineral_mice$Se_all)) / Se_sd
designdata <- svydesign(id=~SDMVPSU, strata=~SDMVSTRA,
                        weights=~WTDR2D, nest=TRUE,
                        data=mineral_mice)

####连续变量####
#Ca
logit.glm <- glm( CKD~ln_Ca, family = binomial,data = mineral_mice)
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

logit.glm <- glm( CKD~ln_Ca+Age+Gender+Race, data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

logit.glm <- glm( CKD~ln_Ca+Age+Gender+Race+BMI
                  +ALQ+SMQ+HBP+DM+CVD+Engergy, data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

#P
logit.glm <- glm( CKD~ln_P, data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

logit.glm <- glm( CKD~ln_P+Age+Gender+Race, data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

logit.glm <- glm( CKD~ln_P+Age+Gender+Race+BMI
                  +ALQ+SMQ+HBP+DM+CVD+Engergy, data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

#Mg
logit.glm <- glm( CKD~ln_Mg, data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

logit.glm <- glm( CKD~ln_Mg+Age+Gender+Race, data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

logit.glm <- glm( CKD~ln_Mg+Age+Gender+Race+BMI
                  +ALQ+SMQ+HBP+DM+CVD+Engergy, data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

#Fe
logit.glm <- glm( CKD~ln_Fe, data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

logit.glm <- glm( CKD~ln_Fe+Age+Gender+Race, data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

logit.glm <- glm( CKD~ln_Fe+Age+Gender+Race+BMI
                  +ALQ+SMQ+HBP+DM+CVD+Engergy,data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

#Zn
logit.glm <- glm( CKD~ln_Zn, data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

logit.glm <- glm( CKD~ln_Zn+Age+Gender+Race, data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

logit.glm <- glm( CKD~ln_Zn+Age+Gender+Race+BMI
                  +ALQ+SMQ+HBP+DM+CVD+Engergy,data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

#Cu
logit.glm <- glm( CKD~ln_Cu, data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

logit.glm <- glm( CKD~ln_Cu+Age+Gender+Race,data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

logit.glm <- glm( CKD~ln_Cu+Age+Gender+Race+BMI
                  +ALQ+SMQ+HBP+DM+CVD+Engergy, data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

#Na
logit.glm <- glm( CKD~ln_Na, data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

logit.glm <- glm( CKD~ln_Na+Age+Gender+Race, data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

logit.glm <- glm( CKD~ln_Na+Age+Gender+Race+BMI
                  +ALQ+SMQ+HBP+DM+CVD+Engergy, data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

#K
logit.glm <- glm( CKD~ln_K, data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

logit.glm <- glm( CKD~ln_K+Age+Gender+Race, data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

logit.glm <- glm( CKD~ln_K+Age+Gender+Race+BMI
                  +ALQ+SMQ+HBP+DM+CVD+Engergy, data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)


#Se
logit.glm <- glm( CKD~ln_Se, data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

logit.glm <- glm( CKD~ln_Se+Age+Gender+Race,data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

logit.glm <- glm( CKD~ln_Se+Age+Gender+Race+BMI
                  +ALQ+SMQ+HBP+DM+CVD+Engergy, data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

####四分位####
#Ca
#计算p for trend#
mineral_mice$Ca_Quintiles <- as.numeric(mineral_mice$Ca_Quintiles)
logit.glm <- glm( CKD~Ca_Quintiles, data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

logit.glm <- glm( CKD~Ca_Quintiles+Age+Gender+Race,data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

logit.glm <- glm( CKD ~ Ca_Quintiles+Age+Gender+Race+BMI+ALQ+SMQ+HBP+DM+
                    CVD+Engergy, data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

#P
#计算p for trend#
mineral_mice$P_Quintiles <- as.numeric(mineral_mice$P_Quintiles)
logit.glm <- glm( CKD~P_Quintiles, data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

logit.glm <- glm( CKD~P_Quintiles+Age+Gender+Race,data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

logit.glm <- glm( CKD~P_Quintiles+Age+Gender+Race+BMI
                  +ALQ+SMQ+HBP+DM+CVD,data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

#Mg
#计算p for trend#
mineral_mice$Mg_Quintiles <- as.numeric(mineral_mice$Mg_Quintiles)
logit.glm <- glm( CKD~Mg_Quintilesg, data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

logit.glm <- glm( CKD~Mg_Quintiles+Age+Gender+Race,data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

logit.glm <- glm( CKD~Mg_Quintiles+Age+Gender+Race+BMI
                  +ALQ+SMQ+HBP+DM+CVD+Engergy, data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

#Fe
#计算p for trend#
mineral_mice$Fe_Quintiles <- as.numeric(mineral_mice$Fe_Quintiles)
logit.glm <- glm( CKD~Fe_Quintiles,data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

logit.glm <- glm( CKD~Fe_Quintiles+Age+Gender+Race,data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

logit.glm <- glm( CKD~Fe_Quintiles+Age+Gender+Race+BMI
                  +ALQ+SMQ+HBP+DM+CVD+Engergy, data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

#Zn
#计算p for trend#
mineral_mice$Zn_Quintiles <- as.numeric(mineral_mice$Zn_Quintiles)
logit.glm <- glm( CKD~Zn_Quintiles, data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

logit.glm <- glm( CKD~Zn_Quintiles+Age+Gender+Race,data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

logit.glm <- glm( CKD~Zn_Quintiles+Age+Gender+Race+BMI
                  +ALQ+SMQ+HBP+DM+CVD+Engergy, data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

#Cu
#计算p for trend#
mineral_mice$Cu_Quintiles <- as.numeric(mineral_mice$Cu_Quintiles)
logit.glm <- glm( CKD ~ Cu_Quintiles,data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

logit.glm <- glm( CKD~Cu_Quintiles+Age+Gender+Race, data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

logit.glm <- glm( CKD~Cu_Quintiles+Age+Gender+Race+BMI
                  +ALQ+SMQ+HBP+DM+CVD+Engergy, data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

#Na
#计算p for trend#
mineral_mice$Na_Quintiles <- as.numeric(mineral_mice$Na_Quintiles)
logit.glm <- glm( CKD~Na_Quintiles,data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

logit.glm <- glm( CKD~Na_Quintiles+Age+Gender+Race, data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

logit.glm <- glm( CKD~Na_Quintiles+Age+Gender+Race+BMI
                  +ALQ+SMQ+HBP+DM+CVD+Engergy, data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

#K
#计算p for trend#
mineral_mice$K_Quintiles <- as.numeric(mineral_mice$K_Quintiles)
logit.glm <- glm( CKD~K_Quintiles, data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

logit.glm <- glm( CKD~K_Quintiles+Age+Gender+Race,data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

logit.glm <- glm( CKD~K_Quintiles+Age+Gender+Race+BMI
                  +ALQ+SMQ+HBP+DM+CVD+Engergy, data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)


#Se
#计算p for trend#
mineral_mice$Se_Quintiles <- as.numeric(mineral_mice$Se_Quintiles)
logit.glm <- glm( CKD~Se_Quintiles,data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

logit.glm <- glm( CKD~Se_Quintiles+Age+Gender+Race, data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)

logit.glm <- glm( CKD~Se_Quintiles+Age+Gender+Race+BMI
                  +ALQ+SMQ+HBP+DM+CVD+Engergy, data = mineral_mice, family = "binomial")
summary(logit.glm)
odds_ratio <- exp(coef(logit.glm))
ci<-confint(logit.glm)
or_conf_interval<-exp(ci)
print(odds_ratio)
print(or_conf_interval)
####计算p for trend####
mineral_mice$Se_Quintiles <- as.numeric(mineral_mice$Se_Quintiles)
model <- glm(CKD ~ Se_Quintiles+Age+Gender+Race, design = designdata, family = quasibinomial)
summary(model)$coefficients
p_value_for_trend <- summary(model)$coefficients["Se_Quintiles", "Pr(>|t|)"]
print(p_value_for_trend)

model <- svyglm(CKD ~ Se_Quintiles+Age+Gender+Race+BMI
                +ALQ+SMQ+PAALL+HBP+DM+CVD+Engergy, design = designdata, family = quasibinomial)
summary(model)$coefficients
p_value_for_trend <- summary(model)$coefficients["Se_Quintiles", "Pr(>|t|)"]
print(p_value_for_trend)

####限制性立方样条####
library(gtsummary)
library(survey)
library(haven)
library(rms)
dput(names(mineral_mice))

designdata <- svydesign(id=~SDMVPSU, strata=~SDMVSTRA,
                        weights=~WTDR2D, nest=TRUE,
                        data=mineral_mice)

svy.fit <- svyglm(CKD ~ rcs(ln_Ca,4)+Age+Gender+Race+BMI
                  +ALQ+SMQ+PAALL+Engergy+HBP+DM+CVD,
                  family="quasibinomial",
                  design=designdata)

data <- svy.fit$survey.design$variables
ori.weight <- 1/(svy.fit$survey.design$prob)
mean.weight <- mean(ori.weight)
data$weights <- ori.weight/mean.weight
dd <- rms::datadist(data)
options(datadist="dd")

data$CKD <- as.numeric(data$CKD)
view(data)

fit.rcs <- rms::Glm(CKD ~ rcs(ln_Ca,4)+Age+Gender+Race+BMI
                    +ALQ+SMQ+PAALL+Engergy+HBP+DM+CVD,
                    data=data,
                    family="quasibinomial", weights=weights,
                    normwt=TRUE, control=list(eps=1e-8))
# AIC的值
extractAIC(fit.rcs)
# 非线性检验
anova(fit.rcs)
OR <- rms::Predict(fit.rcs, ln_Ca, type="predictions", fun=exp, ref.zero=TRUE)
pdf("ln_Ca_rcs.pdf")

ggplot() +
  geom_line(data=OR, aes(x=ln_Ca, y=yhat), linetype="solid", linewidth=1, alpha=0.7, color="red") +
  geom_ribbon(data=OR, aes(x=ln_Ca, ymin=lower, ymax=upper), alpha=0.1, fill="pink") +
  geom_hline(yintercept=1, linetype=2, color="grey") +
  scale_x_continuous("Ca") +
  scale_y_continuous("OR (95% CI)") +
  geom_text(aes(x=0.1, y=2.5,
                label=paste0("P-overall < 0.0001", "\nP-non-linear = 0.1140")), hjust=0) +
  theme_bw() +
  theme(axis.line=element_line(),
        panel.grid=element_blank(),
        panel.border=element_blank())
dev.off()

####WQS####
library(gWQS)
view(mineral_mice)
print(colnames(mineral_mice))
# 设置矿物质名称列
ln_minerals <- names(mineral_mice)[44:52]
#或者
ln_minerals <- c("ln_Ca", "ln_Cu", "ln_Fe", "ln_K", "ln_Mg", "ln_Na", "ln_P", 
                 "ln_Se", "ln_Zn")
table(ln_minerals)

#model 3
results <- gwqs(CKD ~ wqs+Age+Gender+Race+BMI+ALQ+SMQ+HBP+DM, 
                mix_name =ln_minerals, data = mineral_mice,
                q = 10, validation = 0.6, b = 100,
                b1_pos = FALSE, b_constr = FALSE, family = "binomial",
                seed = 213, plots = TRUE, tables = TRUE)
# 输出模型摘要和结果表格
gwqs_summary_tab(results)
summary(results)
# 提取和显示每个矿物质的权重
weights <- results$final_weights
print(weights)
# 提取回归系数和标准误
coef <- summary(results)$coef
wqs_coef <- coef["wqs", "Estimate"]
wqs_se <- coef["wqs", "Std. Error"]

# 计算 OR 值及其 95% 置信区间
OR <- exp(wqs_coef)
CI_lower <- exp(wqs_coef - 1.96 * wqs_se)
CI_upper <- exp(wqs_coef + 1.96 * wqs_se)

# 显示结果
cat("WQS OR 值: ", OR, "\n")
cat("95% 置信区间: [", CI_lower, ", ", CI_upper, "]\n")

w_ord <- order(results$final_weights$mean_weight)
mean_weight <- results$final_weights$mean_weight[w_ord]
mix_name <- factor(results$final_weights$mix_name[w_ord],
                   levels = results$final_weights$mix_name[w_ord])
data_plot <- data.frame(mean_weight, mix_name)
ggplot(data_plot, aes(x = mix_name, y = mean_weight, fill = mix_name)) +
  geom_bar(stat = "identity", color = "black") + theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(color='black'),
        legend.position = "none") + coord_flip()
####qgcomp####
# 加载qgcomp包
library(qgcomp)
# 定义变量
ln_minerals <- c("ln_Ca", "ln_Cu", "ln_Fe", "ln_K", "ln_Mg", "ln_Na", "ln_P", 
                 "ln_Se", "ln_Zn")
covars <- c("Age", "Gender", "Race", "BMI", "ALQ", "SMQ", "HBP", "DM", "CVD",
            "Engergy", "PAALL")  
# 使用reformulate构建模型公式，合并所有变量
all_vars <- c(ln_minerals, covars)
formula_str <- paste("CKD ~", paste(all_vars, collapse = "+"))

# 将字符串转换为公式
full_formula <- as.formula(formula_str)

# 使用qgcomp.noboot拟合模型
qc.fit <- qgcomp.noboot(full_formula, expnms=ln_minerals, 
                        data = mineral_mice[, c(ln_minerals, 'CKD', covars)], 
                        family=binomial(), q=4)
plot(qc.fit)
view(mineral_mice)

weights <- qc.fit$fit$coefficients[names(qc.fit$fit$coefficients) %in% ln_minerals]
normalized_weights <- weights / sum(abs(weights))
print(normalized_weights)
# 计算权重的绝对值之和
abs_sum_weights <- sum(abs(weights))
# 归一化权重
normalized_weights <- weights / abs_sum_weights
# 打印归一化后的权重
print(normalized_weights)
# 计算归一化权重的绝对值之和
sum(abs(normalized_weights))

#或者
# 计算正权重和负权重的总和
positive_sum <- sum(weights[weights > 0])
negative_sum <- sum(abs(weights[weights < 0]))
# 归一化正权重
normalized_positive_weights <- weights[weights > 0] / positive_sum
# 归一化负权重
normalized_negative_weights <- weights[weights < 0] / negative_sum
# 将归一化的正负权重组合起来
normalized_weights_alt <- c(normalized_positive_weights, normalized_negative_weights)
# 打印归一化后的权重
print(normalized_weights_alt)
# 检查正权重之和和负权重之和
positive_sum_check <- sum(normalized_weights_alt[normalized_weights_alt > 0])
negative_sum_check <- sum(abs(normalized_weights_alt[normalized_weights_alt < 0]))
print(positive_sum_check)
print(negative_sum_check)




# 提取整体效应的log(OR)和标准误差
overall_effect <- qc.fit$psi
overall_se <- qc.fit$var.psi

# 计算整体效应的OR值和95%置信区间
overall_or <- exp(overall_effect)
overall_ci_lower <- exp(overall_effect - 1.96 * overall_se)
overall_ci_upper <- exp(overall_effect + 1.96 * overall_se)

# 计算总体效应的p值
z_value <- overall_effect / overall_se
p_value <- 2 * (1 - pnorm(abs(z_value)))

# 输出结果
overall_table <- data.frame(
  Overall_OR = overall_or,
  CI_Lower = overall_ci_lower,
  CI_Upper = overall_ci_upper,
  p_value = p_value
)

print(overall_table)

# 创建用于绘图的数据框
str(qc.fit)
# 提取系数和变量名称
coefficients <- qc.fit$fit$coefficients[ln_minerals]
variable_names <- names(coefficients)

# 创建仅包含ln_minerals的绘图数据框
plot_data <- data.frame(
  Variable = variable_names,
  Weight = coefficients
)
# 给权重赋予正负标签
plot_data$Sign <- ifelse(plot_data$Weight > 0, "Positive", "Negative")
# 使用ggplot2绘图
ggplot(plot_data, aes(x = Variable, y = Weight, fill = Sign)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # 将坐标翻转以获得水平条形图
  scale_fill_manual(values = c("Negative" = "#A1A9D0", "Positive" = "#96CCCB")) +  # 自定义正负权重的颜色
  labs(x = "Variable", y = "Weights", title = "Association between Qgcomp index and CKD") +  # 添加轴标签和标题
  theme_minimal() +  # 使用简洁的主题
  theme(legend.position = "bottom",  # 调整图例位置
        axis.text.y = element_text(size = 12),  # 调整y轴字体大小
        axis.text.x = element_text(size = 12),  # 调整x轴字体大小
        plot.title = element_text(hjust = 0.5, size = 14))  # 调整标题位置和大小
#如何排序？从大到小？排序
# 加载必要的包
library(ggplot2)
# 创建用于绘图的数据框
plot_data <- data.frame(
  Variable = variable_names,
  Weight = coefficients
)
# 给权重赋予正负标签
plot_data$Sign <- ifelse(plot_data$Weight > 0, "Positive", "Negative")
# 使用ggplot2绘图，这次在aes内使用reorder对Variable进行排序
# 按照Weight的绝对值进行排序
ggplot(plot_data, aes(x = reorder(Variable, -abs(Weight)), y = Weight, fill = Sign)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # 将坐标翻转以获得水平条形图
  scale_fill_manual(values = c("Negative" = "#A1A9D0", "Positive" = "#96CCCB")) +  # 自定义正负权重的颜色
  labs(x = "Variable", y = "Weights", title = "Association between Qgcomp index and CKD") +  # 添加轴标签和标题
  theme_minimal() +  # 使用简洁的主题
  theme(legend.position = "bottom",  # 调整图例位置
        axis.text.y = element_text(size = 12),  # 调整y轴字体大小
        axis.text.x = element_text(size = 12),  # 调整x轴字体大小
        plot.title = element_text(hjust = 0.5, size = 14))
# 打印图表
print(ggplot_object)
####ENET####


####pearson####
# 载入需要的包
library(corrplot)
library(ggplot2)
library(ggcorrplot)
library(vcd)
library(psych)
library(ggrepel)
#创建新的数据框#
mineral_pearson<-mineral_mice[, c("ln_Ca","ln_P","ln_Mg","ln_Na","ln_Fe","ln_Zn","ln_Cu","ln_K","ln_Se")]
view(mineral_pearson)
write_xpt(mineral_pearson,"mineral_pearson.xpt")
mineral_pearson<-read_xpt("mineral_pearson.xpt")
mineral_pearson<-as.matrix(mineral_pearson) #利用as.matrix()将所需数据集转换为matrix格式，才可在corrplot中跑
#相关性计算
mineral_pearson<-cor(mineral_pearson,method="spearman") #pearson，spearman和kendall
round(mineral_pearson, 2)#保留两位小数

#计算p值
# 假设dataframe是你的数据帧，column1和column2是你想要分析的列名
# 计算column1和column2之间的Pearson相关性
result <- cor.test(mineral_pearson$ln_K, mineral_pearson$ln_Se, method = "pearson")
# 显示p值
p_value_formatted <- sprintf("%.8f", result$p.value)
print(p_value_formatted)


#相关性热图绘制#
ggcorrplot(mineral_pearson, method="circle") #圆圈大小变化
#调整与美化#
ggcorrplot(mineral_pearson, method = "circle", #"square", "circle"相比corrplot少了很多种，只有方形和圆形，默认方形。
           type ="upper" , #full完全(默认)，lower下三角，upper上三角
           ggtheme = ggplot2::theme_minimal,
           title = "",
           show.legend = TRUE,  #是否显示图例。
           legend.title = "Corr", #指定图例标题。
           show.diag =T ,    #FALSE显示中间
           colors = c("blue", "white", "red"), #需要长度为3的颜色向量，同时指定low,mid和high处的颜色。
           outline.color = "gray", #指定方形或圆形的边线颜色。
           hc.order = FALSE,  #是否按hclust(层次聚类顺序)排列。
           hc.method = "complete", #相当于corrplot中的hclust.method, 指定方法一样，详情见?hclust。
           lab =T , #是否添加相关系数。FALSE
           lab_col = "black", #指定相关系数的颜色，只有当lab=TRUE时有效。
           lab_size = 4, #指定相关系数大小，只有当lab=TRUE时有效。
           p.mat = NULL,  #p.mat= p_mat,insig= "pch", pch.col= "red", pch.cex= 4,
           sig.level = 0.05,
           insig = c("pch", "blank"),
           tl.cex = 12, #指定变量文本的大小，
           tl.col = "black", #指定变量文本的颜色，
           tl.srt = 45, #指定变量文本的旋转角度。
           digits = 2 #指定相关系数的显示小数位数(默认2)。
)
#corrplot包绘图#
corrplot(mineral_pearson)
corrplot(mineral_pearson, method="circle", #square方形，ellipse, 椭圆形，number数值，shade阴影，color颜色，pie饼图
         title = "pearson",   #指定标题
         type="full",  #full完全(默认)，lower下三角，upper上三角
         #col=c("#FF6666", "white", "#0066CC"), #指定图形展示的颜色，默认以均匀的颜色展示。支持grDevices包中的调色板，也支持RColorBrewer包中调色板。
         outline = T,  #是否添加圆形、方形或椭圆形的外边框，默认为FALSE。
         diag = TRUE,  #是否展示对角线上的结果，默认为TRUE
         mar = c(0,0,0,0), #设置图形的四边间距。数字分别对应(bottom, left, top, right)。
         bg="white", #指定背景颜色
         add = FALSE, #表示是否添加到已经存在的plot中。默认FALSE生成新plot。
         is.corr = TRUE, #是否为相关系数绘图，默认为TRUE,FALSE则可将其它数字矩阵进行可视化。
         addgrid.col = "darkgray", #设置网格线颜色，当指定method参数为color或shade时， 默认的网格线颜色为白色，其它method则默认为灰色，也可以自定义颜色。
         addCoef.col = NULL, #设置相关系数值的颜色，只有当method不是number时才有效
         addCoefasPercent = FALSE, #是否将相关系数转化为百分比形式，以节省空间，默认为FALSE。
         order = "original", #指定相关系数排序的方法, 可以是original原始顺序，AOE特征向量角序，FPC第一主成分顺序，hclust层次聚类顺序，alphabet字母顺序。
         hclust.method = "complete", # 指定hclust中细分的方法，只有当指定order参数为hclust时有效。有7种可选：complete,ward,single,average,mcquitty,median,centroid。
         addrect = NULL, #是否添加矩形框，只有当指定order参数为hclust时有效， 默认不添加， 用整数指定即可添加。
         rect.col = "black", #指定矩形框的颜色。
         rect.lwd = 2, #指定矩形框的线宽。
         tl.pos = NULL,  #指定文本标签(变量名称)相对绘图区域的位置，为"lt"(左侧和顶部),"ld"(左侧和对角线),"td"(顶部和对角线),"d"(对角线),"n"(无);当type="full"时默认"lt"。当type="lower"时默认"ld"。当type="upper"时默认"td"。
         tl.cex = 1,  #设置文本标签的大小
         tl.col = "black", #设置文本标签的颜色。
         cl.pos = NULL #设置图例位置，为"r"(右边),"b"(底部),"n"(无)之一。当type="full"/"upper"时，默认"r"; 当type="lower"时，默认"b"。
         #addshade = c("negative", "positive", "all"), # 表示给增加阴影，只有当method="shade"时有效。#为"negative"(对负相关系数增加阴影135度)；"positive"(对正相关系数增加阴影45度)；"all"(对所有相关系数增加阴影)。
         #shade.lwd = 1,  #指定阴影线宽。
         #shade.col = "white",  #指定阴影线的颜色。
         #p.mat= res1$p,sig.level= 0.01,insig= "pch", pch.col= "blue", pch.cex= 3,#只有指定矩阵的P值，sig.level，pch等参数才有效。只有当insig = "pch"时，pch.col和pch.cex参数才有效。
)
#显示数字与图形混合
corrplot(mineral_pearson, method="circle", #square方形，ellipse, 椭圆形，number数值，shade阴影，color颜色，pie饼图
         title = "pearson",   #指定标题
         type="full", #full完全(默认)，lower下三角，upper上三角
         #col=c("#FF6666", "white", "#0066CC"), #指定图形展示的颜色，默认以均匀的颜色展示。支持grDevices包中的调色板，也支持RColorBrewer包中调色板。
         outline = F,  #是否添加圆形、方形或椭圆形的外边框，默认为FALSE。
         diag = TRUE,  #是否展示对角线上的结果，默认为TRUE
         mar = c(0,0,0,0), #设置图形的四边间距。数字分别对应(bottom, left, top, right)。
         bg="white", #指定背景颜色
         add = FALSE, #表示是否添加到已经存在的plot中。默认FALSE生成新plot。
         is.corr = TRUE, #是否为相关系数绘图，默认为TRUE,FALSE则可将其它数字矩阵进行可视化。
         addgrid.col = "darkgray", #设置网格线颜色，当指定method参数为color或shade时， 默认的网格线颜色为白色，其它method则默认为灰色，也可以自定义颜色。
         addCoef.col = NULL, #设置相关系数值的颜色，只有当method不是number时才有效
         addCoefasPercent = FALSE, #是否将相关系数转化为百分比形式，以节省空间，默认为FALSE。
         order = "original", #指定相关系数排序的方法, 可以是original原始顺序，AOE特征向量角序，FPC第一主成分顺序，hclust层次聚类顺序，alphabet字母顺序。
         hclust.method = "complete", # 指定hclust中细分的方法，只有当指定order参数为hclust时有效。有7种可选：complete,ward,single,average,mcquitty,median,centroid。
         addrect = NULL, #是否添加矩形框，只有当指定order参数为hclust时有效， 默认不添加， 用整数指定即可添加。
         rect.col = "black", #指定矩形框的颜色。
         rect.lwd = 2, #指定矩形框的线宽。
         tl.pos = NULL,  #指定文本标签(变量名称)相对绘图区域的位置，为"lt"(左侧和顶部),"ld"(左侧和对角线),"td"(顶部和对角线),"d"(对角线),"n"(无);当type="full"时默认"lt"。当type="lower"时默认"ld"。当type="upper"时默认"td"。
         tl.cex = 1,  #设置文本标签的大小
         tl.col = "black", #设置文本标签的颜色。
         cl.pos = NULL #设置图例位置，为"r"(右边),"b"(底部),"n"(无)之一。当type="full"/"upper"时，默认"r"; 当type="lower"时，默认"b"。
         #addshade = c("negative", "positive", "all"), # 表示给增加阴影，只有当method="shade"时有效。#为"negative"(对负相关系数增加阴影135度)；"positive"(对正相关系数增加阴影45度)；"all"(对所有相关系数增加阴影)。
         #shade.lwd = 1,  #指定阴影线宽。
         #shade.col = "white",  #指定阴影线的颜色。
         #p.mat= res1$p,sig.level= 0.01,insig= "pch", pch.col= "blue", pch.cex= 3,#只有指定矩阵的P值，sig.level，pch等参数才有效。只有当insig = "pch"时，pch.col和pch.cex参数才有效。
)
corrplot(mineral_pearson, title = "",        
         method = "number", #square方形，ellipse, 椭圆形，number数值，shade阴影，color颜色，pie饼图       
         outline = F, #是否添加圆形、方形或椭圆形的外边框，默认为FALSE。
         add = TRUE, #表示是否添加到已经存在的plot中。默认FALSE生成新plot。
         type = "lower", #full完全(默认)，lower下三角，upper上三角       
         order="original",
         col="black", #指定图形展示的颜色，默认以均匀的颜色展示。支持grDevices包中的调色板，也支持RColorBrewer包中调色板。
         diag=FALSE, #是否展示对角线上的结果，默认为TRUE
         tl.pos="n",  #指定文本标签(变量名称)相对绘图区域的位置，为"lt"(左侧和顶部),"ld"(左侧和对角线),"td"(顶部和对角线),"d"(对角线),"n"(无)
         cl.pos=NULL #设置图例位置，为"r"(右边),"b"(底部),"n"(无)之一。
)


# 方法2 使用 corrplot 包绘制相关性热图
cor_matrix <- cor(mineral_pearson, use = "complete.obs", method = "pearson")
corrplot(cor_matrix, method = "circle",  
         order = "hclust", tl.cex = 0.6, tl.col = "black", 
         cl.cex = 0.6, addCoef.col = "black", 
         number.cex = 0.7, sig.level = 0.05, insig = "blank")

####BKMR####
#将分类变量转换为因子型
mineral_mice$Gender = factor(mineral_mice$Gender)
mineral_mice$Race = factor(mineral_mice$Race)
mineral_mice$ALQ = factor(mineral_mice$ALQ)
mineral_mice$SMQ = factor(mineral_mice$SMQ)
mineral_mice$HBP = factor(mineral_mice$HBP)
mineral_mice$DM = factor(mineral_mice$DM)
mineral_mice$CVD = factor(mineral_mice$CVD)

Y = mineral_mice$CKD 
expos = as.matrix(mineral_mice[, c("ln_P","ln_Mg","ln_K","ln_Ca","ln_Fe","ln_Zn",
                                   "ln_Cu","ln_Se","ln_Na")])#是化学物质浓度变量
Group <- c(1, 1, 1, 2, 2, 2, 2, 2, 3) # 化学物质分组
covar = as.matrix(mineral_mice[, c("Age","Gender","Race","BMI","ALQ","SMQ","PAALL",
                                   "HBP","DM","CVD","Engergy")]) # 协变量
covar_matrix <- model.matrix(~ Age + Gender + Race + BMI + ALQ + SMQ + PAALL + HBP + DM + CVD + Engergy, data = mineral_mice)
scale_expos = scale(expos) 

set.seed(274)
#运行 2 个并行马尔可夫链（通常更好）
future::plan(strategy = future::multisession, workers=15)
fit_1w <- kmbayes_parallel(nchains=40, Y, Z = scale_expos, 
 X = covar_matrix, group= Group, iter = 250, family = 'binomial', 
 varsel=TRUE, verbose=TRUE, est.h = TRUE)
# 查看结果
print(fit_1w)
#储存
save(fit_1w, file = "fit_1w.RData")  
load("fit_1w.RData") 
#合并
bigkm = kmbayes_combine(fit_1w, excludeburnin=FALSE)

# 从合并的马尔可夫链结果中提取参数估计
ests = ExtractEsts(bigkm)  

# 从合并的马尔可夫链结果中提取变量选择的后验概率
ExtractPIPs(bigkm)

#绘图，分别绘制参数β，误差方差以及变量之间相关系数的跟踪图。
TracePlot(fit = bigkm, par = "beta")
TracePlot(fit = bigkm, par = "sigsq.eps")
TracePlot(fit = bigkm, par = "r", comp = 1)

#计算单变量预测结果。
pred.resp.univar <- PredictorResponseUnivar(fit = bigkm,q.fixed=0.5)

#绘制混合暴露一阶交互的剂量-反应关系曲线
#计算单变量预测结果。
pred.resp.univar <- PredictorResponseUnivar(fit = bigkm,q.fixed=0.5)
ggplot(pred.resp.univar, aes(z, est, ymin = est - 1.96*se,
                             ymax = est + 1.96*se)) +
  geom_hline(yintercept = 0, lty = 2, col = "brown")+
  geom_smooth(stat = "identity") +
  facet_wrap(~variable, ncol = 4) +
  xlab("Mineral intake (ln)") +
  ylab("Estimated risk in CKD")+
  theme(plot.title = element_text(hjust = 0.5,size = 12, family="serif"),
        axis.text=element_text(size=12,family="serif"),
        axis.title.x=element_text(size=12,family="serif"),
        axis.title.y=element_text(size=12,family="serif"),
        strip.text=element_text(size=12,color="black", family="serif"))+
  theme(legend.title=element_text(size=12,family="serif"))+
  scale_y_continuous(limits = c(-0.3, 0.3)) #设置y值的范围

#计算整体风险的综合评估并显示结果。
risks.overall <- OverallRiskSummaries(fit = bigkm, qs = seq(0.1, 0.9, by = 0.05), q.fixed = 0.5)
ggplot(risks.overall, aes(quantile, est, ymin = est - 1.96*sd,
                          ymax = est + 1.96*sd)) +
  coord_cartesian(ylim = c(-0.3,0.3),xlim = c(0.25,0.75))+
  geom_hline(yintercept = 0, lty = 2, col = "brown") +
  geom_pointrange()+
  xlab("Metals intake (ln)") +
  ylab("Estimated CKD risk")+
  theme(plot.title = element_text(hjust = 0.5,size = 12, family="serif"),axis.text=element_text(size=12,family="serif"),axis.title.x=element_text(size=12,family="serif"),axis.title.y=element_text(size=12,family="serif"),strip.text=element_text(size=12,color="black", family="serif"))+
  theme(legend.title=element_text(size=12,family="serif"))


#估计每个暴露的效应值
risks.singvar <- SingVarRiskSummaries(fit =bigkm, 
                                      qs.diff = c(0.25, 0.75),
                                      q.fixed = c(0.25, 0.50, 0.75))

ggplot(risks.singvar, aes(variable, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd, col = q.fixed)) +
  geom_pointrange(position = position_dodge(width = 0.75)) +
  coord_flip()

#绘制混合暴露一阶交互的剂量-反应关系曲线
pred.resp.bivar <- PredictorResponseBivar(fit = bigkm, min.plot.dist = 1)
pred.resp.bivar.levels <- PredictorResponseBivarLevels(
  pred.resp.df = pred.resp.bivar,
  Z = scale_expos, qs = c(0.25, 0.5, 0.75))
ggplot(pred.resp.bivar.levels, aes(z1, est)) +
  geom_smooth(aes(col = quantile), stat = "identity") +
  facet_grid(variable2 ~ variable1) +
  ggtitle("h(expos1 | quantiles of expos2)") +
  xlab("expos1")

