#############################          INFLATABLES          #############################


install.packages('googlesheets4')
library('googlesheets4')

##Resultaten inlezen##
##T1 = HDPE
T1 <- read_sheet('https://docs.google.com/spreadsheets/d/15faUz2ZYdj8hQc3b9llxWrPSxGyioFlo4C87pzw872s/edit#gid=0')
##T2 = PAP
T2 <- read_sheet('https://docs.google.com/spreadsheets/d/18KvPKyhgynePwedNfQOHeszdRi6sBr7PU4HXMSS_YcE/edit#gid=0')
##T3 = PET
T3 <- read_sheet('https://docs.google.com/spreadsheets/d/19oa7gPIFAmYo-hUL0WH-TajQ1As_-XneFdQ14lAOavQ/edit#gid=0')
##T4 = alles te samen
T4 <- read_sheet('https://docs.google.com/spreadsheets/d/1NmxbzwT-uYsEKerhbsbV1ItFqRuCsYsL8X9oFYGIpnA/edit#gid=0')


#P-WAARDE CONTROLEREN - NORMAAL
install.packages('dgof')
library(dgof)

shapiro.test(as.numeric(T4$`Decibel (dB)`))


##shapiro.test(as.numeric(T4$`Druk (Pa)`))

#HOMEGENIOUS VARIANCES
install.packages('car')
library('car')

#@Niet meer nodig aangezien p-value hoger is dan 0.05
#leveneTest(T4$`Decibel (dB)` ~ T4$`Druk (Pa)`)



### p-value hoger dan 0.05 -> non-parametic


##VORMEN per materiaal
KV_HDPE <- subset(T1, Vorm == "Klein vierkant", `Decibel (dB)`, drop = TRUE)
GV_HDPE <- subset(T1, Vorm == "Groot vierkant", `Decibel (dB)`, drop = TRUE)
H_HDPE <- subset(T1, Vorm == "Hexagon", `Decibel (dB)`, drop = TRUE)

KV_PAP <- subset(T2, Vorm == "Klein vierkant", `Decibel (dB)`, drop = TRUE)
GV_PAP <- subset(T2, Vorm == "Groot vierkant", `Decibel (dB)`, drop = TRUE)
H_PAP <- subset(T2, Vorm == "Hexagon", `Decibel (dB)`, drop = TRUE)

KV_PET <- subset(T3, Vorm == "Klein vierkant", `Decibel (dB)`, drop = TRUE)
GV_PET <- subset(T3, Vorm == "Groot vierkant", `Decibel (dB)`, drop = TRUE)
H_PET <- subset(T3, Vorm == "Hexagon", `Decibel (dB)`, drop = TRUE)


##Alle materialen 
PAP <- subset(T4, Materiaal == "PAP", `Decibel (dB)`, drop = TRUE)
HDPE <- subset(T4, Materiaal == "HDPE", `Decibel (dB)`, drop = TRUE)
PET <- subset(T4, Materiaal == "PET", `Decibel (dB)`, drop = TRUE)




install.packages('ggpubr')
library('ggplot2')
library('ggpubr')
#Grafiek VORMEN
###HDPE
ggboxplot(T1, x = "Vorm", y = "Decibel (dB)", 
          color = "Vorm", palette = c("#00AFBB", "#E7B800","#008000"),
          order = c("Klein vierkant", "Groot vierkant","Hexagon"),
          ylab = "Decibel (dB)", xlab = "Vorm")

###PAP
ggboxplot(T2, x = "Vorm", y = "Decibel (dB)", 
          color = "Vorm", palette = c("#00AFBB", "#E7B800","#008000"),
          order = c("Klein vierkant", "Groot vierkant","Hexagon"),
          ylab = "Decibel (dB)", xlab = "Vorm")

###PET
ggboxplot(T3, x = "Vorm", y = "Decibel (dB)", 
          color = "Vorm", palette = c("#00AFBB", "#E7B800","#008000"),
          order = c("Klein vierkant", "Groot vierkant","Hexagon"),
          ylab = "Decibel (dB)", xlab = "Vorm")

#Grafiek MATERIAAL
ggboxplot(T4, x = "Materiaal", y = "Decibel (dB)", 
          color = "Materiaal", palette = c("#00AFBB", "#E7B800","#008000"),
          order = c("HDPE", "PAP","PET"),
          ylab = "Decibel (dB)", xlab = "Materiaal")




##WILCOXON SIGNED RANK TEST

###HDPE
wilcox.test(KV_HDPE,GV_HDPE, paired=TRUE)
wilcox.test(KV_HDPE,H_HDPE, paired=TRUE)
wilcox.test(H_HDPE,GV_HDPE, paired=TRUE)

###PAP
wilcox.test(KV_PAP,GV_PAP, paired=TRUE)
wilcox.test(KV_PAP,H_PAP, paired=TRUE)
wilcox.test(H_PAP,GV_PAP, paired=TRUE)

###PET
wilcox.test(KV_PET,GV_PET, paired=TRUE)
wilcox.test(KV_PET,H_PET, paired=TRUE)
wilcox.test(H_PET,GV_PET, paired=TRUE)



##KRUSKAL-WALLIS TEST

install.packages('coin')
install.packages('survival')
library('survival')
library('coin')
kruskal.test(T4$`Decibel (dB)` ~ T4$Materiaal, data = T4)


  

#############################          DEFLATABLES          #############################

##INLEZEN DEFLATABLES
T11 <- read_sheet('https://docs.google.com/spreadsheets/d/1-Fo-h8K7oHvF7b_UqGjJ5nXbxDYZRXc2HAtAANc1SPc/edit#gid=0')
T12 <- read_sheet('https://docs.google.com/spreadsheets/d/172bzASoKjM05x10-OTElc5g4o2CYWBj9FmaT9gIssZI/edit#gid=0')
T13 <- read_sheet('https://docs.google.com/spreadsheets/d/1sd9bJVrRRNrJILnACwaeI69htq7WM4ocdlQY6pT9zWk/edit#gid=0')
T14 <- read_sheet('https://docs.google.com/spreadsheets/d/1kZsSgCiPfZNs-BkBLvaMqTp2zjszw2DYrEEmPfv2E7k/edit#gid=0')



##VORMEN per materiaal
D_KV_HDPE <- subset(T11, Vorm == "Klein vierkant", `Decibel (dB)`, drop = TRUE)
D_GV_HDPE <- subset(T11, Vorm == "Groot vierkant", `Decibel (dB)`, drop = TRUE)
D_H_HDPE <- subset(T11, Vorm == "Hexagon", `Decibel (dB)`, drop = TRUE)

D_KV_PAP <- subset(T12, Vorm == "Klein vierkant", `Decibel (dB)`, drop = TRUE)
D_GV_PAP <- subset(T12, Vorm == "Groot vierkant", `Decibel (dB)`, drop = TRUE)
D_H_PAP <- subset(T12, Vorm == "Hexagon", `Decibel (dB)`, drop = TRUE)

D_KV_PET <- subset(T13, Vorm == "Klein vierkant", `Decibel (dB)`, drop = TRUE)
D_GV_PET <- subset(T13, Vorm == "Groot vierkant", `Decibel (dB)`, drop = TRUE)
D_H_PET <- subset(T13, Vorm == "Hexagon", `Decibel (dB)`, drop = TRUE)


##Alle materialen 
D_PAP <- subset(T14, Materiaal == "PAP", `Decibel (dB)`, drop = TRUE)
D_HDPE <- subset(T14, Materiaal == "HDPE", `Decibel (dB)`, drop = TRUE)
D_PET <- subset(T14, Materiaal == "PET", `Decibel (dB)`, drop = TRUE)



#Grafiek VORMEN
###HDPE
ggboxplot(T11, x = "Vorm", y = "Decibel (dB)", 
          color = "Vorm", palette = c("#00AFBB", "#E7B800","#008000"),
          order = c("Klein vierkant", "Groot vierkant","Hexagon"),
          ylab = "Decibel (dB)", xlab = "Vorm")

###PAP
ggboxplot(T12, x = "Vorm", y = "Decibel (dB)", 
          color = "Vorm", palette = c("#00AFBB", "#E7B800","#008000"),
          order = c("Klein vierkant", "Groot vierkant","Hexagon"),
          ylab = "Decibel (dB)", xlab = "Vorm")

###PET
ggboxplot(T13, x = "Vorm", y = "Decibel (dB)", 
          color = "Vorm", palette = c("#00AFBB", "#E7B800","#008000"),
          order = c("Klein vierkant", "Groot vierkant","Hexagon"),
          ylab = "Decibel (dB)", xlab = "Vorm")

#Grafiek MATERIAAL
ggboxplot(T14, x = "Materiaal", y = "Decibel (dB)", 
          color = "Materiaal", palette = c("#00AFBB", "#E7B800","#008000"),
          order = c("HDPE", "PAP","PET"),
          ylab = "Decibel (dB)", xlab = "Materiaal")



# Code suggestions Bas - normality testing & variance testing (ignoring pressure) #
###################################################################################

# read data
library(googlesheets4)
T1 <- read_sheet('https://docs.google.com/spreadsheets/d/15faUz2ZYdj8hQc3b9llxWrPSxGyioFlo4C87pzw872s/edit#gid=0')
T2 <- read_sheet('https://docs.google.com/spreadsheets/d/18KvPKyhgynePwedNfQOHeszdRi6sBr7PU4HXMSS_YcE/edit#gid=0')
T3 <- read_sheet('https://docs.google.com/spreadsheets/d/19oa7gPIFAmYo-hUL0WH-TajQ1As_-XneFdQ14lAOavQ/edit#gid=0')
T4 <- read_sheet('https://docs.google.com/spreadsheets/d/1NmxbzwT-uYsEKerhbsbV1ItFqRuCsYsL8X9oFYGIpnA/edit#gid=0')

# combine data & rename variable
df <- rbind(T1,T2,T3,T4)
names(df) <- c("db","druk", "vorm", "materiaal")

# test normality for independent 1: materiaal
library(nortest)
library(coin)
tapply(df$db,df$materiaal, lillie.test)
tapply(df$db,df$materiaal, shapiro.test)
# All data are suffiently normal (only one group and 1 test barely below 0.05) > parametric test

# test normality for independent 2: vorm
tapply(df$db,df$vorm, lillie.test)
tapply(df$db,df$vorm, shapiro.test)
# All data are suffiently normal (only one group and 1 test barely below 0.05) > parametric test

# testing H1: the dB is different for different materials (ANOVA)
anova <- aov(db ~ materiaal, data = df)
tapply(df$db,df$materiaal,mean)
summary(anova)
TukeyHSD(anova)
# conclusion: H1 supported - the dB is different for all materials, with PAP as the best reducer and HPDE as the worst

# testing H2: the dB is different for different shapes (ANOVA)
anova <- aov(db ~ vorm, data = df)
tapply(df$db,df$vorm,mean)
summary(anova)
# conclusion: H2 not supported there is no dB difference between shapes


