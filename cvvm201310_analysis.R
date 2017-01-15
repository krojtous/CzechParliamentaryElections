#Date: 15.1.2017
#Author: Matouš Pilnáček - Public Opinion Research Centre, Czech Academy of Science
#E-mail: matous.pilnacek@soc.cas.cz
#Description: Basic analysis of survey conducted before czech parliamentary elections 2013(lower house)
#Data source: http://nesstar.soc.cas.cz/webview/velocity?mode=download&analysismode=table&v=2&study=http%3A%2F%2F147.231.52.118%3A80%2Fobj%2FfStudy%2FV1310



library(foreign)
cvvm = read.spss(file = "./data/V1310/V1310_F1.sav",
                 to.data.frame = TRUE,
                 use.value.labels = TRUE,
                 use.missings = TRUE)
#RESULTS - http://www.volby.cz/pls/ps2013/ps2?xjazyk=CZ
results = read.csv(file = "./results.csv",
                   sep = ";",
                   dec = ",")
turnout = 59.48
#RECODE

#PV4 ANALYSIS
PV4 = as.data.frame(table(cvvm$PV_4, exclude = c("ASI K VOLBÁM NEPŮJDE", "NEVÍ, KTEROU STRANU BUDE VOLIT")))
PV4 = cbind(PV4, round( PV4$Freq/sum(PV4$Freq)*100,2 ))
names(PV4) = c("party","abs","rel")
limit = 1 #limit for party in percents
PV4 = PV4[PV4$rel >= limit, ]
PV4 = PV4[order(PV4$rel, decreasing = TRUE),]
