#Date: 15.1.2017
#Author: Matouš Pilnáček - Public Opinion Research Centre, Czech Academy of Science
#E-mail: matous.pilnacek@soc.cas.cz
#Description: Basic analysis of survey conducted before czech parliamentary elections 2013(lower house)
#Data source: http://nesstar.soc.cas.cz/webview/velocity?mode=download&analysismode=table&v=2&study=http%3A%2F%2F147.231.52.118%3A80%2Fobj%2FfStudy%2FV1310



library(foreign)
cvvm = read.spss(file = "./data/V1310/V1310_F1.sav",
                 to.data.frame = TRUE,
                 use.value.labels = FALSE,
                 use.missings = FALSE)

#
#RESULTS - http://www.volby.cz/pls/ps2013/ps2?xjazyk=CZ
results = read.csv(file = "./results.csv",
                   sep = ";",
                   dec = ",")
turnout = 59.48
#RECODE PV_4 (dont know - 0,98,99 to missing)
cvvm$PV_4R = cvvm$PV_4
cvvm[cvvm$PV_4 %in% c(0,98,99), "PV_4R"] = NA

#PV4 ANALYSIS
tableRel = function(data, variable){
  table = as.data.frame(table(data[,variable]))
  table = cbind(table, round( table$Freq/sum(table$Freq)*100,2 ))
  names(table) = c(variable,"abs","rel")
  return(table)
}

PV4 = tableRel(cvvm, "PV_4R")

#limit for party in percents
limit = 3

#RECODE PV_4R (small parties to missing/one category)
others = as.vector(PV4[PV4$rel < limit, "PV_4R"])
cvvm[cvvm$PV_4 %in% others, "PV_4R"] = NA
#cvvm[cvvm$PV_4 %in% others, "PV_4R"] = "97"

PV4R = tableRel(cvvm, "PV_4R")

#extract labels of parties
labels = as.data.frame(cbind(attr(cvvm$PV_4, "value.labels"), names(attr(cvvm$PV_4, "value.labels"))))
names(labels) = c("PV_4R", "label")

PV4R = merge(labels, PV4R)

#order by party number - important for further merging by cbind
PV4R = PV4R[order(as.numeric(as.vector(PV4R$PV_4R))),]


#-------------position of voters in 2D SPACE-----------------

#RECODE FIRST VARIABLE
table(cvvm$OV_1)
cvvm$OV_1R = cvvm$OV_1
cvvm[cvvm$OV_1R %in% c(0,9) , "OV_1R"] = NA

#COMPUTE FIRST VARIABLE
agreg = aggregate(cvvm$OV_1R, by = list(as.vector(cvvm$PV_4R)), mean, na.rm = TRUE)
PV4R = cbind(PV4R, agreg[-1])          


#RECODE SECOND VARIABLE
table(cvvm$PO_2)
cvvm$PO_2R = cvvm$PO_2
cvvm[cvvm$PO_2R %in% c(0,99) , "PO_2R"] = NA

#COMPUTE SECOND VARIABLE
agreg2 = aggregate(cvvm$PO_2R, by = list(cvvm$PV_4R), mean, na.rm = TRUE)
PV4R = cbind(PV4R, agreg2[-1])

