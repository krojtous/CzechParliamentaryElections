#Date: 18.1.2017
#Author: Matouš Pilnáček - Public Opinion Research Centre, Czech Academy of Science
#E-mail: matous.pilnacek@soc.cas.cz
#Description: Multidimensinal scaling of voters by party and clusters of undicided voters


#--------------Load data----------------
library(foreign)
cvvm = read.spss(file = "./data/V1310/V1310_F1.sav",
                 to.data.frame = TRUE,
                 use.value.labels = FALSE,
                 use.missings = FALSE)

#---------------Extract party labels-------------------------------------------
labels = as.data.frame(cbind(attr(cvvm$PV_4, "value.labels"), names(attr(cvvm$PV_4, "value.labels"))))
names(labels) = c("voting", "label")


#-------------Compute voting variable and turnout---------------------------
#No and rather no are voters excluded
cvvm$voting = cvvm$PV_4
cvvm[cvvm$PV_4 %in% c(0,98), "voting"] = NA #dont know and probably wont go to elections
cvvm[cvvm$PV_1 %in% c(0,9,3,4,8), "voting"] = NA

#-------------Compute preferences with undicided voters---------------------
tableRel = function(data, variable){
  table = as.data.frame(table(data[,variable]))
  table = cbind(table, round( table$Freq/sum(table$Freq)*100,2 ))
  names(table) = c(variable,"abs","rel")
  return(table)
}

votingAll = tableRel(cvvm, "voting")

#------------Recode small parties to others---------------------------------
limit = 3
others = as.vector(votingAll[votingAll$rel < limit, "voting"])
cvvm[cvvm$PV_4 %in% others, "voting"] = "97"

votingBig = tableRel(cvvm, "voting")

#------------Remove category others from Big table (but in 100% is included category others)----------------------
votingBig = votingBig[votingBig$voting != "97",]

#-----------Remove category others from var voting---------------------
cvvm = cvvm[cvvm$voting != "97" & cvvm$voting != "99" & !is.na(cvvm$voting),]
table(cvvm$voting) 
#-----------Add labels-------------------------------------------------
merge(labels, votingBig)

attit = c("PV_170a","PV_170b","PV_170c","PV_170d","PV_170g","PV_170h","PV_170i","PV_170j")	#removed "PV_170e" - strana zelenych

#mean of attitudes to toher parties
attitTab = aggregate(cvvm[,attit], by=list(cvvm$voting), FUN = mean)
attitTab = merge(labels, attitTab, by.x = "voting", by.y = "Group.1")


cvvm$
table(cvvm$voting)






