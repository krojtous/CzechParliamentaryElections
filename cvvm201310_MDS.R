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

#------------Remove small parties from variable voting----------------------







