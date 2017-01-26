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


#-----------variables sorted by party number----------------------------
attit = c("PV_170a","PV_170d","PV_170b","PV_170g","PV_170j","PV_170i","PV_170h","PV_170c")	#removed "PV_170e" - strana zelenych
# PV.170a Volil by stranu – ČSSD
# PV.170b Volil by stranu – ODS
# PV.170c Volil by stranu – KSČM
# PV.170d Volil by stranu – TOP 09
# PV.170e Volil by stranu – SZ
# PV.170g Volil by stranu – KDU-ČSL
# PV.170h Volil by stranu – ANO 2011
# PV.170i Volil by stranu – Úsvit
# PV.170j Volil by stranu – SPOZ

#----------remove dont know (99)----------------------------
for(i in attit){
  cvvm[cvvm[,i] %in% "99",i] = NA
}

#----------Mean of attitudes to toher parties-------------------------
attitTab = aggregate(cvvm[,attit], by=list(cvvm$voting), FUN = mean, na.rm = TRUE)
attitTab = merge(labels, attitTab, by.x = "voting", by.y = "Group.1")
attitTab = attitTab[order(as.numeric(as.vector(attitTab$voting))),]

#---------proximity to distance, diag to zero, symetrize-----------------------------
s = attitTab[,c(3:10)]
s = 0.5 * (s + t(s)) 
s = 1/s *10
diag(s) = 0


#--------MDS-------------------------------------------------------------------
fit = cmdscale(s, eig = TRUE, k = 2)

#-------extract data for plot--------------------------------------------
MDS = data.frame(fit$points[, 1])
MDS = cbind (MDS, fit$points[, 2])



votingBig = votingBig[order(as.numeric(as.vector(votingBig$voting))),]
tmp = votingBig[votingBig$voting != 99, c(2,3)]

MDS = cbind ( attitTab[,2], tmp, MDS)

names(MDS) = c("party", "abs", "rel", "x", "y")

#-------Plot in plotly----------------------------------------------------

library(plotly)
p <- plot_ly(MDS, x = ~x, y = ~y, type = 'scatter', mode = 'markers',
             marker = list(size = ~rel*2.5, opacity = 1, color = 'blue'),
             hoverinfo = 'text',
             text = ~paste('<b>',party, '</b><br>')) %>%
  layout(title = 'Rozložení stran - říjen 2013 (MDS)',
         xaxis = list(showgrid = FALSE, title = "x"),
         yaxis = list(showgrid = FALSE, title = "y"))
p






