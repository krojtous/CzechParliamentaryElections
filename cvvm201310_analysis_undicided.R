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

#REAL ELECTION RESULTS ()
#RESULTS - http://www.volby.cz/pls/ps2013/ps2?xjazyk=CZ
results = read.csv(file = "./results.csv",
                   sep = ";",
                   dec = ",")
turnout = 59.48


#------------------BASIC SETTINGS---------------------------
variables = c("PI_1a", "PI_1b", "OV_1", "EV_10", "PS_1", "PO_45b", "OV_132")

variable = variables[2]
recodedVar = paste0(variable,"R")
cluster_count = 2
#limit for party in percents
limit = 3
#------------------------------------------------------------



#RECODE PV_4 (dont know - 0,98,99 to missing)
cvvm$PV_4R = cvvm$PV_4
cvvm[cvvm$PV_4 %in% c(0,98), "PV_4R"] = NA #dont know and probably wont go to elections
cvvm[cvvm$PV_1 %in% c(0,9,3,4,8), "PV_4R"] = NA #people who probably wont go to election are excluded from analysis

#PV4 ANALYSIS
tableRel = function(data, variable){
  table = as.data.frame(table(data[,variable]))
  table = cbind(table, round( table$Freq/sum(table$Freq)*100,2 ))
  names(table) = c(variable,"abs","rel")
  return(table)
}

PV4 = tableRel(cvvm, "PV_4R")

#RECODE PV_4R (small parties to missing/one category)
others = as.vector(PV4[PV4$rel < limit, "PV_4R"])
cvvm[cvvm$PV_4 %in% others, "PV_4R"] = "97"

PV4R = tableRel(cvvm, "PV_4R")

#removing rows with others(97) and dont know (99) but percents are computed with them
PV4_Other = PV4R
PV4R = PV4R[!PV4R$PV_4R %in% c(97,99),]
cvvm[cvvm$PV_4R %in% c(97,99), "PV_4R"] = NA #removing others and dontknow from recoded category


#extract labels of parties
labels = as.data.frame(cbind(attr(cvvm$PV_4, "value.labels"), names(attr(cvvm$PV_4, "value.labels"))))
names(labels) = c("PV_4R", "label")

PV4R = merge(labels, PV4R)

#-------------compute clusters of undicided voters-----------------
#subset of undicided voters and clustering variables
undicided = cvvm[cvvm$PV_1 %in% c(1,2) & cvvm$PV_4 %in% c(99),c("PI_1a", "OV_1", "IDE_1", "EV_10", "PS_1", "OV_132", "PO_2", "IDE_6") ]

#for
# MISSING VALUES PI_1a PI_1b OV_1 EV_10 PS_1 PO_45b OV_132 (0,9).
# MISSING VALUES PO_2 (0,99).
#NORMALIZACE KVLI KLASTROVANI KVULI ROZDILNYM SKALAM?

colnames = c("PI_1a", "OV_1", "IDE_1", "EV_10", "PS_1", "OV_132", "IDE_6" )
for(i in colnames){
  undicided[undicided[,i] %in% c(0,9), i] = NA
  undicided[is.na(undicided[,i]), i] = mean(undicided[,i],na.rm = TRUE)
}
undicided[undicided[,"PO_2"] %in% c(0,99), "PO_2"] = NA
undicided[is.na(undicided[,"PO_2"]), "PO_2"] = mean(undicided[,"PO_2"],na.rm = TRUE)

#cluster function

cluster_fit <- kmeans(undicided, cluster_count)



#-------------position of voters in 2D SPACE-----------------


#RECODE FIRST VARIABLE
cvvm[recodedVar] = cvvm[,variable ]
cvvm[cvvm[,variable ] %in% c(0,9) , recodedVar] = NA

#COMPUTE FIRST VARIABLE
agreg = aggregate(cvvm[,recodedVar], by = list(as.vector(cvvm$PV_4R)), mean, na.rm = TRUE)
PV4R = cbind(PV4R, agreg[-1])

#RECODE SECOND VARIABLE
table(cvvm$PO_2)
cvvm$PO_2R = cvvm$PO_2
cvvm[cvvm$PO_2R %in% c(0,99) , "PO_2R"] = NA

#COMPUTE SECOND VARIABLE
agreg2 = aggregate(cvvm$PO_2R, by = list(cvvm$PV_4R), mean, na.rm = TRUE)
PV4R = cbind(PV4R, agreg2[-1])

#naming and typing of resluts table
names(PV4R) = c("number","party","abs","rel","x","y")
PV4R$number = as.vector(PV4R$number)
PV4R$party = as.vector(PV4R$party)

#-------------------------add undicided clusters and their char-------------------------

#-----------------characteristcs of undicided voters-----------
PO_2O = aggregate(undicided,by=list(cluster_fit$cluster),FUN=mean)$PO_2
var_other = aggregate(undicided,by=list(cluster_fit$cluster),FUN=mean)[,variable]

for(i in c(1:cluster_count)){
  PV4R[nrow(PV4R)+1,"number"] = 80 + i
  PV4R[nrow(PV4R),"party"] = paste0("Nerozhodnutí ", i)
  PV4R[nrow(PV4R),"abs"] =  cluster_fit$size[i]
  PV4R[nrow(PV4R),"rel"] = round(cluster_fit$size[i]/sum(PV4_Other$abs)*100,2)
  PV4R[nrow(PV4R),"x"] = var_other[i]
  PV4R[nrow(PV4R),"y"] = PO_2O[i]
}

#----------------------------add aditional information-------------
#Real result
PV4R = merge(PV4R, results, by.x = "number", by.y = "cislo", all.x = TRUE)

#----------------------------DRAW GRAPH----------------------------
library(plotly)
# 
# ggplot(PV4R, aes(y, x, label = party) ) +
#   geom_point(aes(size = rel), colour = "blue") +
#   geom_text(aes(lineheight = 0.8), size=3, vjust = -1)

#order by party number - important for merging with colors
PV4R = PV4R[order(as.numeric(as.vector(PV4R$number))),]


colors <- c('rgba(255,128,0,1)', 'rgba(127,0,255,1)', 'rgba(0,0,204,1)', 'rgba(255,255,0,1)',
            'rgba(204,204,204,1)', 'rgba(204,204,204,1)', 'rgba(153,153,255,1)', 'rgba(255,0,0,1)')

p <- plot_ly(PV4R, x = ~y, y = ~x, type = 'scatter', mode = 'markers',
             marker = list(size = ~rel*2.5, opacity = 1, color = colors),
             hoverinfo = 'text',
             text = ~paste('<b>',party, '</b><br>Model:', rel, '%, N =', abs,'<br>Real:   ',procenta,' %, N =', celkem)) %>%
  layout(title = 'Rozložení stran - říjen 2013',
         xaxis = list(showgrid = FALSE, title = "Levo-pravé sebezaření (PO.2)"),
         yaxis = list(showgrid = FALSE, title = variable))
p
