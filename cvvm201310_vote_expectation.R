#Date of creation: 23.1.2017
#Author: Matouš Pilnáček - Public Opinion Research Centre, Czech Academy of Science
#E-mail: matous.pilnacek@soc.cas.cz
#License:
#Description: Analysis of vote excpectation in  survey before Czech parliamentary elections in OCtober 2013
#Literature: Graefe, A. “Accuracy of Vote Expectation Surveys in Forecasting Elections.” Public Opinion Quarterly 78, no. S1 (January 1, 2014): 204–32. doi:10.1093/poq/nfu008.


#Load data
library(foreign)
cvvm = read.spss(file = "./data/V1310/V1310_F1.sav",
                 to.data.frame = TRUE,
                 use.value.labels = FALSE,
                 use.missings = FALSE)


#Party which will get to the parliament
SVar = c("PV_169Sa","PV_169Sb","PV_169Sc","PV_169Sd","PV_169Se","PV_169Sf","PV_169Sg","PV_169Sh","PV_169Si","PV_169Sj","PV_169Sk")
#Gain of the party in percent
PVar = c("PV_169Pa","PV_169Pb","PV_169Pc","PV_169Pd","PV_169Pe","PV_169Pf","PV_169Pg","PV_169Ph","PV_169Pi","PV_169Pj","PV_169Pk")

#Extract labels
labels = as.data.frame(cbind(attr(cvvm$PV_169Sa, "value.labels"), names(attr(cvvm$PV_169Sa, "value.labels"))))
names(labels) = c("party", "label")

#Filter cases
# cvvm = cvvm[cvvm$PV_1 %in% c(1),] #only voters
# cvvm = cvvm[cvvm$PO_45a %in% c(1,2),] #only interested in politics in general
cvvm = cvvm[cvvm$PO_45b %in% c(1),] #only interested in politics in CZ


#make a mean of all expectations
expect = data.frame()
for(party in c(1:24)){
  data = numeric()
  for(var in c(1:length(SVar))){
     data = c(
        data,
       cvvm[ cvvm[,SVar[var]] == party, PVar[var] ] #chose in variable var only respondents who expect given party and get percent guess
       )
  }
  data = data[!is.na(data)] #remove NA

  expect[party,"party"] = party
  # expect[party,"expect_avg"] = round(mean(data,na.rm = TRUE))
  expect[party,"expect_med"] = round(median(data,na.rm = TRUE)) #we use median because of outliers
  expect[party,"N"] = length(data)
}

#Add labels
expect = merge(labels, expect)

#cut parties chosen with only few respondetns (relatively 10%)
threshold = nrow(cvvm)/100*10
expect = expect[expect$N > threshold,]


#recompute to 100%
# expect$expect = expect$expect_avg/(sum(expect$expect_avg)/100)


#REAL ELECTION RESULTS
#RESULTS - http://www.volby.cz/pls/ps2013/ps2?xjazyk=CZ
results = read.csv(file = "./results.csv",
                   sep = ";",
                   dec = ",")
results = results[,c("cislo", "procenta")]
expect = merge(expect,results, by.x = "party", by.y = "cislo")

names(expect)[5] = "real"

expect$err = expect$expect_med - expect$real
expect = expect[order(expect$err,decreasing = TRUE),]
