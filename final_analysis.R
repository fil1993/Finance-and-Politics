###THE POLITICAL ECONOMY OF FINANCE EVIDENCE FROM THE U.S. CONGRESS
setwd("C:/Users/Filippo/Desktop/Thesis/Model")
options(max.print=1000000) #dont cut off data view
#options("scipen"=100, "digits"=4) #no exponential notation
##install packages
#install.packages('data.table') formatting tables
#install.packages('pscl') political science package
#install.packages('lattice') data representation
#install.packages('VGAM') for implementing advanced analysis functions
#install.packages('PerformanceAnalytics') for correlation matrices
#install.packages('aod') required for computing statistical tests
#install.packages('xtable') convert R output into LaTeX
#install.packages('ggcorrplot') correlograms
#install.packages('stargazer') regression results into LaTeX
#install.packages('mctest') for advanced multicollinearity test
#install.packages('tidyverse') for easy data manipulaion and visualization
#install.packages('caret') for easy machine learning workflow
#install.packages('car') for multicollinearity test
#install.packages('lmtest') for heteroscedasticity test
#install.packages('ResourceSelection')
#install.packages('ROCR')
#install.packages('texreg')
#install.packages('popbio')
#install.packages('tikzDevice')
#install.packages('sandwich')
#install.packages('broom')finding influential values
library(broom)
library(data.table)
library(ResourceSelection)
library(tikzDevice)
library(sandwich)
library(popbio)
library(ROCR)
library(texreg)
library(pscl)
library(lmtest)
library(lattice)
library(VGAM)
library(PerformanceAnalytics)
library(aod)
library(xtable)
library(stargazer)
library(tidyverse)
library(caret)
library(car)
library(mctest)
library(ggplot2)
library(ggcorrplot)
##RETRIEVE ROLL CALLS OF INTEREST
##create datasets comprehensive of all the roll call votes of interest
res <- voteview_search("financial deregulation", chamber="House", congress = 106:109, startdate = "1999-01-01")
##Download votes into roll call object 
rc <- voteview_download(res2$id)
options(max.print = 1000)
summary(rc, verbose = TRUE, options(max.print = 10000))
computeMargins(rc)
##Constructing the dataset
rc_long <- melt_rollcall(rc)
##Quering for the bills of interest
rc_long_106 <- rc_long[, rollnumber:= ifelse(rollnumber=="202", 3000,
                    ifelse(rollnumber=="262", 3001, ifelse(rollnumber=="266",3002,
                    ifelse(rollnumber=="269", 3003, ifelse(rollnumber=="271", 3004,
                    ifelse(rollnumber=="272", 3005, ifelse(rollnumber=="274", 3006,
                    ifelse(rollnumber=="353", 3007, ifelse(rollnumber=="567", 3008,
                    ifelse(rollnumber=="568", 3009, ifelse(rollnumber=="704", 3010,
                    ifelse(rollnumber=="1146", 3011, ifelse(rollnumber=="368", 3012,
                    ifelse(rollnumber=="381", 3013, ifelse(rollnumber=="382", 3014,
                    ifelse(rollnumber=="385", 3015, NA))))))))))))))))]
                                                                                                                                                                                                  
rc_long_106_fltr<-rc_long_106[rollnumber>2999 & rollnumber<3016]
save(rc_long_106_fltr, file = "rc_long_106_fltr.RData")
###107th Congress###
##construct roll call object
rc_long_107[, rollnumber:= ifelse(rollnumber=="161", 3000,
            ifelse(rollnumber=="162", 3001, ifelse(rollnumber=="164", 3002,
            ifelse(rollnumber=="616", 3003, ifelse(rollnumber=="696", 3004,
            ifelse(rollnumber=="771", 3005, ifelse(rollnumber=="854", 3006,
            ifelse(rollnumber=="24", 3007, NA))))))))]
rc_long_107_fltr <- rc_long_107[rollnumber>2999 & rollnumber<3008]
save(rc_long_107_fltr, file = "rc_long_107_fltr.RData")
###108th Congress###
##retrieve roll calls of interest
rc_long_108_CEOs[, rollnumber:= ifelse(rollnumber=="97", 3000,
           ifelse(rollnumber=="245", 3001,
           ifelse(rollnumber=="280", 3002,
           ifelse(rollnumber=="302", 3003,
           ifelse(rollnumber=="497", 3004,
           ifelse(rollnumber=="636", 3005, 
           ifelse(rollnumber=="665", 3006,
           ifelse(rollnumber=="742", 3007, 
           ifelse(rollnumber=="743", 3008,
           ifelse(rollnumber=="1071", 3009, NA))))))))))]
rc_long_108_fltr <- rc_long_108[rollnumber>2999 & rollnumber<3010]
save(rc_long_108_fltr, file = "rc_long_108_CEOs_fltr.RData")

###109th Congress###
##retrieve roll calls of interest
rc_long_109[, rollnumber:= ifelse(rollnumber=="156", 3000,
            ifelse(rollnumber=="205", 3001,
            ifelse(rollnumber=="694", 3002,
            ifelse(rollnumber=="1032", 3003,
            ifelse(rollnumber=="1033", 3004,
            ifelse(rollnumber=="1036", 3005,
            ifelse(rollnumber=="1072", 3006,
            ifelse(rollnumber=="1162", 3007,
            ifelse(rollnumber=="1190", 3008, NA)))))))))]
rc_long_109_fltr <- rc_long_109[rollnumber>2999 & rollnumber<3009]
save(rc_long_109_fltr, file = "rc_long_109_fltr.RData")
##CONTRIBUTIONS ANALYSIS
####106th - 109th CONGRESS
##contributions 106th congress
##contributions_106 <- as.data.table(read.csv("C:/Users/Filippo/Desktop/Thesis/Model/_RATIONALIZATION/CONTRIBUTIONS/contribDB_1998.csv"))
str(Data)
Data[, summary(contributor.employer)]
Data[,congress:=ifelse(cycle == 1996, 105, ifelse(cycle==1998, 106,
                     ifelse(cycle==2000, 107, ifelse(cycle==2002, 108,
                     ifelse(cycle==2004, 109, ifelse(cycle==2006, 110, NA))))))]
Data <- Data[,. (transaction.id, transaction.type, congress, candidate.cfscore, efec.org.orig, contributor.name, contributor.cfscore, contributor.cfscore,
                 contributor.employer, amount, recipient.name, recipient.state)]
Data[, summary(contributor.name)]
####filtering by contributor name
PAC_106 <- Data[, contributor.name:= 
                          ifelse(contributor.name=="REALTORS POLITICAL ACTION COMMITTEE", 1,
                          ifelse(contributor.name=="AMERICAN BANKERS ASSOCIATION BANKPAC", 2,
                          ifelse(contributor.name=="american insurance association", 3,
                          ifelse(contributor.name=="nationsbank corp", 4, NA
                          ))))]
##apply filter
PAC_106 <- PAC_106[contributor.name > 0 & contributor.name < 5]
save(PAC_106, file = "PAC_106.RData")
##PACs' tables
pac_table_106 <- PAC_106[, sum(amount), by=contributor.name]
##LaTeX output
xtable(pac_table_106)
##contributor employer analysis
Data[, summary(contributor.employer)]
Data[, summary(contributor.employer=="bank")]
grepl("bank", Data$contributor.employer)
Data[, contributor.employer:= 
      ifelse(contributor.employer=="ernst & young llp", 3, ifelse(contributor.employer=="real estate", 4,
      ifelse(contributor.employer=="investments", 5, ifelse(contributor.employer=="deloitte & touche llp", 6,
      ifelse(contributor.employer=="coopers & lybrand llp", 7, ifelse(contributor.employer=="insurance agent", 8,
      ifelse(contributor.employer=="cpa", 9, ifelse(contributor.employer=="arthur andersen llp", 10, 
      ifelse(contributor.employer=="vinson & elkins llp", 11, ifelse(contributor.employer=="merrill lynch", 12,
      ifelse(contributor.employer=="insurance", 13, ifelse(contributor.employer=="bank of america", 14,
      ifelse(contributor.employer=="accountant", 15, ifelse(contributor.employer=="goldman sachs & co", 16,
      ifelse(contributor.employer=="fifth third bank", 17, ifelse(contributor.employer==" real estate developer", 18,
      ifelse(contributor.employer=="deloitte & touche", 19, ifelse(contributor.employer=="bricker & eckler", 20,
      ifelse(contributor.employer=="real estate broker", 21, ifelse(contributor.employer=="squire sanders & dempsey", 22, NA))))))))))))))))))))]

contr_106 <- Data [contributor.employer > 2 & contributor.employer < 23]
save(contr_106, file = "contr_106.RData")
fin_sec_contributions_1998 <- rbind(PAC_106, contr_106)
##table contributions 106
table_106 <- contr_106[, sum(amount), by=contributor.employer]
##LaTeX output
xtable(table_106)
##106th CONGRESS members (House of Representatives)
members_106 <- as.data.table(read.csv("C:/Users/Filippo/Desktop/Thesis/Model/_RATIONALIZATION/Financial commitee/CSV/HS106_members.csv", sep=","))
#filter for house of representatives
members_106[chamber=="House"]
members_106<-members_106[chamber=="House"]

##tailor made for contributions table
fin_sec_contributions_1998$name <- sub("([A-Za-z]+).*", "\\1", fin_sec_contributions_1998$recipient.name)
fin_sec_contributions_1998[, id:= paste(name, recipient.state)]
fin_sec_contributions_1998 <- fin_sec_contributions_1998[, sum(amount), by="id"]
##create tailor made identity for members table
members_106$name <- sub("([A-Za-z]+).*", "\\1", members_106$bioname)
members_106[, id:= paste(name, state_abbrev)]


##check for duplicates
duplicated(fin_sec_contributions_1998$bioname)

##merging
df<-merge(members_106, fin_sec_contributions_1998, by= "id", all.x = TRUE, allow.cartesian = TRUE)
save(df, file = "106_contr_fin_comm.RData")

##merge with roll call votes
#create id variable
rc_long$bioname <- sub("([A-Za-z]+).*", "\\1", rc_long$name)
rc_long[, key:= paste(bioname, state_abbrev)]
##relevant df data only
df2<-df[,. (id,  dim1, V1)]
colnames(df2)[colnames(df2)=="id"] <- "key"
rc_long_106 <- merge(rc_long, df2, by= "key", all.x = TRUE)
save(rc_long_106, file = "rc_106_contr.RData")

###107TH CONGRESS
##contributions_106 <- as.data.table(read.csv("C:/Users/Filippo/Desktop/Thesis/Model/_RATIONALIZATION/CONTRIBUTIONS/contribDB_1998.csv"))
str(Data)
Data[, summary(contributor.employer)]
contributions_107[,congress:= ifelse(cycle==1998, 106,
                        ifelse(cycle==2000, 107, ifelse(cycle==2002, 108,
                     ifelse(cycle==2004, 109, NA))))]
contributions_107 <- contributions_107[,. (transaction.id, transaction.type, congress, candidate.cfscore, efec.org.orig, contributor.name, contributor.cfscore, contributor.cfscore,
                 contributor.employer, amount, recipient.name, recipient.state)]
contributions_107[, summary(contributor.name)]
####filtering by contributor name
PAC_107 <- contributions_107[, contributor.name:= ifelse(contributor.name=="REALTORS POLITICAL ACTION COMMITTEE", 1, 
                                                  ifelse(contributor.name=="bank of america", 2, 
                                                  ifelse(contributor.name=="AMERICAN BANKERS ASSOCIATION BANKPAC", 3,
                                                  ifelse(contributor.name=="american insurance association", 4,
                                                  ifelse(contributor.name=="ERNST & WHINNEY POLITICAL ACTION COMMITTEE", 5, NA)))))]

##apply filter
PAC_107 <- PAC_107[contributor.name > 0 & contributor.name < 6]
save(PAC_107, file = "PAC_107.RData")
##PACs' tables
pac_table_107 <- PAC_107[, sum(amount), by=contributor.name]
##LaTeX output
xtable(pac_table_107)

contributions_107[, summary(contributor.employer)]
contributions_107[, contributor.employer:= 
       ifelse(contributor.employer=="bank of america", 3, ifelse(contributor.employer=="ernst & young llp", 4,
  ifelse(contributor.employer=="real estate", 5, ifelse(contributor.employer=="investments", 6,
  ifelse(contributor.employer=="compass bancshares inc", 7, 
 ifelse(contributor.employer=="arthur andersen llp", 8,
    ifelse(contributor.employer=="cpa", 9, 
    ifelse(contributor.employer=="deloitte & touche", 10, 
    ifelse(contributor.employer=="realtor", 11, ifelse(contributor.employer=="merrill lynch", 12,
      ifelse(contributor.employer=="goldman sachs", 13, 
     ifelse(contributor.employer=="deloitte & touche llp", 14,
    ifelse(contributor.employer=="insurance agent", 15, 
    ifelse(contributor.employer=="insurance", 16,
                        ifelse(contributor.employer=="vinson & elkins llp", 17, 
                                            ifelse(contributor.employer=="vinson & elkins llp", 18, 
                                     ifelse(contributor.employer=="insurance agent", 19, ifelse(contributor.employer=="goldman sachs & co", 20, NA))))))))))))))))))]

fin_sec_contributions_2000 <- contributions_107[contributor.employer > 2 & contributor.employer < 21]
save(fin_sec_contributions_2000, file = "contr_107.RData")
PAC_107 <- PAC_107[,. (transaction.id, transaction.type, congress, candidate.cfscore, efec.org.orig, contributor.name, contributor.cfscore, contributor.cfscore,
                                           contributor.employer, amount, recipient.name, recipient.state)]
fin_sec_contributions_2000 <- rbind(PAC_107, fin_sec_contributions_2000)
##table contributions 107
table_107 <- fin_sec_contributions_2000[, sum(amount), by=contributor.employer]
##LaTeX output
xtable(table_107)
#107th CONGRESS members (House of Representatives)
members_107 <- as.data.table(read.csv("C:/Users/Filippo/Desktop/Thesis/Model/_RATIONALIZATION/Financial commitee/CSV/HS107_members.csv", sep=","))
#filter for house of representatives
members_107<-members_107[chamber=="House"]

##tailor made for contributions table
fin_sec_contributions_2000$name <- sub("([A-Za-z]+).*", "\\1", fin_sec_contributions_2000$recipient.name)
fin_sec_contributions_2000[, id:= paste(name, recipient.state)]
fin_sec_contributions_2000 <- fin_sec_contributions_2000[, sum(amount), by="id"]
##create tailor made identity for members table
members_107$name <- sub("([A-Za-z]+).*", "\\1", members_107$bioname)
members_107[, id:= paste(name, state_abbrev)]


##check for duplicates
duplicated(fin_sec_contributions_2000$bioname)

##merging
df<-merge(members_107, fin_sec_contributions_2000, by= "id", all.x = TRUE, allow.cartesian = TRUE)
save(df, file = "107_contr_fin_comm.RData")

##merge with roll call votes
#create id variable
rc_long$bioname <- sub("([A-Za-z]+).*", "\\1", rc_long$name)
rc_long[, key:= paste(bioname, state_abbrev)]
##relevant df data only
df2<-df[,. (id, fin_comm, nominate_log_likelihood, nokken_poole_dim1, V1)]
colnames(df2)[colnames(df2)=="id"] <- "key"
rc_long_107<- merge(rc_long, df2, by= "key", all.x = TRUE)
save(rc_long_107, file = "rc_107_fin_comm&contr.RData")

##108TH CONGRESS
##contributions_108 <- as.data.table(read.csv("C:/Users/Filippo/Desktop/Thesis/Model/_RATIONALIZATION/CONTRIBUTIONS/contribDB_1998.csv"))
str(Data)
Data[, summary(contributor.employer)]
contributions_108[,congress:= ifelse(cycle==1998, 106,
                           ifelse(cycle==2000, 107, ifelse(cycle==2002, 108,
                           ifelse(cycle==2004, 109, NA))))]
contributions_108 <- contributions_108[,. (transaction.id, transaction.type, congress, candidate.cfscore, efec.org.orig, contributor.name, contributor.cfscore, contributor.cfscore,
                                           contributor.employer, amount, recipient.name, recipient.state)]
contributions_108[, summary(contributor.name)]
####filtering by contributor name
PAC_108 <- contributions_108[, contributor.name:= ifelse(contributor.name=="bank of america", 1,
                ifelse(contributor.name=="REALTORS POLITICAL ACTION COMMITTEE", 2,
                ifelse(contributor.name=="AMERICAN BANKERS ASSOCIATION BANKPAC", 3,
                ifelse(contributor.name=="american insurance association", 4,
                ifelse(contributor.name=="citigroup", 5,
                ifelse(contributor.name=="wells fargo", 6,
                ifelse(contributor.name=="us bank", 7,
                ifelse(contributor.name=="TOUCHE ROSS PARTNERS FEDERAL POLITICAL ACTION COMMITTEE TRPAC", 8,
                ifelse(contributor.name=="missouri bankers association", 9, NA)))))))))]

##apply filter
PAC_108 <- PAC_108[contributor.name > 0 & contributor.name < 10]
save(PAC_108, file = "PAC_108.RData")
##PACs' tables
pac_table_108 <- PAC_108[, sum(amount), by=contributor.name]
##LaTeX output
xtable(pac_table_108)

contributions_108[, summary(contributor.employer)]
contributions_108[, contributor.employer:= ifelse(contributor.employer=="bank of america", 1,
                  ifelse(contributor.employer=="compass bancshares inc", 2,
                  ifelse(contributor.employer=="allstate insurance co", 3,
                  ifelse(contributor.employer=="key bank national assn", 4,
                  ifelse(contributor.employer=="household corp", 5,
                  ifelse(contributor.employer=="national city bank", 6,
                  ifelse(contributor.employer=="jpmorgan chase", 7,
                  ifelse(contributor.employer=="credit suisse first boston corp",8,
                  ifelse(contributor.employer=="citicorp", 9,
                  ifelse(contributor.employer=="amsouth bank", 10,
                  ifelse(contributor.employer=="american general finance corp", 11,
                  ifelse(contributor.employer=="ernst & young llp", 12,
                  ifelse(contributor.employer=="cash america international inc", 13, NA)))))))))))))]

fin_sec_contributions_2002 <- contributions_108[contributor.employer > 0 & contributor.employer < 14]
save(fin_sec_contributions_2002, file = "contr_108.RData")
PAC_107 <- PAC_107[,. (transaction.id, transaction.type, congress, candidate.cfscore, efec.org.orig, contributor.name, contributor.cfscore, contributor.cfscore,
                       contributor.employer, amount, recipient.name, recipient.state)]
##merging the two contributions' dataset
fin_sec_contributions_2002 <- rbind(PAC_108, fin_sec_contributions_2002)
##table contributions 108th Congress
table_108 <- fin_sec_contributions_2002[, sum(amount), by=contributor.employer]
##LaTeX output
xtable(table_108)
#108th CONGRESS members (House of Representatives)
members_108 <- as.data.table(read.csv("C:/Users/Filippo/Desktop/Thesis/Model/_RATIONALIZATION/Financial commitee/CSV/HS108_members.csv", sep=","))
#filter for house of representatives
members_108<-members_108[chamber=="House"]

##tailor made for contributions table
fin_sec_contributions_2002$name <- sub("([A-Za-z]+).*", "\\1", fin_sec_contributions_2002$recipient.name)
fin_sec_contributions_2002[, id:= paste(name, recipient.state)]
fin_sec_contributions_2002 <- fin_sec_contributions_2002[, sum(amount), by="id"]
##create tailor made identity for members table
members_108$name <- sub("([A-Za-z]+).*", "\\1", members_108$bioname)
members_108[, id:= paste(name, state_abbrev)]

##check for duplicates
duplicated(fin_sec_contributions_2002$bioname)

##merging
df<-merge(members_108, fin_sec_contributions_2002, by= "id", all.x = TRUE, allow.cartesian = TRUE)
save(df, file = "108_contr_fin_comm.RData")

##merge with roll call votes
#create id variable
rc_long$bioname <- sub("([A-Za-z]+).*", "\\1", rc_long$name)
rc_long[, key:= paste(bioname, state_abbrev)]
##relevant df data only
df2<-df[,. (id, fin_comm, nominate_log_likelihood, nokken_poole_dim1, V1)]
colnames(df2)[colnames(df2)=="id"] <- "key"
rc_long_108<- merge(rc_long, df2, by= "key", all.x = TRUE)
save(rc_long_108, file = "rc_108_fin_comm&contr.RData")

##109TH CONGRESS
##contributions_109 <- as.data.table(read.csv("C:/Users/Filippo/Desktop/Thesis/Model/_RATIONALIZATION/CONTRIBUTIONS/contribDB_1998.csv"))
##str(Data)
##Data[, summary(contributor.employer)]
contributions_109[,congress:= ifelse(cycle==1998, 106,
                        ifelse(cycle==2000, 107, ifelse(cycle==2002, 108,
                        ifelse(cycle==2004, 109, NA))))]
contributions_109 <- contributions_109[,. (transaction.id, transaction.type, congress, candidate.cfscore, efec.org.orig, contributor.name, contributor.cfscore, contributor.cfscore,
                                           contributor.employer, amount, recipient.name, recipient.state)]
contributions_109[, summary(contributor.name)]
####filtering by contributor name
##the acronym PAC stands for Political Action Committee
PAC_109 <- contributions_109[, contributor.name:= ifelse(contributor.name=="REALTORS POLITICAL ACTION COMMITTEE", 1,
                        ifelse(contributor.name=="bank of america", 2,
                        ifelse(contributor.name=="AMERICAN BANKERS ASSOCIATION BANKPAC", 3,
                        ifelse(contributor.name=="american insurance association", 4,
                        ifelse(contributor.name=="us bank", 5,
                        ifelse(contributor.name=="citigroup", 6,
                        ifelse(contributor.name=="wells fargo", 7,
                        ifelse(contributor.name=="missouri bankers association", 8, NA
                        ))))))))]
##apply filter
PAC_109 <- PAC_109[contributor.name > 0 & contributor.name < 9]
save(PAC_109, file = "PAC_109.RData")
##PACs' tables
pac_table_109 <- PAC_109[, sum(amount), by=contributor.name]
##LaTeX output
xtable(pac_table_109)
###filtering by contributor employer
contributions_109[, summary(contributor.employer)]
contributions_109[, contributor.employer:= ifelse(contributor.employer=="bank of america", 1,
                         ifelse(contributor.employer=="mbna america", 2,
                         ifelse(contributor.employer=="comerica inc", 3,
                         ifelse(contributor.employer=="compass bancshares inc", 4,
                         ifelse(contributor.employer=="compass bancshares, inc", 5,
                         ifelse(contributor.employer=="household corp", 6,
                         ifelse(contributor.employer=="jpmorgan chase bank", 7,
                         ifelse(contributor.employer=="citicorp", 8,
                         ifelse(contributor.employer=="credit suisse first boston corp", 9,
                         ifelse(contributor.employer=="citicorp", 10,
                         ifelse(contributor.employer=="wells fargo", 11,
                         ifelse(contributor.employer=="fannie mae", 12,
                         ifelse(contributor.employer=="bank one; na", 13,
                         ifelse(contributor.employer=="key bank national assn", 14,
                         ifelse(contributor.employer=="national city bank", 15, NA)))))))))))))))]
##apply filter
fin_sec_contributions_2004 <- contributions_109[contributor.employer > 0 & contributor.employer < 16]
save(fin_sec_contributions_2004, file = "contr_109.RData")
##merging the two contributions' dataset
fin_sec_contributions_2004 <- rbind(PAC_109, fin_sec_contributions_2004)
##table contributions 109th Congress
table_109 <- fin_sec_contributions_2004[, sum(amount), by=contributor.employer]
##LaTeX output
xtable(table_109)
##109th CONGRESS members (House of Representatives)
members_109 <- as.data.table(read.csv("C:/Users/Giudici/Desktop/Thesis/HS109_members.csv", sep=","))
#filter for house of representatives
members_109<-members_109[chamber=="House"]

##tailor made for contributions table
fin_sec_contributions_2004$name <- sub("([A-Za-z]+).*", "\\1", fin_sec_contributions_2004$recipient.name)
fin_sec_contributions_2004[, id:= paste(name, recipient.state)]
fin_sec_contributions_2004 <- fin_sec_contributions_2004[, sum(amount), by="id"]
##create tailor made identity for members table
members_109$name <- sub("([A-Za-z]+).*", "\\1", members_109$bioname)
members_109[, id:= paste(name, state_abbrev)]
##check for duplicates
duplicated(fin_sec_contributions_2004$bioname)
##merging
df<-merge(members_109, fin_sec_contributions_2004, by= "id", all.x = TRUE, allow.cartesian = TRUE)
save(df, file = "109_contr_fin_comm.RData")
##load roll call votes of interest related to CONGRESS 109th
##merge corresponding contributions with roll call votes
#create id variable
rc_long$bioname <- sub("([A-Za-z]+).*", "\\1", rc_long$name)
rc_long[, key:= paste(bioname, state_abbrev)]
##relevant df data only
df2<-df[,. (id, fin_comm, nominate_log_likelihood, nokken_poole_dim1, V1)]
colnames(df2)[colnames(df2)=="id"] <- "key"
rc_long_109<- merge(rc_long, df2, by= "key", all.x = TRUE)
rc_long_109[, summary(V1)]
save(rc_long_109, file = "rc_109_fin_comm&contr.RData")

###Top 500 listed Fortunes' Chief Executive Officers contributions 
###grouping CEOs' contribution by Congress and representatives
##Congress 106th - 109th
##exploring the dataset
str(contributions_CEO)
contributions_CEO[, summary(corpname)]
##selecting only pertinent variables
contributions_CEO[,. (dime.cid, corpname, cycle, amount, contributor.employer,
              recipient.name, recipient.party)]
contributions_CEO <- contributions_CEO[,. (dime.cid, corpname, cycle, amount, contributor.employer,
                                  recipient.name, recipient.party)]
##Creating a variable to identify the correspoding congress
contributions_CEO[,congress:=ifelse(cycle == 1996, 105, ifelse(cycle==1998, 106,
                  ifelse(cycle==2000, 107, ifelse(cycle==2002, 108,
                  ifelse(cycle==2004, 109, ifelse(cycle==2006, 110, NA))))))]
##106th CONGRESS
contributions_CEO_106 <- contributions_CEO[congress==106]
##filtering by company's name
contributions_CEO_106[, summary(corpname)]
contributions_CEO_106[, corpname:= ifelse(corpname=="Fifth Third Bancorp", 1000,
                    ifelse(corpname=="Loews Corp", 2000,
                    ifelse(corpname=="Franklin Resources", 3000,
                    ifelse(corpname=="Liberty Mutual Holding Company", 4000,
                    ifelse(corpname=="Berkshire Hathaway Inc", 5000,
                    ifelse(corpname=="CBRE Group", 6000, 
                    ifelse(corpname=="Goldman Sachs Group", 7000,
                    ifelse(corpname=="State Street Corp", 8000,
                    ifelse(corpname=="Charles Schwab Corp", 9000,
                    ifelse(corpname=="Western & Southern Financial Group Inc", 10000,
                    ifelse(corpname=="Bank of New York Mellon Corporation", 11000,
                    ifelse(corpname=="Bank of America Corp", 12000,
                    ifelse(corpname=="Regions Financial", 13000,
                    ifelse(corpname=="Blackrock", 14000,
                    ifelse(corpname=="Ally Financial Inc", 15000,
                    ifelse(corpname=="Fidelity National Financial", 16000,
                    ifelse(corpname=="Lincoln National Corp", 17000, NA)))))))))))))))))] 

str(contributions_CEO_106)
contributions_CEO_106[, summary(corpname)]
contributions_CEO_106[, unique(corpname)]
##apply filter
corp_CEO_106 <- contributions_CEO_106[corpname > 999 & corpname < 17001]
save(corp_CEO_106, file="corp_CEO_106.RData")
corp_CEO_106[, summary(contributor.employer)]
##filtering by contributor employer
contributions_CEO_106[, contributor.employer:= ifelse(contributor.employer=="goldman sachs & co", 1,
                     ifelse(contributor.employer=="fannie mae", 2,
                     ifelse(contributor.employer=="bank of america", 3,
                     ifelse(contributor.employer=="ernst & young llp", 4,
                     ifelse(contributor.employer=="compass bancshares inc", 5,
                     ifelse(contributor.employer=="deloitte & touche llp", 6,
                     ifelse(contributor.employer=="ernst & young", 7,
                     ifelse(contributor.employer=="boston capital partners", 8,
                     ifelse(contributor.employer=="arthur andersen llp", 9,
                     ifelse(contributor.employer=="deloitte & touche", 10,
                     ifelse(contributor.employer=="loews corp", 12,
                     ifelse(contributor.employer=="allen & co", 13,
                     ifelse(contributor.employer=="aea investors inc", 14,
                     ifelse(contributor.employer=="goldman sachs", 15,
                     ifelse(contributor.employer=="cincinnati financial", 16,
                     ifelse(contributor.employer=="goldman, sachs & co", 17,
                     ifelse(contributor.employer=="national city bank", 18, NA
                      )))))))))))))))))]
str(contributions_CEO_106)
contributions_CEO_106[, summary(contributor.employer)]
##apply filter
contributions_CEO_106 <- contributions_CEO_106[contributor.employer > 0 &
                        contributor.employer < 19]
save(contributions_CEO_106, file = "contr_106.RData")
##table 106th Congress CEOs contributions
fin_CEO_1998$corpname[is.na(fin_CEO_1998$corpname)] <- 0
CEO_106 <- fin_CEO_1998[, sum(amount), by=corpname]
##LaTeX table
xtable(CEO_106)
##merging the two contributions files
fin_CEO_1998 <- rbind(contributions_CEO_106, corp_CEO_106)

#106th CONGRESS members (House of Representatives)
##open ideological data set embedded of campaign contributions and PACs
##tailor made id for contribution table
fin_CEO_1998$name <- sub("([A-Za-z]+).*", "\\1", fin_CEO_1998$recipient.name)
fin_CEO_1998[, id:= paste(name, recipient.party)]
fin_CEO_1998<- fin_CEO_1998[, sum(amount), by="id"]
##create tailor made identity for members table
df[, h:= paste(name, party_code)]

##check for duplicates
duplicated(fin_CEO_1998$id)

##merging
colnames(fin_CEO_1998)[colnames(fin_CEO_1998)=="id"] <- "h"
colnames(fin_CEO_1998)[colnames(fin_CEO_1998)=="V1"] <- "CEOs"
df2<-merge(df, fin_CEO_1998, by= "h", all.x = TRUE, allow.cartesian = TRUE)
save(df, file = "106_contr_fin_comm_CEOs.RData")
df2[, summary(CEOs)]
##merge with roll call votes
#create id variable
rc_long_106[, h:= paste(bioname, party_code)]
##relevant df data only
df2<-df2[,. (h, CEOs)]
df2 <- df2[, sum(CEOs), by= "h"]
colnames(df2)[colnames(df2)=="V1"] <- "CEOs"
rc_long_106_CEOs<- merge(df2, rc_long_106, by= "h", all.x = TRUE)
save(rc_long_106_CEOs, file = "rc_106_fin_comm_contr_CEOs.RData")

###grouping CEOs' contribution by Congress and representatives
##exploring the dataset
str(contributions_CEO)
contributions_CEO[, summary(corpname)]
##selecting only pertinent variables
contributions_CEO[,. (dime.cid, corpname, cycle, amount, contributor.employer,
                      recipient.name, recipient.party)]
contributions_CEO <- contributions_CEO[,. (dime.cid, corpname, cycle, amount, contributor.employer,
                                           recipient.name, recipient.party)]
##Creating a variable to identify the correspoding congress
contributions_CEO[,congress:=ifelse(cycle == 1996, 105, ifelse(cycle==1998, 106,
                                       ifelse(cycle==2000, 107, ifelse(cycle==2002, 108,
                                  ifelse(cycle==2004, 109, ifelse(cycle==2006, 110, NA))))))]
##107th CONGRESS
contributions_CEO_107 <- contributions_CEO[congress==107]
##filtering by company's name
contributions_CEO_107[, summary(corpname)]
contributions_CEO_107[, corpname:= ifelse(corpname=="Fifth Third Bancorp", 1000,
                      ifelse(corpname=="Franklin Resources", 1001,
                      ifelse(corpname=="Loews Corp", 1002,
                      ifelse(corpname=="CBRE Group", 1003,
                      ifelse(corpname=="Bank of New York Mellon Corporation", 1004,
                      ifelse(corpname=="Goldman Sachs Group", 1005,
                      ifelse(corpname=="Berkshire Hathaway Inc", 1006,
                      ifelse(corpname=="Ally Financial Inc", 1007,
                      ifelse(corpname=="Bank of America Corp", 1008,
                      ifelse(corpname=="Blackrock", 1009,
                      ifelse(corpname=="CIT Group", 1010,
                      ifelse(corpname=="Liberty Mutual Holding Company", 1011,
                      ifelse(corpname=="Discover Financial Services", 1012,
                      ifelse(corpname=="State Street Corp", 1013,
                      ifelse(corpname=="Regions Financial", 1014,
                      ifelse(corpname=="Western & Southern Financial Group Inc",1015, NA
                      ))))))))))))))))]
##apply filter
corp_CEO_107 <- contributions_CEO_107[corpname > 999 & corpname < 1016]
save(corp_CEO_107, file="corp_CEO_107.RData")
corp_CEO_107[, summary(contributor.employer)]
contributions_CEO_107[, summary(contributor.employer)]
contributions_CEO_107[, contributor.employer:= ifelse(contributor.employer=="goldman sachs & co", 1,
                      ifelse(contributor.employer=="goldman sachs", 2,
                      ifelse(contributor.employer=="fannie mae", 3,
                      ifelse(contributor.employer=="ernst & young llp", 4,
                      ifelse(contributor.employer=="deloitte & touche", 5,
                      ifelse(contributor.employer=="citigroup", 6,
                      ifelse(contributor.employer=="commercial credit corp", 7,
                      ifelse(contributor.employer=="ernst & young", 8,
                      ifelse(contributor.employer=="loews corp", 9,
                      ifelse(contributor.employer=="bank of america", 10,
                      ifelse(contributor.employer=="salomon smith barney holdings", 11,
                      ifelse(contributor.employer=="spo partners & co", 12,
                      ifelse(contributor.employer=="morgan stanley dean witter", 13,
                      ifelse(contributor.employer=="compass bancshares inc", 14,
                      ifelse(contributor.employer=="allen & co", 15,
                      ifelse(contributor.employer=="allen & co inc", 16,
                      ifelse(contributor.employer=="pricewaterhousecoopers llp", 17,
                      ifelse(contributor.employer=="boston capital", 18,
                      ifelse(contributor.employer=="chase manhattan bank", 19,
                      ifelse(contributor.employer=="deloitte & touche llp", 20, NA
                      ))))))))))))))))))))]
str(contributions_CEO_107)
contributions_CEO_107[, summary(contributor.employer)]
##apply filter
contributions_CEO_107 <- contributions_CEO_107[contributor.employer > 0 &
                                                 contributor.employer < 21]
save(contributions_CEO_107, file = "contr_107.RData")
##merging the two contributions files
fin_CEO_2000 <- rbind(contributions_CEO_107, corp_CEO_107)
##table 107th Congress CEOs contributions
fin_CEO_2000$corpname[is.na(fin_CEO_2000$corpname)] <- 0
CEO_107 <- fin_CEO_2000[, sum(amount), by=corpname]
##LaTeX table
xtable(CEO_107)
#107th CONGRESS members (House of Representatives)
##open ideological data set embedded of campaign contributions and PACs
##tailor made id for contribution table
fin_CEO_2000$name <- sub("([A-Za-z]+).*", "\\1", fin_CEO_2000$recipient.name)
fin_CEO_2000[, id:= paste(name, recipient.party)]
fin_CEO_2000<- fin_CEO_2000[, sum(amount), by="id"]
##create tailor made identity for members table
df[, h:= paste(name, party_code)]
##check for duplicates
duplicated(fin_CEO_2000$id)
##merging
colnames(fin_CEO_2000)[colnames(fin_CEO_2000)=="id"] <- "h"
colnames(fin_CEO_2000)[colnames(fin_CEO_2000)=="V1"] <- "CEOs"
df2<-merge(df, fin_CEO_2000, by= "h", all.x = TRUE)
save(df2, file = "107_contr_fin_comm_CEOs.RData")
df2[, summary(CEOs)]
##merge with roll call votes
#create id variable
rc_long_107[, h:= paste(bioname, party_code)]
##relevant df data only
df2<-df2[,. (h, CEOs)]
df2 <- df2[, sum(CEOs), by= "h"]
colnames(df2)[colnames(df2)=="V1"] <- "CEOs"
rc_long_107_CEOs<- merge(df2, rc_long_107, by= "h", all.x = TRUE)
save(rc_long_107_CEOs, file = "rc_107_fin_comm_contr_CEOs.RData")

###grouping CEOs' contribution by Congress and representatives
##exploring the dataset
str(contributions_CEO)
contributions_CEO[, summary(corpname)]
##selecting only pertinent variables
contributions_CEO[,. (dime.cid, corpname, cycle, amount, contributor.employer,
                      recipient.name, recipient.party)]
contributions_CEO <- contributions_CEO[,. (dime.cid, corpname, cycle, amount, contributor.employer,
                                           recipient.name, recipient.party)]
##Creating a variable to identify the correspoding congress
contributions_CEO[,congress:=ifelse(cycle == 1996, 105, ifelse(cycle==1998, 106,
                  ifelse(cycle==2000, 107, ifelse(cycle==2002, 108,
                  ifelse(cycle==2004, 109, ifelse(cycle==2006, 110, NA))))))]
##108th CONGRESS
contributions_CEO_108 <- contributions_CEO[congress==108]
##filtering by company's name
contributions_CEO_108[, summary(corpname)]
contributions_CEO_108[, corpname:= ifelse(corpname=="Bank of New York Mellon Corporation", 1001,
                    ifelse(corpname=="Fifth Third Bancorp", 1002,
                    ifelse(corpname=="Loews Corp", 1003,
                    ifelse(corpname=="Liberty Mutual Holding Company", 1004,
                    ifelse(corpname=="Berkshire Hathaway Inc", 1005,
                    ifelse(corpname=="Ally Financial Inc", 1006,
                    ifelse(corpname=="CBRE Group", 1007,
                    ifelse(corpname=="Regions Financial", 1008,
                    ifelse(corpname=="Marsh & McLennan Companies", 1009,
                    ifelse(corpname=="Lincoln National Corp", 1010,
                    ifelse(corpname=="SLM Corp", 1011,
                    ifelse(corpname=="CIT Group", 1012,
                    ifelse(corpname=="Blackrock", 1013,
                    ifelse(corpname=="State Street Corp", 1014,
                    ifelse(corpname=="Discover Financial Services", 1015,
                    ifelse(corpname=="Goldman Sachs Group", 1016, NA
                           ))))))))))))))))]
##apply filter
corp_CEO_108 <- contributions_CEO_108[corpname > 1000 & corpname < 1017]
save(corp_CEO_108, file="corp_CEO_108.RData")
corp_CEO_108[, summary(contributor.employer)]
contributions_CEO_108[, summary(contributor.employer)]
contributions_CEO_108[, contributor.employer:= ifelse(contributor.employer=="northwestern mutual", 1,
                  ifelse(contributor.employer=="compass bancshares inc", 2,
                  ifelse(contributor.employer=="liberty mutual", 3,
                  ifelse(contributor.employer=="jp morgan chase bank", 4,
                  ifelse(contributor.employer=="goldman sachs & co", 5,
                  ifelse(contributor.employer=="salomon smith barney", 6,
                  ifelse(contributor.employer=="citicorp", 7,
                  ifelse(contributor.employer=="john hancock financial services", 8,
                  ifelse(contributor.employer=="bank of america", 9,
                  ifelse(contributor.employer=="ernst & young llp", 10,
                  ifelse(contributor.employer=="fannie mae", 11,
                  ifelse(contributor.employer=="deloitte & touche", 12,
                  ifelse(contributor.employer=="key bank national assn", 13,
                  ifelse(contributor.employer=="nationwide insurance co", 14,
                  ifelse(contributor.employer=="loews corp", 15, NA)))))))))))))))]
str(contributions_CEO_108)
contributions_CEO_108[, summary(contributor.employer)]
##apply filter
contributions_CEO_108 <- contributions_CEO_108[contributor.employer > 0 &
                                                 contributor.employer < 16]
save(contributions_CEO_108, file = "contr_108.RData")
##merging the two contributions files
fin_CEO_2002 <- rbind(contributions_CEO_108, corp_CEO_108)
##table 108th Congress CEOs contributions
fin_CEO_2002$corpname[is.na(fin_CEO_2002$corpname)] <- 0
CEO_108 <- fin_CEO_2002[, sum(amount), by=corpname]
##LaTeX table
xtable(CEO_108)
#108th CONGRESS members (House of Representatives)
##open ideological data set embedded of campaign contributions and PACs
##tailor made id for contribution table
fin_CEO_2002$name <- sub("([A-Za-z]+).*", "\\1", fin_CEO_2002$recipient.name)
fin_CEO_2002[, id:= paste(name, recipient.party)]
fin_CEO_2002<- fin_CEO_2002[, sum(amount), by="id"]
##load congress and conctributions table
##create tailor made identity for members table
df[, h:= paste(name, party_code)]
##check for duplicates
duplicated(fin_CEO_2002$id)
##merging
colnames(fin_CEO_2002)[colnames(fin_CEO_2002)=="id"] <- "h"
colnames(fin_CEO_2002)[colnames(fin_CEO_2002)=="V1"] <- "CEOs"
df2<-merge(df, fin_CEO_2002, by= "h", all.x = TRUE)
save(df2, file = "108_contr_fin_comm_CEOs.RData")
df2[, summary(CEOs)]
##open roll call votes of interest embedded of campaing contributions
##merge with roll call votes
#create id variable
rc_long_108[, h:= paste(bioname, party_code)]
##relevant df data only
df2<-df2[,. (h, CEOs)]
df2 <- df2[, sum(CEOs), by= "h"]
colnames(df2)[colnames(df2)=="V1"] <- "CEOs"
rc_long_108_CEOs<- merge(df2, rc_long_108, by= "h", all.x = TRUE)
save(rc_long_108_CEOs, file = "rc_108_fin_comm_contr_CEOs.RData")

##109th CONGRESS
###grouping CEOs' contribution by Congress and representatives
##exploring the dataset
str(contributions_CEO)
contributions_CEO[, summary(corpname)]
##selecting only pertinent variables
contributions_CEO[,. (dime.cid, corpname, cycle, amount, contributor.employer,
                      recipient.name, recipient.party)]
contributions_CEO <- contributions_CEO[,. (dime.cid, corpname, cycle, amount, contributor.employer,
                                           recipient.name, recipient.party)]
##Creating a variable to identify the correspoding congress
contributions_CEO[,congress:= ifelse(cycle==1998, 106,
                        ifelse(cycle==2000, 107, ifelse(cycle==2002, 108,
                        ifelse(cycle==2004, 109, NA))))]
##109th CONGRESS
contributions_CEO_109 <- contributions_CEO[congress==109]
##filtering by company's name
contributions_CEO_109[, summary(corpname)]
contributions_CEO_109[, corpname:= ifelse(corpname=="Fifth Third Bancorp", 1001,
                        ifelse(corpname=="Bank of New York Mellon Corporation", 1002,
                        ifelse(corpname=="SLM Corp", 1003,
                        ifelse(corpname=="Ally Financial Inc", 1004,
                        ifelse(corpname=="CBRE Group", 1005,
                        ifelse(corpname=="Marsh & McLennan Companies", 1006,
                        ifelse(corpname=="Liberty Mutual Holding Company", 1007,
                        ifelse(corpname=="Bank of America Corp", 1008,
                        ifelse(corpname=="Blackrock", 1009,
                        ifelse(corpname=="Western & Southern Financial Group Inc", 1010,
                        ifelse(corpname=="Loews Corp", 1011,
                        ifelse(corpname=="Berkshire Hathaway Inc", 1012,
                        ifelse(corpname=="Discover Financial Services", 1013,
                        ifelse(corpname=="Franklin Resources", 1014, NA))))))))))))))]
##apply filter
corp_CEO_109<- contributions_CEO_109[corpname > 1000 & corpname < 1015]
save(corp_CEO_109, file="corp_CEO_109.RData")
###filtering by contributor employer
contributions_CEO_109[, summary(contributor.employer)]
contributions_CEO_109[, contributor.employer:= ifelse(contributor.employer=="jpmorgan chase bank", 1,
                        ifelse(contributor.employer=="liberty mutual", 2,
                        ifelse(contributor.employer=="nationwide insurance", 3,
                        ifelse(contributor.employer=="citicorp", 4,
                        ifelse(contributor.employer=="citigroup corporate", 5,
                        ifelse(contributor.employer=="mbna america", 6,
                        ifelse(contributor.employer=="bank of america", 7,
                        ifelse(contributor.employer=="goldman sachs", 8,
                        ifelse(contributor.employer=="goldman sachs & co", 9,
                        ifelse(contributor.employer=="citigroup", 10,
                        ifelse(contributor.employer=="morgan stanley", 11,
                        ifelse(contributor.employer=="lehman brothers holdings inc", 12,
                        ifelse(contributor.employer=="pncbank, na", 13, NA
                        )))))))))))))] 
##apply filter
contributions_CEO_109 <- contributions_CEO_109[contributor.employer > 0 &
                                                 contributor.employer < 14]
save(contributions_CEO_109, file = "contr_109.RData")
##merging the two contributions files
fin_CEO_2004 <- rbind(contributions_CEO_109, corp_CEO_109)
##table 109th Congress CEOs contributions
fin_CEO_2004$corpname[is.na(fin_CEO_2004$corpname)] <- 0
CEO_109 <- fin_CEO_2004[, sum(amount), by=corpname]
##LaTeX table
xtable(CEO_109)
#109th CONGRESS members (House of Representatives)
##open ideological data set embedded of campaign contributions and PACs
##tailor made id for contribution table
fin_CEO_2004$name <- sub("([A-Za-z]+).*", "\\1", fin_CEO_2004$recipient.name)
fin_CEO_2004[, id:= paste(name, recipient.party)]
fin_CEO_2004<- fin_CEO_2004[, sum(amount), by="id"]
##load congress and conctributions table
##create tailor made identity for members table
df[, h:= paste(name, party_code)]
##check for duplicates
duplicated(fin_CEO_2004$id)
##merging
colnames(fin_CEO_2004)[colnames(fin_CEO_2004)=="id"] <- "h"
colnames(fin_CEO_2004)[colnames(fin_CEO_2004)=="V1"] <- "CEOs"
df2<-merge(df, fin_CEO_2004, by= "h", all.x = TRUE)
save(df2, file = "109_contr_fin_comm_CEOs.RData")
df2[, summary(CEOs)]
##open roll call votes of interest embedded of campaing contributions
##merge with roll call votes
#create id variable
rc_long_109[, h:= paste(bioname, party_code)]
##relevant df data only
df2<-df2[,. (h, CEOs)]
df2 <- df2[, sum(CEOs), by= "h"]
colnames(df2)[colnames(df2)=="V1"] <- "CEOs"
rc_long_109_CEOs<- merge(df2, rc_long_109, by= "h", all.x = TRUE)
save(rc_long_109_CEOs, file = "rc_109_fin_comm_contr_CEOs.RData")

###LOBBYING###
##idenrifying bills and motions which have been 
#influenced by lobbying activities exerted by financial sector
##data sets graciously provided by the Centre for Responsive Politics
##load pertinent data
##bill names
bill <- as.data.table(read.csv("bill.csv", sep = ","))
str(bill)
bill[, unique(congress_no)]
lobbying <- as.data.table(read.csv("lobbying.csv", sep =","))
str(lobbying)
issue <- as.data.table(read.csv("issue.csv", sep = ","))
str(issue)
agency <- as.data.table(read.csv("agency.csv", sep = ","))
str(agency)
lobbyist <- as.data.table(read.csv("lobbyist.csv", sep=","))
str(lobbyist)

#rendering bill's names suitable for merging
#strsplit(bill$bill_name,".")
#bill$bill_name = gsub("//.", "", bill$bill_name)
#save(bill, file = "C:/Users/Filippo/Desktop/Thesis/Model/
#_RATIONALIZATION/LOBBYING/Dataset/bill_lobbied.RData")
#bill <- as.data.table(bill)
#bill_2 <- bill[congress_no > 105 & congress_no < 110]
#the lobbying gathering data starts from 106th CONGRESS
##106th CONGRESS  
##load roll call votes of interests
##congress 106th
bill_106 <- bill[congress_no == 106]
colnames(bill_106)[colnames(bill_106)=="bill_name"] <- "bill_number"
##check bill numbers corresponding in 106th roll calls
bill_106$bill_number %in% rc_long_106$bill_number##check
df <- rc_long_106[, lb:= ifelse(rc_long_106$bill_number
                                    %in% bill_106$bill_number,1, 0)]
df[, summary(lb)]
save(df, file = "C:/Users/Filippo/Desktop/Thesis/Model/
     _RATIONALIZATION/LOBBYING/_106.RData")

##107th CONGRESS
##congress 107th
bill_107 <- bill[congress_no == 107]
colnames(bill_107)[colnames(bill_107)=="bill_name"] <- "bill_number"
##load roll call votes 107th CONGRESS
##check bill numbers corresponding in 107th roll calls
bill_107$bill_number %in% rc_long_107_CEOs$bill_number##check
df <- rc_long_107[, lb:= ifelse(rc_long_107$bill_number
                                     %in% bill_107$bill_number,1, 0)]
df[, summary(lb)]
save(df, file = "C:/Users/Filippo/Desktop/Thesis/Model/
     _RATIONALIZATION/LOBBYING/_107.RData")
##removing irrelevant data sets
rm(bill_107, df, rc_long_107_CEOs)

##108th CONGRESS
##congress 108th
bill_108 <- bill[congress_no == 108]
colnames(bill_108)[colnames(bill_108)=="bill_name"] <- "bill_number"
##load roll call votes 108th CONGRESS
##check bill numbers corresponding in 108th roll calls
bill_108$bill_number %in% rc_long_108$bill_number##check
df <- rc_long_108[, lb:= ifelse(rc_long_108$bill_number
                                     %in% bill_108$bill_number,1, 0)]
df[, summary(lb)]
save(df, file = "C:/Users/Filippo/Desktop/Thesis/Model/
     _RATIONALIZATION/LOBBYING/_108.RData")
##removing irrelevant data sets
rm(bill_108, df, rc_long_108)

##109th CONGRESS
##congress 109th
bill_109 <- bill[congress_no == 109]
colnames(bill_109)[colnames(bill_109)=="bill_name"] <- "bill_number"
##load roll call votes 109th CONGRESS
##check bill numbers corresponding in 109th roll calls
bill_109$bill_number %in% rc_long_109$bill_number##check
df <- rc_long_109[, lb:= ifelse(rc_long_109$bill_number
                                     %in% bill_109$bill_number,1, 0)]
df[, summary(lb)]
save(df, file = "C:/Users/Filippo/Desktop/Thesis/Model/
     _RATIONALIZATION/LOBBYING/_109.RData")
##removing irrelevant data sets
rm(bill_109, df, rc_long_109)


###MODELING
##fitting the data tables: each table has to be the same number of columns
rc_106 <- df
rc_107 <- df
rc_108 <- df
rc_109 <- df
##exploring data sets
str(rc_106)
rc_106 <- rc_106[,. (h, bill_number, id, lb, key, vote, congress, 
                     dim1,  V1, CEOs, party_code)] 
rc_107 <- rc_107[,. (h, bill_number, id, lb, key, vote, congress, 
                     dim1, V1, CEOs, party_code)]
rc_108 <- rc_108[,. (h, bill_number, id, lb, key, vote, congress, 
                     dim1, V1, CEOs, party_code)]
rc_109 <- rc_109[,. (h, bill_number, id, lb, key, vote, congress, 
                     dim1, V1, CEOs, party_code)]
#MERGING ROLL CALL VOTES PROVIDED OF CAMPAIGN CONTRIBUTIONS
#CEOs DIRECT INFLUENCE, LOBBYING AND FINANCIAL COMMISSION DUMMY
rc_long_M <- rbind(rc_106, rc_107)
rc_M2 <- rbind(rc_long_M, rc_108, rc_109)
#save(rc_M2, file = "MASTER_FILE_elaborated.RData")

##DATA EXPLORATION
rc_M2[, summary(V1)]
rc_M2[, histogram(V1)]
##CONTRIBUTIONS TRANSFORMATION
#compute logarithm of V1
rc_M2 <- rc_M2[, lnV1:= ifelse(is.na(rc_M2$V1), 0,  log(V1) )]
rc_M2$V1[ is.na(rc_M2$V1)] <- 0
rc_M2 <- rc_M2[lnV1 !=0]##remove zero entries
rc_M2 <- rc_M2[bill_number != "HR4577"]
##DUMMY CEOs
rc_M2$CEOs[is.na(rc_M2$CEOs)] <- 0
rc_M2 <- rc_M2[, dummy_CEOs:= ifelse(CEOs==0, 0, 1  )]
## Recode votes
rc_M2$voteYes <- ifelse(rc_M2$vote == 1, 1, ifelse(rc_M2$vote == 6, 0, NA))
# Recode party (add independent to democrats)
df2$republican <- ifelse(df2$party_code == "200", 1, 0)
##DUMMY FINANCIAL COMMITTEE
df2$fin_comm[is.na(df2$fin_comm)] <- 0
##SCATTER PLOTS
#nokken poole measure of ideology and campaign contributions
my_data <- rc_M2[, c(13,17)]
chart.Correlation(my_data, histogram=TRUE, pch=19)
rc_M2[,summary(lnV1)]
rc_M2[,is.nan(nokken_poole_dim1)]
rc_M2 <- rc_M2[lnV1 !="-Inf"]##remove zero entries
#save(rc_M2, file = "MASTER_FILE_elaborated.RData")
###analyzing the determinants of ideology
##scatter plot
my_data2 <- df2[, c(10, 20)]
corr <- round(cor(my_data2), 2)
xtable(corr)
head(corr[, 1:5])
chart.Correlation(my_data2, histogram=TRUE, pch=19)
##remove irrelevant roll calls
rc_M2 [, unique(bill_number)]
df[, unique(bill_number)]
##remove the following
#HR3081"  "HR2670"  "HR4577" "HR1714" "HR3194" "HR1474" "HR2500" 
rc_M2 <- rc_M2[! bill_number==("HR2500")]
save(rc_M2, file = "master_file_lm.RData")




###LOGISTIC REGRESSION MODEL
##remove rows containing NA from VoteYes
rc_ <- rc_M2[!is.na(voteYes)]

##run regression
model1 <- glm(voteYes ~  dim1 
               
              , 
              family=binomial(link='logit'),
              data= df2)
summary(model1)

model2 <- glm(voteYes ~  lntot + lb
              
              , 
              family=binomial(link='logit'),
              data= df2)
summary(model2)

model3 <- glm(voteYes ~  lb 
              
              , 
              family=binomial(link='logit'),
              data= df2)
summary(model3)

model4 <- glm(voteYes ~ lntot + lb 
              
              , 
              family=binomial(link='logit'),
              data= df2)
summary(model4)


model5 <- glm(voteYes ~  dim1 
              + lntot + lb 
             , 
             family=binomial(link='logit'),
             data= df2)
summary(model5)
#####z test 
sandwich1 <- function(object, ...) sandwich(object) * nobs(object) / (nobs(object) - 1)
coeftest(model5, vcov = sandwich1)

model6 <- glm(voteYes ~  republican 
              , 
              family=binomial(link='logit'),
              data= df2)
summary(model6)

##machine learning
train <- df2[1:15000]

DATA <- anova(model3, test = "Chisq")
xtable(DATA)
##results table
stargazer( model1, model2, model3, model5, title="Results", align=TRUE)
##plot logistic regression VoteYes ~ campaign contributions
fit = glm(voteYes ~ lnV1, data=rc_M2, family=binomial)
newdat <- data.frame(lnV1=seq(min(rc_M2$lnV1), max(rc_M2$lnV1),len=200))
newdat$voteYes = predict(fit, newdata=newdat, type="response")
plot(voteYes~lnV1, data=rc_M2, col="black")
lines(voteYes ~ lnV1, newdat, col="red2", lwd=2)
##plot logistic regression VoteYes ~ dim1
fit = glm(voteYes ~ dim1, data=rc_M2, family=binomial)
newdat <- data.frame(dim1=seq(min(rc_M2$dim1), max(rc_M2$dim1),len=500))
newdat$voteYes = predict(fit, newdata=newdat, type="response")
plot(voteYes~dim1, data=rc_M2, col="black")
lines(voteYes ~ dim1, newdat, col="red2", lwd=2)
## CIs using profiled log-likelihood
confint(model3)
## CIs using standard errors
confint.default(model3)
## odds ratios only
exp(coef(model3))
## odds ratios and 95% CI
exp(cbind(OR = coef(model3), confint(model3)))
###predicting probabilities
newdata1 <- with(rc_M2, 
          data.frame(dim1 = mean(dim1), lnV1 = mean(lnV1), lb = mean(lb)))
View(newdata1)
newdata1 <- predict(model3, newdata = newdata1, type = "response")
#model diagnostics
anova(model3, test = "Chisq")
#Mc Fadden R2 assessing goodness of fit
pR2(model5)
###assessing predictive ability of the model
library(ROCR)
p <- predict(model3, type="response")
pr <- prediction(p, df2$voteYes)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
##ROC plot
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

fitted.results <- predict(model3,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != rc_M2$voteYes)
print(paste('Accuracy',1-misClasificError))
##checking logistic reggression model assumptions
#1.The dependent variable is binary
#2.The dependent has been coded accordingly to the event of interest
#3.The model should be fitted correctly
#4.The error terms need to be independent
##test for multicollinearity
car::vif(model5)
xtable::dt
#evidence for no multicollinearity
#5.logistic regression assumes linearity of independent variables and log odds.
#6. it requires a quite large sample size
##IT DOES!! THE DATASET PROVIDES 18287 OBSERVATIONS!!
df <- rc_M2[, sum(V1), by=key]
df[, list(key)]
x <- df[, list(key), by= V1==0]

rc_M2[, summary(V1)]
df <- rc_M2[, tot:= (V1 + CEOs)]
df2[, summary(tot)]
df2 <- df[tot !=0]##remove zero entries
df2[, lntot:= log(tot)]

fit = glm(voteYes ~ dim1, data=df2, family=binomial)
newdat <- data.frame(dim1=seq(min(df2$dim1), max(df2$dim1),len=100))
newdat$voteYes = predict(fit, newdata=newdat, type="response")
plot(voteYes~dim1, data=df2, col="black")
lines(voteYes ~ dim1, newdat, col="red", lwd=2)

fit = glm(voteYes ~ lntot, data=df2, family=binomial)
newdat <- data.frame(lntot=seq(min(df2$lntot), max(df2$lntot),len=100))
newdat$voteYes = predict(fit, newdata=newdat, type="response")
plot(voteYes~lntot, data=df2, col="black")
lines(voteYes ~ lntot, newdat, col="red", lwd=2)


plot(model5, which = 4, id.n = 3)
# Extract model results
model.data <- augment(model5) %>% 
  mutate(index = 1:n()) 
model.data %>% top_n(3, .cooksd)

ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = voteYes), alpha = .5) +
  theme_bw()
model.data %>% 
  filter(abs(.std.resid) > 3)


# Predict the probability (p) of diabete positivity
probabilities <- predict(model5, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)

# Select only numeric predictors
mydata <- df2%>%
  dplyr::select_if(is.numeric) 
predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")
