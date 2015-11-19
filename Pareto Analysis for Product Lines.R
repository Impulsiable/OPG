rm(list=ls())
library("RODBC")
library("dplyr")
library(tidyr)
library(ggplot2)


#Connect to MS SQL Server
MAS <- odbcConnect("MAS Operation")
#Check tables
#sqlTables(MAS)
#sqlColumns(MAS, "ViewSOLines")
#Query Data
SOdata <- sqlQuery(MAS, "SELECT SumLine, QtyOrd, ItemID,CreateDate,WhseID,CustID,OPGStat,ExtAmt,OrdStatus
                   FROM ViewSOLines 
                   WHERE WhseID IN ('ROT-EU','DISTRO','AMZN') AND CreateDate >= '2015-01-01'
                   AND SumLine IN ('Jody Coyote','NV-Laid Back',
                                   'NV-Laid Back Kids','Cross of the Trinity','Monarch Inspirations')
                   
                   ")

close(MAS)

summary(SOdata)

SOdata <- tbl_df(SOdata)

sumline <- unique(SOdata$SumLine)

prdlist <- sapply(sumline,FUN = as.list)


for (i in 1:length(sumline)){
  
prdlist[[i]] <- SOdata %>%
           filter(SumLine %in% sumline[i]) %>%
           group_by(SumLine,CustID) %>%
           summarize(OrdAmt = sum(ExtAmt)) %>%
           arrange(desc(OrdAmt)) %>%
           mutate(CumAmt = cumsum(OrdAmt)/sum(OrdAmt))%>%
           mutate(ncut = 1) %>%
           mutate(CumCut = cumsum(ncut)/length(CustID))
}

df <- do.call(rbind,prdlist)

ggplot(df,aes(x=CumCut,y=CumAmt))+geom_line(aes(color=SumLine),size=1.5)+
  xlab("% Customers")+ylab("%Revenue")
           
