#rm(list=ls())
library("magrittr", lib.loc="~/R/R-3.4.1/library")
setwd("C:/MIGUEL/r/RMA Project") 

RD <- as.data.frame(read.csv(file="C:/MIGUEL/r/RMA Project/regressionData.csv"))
iAW <- as.data.frame(read.csv(file="C:/MIGUEL/r/RMA Project/itemAvgWeight.csv"))
iAW <- dplyr::mutate(iAW,  item=gsub('-','.',iAW$item, fixed = TRUE)) # REPLACE - with .

# delete date boxnum 
RD <- subset.data.frame( RD, select = (names(RD)!=c('Date','BoxNum')) ) 
CName <-as.vector(colnames(RD))  
# delete date BOXWEIGHT 
CName <-CName[CName !='BOXWEIGHT']
XVar <-paste(CName, collapse = '+')





# use loop
#1. find coefficients( <0 or na) and use iAW data to multiple with RD's qty to delete those var and reduce RD's BOX WEIGHT
#2. run regression and then check coefficients( <0 or na)
#3. if so, keep run loop, if not, leave the loop
#coeVar <- RegResult$coefficients[which(!names(RegResult$coefficients)=='(Intercept)')]# delete 'intercept' to be coefficient variables
#L <- length(which(coeVar<=0 | is.na(coeVar)))





while(1==1){  
              #run regression n delete outlier 
              RegResult <- lm( as.formula(paste('BOXWEIGHT~',XVar)) , data=RD)
              
              RowsToKeep <- names(subset(RegResult$residuals, RegResult$residuals >mean(RegResult$residuals)-1.96*sd(RegResult$residuals)
                                         & RegResult$residuals <mean(RegResult$residuals)+1.96*sd(RegResult$residuals))) # residuals between mean +-1.96 std
              RD <- RD[RowsToKeep,]# REMOVE OUTLIER
              
              RegResult <- lm( as.formula(paste('BOXWEIGHT~',XVar)) , data=RD) 
              coeVar <- RegResult$coefficients[which(!names(RegResult$coefficients)=='(Intercept)')]# delete 'intercept' to be coefficient variables
              
              
              
              
              if(length(which(coeVar<=0 | is.na(coeVar))) >0 ){
                data.table::setDT(RD)
                TBS <- dplyr::mutate(RD,rowN=row.names(RD)) # TBS to calculate ('to be subtract' from each row of)
                data.table::setDT(TBS)
                TBS <- data.table::melt(TBS, id.var='rowN')
                deVar <- CName[CName %in% (names(coeVar[which(coeVar<=0 | is.na(coeVar))]))] # var to be deleted, na or <0
                deVAW <- as.data.frame(deVar)%>% dplyr::left_join(iAW, c('deVar'='item')) # Column ,needs to be deleted --need avg weight from old data to substract those column from RD
                if(length(which(is.na(deVAW$AvgWeight)))>0){ # if any na or <0 item , we could not find avg weight from old data, quit this code and show warning
                  print(paste(deVAW$deVar[which(is.na(deVAW$AvgWeight))], 'without historical weight') )
                  quit(save = 'yes')
                }
                
                # joint RD with (deVAW, group by rownum) by rownumber and then boxweight - deVAW's num--to get new boxweight
                # delete na or <0 vars from RD , and then delete rownum of RD
                
                RD <- dplyr::mutate(RD,rowN=row.names(RD))
                TBS <- TBS %>% dplyr::inner_join(deVAW, by=c('variable'='deVar'))
                TBS <- dplyr::mutate(TBS, toSubtract=AvgWeight*value)
                TBS <- dplyr::summarize(dplyr::group_by(TBS, rowN), toSubtract= sum(toSubtract))
                RD <- dplyr::mutate(RD %>% dplyr::left_join(TBS, by=c('rowN'='rowN')), BOXWEIGHT=BOXWEIGHT-toSubtract)
                RD <- dplyr::select(RD, -toSubtract,-rowN, -dplyr::one_of(deVar) )
               
                
                CName <- CName[!CName %in% (names(coeVar[which(coeVar<=0 | is.na(coeVar))]))] #CName(only coeffi >0) delete-- coefficients na or <0
                XVar <-paste(CName, collapse = '+')
               
              }
              else {break
              }
}


# 1.get Residual+intercept  as RTR, row total error
# 2.melt RD, and then join with coefficient, coeffi* qty = IER(item error ratio)
# 3. group by 2 with rownum to get sum(IER) as RER (row error ratio)
# 4. join 2 with 3
# 5.  (RTR * (IER/RER)) / qty  -- EI (error for each item), join 1 with 4
# 6. coeffi + EI = predicted weight
# 7. if predicted weight<=0 OR qty=0 , do not use 
# 8. convert data to database format
#1
RS <- as.data.frame(RegResult$residuals)
RS <- dplyr::mutate(RS,rowN=row.names(RS)) # add row number
RS <- merge(RS, RegResult$coefficients['(Intercept)'], all = TRUE) # cross join 'Intercept'
RS <- dplyr::rename(dplyr::rename(RS,intercept=y),residuals='RegResult$residuals')  # rename column
RS <- dplyr::mutate(RS,RTR= residuals+intercept)
RS <- dplyr::select(RS,rowN,RTR) # only keep rowN,RTR

#2
RD <- dplyr::mutate(RD,rowN=row.names(RD)) # add row num
RD <- dplyr::select(RD,-BOXWEIGHT)# delete boxweight
RD <- data.table::melt(RD, id.var='rowN')
RD <- dplyr::rename(RD, part=variable, qty=value)# change column name
coefficients <- RegResult$coefficients[names(RegResult$coefficients)!='(Intercept)']# delete Intercept
coefficients <- data.frame(part=names(coefficients), coefficients=coefficients) # create data frame 'coefficients' to data frame with vector name
RD <- dplyr::inner_join(RD,coefficients, c('part'='part')) 
RD <- dplyr::mutate(RD,IER=coefficients*qty )
#3
GRD <- dplyr::group_by(RD,rowN) %>% dplyr::summarize(RER=sum(IER))
#4
RD <- dplyr::inner_join(RD,GRD, c('rowN'='rowN')) 
#5
RD <- dplyr::inner_join(RD,RS, c('rowN'='rowN')) %>% dplyr::mutate(EI=(RTR * (IER/RER))/qty)
#6
RD <- dplyr::mutate(RD,PW=coefficients + EI)# predicted weight
#7
RD <- dplyr::filter(RD,qty>0 & PW>0)

#8
RD <- dplyr::mutate(RD, rowN=row.names(RD)) 

ND <- data.frame(part=character(15), weight=numeric(5),stringsAsFactors=FALSE) # create ND, new data to upload to database


n <- 1L
for (i in 1:nrow(RD)){
  
  OPD <- dplyr::filter(RD, rowN==i) %>% dplyr::select(part,qty,PW) # OPD, OUTPUT DATA
  for(k in 1:OPD$qty){
    ND[n,] <-c(as.character(OPD$part),OPD$PW) 
    print(paste(k,n,collapse = ',' ))
    n <- n+1
  }
}  
write.table(RD ,file='clipboard-16348', sep='\t', row.names = FALSE)


