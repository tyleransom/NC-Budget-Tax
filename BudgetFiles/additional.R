# This script was written by Artem Streltsov of Duke University (artem.streltsov@duke.edu) and
# Vinod Ramakrishnan of IIT (vinod.ramakrishnan@iitgn.ac.in) for NC Justice Center.
#-------------------------------------------------------------------------------------------------

# These additional functions can do the following:

# ~matcher| 
# operation=1 - combine a line from an odd year budget with the even one
#(see further operation 1 mapping function below)

# operation=2 - create a time series match for some (1) identifier,
# i.e., for example, find encounters in all budgets of Budget code 13510, Fund code 1000,
# Account code 1141, Description "SEC/COUNCIL OF ST SAL-AP" (note: you do not need to specify all identifiers, see function)   

# ~mapping|
# operation=1 - create a matrix with, for instance, identifiers (all codes and descriptions) from revised 2004-05 budget
# and values columns 5-6 the original values from 2003-05 budget, 7-9 the values from the revised budget
# naturally, it uses operation 1 matcher function

# operation=2 - create a matrix with all encounters given a certain base (for instance, take all codes and descriptions
# from 2003-05 budget as base and find all encounters in all other budgets)
# apparently, it makes use of operation 2 matcher function
#-------------------------------------------------------------------------------------

# IMPORTANT! BEFORE PROCEEDING MAKE SURE YOU HAVE ALL THE VOLUMES PROCESSED BY GENERATOR FUNCTION
# OR YOU HAVE THE OUTPUT LOADED INTO THE ENVIRONMENT

# MATCHER:
# Supercode1 (budget code), operation and original are the only required variables to run the function
# (others are optional), where original is (original=NULL) if operation=2
# operation: =1 for pulling in revised years, =2 for time series mapping
# original: =(some number from revised budget) for operation=1. This is nothing but a convenient identifier
# Supercode 2 is fund code and SubsecID is account code
# NOTE: if operation=1, you need to specify ALL variables

matcher <- function(basefile,Supercode1,Supercode2,SubsecID,Description,operation, original){
  
  matcher <- list()
  Supercode1 <- as.character(Supercode1)
  Supercode2 <- as.character(Supercode2)
  
  # allowing for the case when some account codes are renamed. Example: 6006 in 2003-05 transformed into 536006 in later budgets
  if(!(length(as.numeric(SubsecID))==0)){
    if (nchar(SubsecID)>4){
      SubsecID <- substr(SubsecID,3,6)
    }
  }
  
  argList <- list(Supercode1,Supercode2,SubsecID,Description)
  if (operation==2){# finding time series match
    for (t in seq(3,11,by = 2)){
      index=(t+which(seq(3,11,by = 2)==t)-1)/3
      if(t<10){
        files <- get(paste(substr(basefile,1,6),t,sep = "0"))
        nam <- paste("J", t, sep = "")
        # all commands of the type "length(as.numeric(argList[[i]]))==0" check missing values (when you do not specify some variable value)
       if(length(as.numeric(argList[[2]]))==0|length(as.numeric(argList[[3]]))==0|is.null(argList[[4]])==TRUE|is.null(argList[[2]])&length(as.numeric(argList[[3]]))==0|is.null(argList[[2]])&is.null(argList[[4]])==TRUE|is.null(argList[[3]])&is.null(argList[[4]])==TRUE){
          if(length(as.numeric(argList[[2]]))==0){
            if(length(as.numeric(argList[[3]]))==0|is.null(argList[[4]])==TRUE){
              if(length(as.numeric(argList[[3]]))==0){assign(nam, files[intersect(which(files$Supercode1==Supercode1), intersect(agrep(Description, files$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(files$Description)-nchar(Description))<2))),])
              }
              if(is.null(argList[[4]])==TRUE){assign(nam, files[intersect(which(files$Supercode1==Supercode1), grep(SubsecID,files$SubsecID)),])
              }
            }
            else{assign(nam, files[intersect(intersect(which(files$Supercode1==Supercode1), grep(SubsecID,files$SubsecID)), intersect(agrep(Description, files$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(files$Description)-nchar(Description))<2))),])
            }
          }
          if(length(as.numeric(argList[[3]]))==0){
            if(length(as.numeric(argList[[2]]))==0|is.null(argList[[4]])==TRUE){
              if(is.null(argList[[4]])==TRUE){assign(nam, files[intersect(which(files$Supercode1==Supercode1), which(files$Supercode2==Supercode2)),])
              }
              if(length(as.numeric(argList[[2]]))==0){assign(nam, files[intersect(which(files$Supercode1==Supercode1), intersect(agrep(Description, files$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(files$Description)-nchar(Description))<2))),])
              }
            }
            else{assign(nam, files[intersect(intersect(which(files$Supercode1==Supercode1), which(files$Supercode2==Supercode2)), intersect(agrep(Description, files$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(files$Description)-nchar(Description))<2))),])
            }
          }
          if(is.null(argList[[4]])==TRUE){
            if(length(as.numeric(argList[[3]]))==0|length(as.numeric(argList[[2]]))==0){
              if(length(as.numeric(argList[[2]]))==0){assign(nam, files[intersect(which(files$Supercode1==Supercode1), grep(SubsecID,files$SubsecID)),])
              }
              if(length(as.numeric(argList[[3]]))==0){assign(nam, files[intersect(which(files$Supercode1==Supercode1), which(files$Supercode2==Supercode2)),])
              }
            }
            else{
              assign(nam, files[intersect(intersect(which(files$Supercode1==Supercode1), which(files$Supercode2==Supercode2)), grep(SubsecID,files$SubsecID)),])
            }
          }
        }
        else{assign(nam, files[intersect(intersect(which(files$Supercode1==Supercode1), which(files$Supercode2==Supercode2)), intersect(grep(SubsecID,files$SubsecID), intersect(agrep(Description, files$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(files$Description)-nchar(Description))<2)))),])
        }
        
       matcher[[index]]=get(nam)
      }
      else{files <- get(paste(substr(basefile,1,6),t,sep = ""))
      nam <- paste("J", t, sep = "")
      if(length(as.numeric(argList[[2]]))==0|length(as.numeric(argList[[3]]))==0|is.null(argList[[4]])==TRUE|is.null(argList[[2]])&length(as.numeric(argList[[3]]))==0|is.null(argList[[2]])&is.null(argList[[4]])==TRUE|is.null(argList[[3]])&is.null(argList[[4]])==TRUE){
        if(length(as.numeric(argList[[2]]))==0){
          if(length(as.numeric(argList[[3]]))==0|is.null(argList[[4]])==TRUE){
            if(length(as.numeric(argList[[3]]))==0){assign(nam, files[intersect(which(files$Supercode1==Supercode1), intersect(agrep(Description, files$Description, max=list(cost=1, all=1), ignore.case=TRUE), which(abs(nchar(files$Description)-nchar(Description))<2))),])
            }
            if(is.null(argList[[4]])==TRUE){assign(nam, files[intersect(which(files$Supercode1==Supercode1), grep(SubsecID,files$SubsecID)),])
            }
          }
          else{assign(nam, files[intersect(intersect(which(files$Supercode1==Supercode1), grep(SubsecID,files$SubsecID)), intersect(agrep(Description, files$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(files$Description)-nchar(Description))<2))),])
          }
        }
        if(length(as.numeric(argList[[3]]))==0){
          if(length(as.numeric(argList[[2]]))==0|is.null(argList[[4]])==TRUE){
            if(is.null(argList[[4]])==TRUE){assign(nam, files[intersect(which(files$Supercode1==Supercode1), which(files$Supercode2==Supercode2)),])
            }
            if(length(as.numeric(argList[[2]]))==0){assign(nam, files[intersect(which(files$Supercode1==Supercode1), intersect(agrep(Description, files$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(files$Description)-nchar(Description))<2))),])
            }
          }
          else{assign(nam, files[intersect(intersect(which(files$Supercode1==Supercode1), which(files$Supercode2==Supercode2)), intersect(agrep(Description, files$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(files$Description)-nchar(Description))<2))),])
          }
        }
        if(is.null(argList[[4]])==TRUE){
          if(length(as.numeric(argList[[3]]))==0|length(as.numeric(argList[[2]]))==0){
            if(length(as.numeric(argList[[2]]))==0){assign(nam, files[intersect(which(files$Supercode1==Supercode1), grep(SubsecID,files$SubsecID)),])
            }
            if(length(as.numeric(argList[[3]]))==0){assign(nam, files[intersect(which(files$Supercode1==Supercode1), which(files$Supercode2==Supercode2)),])
            }
          }
          else{
            assign(nam, files[intersect(intersect(which(files$Supercode1==Supercode1), which(files$Supercode2==Supercode2)), grep(SubsecID,files$SubsecID)),])
          }
        }
      }
      else{assign(nam, files[intersect(intersect(which(files$Supercode1==Supercode1), which(files$Supercode2==Supercode2)), intersect(grep(SubsecID,files$SubsecID), intersect(agrep(Description, files$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(files$Description)-nchar(Description))<2)))),])
      }
      matcher[[index]]=get(nam)
      }
    }
  }
  
  else{# finding value from odd budget given revised 
    if((as.numeric(substr(basefile,7,8)) %% 2)==0){
    digits = floor(log10(as.numeric(substr(basefile,7,8)))) + 1
    files_rev <- get(basefile)
    rev <- files_rev[intersect(intersect(which(files_rev$Supercode1==Supercode1), intersect(grep(SubsecID,files_rev$SubsecID), intersect(agrep(Description, files_rev$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(files_rev$Description)-nchar(Description))<2)))),which(files_rev[,5]==original)),]
    matcher[[1]]=rev
    if (digits==1){files <- get(paste(substr(basefile,1,6),as.numeric(substr(basefile,7,8))-1,sep = "0"))
    }
    else{files <- get(paste(substr(basefile,1,6),as.numeric(substr(basefile,7,8))-1,sep = ""))
    }
    orig <- files[intersect(intersect(which(files$Supercode1==Supercode1), intersect(grep(SubsecID,files$SubsecID), intersect(agrep(Description, files$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(files$Description)-nchar(Description))<2)))),which(files[,6]==original)),]
    matcher[[2]]=orig
  }
    else{
      digits = floor(log10(as.numeric(substr(basefile,7,8)))) + 1
      files <- get(basefile)
      orig <- files[intersect(intersect(which(files$Supercode1==Supercode1), intersect(grep(SubsecID,files$SubsecID), intersect(agrep(Description, files$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(files$Description)-nchar(Description))<2)))),which(files[,6]==original)),]
      matcher[[1]]=orig
      if (digits==1){files_rev <- get(paste(substr(basefile,1,6),as.numeric(substr(basefile,7,8))+1,sep = "0"))
      }
      else{files_rev <- get(paste(substr(basefile,1,6),as.numeric(substr(basefile,7,8))+1,sep = ""))
      }
      rev <- files_rev[intersect(intersect(which(files_rev$Supercode1==Supercode1), intersect(grep(SubsecID,files_rev$SubsecID), intersect(agrep(Description, files_rev$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(files_rev$Description)-nchar(Description))<2)))),which(files_rev[,5]==original)),]
      matcher[[2]]=rev
    }
  }
  return(matcher)
}

# Some examples of syntax:
#Match <- matcher("vol62003", 84210, 0002,SubsecID=NULL, "SOCIAL SEC CONTRIB-APPRO",2,original=NULL)
#Match <- matcher("vol62003", vol62004[i,1], vol62004[i,2],vol62004[i,3], vol62004[i,4], 1, vol62004[i,5])

#---------------------------------
# conveniently store output from matcher in dataframes if operation==2
for (t in seq(3,11,by = 2)){
  index=(t+which(seq(3,11,by = 2)==t)-1)/3
  if(t<10){nam <- paste("Match", t, sep = "0")
  assign(nam, Match[[index]])}
  else{nam <- paste("Match", t, sep = "")
  assign(nam, Match[[index]])}
}
#---------------------------------


# MAPPING
# operation: 1 for gluing revised budgets with orignal ones, 2 to form a time series panel
mapping <- function(basefile, operation){
  # pull the data and create empty matrices to fill
  basedata <- get(basefile)
  base_t <- as.numeric(substr(colnames(basedata)[5],4,5))
  base_index <- (base_t+which(seq(3,11,by = 2)==base_t)-1)/3
  if ((as.numeric(substr(basefile,7,8)) %% 2) == 0){
    base_index1 <- 2
    base_index2 <- 1
  }else{base_index1 <- 1
  base_index2 <- 2}
  cols=length(basedata[,5:length(basedata[1,])])
  test2 <- matrix(,nrow=length(basedata[,1]),ncol=cols*length(seq(3,11,by = 2)))
  test2 <- cbind(basedata[,1:4],test2)
  test3 <- matrix(,nrow=length(basedata[,1]),ncol=5)
  weirdos1 <- matrix(0,length(basedata[,1]),5)
  test3 <- cbind(basedata[,1:4],test3)
  weirdos <- matrix(0,length(basedata[,1]),length(seq(3,11,by = 2)))
  
  
  if (operation==2){# creating a time series panel
    for (i in 1:length(basedata[,1])){
      DMatch <- matcher(basefile, basedata$Supercode1[i], basedata$Supercode2[i], basedata$SubsecID[i], basedata$Description[i], 2, original=NULL)
      for (t in seq(3,11,by = 2)){
        index <- (t+which(seq(3,11,by = 2)==t)-1)/3
        l <- length((DMatch[[index]])[,5])
        # if we have several matches for one identifier, introduce counting principle
        if (!(l == 0)){
          if(l > 1){
            weirdos[i,index] <- l 
            counter <- l
            repeat{
              ialt <-which(row.names(DMatch[[base_index]][counter,])==row.names(basedata))
              if(t<10){nam1 <- paste("DMatch1", t, sep = "0")
              assign(nam1, (DMatch[[index]])[counter,5:length(DMatch[[index]])])}
              else{nam1 <- paste("DMatch1", t, sep = "")
              assign(nam1, (DMatch[[index]])[counter,5:length(DMatch[[index]])])}
              counter=counter-1
              outputw <- get(nam1)
              test2[ialt,(5+cols*(index-1)):(5+cols*(index-1)+length(outputw)-1)] <- outputw
              colnames(test2)[(5+cols*(index-1)):(5+cols*(index-1)+length(outputw)-1)] <- colnames(get(nam1))
              if (counter==0) {break} }
          }
          else{
            if(t<10){nam <- paste("DMatch", t, sep = "0")
            assign(nam, (DMatch[[index]])[1,5:length(DMatch[[index]])])}
            else{nam <- paste("DMatch", t, sep = "")
            assign(nam, (DMatch[[index]])[1,5:length(DMatch[[index]])])}
            output <- get(nam)
            test2[i,(5+cols*(index-1)):(5+cols*(index-1)+length(output)-1)] <- output
            colnames(test2)[(5+cols*(index-1)):(5+cols*(index-1)+length(output)-1)] <- colnames(get(nam))
          }
        }
      }
    }
    mapping <- test2
  }
  else{
    for (i in 1:length(basedata[,1])){# to combine revised and original budgets
      if (base_index1==2){
        DMatch <- matcher(basefile, basedata[i,1],basedata[i,2],basedata[i,3],basedata[i,4],1, basedata[i,5])
      }
      else{
        DMatch <- matcher(basefile, basedata[i,1],basedata[i,2],basedata[i,3],basedata[i,4],1, basedata[i,6])
      }
      l1 <- length((DMatch[[1]])[,5])
      l2 <- length((DMatch[[2]])[,5])
      if (!(l1==0)){
        if(l1 > 1){
          weirdos1[i,1] <- l1 
          counter1 <- l1
          repeat{
            ialt1 <-which(row.names(DMatch[[1]][counter1,])==row.names(basedata))
            nam1=(DMatch[[1]])[counter1,5:length(DMatch[[1]])]
            counter1=counter1-1
            outputw <- nam1
            test3[ialt1,(3+2*base_index1):(3+2*base_index1+length(outputw)-1)] <- outputw
            colnames(test3)[(3+2*base_index1):(3+2*base_index1+length(outputw)-1)] <- colnames(nam1)
            if (counter1==0) {break} }
        }
        else{
          DMatch1=DMatch[[1]][,5:length(DMatch[[1]])]
          test3[i,(3+2*base_index1):(3+2*base_index1+length(DMatch1)-1)] <- DMatch1
          colnames(test3)[(3+2*base_index1):(3+2*base_index1+length(DMatch1)-1)] <- colnames(DMatch1)
        }
      }
      if(!(l2==0)){
        if(l2 > 1){
          weirdos1[i,2] <- l2 
          counter2 <- l2
          repeat{ialt2 <-which(row.names(DMatch[[base_index2]][counter2,])==row.names(basedata))
            nam2=(DMatch[[2]])[counter2,5:length(DMatch[[2]])]
            counter2=counter2-1
            outputw <- nam2
            test3[ialt2,(3+2*base_index2):(3+2*base_index2+length(outputw)-1)] <- outputw
            colnames(test3)[(3+2*base_index2):(3+2*base_index2+length(outputw)-1)] <- colnames(outputw)
            if (counter2==0) {break} }
        }
        else{
          DMatch2=DMatch[[2]][,5:length(DMatch[[2]])]
          test3[i,(3+2*base_index2):(3+2*base_index2+length(DMatch2)-1)] <- DMatch2
          colnames(test3)[(3+2*base_index2):(3+2*base_index2+length(DMatch2)-1)] <- colnames(DMatch2)
        }
      }
    }
    mapping <-test3
  }
  return(mapping)
}

#example of combining revised and original budgets
vol12003comb <- mapping("vol12004",1)
vol12005comb <- mapping("vol12006",1)
vol12007comb <- mapping("vol12008",1)
vol12009comb <- mapping("vol12010",1)
vol12011comb <- mapping("vol12012",1)

vol22003comb <- mapping("vol22004",1)
vol22005comb <- mapping("vol22006",1)
vol22007comb <- mapping("vol22008",1)
vol22009comb <- mapping("vol22010",1)
vol22011comb <- mapping("vol22012",1)

vol32003comb <- mapping("vol32004",1)
vol32005comb <- mapping("vol32006",1)
vol32007comb <- mapping("vol32008",1)
vol32009comb <- mapping("vol32010",1)
vol32011comb <- mapping("vol32012",1)

vol42003comb <- mapping("vol42004",1)
vol42005comb <- mapping("vol42006",1)
vol42007comb <- mapping("vol42008",1)
vol42009comb <- mapping("vol42010",1)
vol42011comb <- mapping("vol42012",1)

vol52003comb <- mapping("vol52004",1)
vol52005comb <- mapping("vol52006",1)
vol52007comb <- mapping("vol52008",1)
vol52009comb <- mapping("vol52010",1)
vol52011comb <- mapping("vol52012",1)

vol62003comb <- mapping("vol62004",1)
vol62005comb <- mapping("vol62006",1)
vol62007comb <- mapping("vol62008",1)
vol62009comb <- mapping("vol62010",1)
vol62011comb <- mapping("vol62012",1)

# example of creating time series panels: final output for each volume is Map_finale(1-6)
Map03 <- mapping("vol62003",2)
Map05 <- mapping("vol62005",2)
Map07 <- mapping("vol62007",2)
Map09 <- mapping("vol62009",2)
Map11 <- mapping("vol62011",2)

Map <- rbind(Map03, Map05, Map07, Map09, Map11)
Map_finale <- Map[!duplicated(Map),]
Map_finale6 <- Map_finale

Map03vol1 <- mapping("vol12003")
Map05vol1 <- mapping("vol12005")
Map07vol1 <- mapping("vol12007")
Map09vol1 <- mapping("vol12009")
Map11vol1 <- mapping("vol12011")

Mapvol1 <- rbind(Map03vol1, Map05vol1, Map07vol1, Map09vol1, Map11vol1)
Map_finale <- Mapvol1[!duplicated(Mapvol1),]
Map_finale1 <- Map_finale

Map03vol2 <- mapping("vol22003")
Map05vol2 <- mapping("vol22005")
Map07vol2 <- mapping("vol22007")
Map09vol2 <- mapping("vol22009")
Map11vol2 <- mapping("vol22011")

Mapvol2 <- rbind(Map03vol2, Map05vol2, Map07vol2, Map09vol2, Map11vol2)
Map_finale <- Mapvol2[!duplicated(Mapvol2),]
Map_finale2 <- Map_finale

Map03vol3 <- mapping("vol32003")
Map05vol3 <- mapping("vol32005")
Map07vol3 <- mapping("vol32007")
Map09vol3 <- mapping("vol32009")
Map11vol3 <- mapping("vol32011")

Mapvol3 <- rbind(Map03vol3, Map05vol3, Map07vol3, Map09vol3, Map11vol3)
Map_finale <- Mapvol3[!duplicated(Mapvol3),]
Map_finale3 <- Map_finale

Map03vol4 <- mapping("vol42003")
Map05vol4 <- mapping("vol42005")
Map07vol4 <- mapping("vol42007")
Map09vol4 <- mapping("vol42009")
Map11vol4 <- mapping("vol42011")

Mapvol4 <- rbind(Map03vol4, Map05vol4, Map07vol4, Map09vol4, Map11vol4)
Map_finale <- Mapvol4[!duplicated(Mapvol4),]
Map_finale4 <- Map_finale

Map03vol5 <- mapping("vol52003")
Map05vol5 <- mapping("vol52005")
Map07vol5 <- mapping("vol52007")
Map09vol5 <- mapping("vol52009")
Map11vol5 <- mapping("vol52011")

Mapvol5 <- rbind(Map03vol5, Map05vol5, Map07vol5, Map09vol5, Map11vol5)
Map_finale <- Mapvol5[!duplicated(Mapvol5),]
Map_finale5 <- Map_finale
