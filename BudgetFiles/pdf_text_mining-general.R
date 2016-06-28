rm(list = ls())
cat("\014") 

# Script to install and run tabulizer to extract tables from PDFs 

# packages necessary to install and run tabulizer. I had to install a different version of Java, which you may have to do as well. 
require(devtools)
require(rJava)
require(tabulizer)

# if(!require("ghit")){
#    install.packages("ghit")
# }

# on 64-bit Windows
# ghit::install_github(c("leeper/tabulizerjars", "leeper/tabulizer"), INSTALL_opts = "--no-multiarch")
# elsewhere

# tell it where the PDF is
#file <- "C:/Users/admin/Desktop/DUKE Data+/2003_5/vol6.pdf"

generator <- function(file){
  a <- get_n_pages(file=file)
  
  digits = floor(log10(as.numeric(substr(gsub("[^0-9]", "", file),3,4)))) + 1
  if (digits==1){
    year1 <- paste(substr(gsub("[^0-9]", "", file),1,4), as.numeric(substr(gsub("[^0-9]", "", file),3,4))+1, sep = "0")
    year1 <- paste("y",year1, sep = "")
    year2 <- paste(substr(gsub("[^0-9]", "", file),1,3), as.numeric(substr(gsub("[^0-9]", "", file),3,4))+1, sep = "")
    year2 <- paste(year2, as.numeric(substr(gsub("[^0-9]", "", file),3,4))+2, sep = "0")
    year2 <- paste("y",year2, sep = "")
  } else {
    year1 <- paste(substr(gsub("[^0-9]", "", file),1,4), as.numeric(substr(gsub("[^0-9]", "", file),3,4))+1, sep = "")
    year1 <- paste("y",year1, sep = "")
    year2 <- paste(substr(gsub("[^0-9]", "", file),1,2), as.numeric(substr(gsub("[^0-9]", "", file),3,4))+1, sep = "")
    year2 <- paste(year2, as.numeric(substr(gsub("[^0-9]", "", file),3,4))+2, sep = "")
    year2 <- paste("y",year2, sep = "")
  }
  
  #-----------------------------------EVEN
  if((as.numeric(substr(gsub("[^0-9]", "", file),3,4)) %% 2) == 0){
    # extract tables from the PDF
    Genout1 <- as.data.frame(extract_tables(file, pages = 1, guess = FALSE, method = "data.frame", columns = list(c(250, 345, 420)), stringsAsFactors=FALSE))
    for (i in 2:a) {
      try({
        Out <- as.data.frame(extract_tables(file, pages = i, guess = FALSE, method = "data.frame", columns = list(c(250, 345, 420)), stringsAsFactors=FALSE))
        names(Out) <- names(Genout1)
        Genout1 <- rbind(Genout1,Out)
      })
    }
    
    Genout1 <- as.data.frame(Genout1[!(as.data.frame(substr(Genout1[,1],1,1)) == "-"), ], stringsAsFactors=FALSE)
    
    Genout2=Genout1
    Gen2 <- as.data.frame(gsub("TOTAL", "00001 TOTAL", Genout2[,1]), stringsAsFactors=FALSE)
    Gen3 <- cbind(Gen2, Genout2[,2:4])
    
    Gen2 <- as.data.frame(gsub("NET", "00002 NET", Gen3[,1]), stringsAsFactors=FALSE)
    Gen3 <- cbind(Gen2, Gen3[,2:4])
    
    Gen2 <- as.data.frame(gsub("HIGHWAY FUND APPROPRIATION", "00003 HIGHWAY FUND APPROPRIATION", Gen3[,1]), stringsAsFactors=FALSE)
    Gen3 <- cbind(Gen2, Gen3[,2:4])
    
    Gen2 <- as.data.frame(gsub("HIGHWAY TRUST FUND APPROPRTN", "00004 HIGHWAY TRUST FUND APPROPRTN", Gen3[,1]), stringsAsFactors=FALSE)
    Gen3 <- cbind(Gen2, Gen3[,2:4])
    
    Gen2 <- as.data.frame(gsub("CHANGE IN FUND BALANCE", "00005 CHANGE IN FUND BALANCE", Gen3[,1]),stringsAsFactors=FALSE)
    Gen3 <- cbind(Gen2, Gen3[,2:4])
    
    Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,3],1,4)) == "PAGE"), ],stringsAsFactors=FALSE)
    Gen3 <- as.data.frame(Gen3[!(substr(Gen3[,1],1,3) %in% c("APP", "BUD", "REQ", "DES", "EST", "POS", "SUM")), ],stringsAsFactors=FALSE)
    Gen3 <- as.data.frame(Gen3[!(substr(Gen3[,3],1,3) %in% "AWG"), ],stringsAsFactors=FALSE)
    
    Gen2 <- as.data.frame(gsub(",","",Gen3[,2]),stringsAsFactors=FALSE)
    Gen42 <- as.data.frame(gsub(",","",Gen3[,3]),stringsAsFactors=FALSE)
    Gen43 <- as.data.frame(gsub(",","",Gen3[,4]),stringsAsFactors=FALSE)
    Gen3 <- cbind(Gen3[,1], Gen2, Gen42, Gen43)
    Gen3 <- Gen3[!apply(Gen3 == "", 1, all),]
    Gen3 <- Gen3
    
    Gen3 <- cbind(as.data.frame(gsub("  "," ",Gen3[,1]), stringsAsFactors=FALSE),Gen3[,2:4])
    
    is.number <- function(x) grepl("[[:digit:]]", x)
    
    L=rep(0,length(Gen3[,1]))
    for (j in 1:12){
      k <- as.data.frame(substr(Gen3[,1],j,j),stringsAsFactors=FALSE)
      for (i in 1:length(k[,1])){
        if(is.number(k[i,1])==TRUE){L[i]=j}
      }}
    
    is.space <- function(x) grepl(" ", x)
    numbersnew=rep(0,length(L))
    lettersnew=c()
    h=matrix(0,length(L),3)
    
    for (i in 1:length(L)){
      numbersnew[i]=as.data.frame(substr(Gen3[i,1],1,L[i]),stringsAsFactors=FALSE)
      
      lettersnew[i] <- as.data.frame(substr(Gen3[i,1],L[i]+1,100),stringsAsFactors=FALSE)
      for (j in 1:3){
        if(is.space(substr(lettersnew[i],j,j))==TRUE){
          h[i,j]=1
        }
      }
      lettersnew[i] <- as.data.frame(substr(Gen3[i,1],L[i]+1+sum(h[i,]),100),stringsAsFactors=FALSE)
    }
    
    w <- cbind(numbersnew,lettersnew)
    Genfin <- cbind(w, Gen3[,2:4])
    
    Y <- as.data.frame(gsub("[^0-9]", "", Genfin[,1]),stringsAsFactors=FALSE)
    Genfin <- cbind (Y, Genfin[,2:length(Genfin)])
    
    M1 <- intersect(which(as.numeric(Genfin[,3])<0),which(!Genfin[,3]==""))
    M2 <- intersect(which(as.numeric(Genfin[,4])<0),which(!Genfin[,4]==""))
    M3 <- intersect(which(as.numeric(Genfin[,5])<0),which(!Genfin[,5]==""))
    R1 <- as.data.frame(gsub("[^0-9]", "", Genfin[,3]),stringsAsFactors=FALSE)
    R2 <- as.data.frame(gsub("[^0-9]", "", Genfin[,4]),stringsAsFactors=FALSE)
    R3 <- as.data.frame(gsub("[^0-9]", "", Genfin[,5]),stringsAsFactors=FALSE)
    R1[M1,] <- paste("-",R1[M1,],sep = "")
    R2[M2,] <- paste("-",R2[M2,],sep = "")
    R3[M3,] <- paste("-",R3[M3,],sep = "")
    
    for (i in 2:10){
      D1=rep(0,length(Genfin[,3]))
      D2=rep(0,length(Genfin[,3]))
      D3=rep(0,length(Genfin[,3]))
      D1 [which(substr(Genfin[,3],i,i)==".")] <- which(substr(Genfin[,3],i,i)==".")
      D1 <- D1[!(D1==0)]
      D2 [which(substr(Genfin[,4],i,i)==".")] <- which(substr(Genfin[,4],i,i)==".")
      D2 <- D2[!(D2==0)]
      D3 [which(substr(Genfin[,5],i,i)==".")] <- which(substr(Genfin[,5],i,i)==".")
      D3 <- D3[!(D3==0)]
      R1[D1,] <- paste(substr(R1[D1,],1,i-1),substr(R1[D1,],i,15),sep = ".")
      R2[D2,] <- paste(substr(R2[D2,],1,i-1),substr(R2[D2,],i,15),sep = ".")
      R3[D3,] <- paste(substr(R3[D3,],1,i-1),substr(R3[D3,],i,15),sep = ".")
    }
    
    Genfin <- cbind(Genfin[,1:2],R1,R2,R3)
    J <- !((as.data.frame(substr(Genfin[,1],1,1)) == "") & (as.data.frame(substr(Genfin[,3],1,1) == "")))
    Genfin <- as.data.frame(Genfin[J, ], stringsAsFactors=FALSE)
    
    colnames(Genfin) <- c("SubsecID","Description",paste(year1,"original",sep = ""),"Revision",paste(year1,"revised",sep = ""))
    Genfin$SubsecID  <- as.numeric(Genfin$SubsecID)
    Genfin[,3]   <- type.convert(Genfin[,3], numerals="warn.loss");
    Genfin[,4]   <- type.convert(Genfin[,4], numerals="warn.loss");
    Genfin[,5]   <- type.convert(Genfin[,5], numerals="warn.loss");
  }
  
  #-------------------------------------ODD
  else{
   Genout1 <- as.data.frame(extract_tables(file, pages = 1, guess = FALSE, method = "data.frame", columns = list(c(320, 420)), stringsAsFactors=FALSE))
    for (i in 2:a) {
      try({
        Out <- as.data.frame(extract_tables(file, pages = i, guess = FALSE, method = "data.frame", columns = list(c(320, 420)), stringsAsFactors=FALSE))
        names(Out) <- names(Genout1)
        Genout1 <- rbind(Genout1,Out)
      })
    }
    
    
    Genout1 <- as.data.frame(Genout1[!(as.data.frame(substr(Genout1[,1],1,1)) == "-"), ], stringsAsFactors=FALSE)
    
    Genout2=Genout1
    Gen2 <- as.data.frame(gsub("TOTAL", "00001 TOTAL", Genout2[,1]), stringsAsFactors=FALSE)
    Gen3 <- cbind(Gen2, Genout2[,2:3])
    
    Gen2 <- as.data.frame(gsub("NET", "00002 NET", Gen3[,1]), stringsAsFactors=FALSE)
    Gen3 <- cbind(Gen2, Gen3[,2:3])
    
    Gen2 <- as.data.frame(gsub("HIGHWAY FUND APPROPRIATION", "00003 HIGHWAY FUND APPROPRIATION", Gen3[,1]), stringsAsFactors=FALSE)
    Gen3 <- cbind(Gen2, Gen3[,2:3])
    
    Gen2 <- as.data.frame(gsub("HIGHWAY TRUST FUND APPROPRTN", "00004 HIGHWAY TRUST FUND APPROPRTN", Gen3[,1]), stringsAsFactors=FALSE)
    Gen3 <- cbind(Gen2, Gen3[,2:3])
    
    Gen2 <- as.data.frame(gsub("CHANGE IN FUND BALANCE", "00005 CHANGE IN FUND BALANCE", Gen3[,1]),stringsAsFactors=FALSE)
    Gen3 <- cbind(Gen2, Gen3[,2:3])
    
    Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,3],1,4)) == "PAGE"), ],stringsAsFactors=FALSE)
    Gen3 <- as.data.frame(Gen3[!(substr(Gen3[,1],1,3) %in% c("APP", "BUD", "REQ", "DES", "EST", "POS", "SUM")), ],stringsAsFactors=FALSE)
    Gen3 <- as.data.frame(Gen3[!(substr(Gen3[,3],1,3) %in% "AWG"), ],stringsAsFactors=FALSE)
    
    Gen2 <- as.data.frame(gsub(",","",Gen3[,2]),stringsAsFactors=FALSE)
    Gen42 <- as.data.frame(gsub(",","",Gen3[,3]),stringsAsFactors=FALSE)
    Gen3 <- cbind(Gen3[,1], Gen2, Gen42)
    Gen3 <- Gen3[!apply(Gen3 == "", 1, all),]
    Gen3 <- Gen3
    
    Gen3 <- cbind(as.data.frame(gsub("  "," ",Gen3[,1]), stringsAsFactors=FALSE),Gen3[,2:3])
    
    is.number <- function(x) grepl("[[:digit:]]", x)
    
    L=rep(0,length(Gen3[,1]))
    for (j in 1:12){
      k <- as.data.frame(substr(Gen3[,1],j,j),stringsAsFactors=FALSE)
      for (i in 1:length(k[,1])){
        if(is.number(k[i,1])==TRUE){L[i]=j}
      }}
    
    is.space <- function(x) grepl(" ", x)
    numbersnew=rep(0,length(L))
    lettersnew=c()
    h=matrix(0,length(L),3)
    
    for (i in 1:length(L)){
      numbersnew[i]=as.data.frame(substr(Gen3[i,1],1,L[i]),stringsAsFactors=FALSE)
      
      lettersnew[i] <- as.data.frame(substr(Gen3[i,1],L[i]+1,100),stringsAsFactors=FALSE)
      for (j in 1:3){
        if(is.space(substr(lettersnew[i],j,j))==TRUE){
          h[i,j]=1
        }
      }
      lettersnew[i] <- as.data.frame(substr(Gen3[i,1],L[i]+1+sum(h[i,]),100),stringsAsFactors=FALSE)
    }
    
    w <- cbind(numbersnew,lettersnew)
    Genfin <- cbind(w, Gen3[,2:3])
    Genfin <- Genfin[!(Genfin[,1]==""),]
    Y <- as.data.frame(gsub("[^0-9]", "", Genfin[,1]),stringsAsFactors=FALSE)
    Genfin <- cbind (Y, Genfin[,2:length(Genfin)])
    
    M1 <- intersect(which(as.numeric(Genfin[,3])<0),which(!Genfin[,3]==""))
    M2 <- intersect(which(as.numeric(Genfin[,4])<0),which(!Genfin[,4]==""))
    R1 <- as.data.frame(gsub("[^0-9]", "", Genfin[,3]),stringsAsFactors=FALSE)
    R2 <- as.data.frame(gsub("[^0-9]", "", Genfin[,4]),stringsAsFactors=FALSE)
    R1[M1,] <- paste("-",R1[M1,],sep = "")
    R2[M2,] <- paste("-",R2[M2,],sep = "")
   
    for (i in 2:10){
    D1=rep(0,length(Genfin[,3]))
    D2=rep(0,length(Genfin[,3]))
    D1 [which(substr(Genfin[,3],i,i)==".")] <- which(substr(Genfin[,3],i,i)==".")
    D1 <- D1[!(D1==0)]
    D2 [which(substr(Genfin[,4],i,i)==".")] <- which(substr(Genfin[,4],i,i)==".")
    D2 <- D2[!(D2==0)]
    R1[D1,] <- paste(substr(R1[D1,],1,i-1),substr(R1[D1,],i,15),sep = ".")
    R2[D2,] <- paste(substr(R2[D2,],1,i-1),substr(R2[D2,],i,15),sep = ".")
    }
    
    Genfin <- cbind(Genfin[,1:2],R1,R2)
    J <- !((as.data.frame(substr(Genfin[,1],1,1)) == "") & (as.data.frame(substr(Genfin[,3],1,1) == "")))
    Genfin <- as.data.frame(Genfin[J, ], stringsAsFactors=FALSE)
    
    
    colnames(Genfin) <- c("SubsecID","Description",year1,year2)
    Genfin$SubsecID  <- as.numeric(Genfin$SubsecID)
    Genfin[,3]   <- type.convert(Genfin[,3], numerals="warn.loss");
    Genfin[,4]   <- type.convert(Genfin[,4], numerals="warn.loss");
  }
  
  #indices with 5 digit indices
  M <- which(is.na(Genfin[,3]))
  N <- intersect(which(!substr(Genfin[,1],5,5) %in% ""),M)
  
  #indices for 2nd NA
  T <- setdiff(M,N)
  
  n <- length(Genfin[,1])
  O1 <- rep(0,n)
  O2 <- rep(0,n)
  
  
  for (i in 1:length(N)){
    O1[N[i]:n]=Genfin[N[i],1]
  }
  O1 <- O1[-which(is.na(Genfin[,3]))]
  
  for (i in 1:length(T)){
    O2[T[i]:n]=Genfin[T[i],1]
  }
  O2 <- O2[-which(is.na(Genfin[,3]))]
  
  O <- cbind(O1,O2)
  Genfin <- as.data.frame(Genfin[-(which(is.na(Genfin[,3]))), ])
  colnames(O) <- c("Supercode1","Supercode2")
  Genfin <- cbind(O,Genfin)
  
  
 tot1 <- which(substr(Genfin$Description,1,5) %in% "TOTAL")
 net2 <- which(substr(Genfin$Description,1,3) %in% "NET")
 hfa3 <- which(Genfin$Description=="HIGHWAY FUND APPROPRIATION")
 htf4 <- which(Genfin$Description=="HIGHWAY TRUST FUND APPROPRTN")
 cfb5 <- which(Genfin$Description=="CHANGE IN FUND BALANCE")

 C3 <- Genfin$SubsecID
 C3[tot1] <- paste(Genfin$Supercode1[tot1],Genfin$SubsecID[tot1],sep = "")
 C3[net2] <- paste(Genfin$Supercode1[net2],Genfin$SubsecID[net2],sep = "")
 C3[hfa3] <- paste(Genfin$Supercode1[hfa3],Genfin$SubsecID[hfa3],sep = "")
 C3[htf4] <- paste(Genfin$Supercode1[htf4],Genfin$SubsecID[htf4],sep = "")
 C3[cfb5] <- paste(Genfin$Supercode1[cfb5],Genfin$SubsecID[cfb5],sep = "")
 
 Genfin$SubsecID <- C3
 Genfin <- Genfin[!duplicated(Genfin),]
 Genfin[which(Genfin$Description==""),4] <- "ESTIMATED RECEIPTS MISSING"
 return(Genfin)
}

#for now the function ends here

vol12003 <- generator("C:/Users/Tom/Desktop/Data+/2003_5/vol1.pdf")
vol12005 <- generator("C:/Users/Tom/Desktop/Data+/2005_7/vol1.pdf")
vol12007 <- generator("C:/Users/Tom/Desktop/Data+/2007_9/vol1.pdf")
vol12009 <- generator("C:/Users/Tom/Desktop/Data+/2009_11/vol1.pdf")
vol12011 <- generator("C:/Users/Tom/Desktop/Data+/2011_13/vol1.pdf")

vol22003 <- generator("C:/Users/Tom/Desktop/Data+/2003_5/vol2.pdf")
vol22005 <- generator("C:/Users/Tom/Desktop/Data+/2005_7/vol2.pdf")
vol22007 <- generator("C:/Users/Tom/Desktop/Data+/2007_9/vol2.pdf")
vol22009 <- generator("C:/Users/Tom/Desktop/Data+/2009_11/vol2.pdf")
vol22011 <- generator("C:/Users/Tom/Desktop/Data+/2011_13/vol2.pdf")

vol32003 <- generator("C:/Users/Tom/Desktop/Data+/2003_5/vol3.pdf")
vol32005 <- generator("C:/Users/Tom/Desktop/Data+/2005_7/vol3.pdf")
vol32007 <- generator("C:/Users/Tom/Desktop/Data+/2007_9/vol3.pdf")
vol32009 <- generator("C:/Users/Tom/Desktop/Data+/2009_11/vol3.pdf")
vol32011 <- generator("C:/Users/Tom/Desktop/Data+/2011_13/vol3.pdf")

vol42003 <- generator("C:/Users/Tom/Desktop/Data+/2003_5/vol4.pdf")
vol42005 <- generator("C:/Users/Tom/Desktop/Data+/2005_7/vol4.pdf")
vol42007 <- generator("C:/Users/Tom/Desktop/Data+/2007_9/vol4.pdf")
vol42009 <- generator("C:/Users/Tom/Desktop/Data+/2009_11/vol4.pdf")
vol42011 <- generator("C:/Users/Tom/Desktop/Data+/2011_13/vol4.pdf")

vol52003 <- generator("C:/Users/Tom/Desktop/Data+/2003_5/vol5.pdf")
vol52005 <- generator("C:/Users/Tom/Desktop/Data+/2005_7/vol5.pdf")
vol52007 <- generator("C:/Users/Tom/Desktop/Data+/2007_9/vol5.pdf")
vol52009 <- generator("C:/Users/Tom/Desktop/Data+/2009_11/vol5.pdf")
vol52011 <- generator("C:/Users/Tom/Desktop/Data+/2011_13/vol5.pdf")

vol62003 <- generator("C:/Users/Tom/Desktop/Data+/2003_5/vol6.pdf")
vol62005 <- generator("C:/Users/Tom/Desktop/Data+/2005_7/vol6.pdf")
vol62007 <- generator("C:/Users/Tom/Desktop/Data+/2007_9/vol6.pdf")
vol62009 <- generator("C:/Users/Tom/Desktop/Data+/2009_11/vol6.pdf")
vol62011 <- generator("C:/Users/Tom/Desktop/Data+/2011_13/vol6.pdf")


# matching function: give me supercode1 and some text from description column,
# I give you matrices with encounters in all data frames


# Supcode1 is the only required variable to run the code (others are optional), need to specify at least 2 variables
matcher <- function(basefile,Supercode1,Supercode2,SubsecID,Description,operation){
  
  matcher <- list()
  Supercode1 <- as.numeric(Supercode1)
  Supercode2 <- as.numeric(Supercode2)
  SubsecID <- as.numeric(SubsecID)
  
  if(!(length(as.numeric(SubsecID))==0)){
    if (nchar(SubsecID)>4){
      SubsecID <- substr(SubsecID,3,6)
    }
  }
  
  argList <- list(Supercode1,Supercode2,SubsecID,Description)
  if (operation==2){
    for (t in seq(3,11,by = 2)){
      index=(t+which(seq(3,11,by = 2)==t)-1)/3
      if(t<10){
        files <- get(paste(substr(basefile,1,6),t,sep = "0"))
        nam <- paste("J", t, sep = "")
        #assign(nam, files[files$Supercode1==Supercode1 & grep(Description,files$Description),])
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
        
        #if( union(!is.null(Supercode1),!is.null(SubsecID),!is.null(Description))){assign(nam, files[intersect(which(files$Supercode1==Supercode1), grep(SubsecID,files$SubsecID),intersect(agrep(Description, files$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(files$Description)-nchar(Description))<2),])
        #}
        matcher[[index]]=get(nam)
      }
      else{files <- get(paste(substr(basefile,1,6),t,sep = ""))
      nam <- paste("J", t, sep = "")
      #assign(nam, files[files$Supercode1==Supercode1 & grep(Description,files$Description),])
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
  
  else{digits = floor(log10(as.numeric(substr(basefile,7,8)))) + 1
  files <- get(basefile)
  orig <- files[intersect(intersect(which(files$Supercode1==Supercode1), which(files$Supercode2==Supercode2)), intersect(grep(SubsecID,files$SubsecID), intersect(agrep(Description, files$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(files$Description)-nchar(Description))<2)))),]
  matcher[[1]]=orig
  if (digits==1){files_rev <- get(paste(substr(basefile,1,6),as.numeric(substr(basefile,7,8))+1,sep = "0"))
  #nam <- paste("J", t, sep = "")
  }
  else{files_rev <- get(paste(substr(basefile,1,6),as.numeric(substr(basefile,7,8))+1,sep = ""))
  #nam <- paste("J", t, sep = "")
  }
  rev <- files_rev[intersect(intersect(which(files_rev$Supercode1==Supercode1), which(files_rev$Supercode2==Supercode2)), intersect(grep(SubsecID,files_rev$SubsecID), intersect(agrep(Description, files_rev$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(files_rev$Description)-nchar(Description))<2)))),]
  matcher[[2]]=rev
  }
  return(matcher)
}


#-------------------NEW EXAMPLE!!!!
#Match <- matcher("vol62003", 84210, 54,842101, "total receipts",2)

##------------------OUTDATED!!!
## example
##U <- matcher(84210,"STATE AID")
##I <- U[[1]] #these are encounters in vol6 2003

##example with optional variables
##U2 <- matcher(84210,7828,SubsecID=NULL,Desc=NULL)
##I2 <- U2[[1]]
##------------------------------------------------

# JUST an idea used in further codes
## conveniently store output from matcher in dataframes
#for (t in seq(3,11,by = 2)){
  #index=(t+which(seq(3,11,by = 2)==t)-1)/3
  #if(t<10){nam <- paste("Match", t, sep = "0")
  #assign(nam, Match[[index]])}
 # else{nam <- paste("Match", t, sep = "")
 # assign(nam, Match[[index]])}
#}


#--------------------------------------------MERGING

# merge this stuff
#test <- vol62003
#for (t in seq(5,11,by = 2)){
  #index=(t+which(seq(3,11,by = 2)==t)-1)/3
 # if(t<10){nam <- paste("vol620", t, sep = "0")}
  
 # else{nam <- paste("vol620", t, sep = "")}
  
 # test3 <- get(nam)
  #test <- merge(test,test3,by=c("Supercode1", "Supercode2", "Description"),all=TRUE)
#}


#-------------------MAPPING
mapping <- function(basefile, operation){
  basedata <- get(basefile)
  base_t <- as.numeric(substr(colnames(basedata)[5],4,5))
  base_index <- (base_t+which(seq(3,11,by = 2)==base_t)-1)/3
  if((as.numeric(substr(basefile,7,8)) %% 2) == 0){
    base_index2 <- 2}
  else{base_index2 <- 1}
  #basefile <- rbind(dvol62003,dvol62005,dvol62007,dvol62009,dvol62011,setNames( rev(vol62003) , names( vol62003) ) )
  test2 <- matrix(,nrow=length(basedata[,1]),ncol=2*length(seq(3,11,by = 2)))
  test2 <- cbind(basedata[,1:4],test2)
  test3 <- matrix(,nrow=length(basedata[,1]),ncol=5)
  test3 <- cbind(basedata[,1:4],test3)
  weirdos <- matrix(0,length(basedata[,1]),length(seq(3,11,by = 2)))
  
  
  if (operation==2){
    for (i in 1:length(basedata[,1])){
      #DMatch <- do.call(matcher, as.list(cbind(basefile, basedata[i,1:4])))
      DMatch <- matcher(basefile, basedata$Supercode1[i], basedata$Supercode2[i], basedata$SubsecID[i], basedata$Description[i], 2)
      #o <- rep(0,1)
      #ow <- rep(0,1)
      for (t in seq(3,11,by = 2)){
        index <- (t+which(seq(3,11,by = 2)==t)-1)/3
        l <- length((DMatch[[index]])[,5])
        if (!(l == 0)){
          if(l > 1){
            weirdos[i,index] <- l 
            counter <- l
            repeat{#H<-do.call(matcher, as.list(b1))[[index]]
              ialt <-which(row.names(DMatch[[base_index]][counter,])==row.names(basedata))
              if(t<10){nam1 <- paste("DMatch1", t, sep = "0")
              assign(nam1, (DMatch[[index]])[counter,5:length(DMatch[[index]])])}
              else{nam1 <- paste("DMatch1", t, sep = "")
              assign(nam1, (DMatch[[index]])[counter,5:length(DMatch[[index]])])}
              #ow <- cbind(ow,get(nam1))
              counter=counter-1
              #outputw <- ow[-1]
              outputw <- get(nam1)
              test2[ialt,(3+2*index):(3+2*index+length(outputw)-1)] <- outputw
              if (counter==0) {break} }
          }
          else{
            if(t<10){nam <- paste("DMatch", t, sep = "0")
            assign(nam, (DMatch[[index]])[1,5:length(DMatch[[index]])])}
            else{nam <- paste("DMatch", t, sep = "")
            assign(nam, (DMatch[[index]])[1,5:length(DMatch[[index]])])}
            #o <- cbind(o,get(nam))
            output <- get(nam)
            test2[i,(3+2*index):(3+2*index+length(output)-1)] <- output
          }
        }
      }
    }
    mapping <- test2
  }
  else{
    for (i in 1:length(basedata[,1])){
      #operation
      #b <- cbind(basefile, basedata[i,1:4])
      #DMatch <- do.call(matcher, as.list(cbind(b, operation)))
      DMatch <- matcher(basefile, basedata[i,1],basedata[i,2],basedata[i,3],basedata[i,4],1)
      l1 <- length((DMatch[[1]])[,5])
      l2 <- length((DMatch[[2]])[,5])
      if (!(l1==0)){
        if(l1 > 1){
          counter1 <- l1
          repeat{#H<-do.call(matcher, as.list(b1))[[index]]
            ialt1 <-which(row.names(DMatch[[base_index2]][counter1,])==row.names(basedata))
            #if(t<10){nam1 <- paste("DMatch1", t, sep = "0")
            nam1=(DMatch[[1]])[counter1,5:length(DMatch[[1]])]
            #else{nam1 <- paste("DMatch1", t, sep = "")
            #assign(nam1, (DMatch[[index]])[counter,5:length(DMatch[[index]])])}
            #ow <- cbind(ow,get(nam1))
            counter1=counter1-1
            #outputw <- ow[-1]
            outputw <- nam1
            test3[ialt1,(3+2*1):(3+2*1+length(outputw)-1)] <- outputw
            if (counter1==0) {break} }
        }
        else{
          DMatch1=DMatch[[1]][,5:length(DMatch[[1]])]
          test3[i,(3+2*1):(3+2*1+length(DMatch1)-1)] <- DMatch1
        }
      }
      #nam1 <- paste("DMatch1", t, sep = "0")
      if(!(l2==0)){
        if(l2 > 1){
          counter2 <- l2
          repeat{#H<-do.call(matcher, as.list(b1))[[index]]
            ialt2 <-which(row.names(DMatch[[base_index2]][counter2,])==row.names(basedata))
            #if(t<10){nam1 <- paste("DMatch1", t, sep = "0")
            nam2=(DMatch[[2]])[counter2,5:length(DMatch[[2]])]
            #else{nam1 <- paste("DMatch1", t, sep = "")
            #assign(nam1, (DMatch[[index]])[counter,5:length(DMatch[[index]])])}
            #ow <- cbind(ow,get(nam1))
            counter2=counter2-1
            #outputw <- ow[-1]
            outputw <- nam2
            test3[ialt2,(3+2*2):(3+2*2+length(outputw)-1)] <- outputw
            if (counter2==0) {break} }
        }
        else{
          DMatch2=DMatch[[2]][,5:length(DMatch[[2]])]
          #assign(nam1, (DMatch[[index]])[counter,5:length(DMatch[[index]])])}
          test3[i,(3+2*2):(3+2*2+length(DMatch2)-1)] <- DMatch2
        }
      }
      #mapping <- test3
    }
    mapping <-test3
  }
  # if (operation == 2){mapping <- test2}
  # else{mapping <-test3}
  return(mapping)
}

# example of match with revised budget
Newb <- mapping("vol62003",1)
#------------


Map03 <- mapping("vol62003",2)
Map05 <- mapping("vol62005",2)
Map07 <- mapping("vol62007",2)
Map09 <- mapping("vol62009",2)
Map11 <- mapping("vol62011",2)

Map <- rbind(Map03, Map05, Map07, Map09, Map11)
Map_finale <- Map[!duplicated(Map),]
Map_finale6 <- Map_finale
#Mapd <- Map[intersect(is.na(Map),duplicated(Map)),]



#result: test2 is a panel table featuring all corresponding to the first year articles figures from all years with minimal loss
#and NAs if the these articles are absent



#-----------------------------SUMMARY
#track by supercode1 and description name
totalsprel <- function(Supercode1,Description){
  indices <- intersect(which(Map_finale$Supercode1==Supercode1),intersect(agrep(Description, Map_finale$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(Map_finale$Description)-nchar(Description))<2)))
  filtered <- Map_finale[indices,5:length(Map_finale)]
  filtered[is.na(filtered)] <- 0
  summ <- colSums(filtered)
  totalsprel <- summ
  return(totalsprel)
}

totals <- function (Description){
  S1 <- unique(Map_finale$Supercode1)
  l <- length(S1)
  totp <- totalsprel(S1[1],Description)
  for (i in 2:l){
    nam <- paste("totp", i, sep = "")
    assign(nam, totalsprel(S1[i],Description))
    totp <- rbind(totp,get(nam))
  }
  totrec <- t(colSums(totp))
  totals <- totrec
  return(totals)
}

totrec <- totals("Total Receipts")
totreq <- totals("Total Requirements")
netapp <- cbind("Net Appropriation", -diff(rbind(totreq,totrec)) ) 
vol6tot <- rbind(cbind("Total Requirements",totreq),cbind("Total Receipts",totrec),netapp) #final table

Map03vol1 <- mapping("vol12003")
Map05vol1 <- mapping("vol12005")
Map07vol1 <- mapping("vol12007")
Map09vol1 <- mapping("vol12009")
Map11vol1 <- mapping("vol12011")

Mapvol1 <- rbind(Map03vol1, Map05vol1, Map07vol1, Map09vol1, Map11vol1)
Map_finale <- Mapvol1[!duplicated(Mapvol1),]
Map_finale1 <- Map_finale

totrec <- totals("Total Receipts")
totreq <- totals("Total Requirements")
netapp <- cbind("Net Appropriation", -diff(rbind(totreq,totrec)) ) 
vol1tot <- rbind(cbind("Total Requirements",totreq),cbind("Total Receipts",totrec),netapp)

Map03vol2 <- mapping("vol22003")
Map05vol2 <- mapping("vol22005")
Map07vol2 <- mapping("vol22007")
Map09vol2 <- mapping("vol22009")
Map11vol2 <- mapping("vol22011")

Mapvol2 <- rbind(Map03vol2, Map05vol2, Map07vol2, Map09vol2, Map11vol2)
Map_finale <- Mapvol2[!duplicated(Mapvol2),]
Map_finale2 <- Map_finale

totrec <- totals("Total Receipts")
totreq <- totals("Total Requirements")
netapp <- cbind("Net Appropriation", -diff(rbind(totreq,totrec)) ) 
vol2tot <- rbind(cbind("Total Requirements",totreq),cbind("Total Receipts",totrec),netapp)

Map03vol3 <- mapping("vol32003")
Map05vol3 <- mapping("vol32005")
Map07vol3 <- mapping("vol32007")
Map09vol3 <- mapping("vol32009")
Map11vol3 <- mapping("vol32011")

Mapvol3 <- rbind(Map03vol3, Map05vol3, Map07vol3, Map09vol3, Map11vol3)
Map_finale <- Mapvol3[!duplicated(Mapvol3),]
Map_finale3 <- Map_finale

totrec <- totals("Total Receipts")
totreq <- totals("Total Requirements")
netapp <- cbind("Net Appropriation", -diff(rbind(totreq,totrec)) ) 
vol3tot <- rbind(cbind("Total Requirements",totreq),cbind("Total Receipts",totrec),netapp)

Map03vol4 <- mapping("vol42003")
Map05vol4 <- mapping("vol42005")
Map07vol4 <- mapping("vol42007")
Map09vol4 <- mapping("vol42009")
Map11vol4 <- mapping("vol42011")

Mapvol4 <- rbind(Map03vol4, Map05vol4, Map07vol4, Map09vol4, Map11vol4)
Map_finale <- Mapvol4[!duplicated(Mapvol4),]
Map_finale4 <- Map_finale

totrec <- totals("Total Receipts")
totreq <- totals("Total Requirements")
netapp <- cbind("Net Appropriation", -diff(rbind(totreq,totrec)) ) 
vol4tot <- rbind(cbind("Total Requirements",totreq),cbind("Total Receipts",totrec),netapp)

Map03vol5 <- mapping("vol52003")
Map05vol5 <- mapping("vol52005")
Map07vol5 <- mapping("vol52007")
Map09vol5 <- mapping("vol52009")
Map11vol5 <- mapping("vol52011")

Mapvol5 <- rbind(Map03vol5, Map05vol5, Map07vol5, Map09vol5, Map11vol5)
Map_finale <- Mapvol5[!duplicated(Mapvol5),]
Map_finale5 <- Map_finale

totrec <- totals("Total Receipts")
totreq <- totals("Total Requirements")
netapp <- cbind("Net Appropriation", -diff(rbind(totreq,totrec)) ) 
vol5tot <- rbind(cbind("Total Requirements",totreq),cbind("Total Receipts",totrec),netapp)

# LOAD NEW VARIABLES
inflation <- read.csv("C:/Users/Tom/Desktop/Data+/cpi.csv", header=TRUE)
population <- read.csv("C:/Users/Tom/Desktop/Data+/corrected pop.csv",header=TRUE)
population <- as.data.frame(apply(population, 2, rev),stringsAsFactors = FALSE)

totsp <- rbind(vol1tot[1,],vol2tot[1,],vol3tot[1,],vol4tot[1,],vol5tot[1,],vol6tot[1,])
#alternatively use apply
netsp <- rbind(vol1tot[3,],vol2tot[3,],vol3tot[3,],vol4tot[3,],vol5tot[3,],vol6tot[3,])
