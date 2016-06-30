rm(list = ls())
cat("\014")

# Script to install and run tabulizer to extract tables from PDFs 

# packages necessary to install and run tabulizer. I had to install a different version of Java, which you may have to do as well. 
require(devtools)
require(rJava)
require(tabulizer)

#load("C:/Users/Tom/Desktop/Data+/vol6datato11.RData")
#require(prodlim)
#install.packages("prodlim")

# if(!require("ghit")){
#    install.packages("ghit")
# }

# on 64-bit Windows
# ghit::install_github(c("leeper/tabulizerjars", "leeper/tabulizer"), INSTALL_opts = "--no-multiarch")
# elsewhere

# tell it where the PDF is
#file <- "C:/Users/Tom/Desktop/Data+/2003_5/vol6.pdf"

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
    Genout1 <- as.data.frame(extract_tables(file, pages = 1, guess = FALSE, method = "data.frame", columns = list(c(250, 355, 420)), stringsAsFactors=FALSE))
    for (i in 2:a) {
      try({
        Out <- as.data.frame(extract_tables(file, pages = i, guess = FALSE, method = "data.frame", columns = list(c(250, 355, 420)), stringsAsFactors=FALSE))
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
    
    #Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,2)) == "42"), ])
    #Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,4)) == "4200"), ])
    #Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,4)) == "4210"), ])
    #Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,4)) == "3510"), ])
    #Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,4)) == "6020"), ])
    #Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,4)) == "1000"), ])
    #Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,4)) == "3000"), ])
    #Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,4)) == "3005"), ])
    Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,4],1,4)) == "PAGE"), ],stringsAsFactors=FALSE)
    #Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,3)) == "APP"), ])
    Gen3 <- as.data.frame(Gen3[!(substr(Gen3[,1],1,3) %in% c("APP", "BUD", "REQ", "DES", "EST", "POS", "SUM")), ],stringsAsFactors=FALSE)
    Gen3 <- as.data.frame(Gen3[!(substr(Gen3[,3],1,3) %in% "AWG"), ],stringsAsFactors=FALSE)
    Gen3 <- as.data.frame(Gen3[!(substr(Gen3[,4],1,3) %in% "REV"), ],stringsAsFactors=FALSE)
    #Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,3)) == "BUD"), ])
    #Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,3)) == "REQ"), ])
    #Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,3)) == "142"), ])
    #Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,3)) == "DES"), ])
    #Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,3)) == "EST"), ])
    #Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,3)) == "POS"), ])
    #Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,3)) == "SUM"), ])
    
    Gen2 <- as.data.frame(gsub(",","",Gen3[,2]),stringsAsFactors=FALSE)
    Gen42 <- as.data.frame(gsub(",","",Gen3[,3]),stringsAsFactors=FALSE)
    Gen43 <- as.data.frame(gsub(",","",Gen3[,4]),stringsAsFactors=FALSE)
    Gen3 <- cbind(Gen3[,1], Gen2, Gen42, Gen43)
    Gen3 <- Gen3[!apply(Gen3 == "", 1, all),]
    Gen3 <- Gen3
    
    Gen3 <- cbind(as.data.frame(gsub("  "," ",Gen3[,1]), stringsAsFactors=FALSE),Gen3[,2:4])
    
    #is.letter <- function(x) grepl("[[:alpha:]]", x)
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
    #numbersnew2=as.data.frame(substr(Gen3[1,1],1,L[1]),stringsAsFactors=FALSE)
    
    for (i in 1:length(L)){
      numbersnew[i]=as.data.frame(substr(Gen3[i,1],1,L[i]),stringsAsFactors=FALSE)
      # numbersnew2 <- rbind(numbersnew2,numprel)
      
      lettersnew[i] <- as.data.frame(substr(Gen3[i,1],L[i]+1,100),stringsAsFactors=FALSE)
      for (j in 1:3){
        if(is.space(substr(lettersnew[i],j,j))==TRUE){
          h[i,j]=1
        }
      }
      lettersnew[i] <- as.data.frame(substr(Gen3[i,1],L[i]+1+sum(h[i,]),100),stringsAsFactors=FALSE)
    }
    
    #numbersnew <- as.numeric(gsub(" ","",numbersnew))
    #numbers <- as.data.frame(substr(Gen3[,1],1,5),stringsAsFactors=FALSE)
    #letters <- as.data.frame(substr(Gen3[,1],6,100),stringsAsFactors=FALSE)
    
    w <- cbind(numbersnew,lettersnew)
    Genfin <- cbind(w, Gen3[,2:4])
    
    Y <- as.data.frame(gsub("[^0-9]", "", Genfin[,1]),stringsAsFactors=FALSE)
    Genfin <- cbind (Y, Genfin[,2:length(Genfin)])
    
    M1 <- intersect(which(as.numeric(Genfin[,3])<0),which(!Genfin[,3]==""))
    M2 <- intersect(which(as.numeric(Genfin[,4])<0),which(!Genfin[,4]==""))
    M3 <- intersect(which(as.numeric(Genfin[,5])<0),which(!Genfin[,5]==""))
    R1 <- as.data.frame(gsub("[^0-9]", "", Genfin[,3]),stringsAsFactors=FALSE)
    R2 <- as.data.frame(gsub("[^0-9]", "", Genfin[,4]),stringsAsFactors=FALSE)
    R3 <- as.data.frame(gsub("[^0-9]", "", Genfin[,4]),stringsAsFactors=FALSE)
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
    #Genfin1 <- Genfin
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
    # extract tables from the PDF
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
    
    #Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,2)) == "42"), ])
    #Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,4)) == "4200"), ])
    #Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,4)) == "4210"), ])
    #Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,4)) == "3510"), ])
    #Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,4)) == "6020"), ])
    #Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,4)) == "1000"), ])
    #Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,4)) == "3000"), ])
    #Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,4)) == "3005"), ])
    Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,3],1,4)) == "PAGE"), ],stringsAsFactors=FALSE)
    #Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,3)) == "APP"), ])
    Gen3 <- as.data.frame(Gen3[!(substr(Gen3[,1],1,3) %in% c("APP", "BUD", "REQ", "DES", "EST", "POS", "SUM")), ],stringsAsFactors=FALSE)
    Gen3 <- as.data.frame(Gen3[!(substr(Gen3[,3],1,3) %in% "AWG"), ],stringsAsFactors=FALSE)
    #Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,3)) == "BUD"), ])
    #Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,3)) == "REQ"), ])
    #Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,3)) == "142"), ])
    #Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,3)) == "DES"), ])
    #Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,3)) == "EST"), ])
    #Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,3)) == "POS"), ])
    #Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,3)) == "SUM"), ])
    
    Gen2 <- as.data.frame(gsub(",","",Gen3[,2]),stringsAsFactors=FALSE)
    Gen42 <- as.data.frame(gsub(",","",Gen3[,3]),stringsAsFactors=FALSE)
    Gen3 <- cbind(Gen3[,1], Gen2, Gen42)
    Gen3 <- Gen3[!apply(Gen3 == "", 1, all),]
    Gen3 <- Gen3
    
    Gen3 <- cbind(as.data.frame(gsub("  "," ",Gen3[,1]), stringsAsFactors=FALSE),Gen3[,2:3])
    
    #is.letter <- function(x) grepl("[[:alpha:]]", x)
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
    #numbersnew2=as.data.frame(substr(Gen3[1,1],1,L[1]),stringsAsFactors=FALSE)
    
    for (i in 1:length(L)){
      numbersnew[i]=as.data.frame(substr(Gen3[i,1],1,L[i]),stringsAsFactors=FALSE)
      # numbersnew2 <- rbind(numbersnew2,numprel)
      
      lettersnew[i] <- as.data.frame(substr(Gen3[i,1],L[i]+1,100),stringsAsFactors=FALSE)
      for (j in 1:3){
        if(is.space(substr(lettersnew[i],j,j))==TRUE){
          h[i,j]=1
        }
      }
      lettersnew[i] <- as.data.frame(substr(Gen3[i,1],L[i]+1+sum(h[i,]),100),stringsAsFactors=FALSE)
    }
    
    #numbersnew <- as.numeric(gsub(" ","",numbersnew))
    #numbers <- as.data.frame(substr(Gen3[,1],1,5),stringsAsFactors=FALSE)
    #letters <- as.data.frame(substr(Gen3[,1],6,100),stringsAsFactors=FALSE)
    
    w <- cbind(numbersnew,lettersnew)
    Genfin <- cbind(w, Gen3[,2:3])
    Genfin <- Genfin[!(Genfin[,1]==""),]
    #Genfin[which(substr(Genfin[,1],5,5) %in% " "),1]=substr(Genfin[which(substr(Genfin[,1],5,5) %in% " "),1],1,4)
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
    #Genfin1 <- Genfin
    J <- !((as.data.frame(substr(Genfin[,1],1,1)) == "") & (as.data.frame(substr(Genfin[,3],1,1) == "")))
    Genfin <- as.data.frame(Genfin[J, ], stringsAsFactors=FALSE)
    
    
    colnames(Genfin) <- c("SubsecID","Description",year1,year2)
    Genfin$SubsecID  <- as.numeric(Genfin$SubsecID)
    Genfin[,3]   <- type.convert(Genfin[,3], numerals="warn.loss");
    Genfin[,4]   <- type.convert(Genfin[,4], numerals="warn.loss");
  }
  #values with 5 digit indices
  #vol[which(!substr(vol[,1],5,5) %in% ""),1]
  #indices with 5 digit indices
  M <- which(is.na(Genfin[,3]))
  N <- intersect(which(!substr(Genfin[,1],5,5) %in% ""),M)
  
  #indices for 2nd NA
  T <- setdiff(M,N)
  #T <- which(is.null(Genfin[,3]))
  #T <- T[!(T %in% c(which(!substr(Genfin[,1],5,5) %in% "")))]
  #vol[T,1]
  
  n <- length(Genfin[,1])
  O1 <- rep(0,n)
  O2 <- rep(0,n)
  
  #O2 <- O2[-which(is.null(Genfin[,3]))]
  #counter1=1
  #counter2=0
  
  #for (i in 1:n){
  #  if (i==M[counter1]& counter1<=length(M)){
  #    if(i==T[counter1-counter2]& (counter1-counter2)<=length(T)){
  #      O2[i-counter1+1]=Genfin[T[counter1-counter2],1]
  #    }
  #    else{counter2=counter2+1}
  #    counter1=1+counter1}
  #}
  
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
#vol620041 <- generator("C:/Users/Tom/Desktop/Data+/2004_5/vol6.pdf")

vol62003 <- generator("2003_5/vol6.pdf")
vol62005 <- generator("2005_7/vol6.pdf")
vol62007 <- generator("2007_9/vol6.pdf")
vol62009 <- generator("2009_11/vol6.pdf")
vol62011 <- generator("2011_13/vol6.pdf")

save(vol62003,file="vol62003.Rda")
save(vol62005,file="vol62005.Rda")
save(vol62007,file="vol62007.Rda")
save(vol62009,file="vol62009.Rda")
save(vol62011,file="vol62011.Rda")
