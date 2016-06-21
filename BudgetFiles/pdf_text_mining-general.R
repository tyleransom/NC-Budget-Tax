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
    
    #Genfin[which(substr(Genfin[,1],5,5) %in% " "),1]=substr(Genfin[which(substr(Genfin[,1],5,5) %in% " "),1],1,4)
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
    R3[M3,] <- paste("-",R3[M3],sep = "")
    
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

#for now the function ends here
vol62003 <- generator("C:/Users/Tom/Desktop/Data+/2003_5/vol6.pdf")
vol62005 <- generator("C:/Users/Tom/Desktop/Data+/2005_7/vol6.pdf")
vol62007 <- generator("C:/Users/Tom/Desktop/Data+/2007_9/vol6.pdf")
vol62009 <- generator("C:/Users/Tom/Desktop/Data+/2009_11/vol6.pdf")
vol62011 <- generator("C:/Users/Tom/Desktop/Data+/2011_13/vol6.pdf")

# matching function: give me supercode1 and some text from description column,
# I give you matrices with encounters in all data frames


# Supcode1 is the only required variable to run the code (others are optional), need to specify at least 2 variables
matcher <- function(Supcode1,Supcode2,SubsecID,Desc){
  matcher <- list()
  argList <- list(Supcode1,Supcode2,SubsecID,Desc)
  for (t in seq(3,11,by = 2)){
    index=(t+which(seq(3,11,by = 2)==t)-1)/3
    if(t<10){
      files <- get(paste("vol6200",t,sep = ""))
      nam <- paste("J", t, sep = "")
      #assign(nam, files[files$Supercode1==Supcode1 & grep(Desc,files$Description),])
      if(is.null(argList[[2]])==TRUE|is.null(argList[[3]])==TRUE|is.null(argList[[4]])==TRUE|is.null(argList[[2]])&is.null(argList[[3]])==TRUE|is.null(argList[[2]])&is.null(argList[[4]])==TRUE|is.null(argList[[3]])&is.null(argList[[4]])==TRUE){
        if(is.null(argList[[2]])==TRUE){
          if(is.null(argList[[3]])==TRUE|is.null(argList[[4]])==TRUE){
            if(is.null(argList[[3]])==TRUE){assign(nam, files[intersect(which(files$Supercode1==Supcode1), intersect(agrep(Desc, files$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(files$Description)-nchar(Desc))<2))),])
            }
            if(is.null(argList[[4]])==TRUE){assign(nam, files[intersect(which(files$Supercode1==Supcode1), grep(SubsecID,files$SubsecID)),])
            }
          }
          else{assign(nam, files[intersect(intersect(which(files$Supercode1==Supcode1), grep(SubsecID,files$SubsecID)), intersect(agrep(Desc, files$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(files$Description)-nchar(Desc))<2))),])
          }
        }
        if(is.null(argList[[3]])==TRUE){
          if(is.null(argList[[2]])==TRUE|is.null(argList[[4]])==TRUE){
            if(is.null(argList[[4]])==TRUE){assign(nam, files[intersect(which(files$Supercode1==Supcode1), which(files$Supercode2==Supcode2)),])
            }
            if(is.null(argList[[2]])==TRUE){assign(nam, files[intersect(which(files$Supercode1==Supcode1), intersect(agrep(Desc, files$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(files$Description)-nchar(Desc))<2))),])
            }
          }
          else{assign(nam, files[intersect(intersect(which(files$Supercode1==Supcode1), which(files$Supercode2==Supcode2)), intersect(agrep(Desc, files$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(files$Description)-nchar(Desc))<2))),])
          }
        }
        if(is.null(argList[[4]])==TRUE){
          if(is.null(argList[[3]])==TRUE|is.null(argList[[2]])==TRUE){
            if(is.null(argList[[2]])==TRUE){assign(nam, files[intersect(which(files$Supercode1==Supcode1), grep(SubsecID,files$SubsecID)),])
            }
            if(is.null(argList[[3]])==TRUE){assign(nam, files[intersect(which(files$Supercode1==Supcode1), which(files$Supercode2==Supcode2)),])
            }
          }
          else{
            assign(nam, files[intersect(intersect(which(files$Supercode1==Supcode1), which(files$Supercode2==Supcode2)), grep(SubsecID,files$SubsecID)),])
          }
        }
      }
      else{assign(nam, files[intersect(intersect(which(files$Supercode1==Supcode1), which(files$Supercode2==Supcode2)), intersect(grep(SubsecID,files$SubsecID), intersect(agrep(Desc, files$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(files$Description)-nchar(Desc))<2)))),])
      }
      
      #if( union(!is.null(Supcode1),!is.null(SubsecID),!is.null(Desc))){assign(nam, files[intersect(which(files$Supercode1==Supcode1), grep(SubsecID,files$SubsecID),intersect(agrep(Desc, files$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(files$Description)-nchar(Desc))<2),])
      #}
      matcher[[index]]=get(nam)
    }
    else{files <- get(paste("vol620",t,sep = ""))
    nam <- paste("J", t, sep = "")
    #assign(nam, files[files$Supercode1==Supcode1 & grep(Desc,files$Description),])
    if(is.null(argList[[2]])==TRUE|is.null(argList[[3]])==TRUE|is.null(argList[[4]])==TRUE|is.null(argList[[2]])&is.null(argList[[3]])==TRUE|is.null(argList[[2]])&is.null(argList[[4]])==TRUE|is.null(argList[[3]])&is.null(argList[[4]])==TRUE){
      if(is.null(argList[[2]])==TRUE){
        if(is.null(argList[[3]])==TRUE|is.null(argList[[4]])==TRUE){
          if(is.null(argList[[3]])==TRUE){assign(nam, files[intersect(which(files$Supercode1==Supcode1), intersect(agrep(Desc, files$Description, max=list(cost=1, all=1), ignore.case=TRUE), which(abs(nchar(files$Description)-nchar(Desc))<2))),])
          }
          if(is.null(argList[[4]])==TRUE){assign(nam, files[intersect(which(files$Supercode1==Supcode1), grep(SubsecID,files$SubsecID)),])
          }
        }
        else{assign(nam, files[intersect(intersect(which(files$Supercode1==Supcode1), grep(SubsecID,files$SubsecID)), intersect(agrep(Desc, files$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(files$Description)-nchar(Desc))<2))),])
        }
      }
      if(is.null(argList[[3]])==TRUE){
        if(is.null(argList[[2]])==TRUE|is.null(argList[[4]])==TRUE){
          if(is.null(argList[[4]])==TRUE){assign(nam, files[intersect(which(files$Supercode1==Supcode1), which(files$Supercode2==Supcode2)),])
          }
          if(is.null(argList[[2]])==TRUE){assign(nam, files[intersect(which(files$Supercode1==Supcode1), intersect(agrep(Desc, files$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(files$Description)-nchar(Desc))<2))),])
          }
        }
        else{assign(nam, files[intersect(intersect(which(files$Supercode1==Supcode1), which(files$Supercode2==Supcode2)), intersect(agrep(Desc, files$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(files$Description)-nchar(Desc))<2))),])
        }
      }
      if(is.null(argList[[4]])==TRUE){
        if(is.null(argList[[3]])==TRUE|is.null(argList[[2]])==TRUE){
          if(is.null(argList[[2]])==TRUE){assign(nam, files[intersect(which(files$Supercode1==Supcode1), grep(SubsecID,files$SubsecID)),])
          }
          if(is.null(argList[[3]])==TRUE){assign(nam, files[intersect(which(files$Supercode1==Supcode1), which(files$Supercode2==Supcode2)),])
          }
        }
        else{
          assign(nam, files[intersect(intersect(which(files$Supercode1==Supcode1), which(files$Supercode2==Supcode2)), grep(SubsecID,files$SubsecID)),])
        }
      }
    }
    else{assign(nam, files[intersect(intersect(which(files$Supercode1==Supcode1), which(files$Supercode2==Supcode2)), intersect(grep(SubsecID,files$SubsecID), intersect(agrep(Desc, files$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(files$Description)-nchar(Desc))<2)))),])
    }
    matcher[[index]]=get(nam)
    }
  }
  return(matcher)
}


#------------------------------------------------
# example
U <- matcher(84210,"STATE AID")
I <- U[[1]] #these are encounters in vol6 2003

#example with optional variables
U2 <- matcher(84210,7828,SubsecID=NULL,Desc=NULL)
I2 <- U2[[1]]
#------------------------------------------------

Match <- matcher(14222, Supcode2=NULL, SubsecID=NULL, "STATE AID")

# conveniently store output from matcher in dataframes
for (t in seq(3,11,by = 2)){
  index=(t+which(seq(3,11,by = 2)==t)-1)/3
  if(t<10){nam <- paste("Match", t, sep = "0")
  assign(nam, Match[[index]])}
  else{nam <- paste("Match", t, sep = "")
  assign(nam, Match[[index]])}
}


#--------------------------------------------MERGING
# delete duplicates (approach for now, will make it better perhaps with use of my matcher function)
dvol62003 <- vol62003[!duplicated(vol62003),]
dvol62005 <- vol62005[!duplicated(vol62005),]
dvol62007 <- vol62007[!duplicated(vol62007),]
dvol62009 <- vol62009[!duplicated(vol62009),]
dvol62011 <- vol62011[!duplicated(vol62011),]

# merge this stuff
test <- dvol62003
for (t in seq(5,11,by = 2)){
  #index=(t+which(seq(3,11,by = 2)==t)-1)/3
  if(t<10){nam <- paste("dvol620", t, sep = "0")}
  #assign(nam, (Match[[index]])[,5:length(Match[[index]])])}
  else{nam <- paste("dvol620", t, sep = "")}
  #assign(nam, (Match[[index]])[,5:length(Match[[index]])])}
  test3 <- get(nam)
  test <- merge(test,test3,by=c("Supercode1", "Supercode2", "Description"))
}


#-------------------MAPPING


#dvol62003un <- unname(dvol62003)
mapping <- function(basefile){
#basefile <- rbind(dvol62003,dvol62005,dvol62007,dvol62009,dvol62011,setNames( rev(vol62003) , names( vol62003) ) )
test2 <- matrix(,nrow=length(basefile[,1]),ncol=2*length(seq(3,11,by = 2)))
test2 <- cbind(basefile[,1:4],test2)

dmatcher <- function(Supcode1,Supcode2,SubsecID,Desc){
  dmatcher <- list()
  argList<-list(Supcode1,Supcode2,SubsecID,Desc)
  for (t in seq(3,11,by = 2)){
    index=(t+which(seq(3,11,by = 2)==t)-1)/3
    if(t<10){
      files <- get(paste("vol6200",t,sep = ""))
      nam <- paste("J", t, sep = "")
      #assign(nam, files[files$Supercode1==Supcode1 & grep(Desc,files$Description),])
      if(is.null(argList[[2]])==TRUE|is.null(argList[[3]])==TRUE|is.null(argList[[4]])==TRUE|is.null(argList[[2]])&is.null(argList[[3]])==TRUE|is.null(argList[[2]])&is.null(argList[[4]])==TRUE|is.null(argList[[3]])&is.null(argList[[4]])==TRUE){
        if(is.null(argList[[2]])==TRUE){
          if(is.null(argList[[3]])==TRUE|is.null(argList[[4]])==TRUE){
            if(is.null(argList[[3]])==TRUE){assign(nam, files[intersect(which(files$Supercode1==Supcode1), intersect(agrep(Desc, files$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(files$Description)-nchar(Desc))<2))),])
            }
            if(is.null(argList[[4]])==TRUE){assign(nam, files[intersect(which(files$Supercode1==Supcode1), grep(SubsecID,files$SubsecID)),])
            }
          }
          else{assign(nam, files[intersect(intersect(which(files$Supercode1==Supcode1), grep(SubsecID,files$SubsecID)), intersect(agrep(Desc, files$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(files$Description)-nchar(Desc))<2))),])
          }
        }
        if(is.null(argList[[3]])==TRUE){
          if(is.null(argList[[2]])==TRUE|is.null(argList[[4]])==TRUE){
            if(is.null(argList[[4]])==TRUE){assign(nam, files[intersect(which(files$Supercode1==Supcode1), which(files$Supercode2==Supcode2)),])
            }
            if(is.null(argList[[2]])==TRUE){assign(nam, files[intersect(which(files$Supercode1==Supcode1), intersect(agrep(Desc, files$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(files$Description)-nchar(Desc))<2))),])
            }
          }
          else{assign(nam, files[intersect(intersect(which(files$Supercode1==Supcode1), which(files$Supercode2==Supcode2)), intersect(agrep(Desc, files$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(files$Description)-nchar(Desc))<2))),])
          }
        }
        if(is.null(argList[[4]])==TRUE){
          if(is.null(argList[[3]])==TRUE|is.null(argList[[2]])==TRUE){
            if(is.null(argList[[2]])==TRUE){assign(nam, files[intersect(which(files$Supercode1==Supcode1), grep(SubsecID,files$SubsecID)),])
            }
            if(is.null(argList[[3]])==TRUE){assign(nam, files[intersect(which(files$Supercode1==Supcode1), which(files$Supercode2==Supcode2)),])
            }
          }
          else{
            assign(nam, files[intersect(intersect(which(files$Supercode1==Supcode1), which(files$Supercode2==Supcode2)), grep(SubsecID,files$SubsecID)),])
          }
        }
      }
      else{assign(nam, files[intersect(intersect(which(files$Supercode1==Supcode1), which(files$Supercode2==Supcode2)), intersect(grep(SubsecID,files$SubsecID), intersect(agrep(Desc, files$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(files$Description)-nchar(Desc))<2)))),])
      }
      
      #if( union(!is.null(Supcode1),!is.null(SubsecID),!is.null(Desc))){assign(nam, files[intersect(which(files$Supercode1==Supcode1), grep(SubsecID,files$SubsecID),intersect(agrep(Desc, files$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(files$Description)-nchar(Desc))<2),])
      #}
      dmatcher[[index]]=get(nam)
    }
    else{files <- get(paste("vol620",t,sep = ""))
    nam <- paste("J", t, sep = "")
    #assign(nam, files[files$Supercode1==Supcode1 & grep(Desc,files$Description),])
    if(is.null(argList[[2]])==TRUE|is.null(argList[[3]])==TRUE|is.null(argList[[4]])==TRUE|is.null(argList[[2]])&is.null(argList[[3]])==TRUE|is.null(argList[[2]])&is.null(argList[[4]])==TRUE|is.null(argList[[3]])&is.null(argList[[4]])==TRUE){
      if(is.null(argList[[2]])==TRUE){
        if(is.null(argList[[3]])==TRUE|is.null(argList[[4]])==TRUE){
          if(is.null(argList[[3]])==TRUE){assign(nam, files[intersect(which(files$Supercode1==Supcode1), intersect(agrep(Desc, files$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(files$Description)-nchar(Desc))<2))),])
          }
          if(is.null(argList[[4]])==TRUE){assign(nam, files[intersect(which(files$Supercode1==Supcode1), grep(SubsecID,files$SubsecID)),])
          }
        }
        else{assign(nam, files[intersect(intersect(which(files$Supercode1==Supcode1), grep(SubsecID,files$SubsecID)), intersect(agrep(Desc, files$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(files$Description)-nchar(Desc))<2))),])
        }
      }
      if(is.null(argList[[3]])==TRUE){
        if(is.null(argList[[2]])==TRUE|is.null(argList[[4]])==TRUE){
          if(is.null(argList[[4]])==TRUE){assign(nam, files[intersect(which(files$Supercode1==Supcode1), which(files$Supercode2==Supcode2)),])
          }
          if(is.null(argList[[2]])==TRUE){assign(nam, files[intersect(which(files$Supercode1==Supcode1), intersect(agrep(Desc, files$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(files$Description)-nchar(Desc))<2))),])
          }
        }
        else{assign(nam, files[intersect(intersect(which(files$Supercode1==Supcode1), which(files$Supercode2==Supcode2)), intersect(agrep(Desc, files$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(files$Description)-nchar(Desc))<2))),])
        }
      }
      if(is.null(argList[[4]])==TRUE){
        if(is.null(argList[[3]])==TRUE|is.null(argList[[2]])==TRUE){
          if(is.null(argList[[2]])==TRUE){assign(nam, files[intersect(which(files$Supercode1==Supcode1), grep(SubsecID,files$SubsecID)),])
          }
          if(is.null(argList[[3]])==TRUE){assign(nam, files[intersect(which(files$Supercode1==Supcode1), which(files$Supercode2==Supcode2)),])
          }
        }
        else{
          assign(nam, files[intersect(intersect(which(files$Supercode1==Supcode1), which(files$Supercode2==Supcode2)), grep(SubsecID,files$SubsecID)),])
        }
      }
    }
    else{assign(nam, files[intersect(intersect(which(files$Supercode1==Supcode1), which(files$Supercode2==Supcode2)), intersect(grep(SubsecID,files$SubsecID), intersect(agrep(Desc, files$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(files$Description)-nchar(Desc))<2)))),])
    }
    dmatcher[[index]]=get(nam)
    }
  }
  return(dmatcher)
}
#is.empty <- function(x) grepl(numeric(0), x)
weirdos <- matrix(0,length(basefile[,1]),length(seq(3,11,by = 2)))

for (i in 1:length(basefile[,1])){
DMatch <- dmatcher(basefile$Supercode1[i],basefile$Supercode2[i], SubsecID= NULL, basefile$Description[[i]])
#o <- rep(0,1)
#ow <- rep(0,1)
for (t in seq(3,11,by = 2)){
  index=(t+which(seq(3,11,by = 2)==t)-1)/3
  l <- length((DMatch[[index]])[,5])
  if (!(l == 0)){
    if(l > 1){
      weirdos[i,index] <- l 
      counter <- l
      repeat{ialt <- which(row.names(DMatch[[index]][counter,])==row.names(basefile))
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
#output <- o
#if (length(output)>2){
#output <- output[-1]}
#test2[i,5:(5+length(output)-1)] <- output
}
mapping <- test2
return(mapping)
}


Map03 <- mapping(dvol62003)
Map05 <- mapping(dvol62005)
Map07 <- mapping(dvol62007)
Map09 <- mapping(dvol62009)
Map11 <- mapping(dvol62011)

Map <- rbind(Map03,Map05, Map07, Map09, Map11)
Mapd <- Map[duplicated(Map),]



#result: test2 is a panel table featuring all corresponding to the first year articles figures from all years with minimal loss
#and NAs if the these articles are absent






















###############################################################################################################################
###CODE 2 for same function###

matcher2.0 <- function(Supcode1,ID=NULL,Desc=NULL){
  matcher2.0 <- list()
  for (t in seq(3,11,by = 2)){
    index=(t+which(seq(3,11,by = 2)==t)-1)/3
    if(t<10){
      if(is.null(ID)){
        files <- get(paste("vol6200",t,sep = ""))
        nam <- paste("J", t, sep = "")
        assign(nam, files[files$Supercode1==Supcode1 & files$Description==Desc,])
        matcher2.0[[index]]=get(nam)}
      if(is.null(Desc)){
        files <- get(paste("vol6200",t,sep = ""))
        nam <- paste("J", t, sep = "")
        assign(nam, files[files$Supercode1==Supcode1 & files$SubsecID==ID,])
        matcher2.0[[index]]=get(nam)}
      if(is.null(ID) & is.null(Desc)){
        files <- get(paste("vol6200",t,sep = ""))
        nam <- paste("J", t, sep = "")
        assign(nam, files[files$Supercode1==Supcode1,])
        matcher2.0[[index]]=get(nam)}
      if(!is.null(ID) & !is.null(Desc)){
        files <- get(paste("vol6200",t,sep = ""))
        nam <- paste("J", t, sep = "")
        assign(nam, files[files$SubsecID==ID & files$Description==Desc & files$Supercode1 == Supcode1,])
        matcher2.0[[index]]=get(nam)}}
    if (t>10){
      if(is.null(ID)){
        files <- get(paste("vol620",t,sep = ""))
        nam <- paste("J", t, sep = "")
        assign(nam, files[files$Supercode1==Supcode1 & files$Description==Desc,])
        matcher2.0[[index]]=get(nam)}
      if (is.null(Desc)){
        files <- get(paste("vol620",t,sep = ""))
        nam <- paste("J", t, sep = "")
        assign(nam, files[files$Supercode1==Supcode1 & files$SubsecID==ID,])
        matcher2.0[[index]]=get(nam)}
      if(is.null(ID) & is.null(Desc)){
        files <- get(paste("vol620",t,sep = ""))
        nam <- paste("J", t, sep = "")
        assign(nam, files[files$Supercode1==Supcode1,])
        matcher2.0[[index]]=get(nam)}
      if(!is.null(ID) & !is.null(Desc)){
        files <- get(paste("vol620",t,sep = ""))
        nam <- paste("J", t, sep = "")
        assign(nam, files[files$SubsecID==ID & files$Description==Desc & files$Supercode1 == Supcode1,])
        matcher2.0[[index]]=get(nam)}
      }
    }
return(matcher2.0)}

#is.null(ID) & !is.null(Desc
# example
U2 <- matcher2.0(84210)
I2 <- U2[[1]] #these are encounters in vol6 2003

###################################################################################################################################





















# List all categories with given ID
Genfin[Genfin$SubsecID==6990,]


### LEFT TO DO:
# Somehow map the "big" and "small" codes where there are currently NA's
# into two new columns labeled "Category" and "Subcategory"

# I found this function at http://www.r-bloggers.com/generating-a-laglead-variables/
shift<-function(x,shift_by){
    stopifnot(is.numeric(shift_by))
    stopifnot(is.numeric(x))
 
    if (length(shift_by)>1)
        return(sapply(shift_by,shift, x=x))
 
    out<-NULL
    abs_shift_by=abs(shift_by)
    if (shift_by > 0 )
        out<-c(tail(x,-abs_shift_by),rep(NA,abs_shift_by))
    else if (shift_by < 0 )
        out<-c(rep(NA,abs_shift_by), head(x,-abs_shift_by))
    else
        out<-x
    out
}


# Flag aggregate codes and sub-aggregate codes so that we can get a unique identifier
Genfin$aggflag    <- is.na(Genfin$y200304)
Genfin$subaggflag <- is.na(Genfin$y200304)
Genfin$IDflag     <- as.logical(shift(as.numeric((!is.na(Genfin$y200304) & Genfin$lead1==TRUE)),-1))
Genfin$subIDflag  <- as.logical(shift(as.numeric((!is.na(Genfin$y200304) & Genfin$lag1==TRUE)),1))
Genfin$Category   <- as.numeric(Genfin$IDflag)*Genfin$SubsecID
Genfin$Subategory <- as.numeric(Genfin$subIDflag)*Genfin$SubsecID
Genfin$Category[1]<- Genfin$SubsecID[1]

# Now we need to fill in the zeros between the numbers with the previously found number.
# This will probably require looping through values
# i.e.
# look at next observation
# if next observation is 0, replace it with current observation
# otherwise continue to the next observation and repeat
Gen$Supercode1 <- c(0)
Gen$Supercode2 <- c(0)
Gen <- Gen[c("Supercode1","Supercode2","SubsecID","Description","y200304","y200405")]
# Add two columns with all zero entries to data set, and rearranging the columns using the column names
for (i in 1:2485)
  if (as.numeric(Gen[i,3]) == 14222){
    Gen[i,1] = 14222} 
for (i in 1:2485)
  if (as.numeric(Gen[i,3]) == 84210){
    Gen[i,1] = 84210}
for (i in 1:2485)
  if (as.numeric(Gen[i,3]) == 84290){
    Gen[i,1] = 84290}
for (i in 1:2485)
  if (as.numeric(Gen[i,3]) == 14222 & is.na(Gen[i+1,5]) ){
    Gen[i+1,2] = Gen[i+1,3]} 
for (i in 1:2485)
  if (as.numeric(Gen[i,3]) == 84210 & is.na(Gen[i+1,5]) ){
    Gen[i+1,2] = Gen[i+1,3]}
for (i in 1:2485)
  if (as.numeric(Gen[i,1]) == 14222 & (as.numeric(Gen[i+1,1]) == 0)){
    Gen[i+1,1] = 14222}
for (i in 1:2485)
  if (as.numeric(Gen[i,1]) == 84210 & (as.numeric(Gen[i+1,1]) == 0)){
    Gen[i+1,1] = 84210}
for (i in 1:2485)
  if (as.numeric(Gen[i,1]) == 84290 & (as.numeric(Gen[i+1,1]) == 0)){
    Gen[i+1,1] = 84290}
for (i in 1:2485)
  if ((as.numeric(Gen[i+1,2]) == 0) & !is.na(Gen[i+1,4])){
    Gen[i+1,2] = Gen[i,2]}
for (i in 1:2485)
  if ((as.numeric(Gen[i,3]) == 14222)){
    Gen[i,2] = 0}
for (i in 1:2485)
  if ((as.numeric(Gen[i,3]) == 84210)){
    Gen[i,2] = 0}
for (i in 1:2485)
  if ((as.numeric(Gen[i,3]) == 84290)){
    Gen[i,2] = 0}
# Assigning Supercodes to SubSecID's in that page


#values with 5 digit indeces
vol[which(!substr(vol[,1],5,5) %in% ""),1]
#indeces with 5 digit indeces
N <- which(!substr(vol[,1],5,5) %in% "")
M <- which(is.na(vol[,3]))

#indeces for 2nd NA
T <- which(is.na(vol[,3]))
T <- T[!(T %in% c(which(!substr(vol[,1],5,5) %in% "")))]
vol[T,1]

#n <- length(vol[,1])-length(which(is.na(vol[,3])))
n <- length(vol[,1])
O1 <- rep(0,n)
O2 <- rep(0,n)
counter=1

for (i in 1:length(M)){
  if (M[i]==T[counter]& counter<=length(T)){
  O2[T[counter]]=vol[T[counter],1]
  counter=1+counter}
}

for (i in 1:length(N)){
  O1[N[i]:n]=vol[N[i],1]
}
O1 <- O1[-which(is.na(vol[,3]))]
