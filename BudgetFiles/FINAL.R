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
#vol <- 6
#year<- 2003
# tell it where the PDF is
#file <- "C:/Users/admin/Desktop/DUKE Data+/2003_5/vol6.pdf"
DataGen <- function(vol,year){
  

  generator <- function(file){
  a <- get_n_pages(file=file)
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
  Gen2 <- as.data.frame(gsub("TOTAL", "00000 TOTAL", Genout2[,1]), stringsAsFactors=FALSE)
  Gen3 <- cbind(Gen2, Genout2[,2:3])
  
  Gen2 <- as.data.frame(gsub("NET", "00001 NET", Gen3[,1]), stringsAsFactors=FALSE)
  Gen3 <- cbind(Gen2, Gen3[,2:3])
  
  Gen2 <- as.data.frame(gsub("HIGHWAY FUND APPROPRIATION", "00002 HIGHWAY FUND APPROPRIATION", Gen3[,1]), stringsAsFactors=FALSE)
  Gen3 <- cbind(Gen2, Gen3[,2:3])
  
  Gen2 <- as.data.frame(gsub("HIGHWAY TRUST FUND APPROPRTN", "00003 HIGHWAY TRUST FUND APPROPRTN", Gen3[,1]), stringsAsFactors=FALSE)
  Gen3 <- cbind(Gen2, Gen3[,2:3])
  
  Gen2 <- as.data.frame(gsub("CHANGE IN FUND BALANCE", "00004 CHANGE IN FUND BALANCE", Gen3[,1]),stringsAsFactors=FALSE)
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
  #Gen3 <- cbind(as.data.frame(substr(Gen3[,2],1,5)),as.data.frame(substr(Gen3[,2],7,100)),Gen3[,2:3])
  
  Gen3 <- cbind(as.data.frame(gsub("  "," ",Gen3[,1]), stringsAsFactors=FALSE),Gen3[,2:3])
  
  #is.letter <- function(x) grepl("[[:alpha:]]", x)
  is.number <- function(x) grepl("[[:digit:]]", x)
  
  L=rep(0,length(Gen3[,1]))
  for (j in 1:8){
    k <- as.data.frame(substr(Gen3[,1],j,j),stringsAsFactors=FALSE)
    for (i in 1:length(k[,1])){
      if(is.number(k[i,1])==TRUE){L[i]=j}
    }}
  
  numbersnew=rep(0,length(L))
  lettersnew=c()
  #numbersnew2=as.data.frame(substr(Gen3[1,1],1,L[1]),stringsAsFactors=FALSE)
  
  for (i in 1:length(L)){
    numbersnew[i]=as.data.frame(substr(Gen3[i,1],1,L[i]),stringsAsFactors=FALSE)
    # numbersnew2 <- rbind(numbersnew2,numprel)
    lettersnew[i] <- as.data.frame(substr(Gen3[i,1],L[i]+1,100),stringsAsFactors=FALSE)
  }
  #numbersnew <- as.numeric(gsub(" ","",numbersnew))
  #numbers <- as.data.frame(substr(Gen3[,1],1,5),stringsAsFactors=FALSE)
  #letters <- as.data.frame(substr(Gen3[,1],6,100),stringsAsFactors=FALSE)
  
  w <- cbind(numbersnew,lettersnew)
  Genfin <- cbind(w, Gen3[,2:3])
  
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
  R1 <- as.data.frame(gsub("[^0-9]", "", Genfin[,3]),stringsAsFactors=FALSE)
  R2 <- as.data.frame(gsub("[^0-9]", "", Genfin[,4]),stringsAsFactors=FALSE)
  Genfin <- cbind(Genfin[,1:2],R1,R2)
  J <- !((as.data.frame(substr(Genfin[,1],1,1)) == "") & (as.data.frame(substr(Genfin[,3],1,1) == "")))
  Genfin <- as.data.frame(Genfin[J, ], stringsAsFactors=FALSE)
  
  #Genfin[which(substr(Genfin[,1],5,5) %in% " "),1]=substr(Genfin[which(substr(Genfin[,1],5,5) %in% " "),1],1,4)
  Y <- as.data.frame(gsub("[^0-9]", "", Genfin[,1]),stringsAsFactors=FALSE)
  Genfin <- cbind (Y, Genfin[,2:length(Genfin)])
  colnames(Genfin) <- c("SubsecID","Description",year1,year2)
  Genfin$SubsecID  <- as.numeric(Genfin$SubsecID)
  Genfin[,3]   <- type.convert(Genfin[,3], numerals="warn.loss");
  Genfin[,4]   <- type.convert(Genfin[,4], numerals="warn.loss");
  
  #values with 5 digit indices
  #vol[which(!substr(vol[,1],5,5) %in% ""),1]
  #indices with 5 digit indices
  M <- which(is.na(Genfin[,3]))
  N <- intersect(which(!substr(Genfin[,1],5,5) %in% ""),M)
  
  
  #indices for 2nd NA
  T <- setdiff(M,N)
  #T <- which(is.na(Genfin[,3]))
  #T <- T[!(T %in% c(which(!substr(Genfin[,1],5,5) %in% "")))]
  #vol[T,1]
  
  n <- length(Genfin[,1])
  O1 <- rep(0,n)
  O2 <- rep(0,n)
  O2 <- O2[-which(is.na(Genfin[,3]))]
  counter1=1
  counter2=0
  
  for (i in 1:n){
    if (i==M[counter1]& counter1<=length(M)){
      if(i==T[counter1-counter2]& (counter1-counter2)<=length(T)){
        O2[i-counter1+1]=Genfin[T[counter1-counter2],1]
      }
      else{counter2=counter2+1}
      counter1=1+counter1}
  }
  
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
  Genfin <- cbind(Genfin[,1:3],as.data.frame(substr(Genfin[,4],2,100),stringsAsFactors=FALSE),Genfin[,5:6])
  colnames(Genfin)[4] <- "Description"
  
  return(Genfin)
}
  if(year<10){
    V1 <- generator(as.character(paste("C:/Users/admin/Desktop/DUKE Data+/",paste(year,"_",as.numeric(substr(year,4,4))+2,sep=""),"/vol",vol,".pdf",sep ="")))
  }
  else{
    V1 <- generator(as.character(paste("C:/Users/admin/Desktop/DUKE Data+/",paste(year,"_",as.numeric(substr(year,3,4))+2,sep=""),"/vol",vol,".pdf",sep ="")))
  }

#################################################################################
  generator2 <- function(file){
  a <- get_n_pages(file=file)
  # extract tables from the PDF
  Genout1 <- as.data.frame(extract_tables(file, pages = 1, guess = FALSE, method = "data.frame", columns = list(c(250,345,420)), stringsAsFactors=FALSE))
  for (i in 2:a) {
    try({
      Out <- as.data.frame(extract_tables(file, pages = i, guess = FALSE, method = "data.frame", columns = list(c(250,345,420)), stringsAsFactors=FALSE))
      names(Out) <- names(Genout1)
      Genout1 <- rbind(Genout1,Out)
    })
  }
  
  
  Genout1 <- as.data.frame(Genout1[!(as.data.frame(substr(Genout1[,1],1,1)) == "-"), ], stringsAsFactors=FALSE)
  
  Genout2=Genout1
  Gen2 <- as.data.frame(gsub("TOTAL", "00000 TOTAL", Genout2[,1]), stringsAsFactors=FALSE)
  Gen3 <- cbind(Gen2, Genout2[,2:4])
  
  Gen2 <- as.data.frame(gsub("NET", "00001 NET", Gen3[,1]), stringsAsFactors=FALSE)
  Gen3 <- cbind(Gen2, Gen3[,2:4])
  
  Gen2 <- as.data.frame(gsub("HIGHWAY FUND APPROPRIATION", "00002 HIGHWAY FUND APPROPRIATION", Gen3[,1]), stringsAsFactors=FALSE)
  Gen3 <- cbind(Gen2, Gen3[,2:4])
  
  Gen2 <- as.data.frame(gsub("HIGHWAY TRUST FUND APPROPRTN", "00003 HIGHWAY TRUST FUND APPROPRTN", Gen3[,1]), stringsAsFactors=FALSE)
  Gen3 <- cbind(Gen2, Gen3[,2:4])
  
  Gen2 <- as.data.frame(gsub("CHANGE IN FUND BALANCE", "00004 CHANGE IN FUND BALANCE", Gen3[,1]),stringsAsFactors=FALSE)
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
  # Gen2 <- as.data.frame(gsub(",","",Gen3[,1]),stringsAsFactors=FALSE)
  Gen42 <- as.data.frame(gsub(",","",Gen3[,3]),stringsAsFactors=FALSE)
  Gen43 <- as.data.frame(gsub(",","",Gen3[,4]),stringsAsFactors=FALSE)
  Gen3 <- cbind(Gen3[,1], Gen2, Gen42,Gen43)
  Gen3 <- Gen3[!apply(Gen3 == "", 1, all),]
  Gen3 <- Gen3
  
  Gen3 <- cbind(as.data.frame(gsub("  "," ",Gen3[,1]), stringsAsFactors=FALSE),Gen3[,2:4])
  
  #is.letter <- function(x) grepl("[[:alpha:]]", x)
  is.number <- function(x) grepl("[[:digit:]]", x)
  
  L=rep(0,length(Gen3[,1]))
  for (j in 1:8){
    k <- as.data.frame(substr(Gen3[,1],j,j),stringsAsFactors=FALSE)
    for (i in 1:length(k[,1])){
      if(is.number(k[i,1])==TRUE){L[i]=j}
    }}
  
  numbersnew=rep(0,length(L))
  lettersnew=c()
  #numbersnew2=as.data.frame(substr(Gen3[1,1],1,L[1]),stringsAsFactors=FALSE)
  
  for (i in 1:length(L)){
    numbersnew[i]=as.data.frame(substr(Gen3[i,1],1,L[i]),stringsAsFactors=FALSE)
    # numbersnew2 <- rbind(numbersnew2,numprel)
    lettersnew[i] <- as.data.frame(substr(Gen3[i,1],L[i]+1,100),stringsAsFactors=FALSE)
  }
  #numbersnew <- as.numeric(gsub(" ","",numbersnew))
  #numbers <- as.data.frame(substr(Gen3[,1],1,5),stringsAsFactors=FALSE)
  #letters <- as.data.frame(substr(Gen3[,1],6,100),stringsAsFactors=FALSE)
  
  w <- cbind(numbersnew,lettersnew)
  Genfin <- cbind(w, Gen3[,2:4])
  
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
  R1 <- as.data.frame(gsub("[^0-9]", "", Genfin[,3]),stringsAsFactors=FALSE)
  R2 <- as.data.frame(gsub("[^0-9]", "", Genfin[,4]),stringsAsFactors=FALSE)
  R3 <- as.data.frame(gsub("[^0-9]", "", Genfin[,5]),stringsAsFactors=FALSE)
  Genfin <- cbind(Genfin[,1:2],R1,R2,R3)
  J <- !((as.data.frame(substr(Genfin[,1],1,1)) == "") & (as.data.frame(substr(Genfin[,3],1,1) == "")))
  Genfin <- as.data.frame(Genfin[J, ], stringsAsFactors=FALSE)
  
  #Genfin[which(substr(Genfin[,1],5,5) %in% " "),1]=substr(Genfin[which(substr(Genfin[,1],5,5) %in% " "),1],1,4)
  Y <- as.data.frame(gsub("[^0-9]", "", Genfin[,1]),stringsAsFactors=FALSE)
  Genfin <- cbind (Y, Genfin[,2:length(Genfin)])
  colnames(Genfin) <- c("SubsecID","Description",paste(year1,"original",sep = ""),"Revision",paste(year1,"revised",sep = ""))
  Genfin$SubsecID  <- as.numeric(Genfin$SubsecID)
  Genfin[,3]   <- type.convert(Genfin[,3], numerals="warn.loss");
  Genfin[,4]   <- type.convert(Genfin[,4], numerals="warn.loss");
  Genfin[,5]   <- type.convert(Genfin[,5], numerals="warn.loss");
  
  #values with 5 digit indices
  #vol[which(!substr(vol[,1],5,5) %in% ""),1]
  #indices with 5 digit indices
  M <- which(is.na(Genfin[,3]))
  N <- intersect(which(!substr(Genfin[,1],5,5) %in% ""),M)
  
  
  #indices for 2nd NA
  T <- setdiff(M,N)
  #T <- which(is.na(Genfin[,3]))
  #T <- T[!(T %in% c(which(!substr(Genfin[,1],5,5) %in% "")))]
  #vol[T,1]
  
  n <- length(Genfin[,1])
  O1 <- rep(0,n)
  O2 <- rep(0,n)
  O2 <- O2[-which(is.na(Genfin[,3]))]
  counter1=1
  counter2=0
  
  for (i in 1:n){
    if (i==M[counter1]& counter1<=length(M)){
      if(i==T[counter1-counter2]& (counter1-counter2)<=length(T)){
        O2[i-counter1+1]=Genfin[T[counter1-counter2],1]
      }
      else{counter2=counter2+1}
      counter1=1+counter1}
  }
  
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
  Genfin <- cbind(O,Genfin)
  Genfin <- cbind(Genfin[,1],as.data.frame(as.numeric(gsub("42", "0", Genfin[,2])), stringsAsFactors=FALSE),Genfin[,3:7])
  Genfin <- cbind(Genfin[,1:3],as.data.frame(substr(Genfin[,4],2,100),stringsAsFactors=FALSE),Genfin[,5:7])
  colnames(Genfin)[1:2] <- c("Supercode1", "Supercode2")
  colnames(Genfin)[4] <- c("Description")
  
  return(Genfin)
}
  if(year<10){
    V2 <- generator2(as.character(paste("C:/Users/admin/Desktop/DUKE Data+/",paste(year+1,"_",as.numeric(substr(year,4,4))+2,sep = ""),"/vol",vol,".pdf",sep="")))
  }
  else{V2 <- generator2(as.character(paste("C:/Users/admin/Desktop/DUKE Data+/",paste(year+1,"_",as.numeric(substr(year,3,4))+2,sep = ""),"/vol",vol,".pdf",sep="")))}
#########################
 for (i in 1:nrow(V1)) {
   if(is.na(V1[i,3])){V1 <- V1[-i,]}
 }
  for (i in 1:nrow(V2)) {
    if(is.na(V2[i,3])){V2 <- V2[-i,]}
 } 
  
  V22 <- V2[,-c(2,6,7)]
  V23 <- V2
  V12 <- V1[,-c(2,5)]
  V13 <- cbind(V1,0,0,0)
  colnames(V13)[7:9] <- c(colnames(V2)[5],colnames(V2)[6],colnames(V2)[7])
  V24 <- cbind(V2[1:4],0,0,V2[5:7])
  colnames(V24)[5:6] <- c(colnames(V1)[5],colnames(V1)[6])
  
  for (j in 1:nrow(V22)){
    for(i in 1:nrow(V12)){
      if(V12[i,1] == V22[j,1]&V12[i,2] == V22[j,2]&V12[i,3] == V22[j,3]&V12[i,4] == V22[j,4]){
        V24[j,5] <- V1[i,5]
        V24[j,6] <- V1[i,6]
      }
    }
  }



  return(V24[,-c(6:8)])}

Edu03_5 <- DataGen(1,2003)
GenGov03_5 <- DataGen(2,2003)
Health03_5 <- DataGen(3,2003)
Justice03_5 <- DataGen(4,2003)
NatEcoRe03_5 <- DataGen(5,2003)
Trans03_5 <- DataGen(6,2003)

Edu05_7 <- DataGen(1,2005)
GenGov05_7 <- DataGen(2,2005)
Health05_7 <- DataGen(3,2005)
Justice05_7 <- DataGen(4,2005)
NatEcoRe05_7 <- DataGen(5,2005)
Trans05_7 <- DataGen(6,2005)

Edu07_9 <- DataGen(1,2007)
GenGov07_9 <- DataGen(2,2007)
Health07_9 <- DataGen(3,2007)
Justice07_9 <- DataGen(4,2007)
NatEcoRe07_9 <- DataGen(5,2007)
Trans07_9 <- DataGen(6,2007)

Edu09_11 <- DataGen(1,2009)
GenGov09_11 <- DataGen(2,2009)
Health09_11 <- DataGen(3,2009)
Justice09_11 <- DataGen(4,2009)
NatEcoRe09_11 <- DataGen(5,2009)
Trans09_11 <- DataGen(6,2009)

Edu11_13 <- DataGen(1,2011)
GenGov11_13 <- DataGen(2,2011)
Health11_13 <- DataGen(3,2011)
Justice11_13 <- DataGen(4,2011)
NatEcoRe11_13 <- DataGen(5,2011)
Trans11_13 <- DataGen(6,2011)
