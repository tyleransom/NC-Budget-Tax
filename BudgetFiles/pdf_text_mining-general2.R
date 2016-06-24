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
  # extract tables from the PDF
  Genout1 <- as.data.frame(extract_tables(file, pages = 1, guess = FALSE, method = "data.frame", columns = list(c(250,355,420)), stringsAsFactors=FALSE))
  for (i in 2:a) {
    try({
      Out <- as.data.frame(extract_tables(file, pages = i, guess = FALSE, method = "data.frame", columns = list(c(250,355,420)), stringsAsFactors=FALSE))
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
  colnames(Genfin) <- c("SubsecID","Description",paste(year2,"original",sep = ""),"Revision",paste(year2,"revised",sep = ""))
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
  
  O <- cbind(O1,O2)
  Genfin <- as.data.frame(Genfin[-(which(is.na(Genfin[,3]))), ])
  Genfin <- cbind(O,Genfin)
  Genfin <- cbind(Genfin[,1:3],as.data.frame(substr(Genfin[,4],2,100),stringsAsFactors=FALSE),Genfin[,5:7])
  colnames(Genfin)[1:2] <- c("Supercode1", "Supercode2")
  colnames(Genfin)[4] <- c("Description")
  
  return(Genfin)
}

vol62003 <- generator("C:/Users/admin/Desktop/DUKE Data+/2003_5/vol6.pdf")
vol62004 <- generator2("C:/Users/admin/Desktop/DUKE Data+/2004_5/vol6.pdf")


vol620042 <- vol62004[,-c(2,6,7)]
vol620043 <- vol62004
vol620032 <- vol62003[,-c(2,5)]
vol620033 <- cbind(vol62003,0,0,0)
colnames(vol620033)[7:9] <- c(colnames(vol62004)[5],colnames(vol62004)[6],colnames(vol62004)[7])
vol620044 <- cbind(vol62004[1:4],0,0,vol62004[5:7])
colnames(vol620044)[5:6] <- c(colnames(vol62003)[5],colnames(vol62003)[6])


#Loop to merge revised figures of 2nd Year budget with the proposed 2 year budget
#Change variable name according to the year and area of interest
for (i in 1:nrow(vol62003)){
  if (vol620032[i,1:3] == vol620042[i,1:3]){}
      #vol620042 <- vol620042[-i,]
  else{vol620043 <- vol620043[-i,]}
    }
vol620033 <- cbind(vol62003,vol620043[6:7])

#Updated Loop 1
for(i in 1:nrow(vol620032)){
  for (j in 1:nrow(vol620042)){
    if(vol620032[i,1] == vol620042[j,1]&vol620032[i,2] == vol620042[j,2]&vol620032[i,3] == vol620042[j,3]&vol620032[i,4] == vol620042[j,4]){
      vol620033[i,7] = vol62004[j,5]
      vol620033[i,8] = vol62004[j,6]
      vol620033[i,9] = vol62004[j,7]
       }
    }
}
#Updated Loop 2
for (j in 1:nrow(vol620042)){
  for(i in 1:nrow(vol620032)){
    if(vol620032[i,1] == vol620042[j,1]&vol620032[i,2] == vol620042[j,2]&vol620032[i,3] == vol620042[j,3]&vol620032[i,4] == vol620042[j,4]){
      vol62003[i,5] -> vol620044[j,5]
      vol62003[i,6] -> vol620044[j,6]
    }
  }
}



#Change path according to the files on your PC

vol62004 <- generator("C:/Users/admin/Desktop/DUKE Data+/2004_5/vol6.pdf")
vol62006 <- generator("C:/Users/admin/Desktop/DUKE Data+/2006_7/vol6.pdf")
vol62008 <- generator("C:/Users/admin/Desktop/DUKE Data+/2008_9/vol6.pdf")
vol62010 <- generator("C:/Users/admin/Desktop/DUKE Data+/2010_11/vol6.pdf")
vol62012 <- generator("C:/Users/admin/Desktop/DUKE Data+/2011_13/vol6.pdf")

vol52004 <- generator("C:/Users/admin/Desktop/DUKE Data+/2004_5/vol5.pdf")
vol52006 <- generator("C:/Users/admin/Desktop/DUKE Data+/2006_7/vol5.pdf")
vol52008 <- generator("C:/Users/admin/Desktop/DUKE Data+/2008_9/vol5.pdf")
vol52010 <- generator("C:/Users/admin/Desktop/DUKE Data+/2010_11/vol5.pdf")
vol52012 <- generator("C:/Users/admin/Desktop/DUKE Data+/2011_13/vol5.pdf")

vol42004 <- generator("C:/Users/admin/Desktop/DUKE Data+/2004_5/vol4.pdf")
vol42006 <- generator("C:/Users/admin/Desktop/DUKE Data+/2006_7/vol4.pdf")
vol42008 <- generator("C:/Users/admin/Desktop/DUKE Data+/2008_9/vol4.pdf")
vol42010 <- generator("C:/Users/admin/Desktop/DUKE Data+/2010_11/vol4.pdf")
vol42012 <- generator("C:/Users/admin/Desktop/DUKE Data+/2011_13/vol4.pdf")

vol32004 <- generator("C:/Users/admin/Desktop/DUKE Data+/2004_5/vol3.pdf")
vol32006 <- generator("C:/Users/admin/Desktop/DUKE Data+/2006_7/vol3.pdf")
vol32008 <- generator("C:/Users/admin/Desktop/DUKE Data+/2008_9/vol3.pdf")
vol32010 <- generator("C:/Users/admin/Desktop/DUKE Data+/2010_11/vol3.pdf")
vol32012 <- generator("C:/Users/admin/Desktop/DUKE Data+/2011_13/vol3.pdf")

vol22004 <- generator("C:/Users/admin/Desktop/DUKE Data+/2004_5/vol2.pdf")
vol22006 <- generator("C:/Users/admin/Desktop/DUKE Data+/2006_7/vol2.pdf")
vol22008 <- generator("C:/Users/admin/Desktop/DUKE Data+/2008_9/vol2.pdf")
vol22010 <- generator("C:/Users/admin/Desktop/DUKE Data+/2010_11/vol2.pdf")
vol22012 <- generator("C:/Users/admin/Desktop/DUKE Data+/2011_13/vol2.pdf")

vol12004 <- generator("C:/Users/admin/Desktop/DUKE Data+/2004_5/vol1.pdf")
vol12006 <- generator("C:/Users/admin/Desktop/DUKE Data+/2006_7/vol1.pdf")
vol12008 <- generator("C:/Users/admin/Desktop/DUKE Data+/2008_9/vol1.pdf")
vol12010 <- generator("C:/Users/admin/Desktop/DUKE Data+/2010_11/vol1.pdf")
vol12012 <- generator("C:/Users/admin/Desktop/DUKE Data+/2011_13/vol1.pdf")
