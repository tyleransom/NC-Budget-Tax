rm(list = ls())
cat("\014")

# Script to install and run tabulizer to extract tables from PDFs 

# packages necessary to install and run tabulizer. You might have to reinstall Java (see tabulizer package website)
require(devtools)
require(rJava)
require(tabulizer)

# change to your location like: "C:/Users/BLAH/Desktop/Data+/downloader.bat"
dloader <- read.csv("C:/Users/Tom/Desktop/Data+/downloader.bat")

# create a database of URLs for further extraction
for (i in 1:100){
  if (substr(droplevels(dloader[2,]),i,i+3)=="http"){
    n <- i
  }
}
N=list()
for (j in 1:nrow(dloader)){
  N[j]=substr(droplevels(dloader[j,]),n,200)
}
N <- N[-which(N=="")]
N <- gsub("\\\\", "/",N)

# this is our URL library
URLLib<- unname(t(as.data.frame(strsplit(as.character(N)," -O "),stringsAsFactors = FALSE)))

# main function to extract data from pdfs and create tables
generator <- function(file){# associate the file you want to process with a URL from library
  file1 <- file
  file <- URLLib[which(URLLib[,2]==file),1]
  a <- get_n_pages(file=file)
  
  # if you have pdf files downloaded and want to process them offline add  here the following:
  #file <- paste("C:/Users/BLAH/Desktop/Data+/",file1,sep="")
  # naturally, make sure the path is correct
  
  # calculate number of digits and assign years for column names
  digits = floor(log10(as.numeric(substr(gsub("[^0-9]", "", file1),3,4)))) + 1
  if (digits==1){
    year1 <- paste(substr(gsub("[^0-9]", "", file1),1,4), as.numeric(substr(gsub("[^0-9]", "", file1),3,4))+1, sep = "0")
    year1 <- paste("y",year1, sep = "")
    year2 <- paste(substr(gsub("[^0-9]", "", file1),1,3), as.numeric(substr(gsub("[^0-9]", "", file1),3,4))+1, sep = "")
    year2 <- paste(year2, as.numeric(substr(gsub("[^0-9]", "", file1),3,4))+2, sep = "0")
    year2 <- paste("y",year2, sep = "")
  } else {
    year1 <- paste(substr(gsub("[^0-9]", "", file1),1,4), as.numeric(substr(gsub("[^0-9]", "", file1),3,4))+1, sep = "")
    year1 <- paste("y",year1, sep = "")
    year2 <- paste(substr(gsub("[^0-9]", "", file1),1,2), as.numeric(substr(gsub("[^0-9]", "", file1),3,4))+1, sep = "")
    year2 <- paste(year2, as.numeric(substr(gsub("[^0-9]", "", file1),3,4))+2, sep = "")
    year2 <- paste("y",year2, sep = "")
  }
  
  #-----------------------------------EVEN budgets (for instance, revised 2004-2005)
  if((as.numeric(substr(gsub("[^0-9]", "", file1),3,4)) %% 2) == 0){
    # extract tables from the PDF
    Genout1 <- as.data.frame(extract_tables(file, pages = 1, guess = FALSE, method = "data.frame", stringsAsFactors=FALSE))
    for (i in 2:a) {
      try({
        Out <- as.data.frame(extract_tables(file, pages = i, guess = FALSE, method = "data.frame", stringsAsFactors=FALSE))
        if (length(Out)==3){Out=data.frame(Out[,1],paste(Out[,2],Out[,3],sep=""))}
        names(Out) <- names(Genout1)
        # skip any pages that contain the word "SUMMARY"
        if ((length(grep("SUMMARY", Out[,1]))==0)&(length(grep("SUMMARY", Out[,2]))==0)){
          Genout1 <- rbind(Genout1,Out)
        }
      })
    }
    
    # delete all "------------------------------"s
    Genout1 <- as.data.frame(Genout1[!(as.data.frame(substr(Genout1[,1],1,1)) == "-"), ], stringsAsFactors=FALSE)
    
    # assign synthetic codes to collective articles
    Genout2=Genout1
    Gen2 <- as.data.frame(gsub("TOTAL", "00001 TOTAL", Genout2[,1]), stringsAsFactors=FALSE)
    Gen3 <- cbind(Gen2, Genout2[,2])
    
    Gen2 <- as.data.frame(gsub("NET", "00002 NET", Gen3[,1]), stringsAsFactors=FALSE)
    Gen3 <- cbind(Gen2, Gen3[,2])
    
    Gen2 <- as.data.frame(gsub("HIGHWAY FUND APPROPRIATION", "00003 HIGHWAY FUND APPROPRIATION", Gen3[,1]), stringsAsFactors=FALSE)
    Gen3 <- cbind(Gen2, Gen3[,2])
    
    Gen2 <- as.data.frame(gsub("HIGHWAY TRUST FUND APPROPRTN", "00004 HIGHWAY TRUST FUND APPROPRTN", Gen3[,1]), stringsAsFactors=FALSE)
    Gen3 <- cbind(Gen2, Gen3[,2])
    
    Gen2 <- as.data.frame(gsub("CHANGE IN FUND BALANCE", "00005 CHANGE IN FUND BALANCE", Gen3[,1]),stringsAsFactors=FALSE)
    Gen3 <- cbind(Gen2, Gen3[,2])
    
    # delete weird spaces and some lines that have nothing to contribute
    Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,2],1,4)) == "PAGE"), ],stringsAsFactors=FALSE)
    Gen3 <- as.data.frame(Gen3[!(substr(Gen3[,1],1,3) %in% c("APP", "BUD", "REQ", "DES", "EST", "POS", "SUM")), ],stringsAsFactors=FALSE)
    Gen3 <- as.data.frame(Gen3[!(substr(Gen3[,2],1,3) %in% "AWG"), ],stringsAsFactors=FALSE)
    Gen3 <- as.data.frame(Gen3[!(substr(Gen3[,2],1,3) %in% "REV"), ],stringsAsFactors=FALSE)
    Gen2 <- as.data.frame(gsub(",","",Gen3[,1]),stringsAsFactors=FALSE)
    Gen42 <- as.data.frame(gsub(",","",Gen3[,2]),stringsAsFactors=FALSE)
    Gen3 <- cbind(Gen2, Gen42)
    Gen3 <- Gen3[!apply(Gen3 == "", 1, all),]
    Gen3 <- Gen3
    Gen3 <- cbind(as.data.frame(gsub("  "," ",Gen3[,1]), stringsAsFactors=FALSE),Gen3[,2])
    Gen31 <- Gen3
    
    # identify the length of the indices (account code/budget code or whatever a line starts with as a code)
    is.number <- function(x) grepl("[[:digit:]]", x)
    L=rep(0,length(Gen3[,1]))
    for (i in 1:length(Gen3[,1])){
      j=3
      repeat{
        j=j+1
        k <- as.data.frame(substr(Gen31[i,1],j,j),stringsAsFactors=FALSE)
        if(is.number(k)==FALSE){L[i]=j-1 
        break}
      }
    }
    
    # whereever description is missing assign "MISSING"
    Gen31[which(L>8),1]=paste("0000MISSING",Gen31[which(L>8),1],sep="")
    L[which(L>8)]=4
    
    Gen3 <- paste(Gen31[,1], gsub(" ","", Gen31[,2]), sep="")
    
    # identify the lines that have some numbers (that is are not just fund code with description from top of page)
    # call those Targetrows
    L2=rep(0,length(Gen3))
    for (j in 1:60){
      k <- as.data.frame(substr(Gen3,j,j),stringsAsFactors=FALSE)
      for (i in 1:length(k[,1])){
        if(is.number(k[i,1])==TRUE){L2[i]=j}
      }}
    
    Gen31[,2] <- gsub("-","",Gen31[,2])  
    I2 <- which(is.na(as.numeric(as.character(Gen31[,2]))))
    Targetrows <- setdiff(which(L2>10),I2)
    
    # find the values (numbers at the end) associated with each row, call them Targevalues
    Targetmatrix <- Gen3[Targetrows]
    L1=rep(0,length(Targetmatrix))
    Targetvalues <- rep(0,length(Targetmatrix))
    for (i in 1:length(Targetmatrix)){
      j=0
      repeat{
        j=j+1
        k <- as.data.frame(substr(Targetmatrix[i],nchar(Targetmatrix[i])-j+1,nchar(Targetmatrix[i])-j+1),stringsAsFactors=FALSE)
        if((is.number(k)==FALSE)&((k=="-")==FALSE)&((k==" ")==FALSE)){L1[i]=j-1 
        break}
      }
      Targetvalues[i]=substr(Targetmatrix[i],nchar(Targetmatrix[i])+1-L1[i],nchar(Targetmatrix[i]))
    }
    Targetvalues <- gsub(" ","",Targetvalues)
    
    # "Triple centrifuge" algorithm to split strings into columns
    # Idea: you have a string of the type "SomeDescription123456123456" (stored in Targetmatrix)
    # and the numbers at the end "123456123456" stored in Targetvalues
    # find combinations of the numbers in this string such that number1+number2=number3
    # equation comes from: original year+revision=revised year
    Negatives <- grep('-', Targetvalues)
    col1 <- rep(NA,length(Targetmatrix))
    col2 <- rep(NA,length(Targetmatrix))
    col3 <- rep(NA,length(Targetmatrix))
    
    for (i in 1:length(Targetmatrix)){# case 1: no minuses in the number
      if (length(which(i==Negatives))==0){ # simplest case every number is a separate column
        if(nchar(Targetvalues[i])==3){
          col1[i] <- as.numeric(substr(Targetvalues[i],1,1))
          col2[i] <- as.numeric(substr(Targetvalues[i],2,2))
          col3[i] <- as.numeric(substr(Targetvalues[i],3,3))
        }
        else{# weird case with just one digit => push it to all columns
          if(nchar(Targetvalues[i])==1){
            col1[i] <- as.numeric(substr(Targetvalues[i],1,1))
            col2[i] <- as.numeric(substr(Targetvalues[i],1,1))
            col3[i] <- as.numeric(substr(Targetvalues[i],1,1))
          }
          else{# standard case (one string, three columns)/ triple centrifuge
            fixlast=0
            repeat{fixlast=fixlast+1
            if (fixlast>nchar(Targetvalues[i])){break}
            v3 <- as.numeric(substr(Targetvalues[i],nchar(Targetvalues[i])-fixlast+1,nchar(Targetvalues[i])))
            # hereinafter variations of these 6 lines are "checksums" for big numbers
            #--------------------
            newlength3=nchar(v3)
            if (!(length(grep("e",v3))==0)){j=0 
            repeat{j=j+1 
            if (substr(v3,j,j)=="e"){newlength3 <- nchar(floor(as.numeric(as.character(substr(v3,1,j-1)))))+(as.numeric(substr(v3,j+1,nchar(v3))))
            break}}}
            if (!(newlength3==nchar(substr(Targetvalues[i],nchar(Targetvalues[i])-fixlast+1,nchar(Targetvalues[i]))))){next}
            #--------------------
            uroll=0
            repeat{uroll=uroll+1
            if ((uroll+fixlast)>nchar(Targetvalues[i])){break}
            v2 <- as.numeric(substr(Targetvalues[i],nchar(Targetvalues[i])-fixlast-uroll+1,nchar(Targetvalues[i])-fixlast))
            newlength2=nchar(v2)
            if (!(length(grep("e",v2))==0)){j=0 
            repeat{j=j+1 
            if (substr(v2,j,j)=="e"){newlength2 <- nchar(floor(as.numeric(as.character(substr(v2,1,j-1)))))+(as.numeric(substr(v2,j+1,nchar(v2))))
            break}}}
            if (!(newlength2==nchar(substr(Targetvalues[i],nchar(Targetvalues[i])-fixlast-uroll+1,nchar(Targetvalues[i])-fixlast)))){next}
            vroll=0
            repeat{vroll=vroll+1
            if ((uroll+fixlast+vroll)>nchar(Targetvalues[i])){break}
            v1 <- as.numeric(substr(Targetvalues[i],nchar(Targetvalues[i])-fixlast-uroll-vroll+1,nchar(Targetvalues[i])-fixlast-uroll))
            if (((v1+v2)==v3)&(!(length(which(cbind(v1,v2,v3)==0))==3))){
              col1[i] <- v1
              col2[i] <- v2
              col3[i] <- v3
              next
            }
            }
            }
            }
            
            if (is.na(col1[i])|is.na(col2[i])|is.na(col3[i])){# case of all zeros in the line (000)
              v3 <- as.numeric(as.character(substr(Targetvalues[i],nchar(Targetvalues[i]),nchar(Targetvalues[i]))))
              v2 <- as.numeric(as.character(substr(Targetvalues[i],nchar(Targetvalues[i])-1,nchar(Targetvalues[i])-1))) 
              v1 <- as.numeric(as.character(substr(Targetvalues[i],nchar(Targetvalues[i])-2,nchar(Targetvalues[i])-2))) 
              if (length(which(cbind(v1,v2,v3)==0))==3){
                col1[i] <- v1
                col2[i] <- v2
                col3[i] <- v3
              }
            }
          }
        }
      }
      else{# case 2: lines with identified negatives
        # prepare the values
        A <- unlist(strsplit(Targetvalues[i],"(?<=-)",perl=TRUE))
        Minuses <- rep(0,length(A))
        for (j in 2:length(A)){
          if(substr(A[j-1],nchar(A[j-1]),nchar(A[j-1]))=="-"){
            A[j-1] <- substr(A[j-1],1,nchar(A[j-1])-1)
            A[j] <- paste("-",A[j],sep="")
            Minuses[j] <- 1
          }
        }
        if (A[1]=="") {A <- A[-1]
        Minuses <- Minuses[2:length(Minuses)]}
        
        if (length(A)==3){# simplest case when every element is the column itself
          if ((as.numeric(as.character(A[1]))+as.numeric(as.character(A[2])))==as.numeric(as.character(A[3]))){
            col1[i] <- A[1]
            col2[i] <- A[2]
            col3[i] <- A[3]
          }
          else{# complicated case with A[1] storing weird stuff:
            # CASE1: col 3 is big => double centrifuge
            v1 <- as.numeric(as.character(A[2]))
            dummy32=0
            repeat{dummy32=dummy32+1
            if (dummy32>=nchar(A[3])-Minuses[3]){break}
            v3 <- as.numeric(as.character(substr(A[3],nchar(A[3])-dummy32+1,nchar(A[3]))))
            newlength3=nchar(v3)
            if (!(length(grep("e",v3))==0)){j=0 
            repeat{j=j+1 
            if (substr(v3,j,j)=="e"){newlength3 <- nchar(round(as.numeric(as.character(substr(v3,1,j-1)))))+(as.numeric(substr(v3,j+1,nchar(v3))))
            break}}}
            if (!(newlength3==nchar(substr(A[3],nchar(A[3])-dummy32+1,nchar(A[3]))))){next}
            dummy31=0
            repeat{dummy31=dummy31+1
            if ((dummy32+dummy31)>nchar(A[3])){break}
            v2 <- as.numeric(as.character(substr(A[3],nchar(A[3])-dummy32-dummy31+1,nchar(A[3])-dummy32)))
            newlength2=nchar(v2)
            if (!(length(grep("e",v2))==0)){j=0 
            repeat{j=j+1 
            if (substr(v2,j,j)=="e"){newlength2 <- nchar(round(as.numeric(as.character(substr(v2,1,j-1)))))+(as.numeric(substr(v2,j+1,nchar(v2))))
            break}}}
            if (!(newlength2==nchar(substr(A[3],nchar(A[3])-dummy32-dummy31+1,nchar(A[3])-dummy32)))){next}
            if (((v1+v2)==v3)&(!(length(which(cbind(v1,v2,v3)==0))==3))){
              col1[i] <- v1
              col2[i] <- v2
              col3[i] <- v3
              next
            }
            }
            }
            
            
            # CASE 2: col 2 is big => double centrifuge
            if (is.na(col1[i])|is.na(col2[i])|is.na(col3[i])){
              v3 <- as.numeric(as.character(A[3]))
              dummy22=0
              repeat{dummy22=dummy22+1
              if (dummy22>=nchar(A[2])-Minuses[2]){break}
              v2 <- as.numeric(as.character(substr(A[2],nchar(A[2])-dummy22+1,nchar(A[2]))))
              newlength2=nchar(v2)
              if (!(length(grep("e",v2))==0)){j=0 
              repeat{j=j+1 
              if (substr(v2,j,j)=="e"){newlength2 <- nchar(round(as.numeric(as.character(substr(v2,1,j-1)))))+(as.numeric(substr(v2,j+1,nchar(v2))))
              break}}}
              if (!(newlength2==nchar(substr(A[2],nchar(A[2])-dummy22+1,nchar(A[2]))))){next}
              dummy21=0
              repeat{dummy21=dummy21+1
              if ((dummy22+dummy21)>nchar(A[2])){break}
              v1 <- as.numeric(as.character(substr(A[2],nchar(A[2])-dummy22-dummy21+1,nchar(A[2])-dummy22)))
              newlength1=nchar(v1)
              if (!(length(grep("e",v1))==0)){j=0 
              repeat{j=j+1 
              if (substr(v1,j,j)=="e"){newlength1 <- nchar(round(as.numeric(as.character(substr(v1,1,j-1)))))+(as.numeric(substr(v1,j+1,nchar(v1))))
              break}}}
              if (!(newlength1==nchar(substr(A[2],nchar(A[2])-dummy22-dummy21+1,nchar(A[2])-dummy22)))){next}
              if (((v1+v2)==v3)&(!(length(which(cbind(v1,v2,v3)==0))==3))){
                col1[i] <- v1
                col2[i] <- v2
                col3[i] <- v3
                next
              }
              }
              }
            }
          }
        }
        
        else{# main case of negatives without complications
          if (length(A)==2){# 2nd col is big/double centrifuge
            v1 <- as.numeric(as.character(A[1]))
            uroll=0
            repeat{uroll=uroll+1
            if (uroll>=nchar(A[2])-Minuses[2]){break}
            v3 <- as.numeric(as.character(substr(A[2],nchar(A[2])-uroll+1,nchar(A[2]))))
            newlength3=nchar(v3)
            if (!(length(grep("e",v3))==0)){j=0 
            repeat{j=j+1 
            if (substr(v3,j,j)=="e"){newlength3 <- nchar(round(as.numeric(as.character(substr(v3,1,j-1)))))+(as.numeric(substr(v3,j+1,nchar(v3))))
            break}}}
            if (!(newlength3==nchar(substr(A[2],nchar(A[2])-uroll+1,nchar(A[2]))))){next}
            v2 <- as.numeric(as.character(substr(A[2],1,nchar(A[2])-uroll)))
            newlength2=nchar(v2)
            if (!(length(grep("e",v2))==0)){j=0 
            repeat{j=j+1 
            if (substr(v2,j,j)=="e"){newlength2 <- nchar(round(as.numeric(as.character(substr(v2,1,j-1)))))+(as.numeric(substr(v2,j+1,nchar(v2))))
            break}}}
            if (!(newlength2==nchar(substr(A[2],1,nchar(A[2])-uroll)))){next}
            if (((v1+v2)==v3)&(!(length(which(cbind(v1,v2,v3)==0))==3))){
              col1[i] <- v1
              col2[i] <- v2
              col3[i] <- v3
              next
            }
            }
            
            
            # 1st col is big
            if (is.na(col1[i])|is.na(col2[i])|is.na(col3[i])){
              v3 <- as.numeric(as.character(A[2]))
              vroll=0
              repeat{vroll=vroll+1
              if (vroll>=nchar(A[1])-Minuses[1]){break}
              v2 <- as.numeric(as.character(substr(A[1],nchar(A[1])-vroll+1,nchar(A[1]))))
              newlength2=nchar(v2)
              if (!(length(grep("e",v2))==0)){j=0 
              repeat{j=j+1 
              if (substr(v2,j,j)=="e"){newlength2 <- nchar(round(as.numeric(as.character(substr(v2,1,j-1)))))+(as.numeric(substr(v2,j+1,nchar(v2))))
              break}}}
              if (!(newlength2==nchar(substr(A[1],nchar(A[1])-vroll+1,nchar(A[1]))))){next}
              v1 <- as.numeric(as.character(substr(A[1],1,nchar(A[1])-vroll)))
              newlength1=nchar(v1)
              if (!(length(grep("e",v1))==0)){j=0 
              repeat{j=j+1 
              if (substr(v1,j,j)=="e"){newlength1 <- nchar(round(as.numeric(as.character(substr(v1,1,j-1)))))+(as.numeric(substr(v1,j+1,nchar(v1))))
              break}}}
              if (!(newlength1==nchar(substr(A[1],1,nchar(A[1])-vroll)))){next}
              if (((v1+v2)==v3)&(!(length(which(cbind(v1,v2,v3)==0))==3))){
                col1[i] <- v1
                col2[i] <- v2
                col3[i] <- v3
                next
              }
              }
            }
            
            # A1 is weird and IS needed (description contains numbers and weird symbols inbetween)
            # triple centrifuge
            if (is.na(col1[i])|is.na(col2[i])|is.na(col3[i])){dummy22=0
            #2nd col is big (2 values)
            repeat{dummy22=dummy22+1
            if (dummy22>=nchar(A[2])-Minuses[2]){break}
            v3 <- as.numeric(as.character(substr(A[2],nchar(A[2])-dummy22+1,nchar(A[2]))))
            newlength3=nchar(v3)
            if (!(length(grep("e",v3))==0)){j=0 
            repeat{j=j+1 
            if (substr(v3,j,j)=="e"){newlength3 <- nchar(round(as.numeric(as.character(substr(v3,1,j-1)))))+(as.numeric(substr(v3,j+1,nchar(v3))))
            break}}}
            if (!(newlength3==nchar(substr(A[2],nchar(A[2])-dummy22+1,nchar(A[2]))))){next}
            
            dummy21=0
            repeat{dummy21=dummy21+1
            if ((dummy22+dummy21)>nchar(A[2])){break}
            v2 <- as.numeric(as.character(substr(A[2],nchar(A[2])-dummy22-dummy21+1,nchar(A[2])-dummy22)))
            newlength2=nchar(v2)
            if (!(length(grep("e",v2))==0)){j=0 
            repeat{j=j+1 
            if (substr(v2,j,j)=="e"){newlength2 <- nchar(round(as.numeric(as.character(substr(v2,1,j-1)))))+(as.numeric(substr(v2,j+1,nchar(v2))))
            break}}}
            if (!(newlength2==nchar(substr(A[2],nchar(A[2])-dummy22-dummy21+1,nchar(A[2])-dummy22)))){next}
            
            dummy1=0  
            repeat{dummy1=dummy1+1
            if (dummy1>nchar(A[1])){break}
            v1 <- as.numeric(as.character(substr(A[1],nchar(A[1])-dummy1+1,nchar(A[1]))))
            newlength1=nchar(v1)
            if (!(length(grep("e",v1))==0)){j=0 
            repeat{j=j+1 
            if (substr(v1,j,j)=="e"){newlength1 <- nchar(round(as.numeric(as.character(substr(v1,1,j-1)))))+(as.numeric(substr(v1,j+1,nchar(v1))))
            break}}}
            if (!(newlength1==nchar(substr(A[1],nchar(A[1])-dummy1+1,nchar(A[1]))))){next}
            
            if (((v1+v2)==v3)&(!(length(which(cbind(v1,v2,v3)==0))==3))){
              col1[i] <- v1
              col2[i] <- v2
              col3[i] <- v3
              next
            }
            }
            }
            }
            
            #1st col is big (2 values => 2nd col is v3)
            #double centrifuge
            if (is.na(col1[i])|is.na(col2[i])|is.na(col3[i])){
              v3 <- as.numeric(as.character(A[2]))
              dummy12=0
              repeat{dummy12=dummy12+1
              if (dummy12>=nchar(A[1])-Minuses[1]){break}
              v2 <- as.numeric(as.character(substr(A[1],nchar(A[1])-dummy12+1,nchar(A[1]))))
              newlength2=nchar(v2)
              if (!(length(grep("e",v2))==0)){j=0 
              repeat{j=j+1 
              if (substr(v2,j,j)=="e"){newlength2 <- nchar(round(as.numeric(as.character(substr(v2,1,j-1)))))+(as.numeric(substr(v2,j+1,nchar(v2))))
              break}}}
              if (!(newlength2==nchar(substr(A[1],nchar(A[1])-dummy12+1,nchar(A[1]))))){next}
              dummy11=0
              repeat{dummy11=dummy11+1
              if ((dummy12+dummy11)>nchar(A[1])){break}
              v1 <- as.numeric(as.character(substr(A[1],nchar(A[1])-dummy12-dummy11+1,nchar(A[1])-dummy12)))
              newlength2=nchar(v1)
              if (!(length(grep("e",v1))==0)){j=0 
              repeat{j=j+1 
              if (substr(v1,j,j)=="e"){newlength1 <- nchar(round(as.numeric(as.character(substr(v1,1,j-1)))))+(as.numeric(substr(v1,j+1,nchar(v1))))
              break}}}
              if (!(newlength1==nchar(substr(A[1],nchar(A[1])-dummy12-dummy11+1,nchar(A[1])-dummy12)))){next}
              
              if (((v1+v2)==v3)&(!(length(which(cbind(v1,v2,v3)==0))==3))){
                col1[i] <- v1
                col2[i] <- v2
                col3[i] <- v3
                next
              }
              }
              }
            }
            }
            
            
            # A1 is weird and IS NOT needed (description contains numbers and weird symbols inbetween)
            # triple centrifuge
            if (is.na(col1[i])|is.na(col2[i])|is.na(col3[i])){
              fixlast=0
              repeat{fixlast=fixlast+1
              if (fixlast>=nchar(A[2])-Minuses[2]){break}
              v3 <- as.numeric(substr(A[2],nchar(A[2])-fixlast+1,nchar(A[2])))
              newlength3=nchar(v3)
              if (!(length(grep("e",v3))==0)){j=0 
              repeat{j=j+1 
              if (substr(v3,j,j)=="e"){newlength3 <- nchar(round(as.numeric(as.character(substr(v3,1,j-1)))))+(as.numeric(substr(v3,j+1,nchar(v3))))
              break}}}
              if (!(newlength3==nchar(substr(A[2],nchar(A[2])-fixlast+1,nchar(A[2]))))){next}
              uroll=0
              repeat{uroll=uroll+1
              if ((uroll+fixlast)>=nchar(A[2])-Minuses[2]){break}
              v2 <- as.numeric(substr(A[2],nchar(A[2])-fixlast-uroll+1,nchar(A[2])-fixlast))
              newlength2=nchar(v2)
              if (!(length(grep("e",v2))==0)){j=0 
              repeat{j=j+1 
              if (substr(v2,j,j)=="e"){newlength2 <- nchar(round(as.numeric(as.character(substr(v2,1,j-1)))))+(as.numeric(substr(v2,j+1,nchar(v2))))
              break}}}
              if (!(newlength2==nchar(substr(A[2],nchar(A[2])-fixlast-uroll+1,nchar(A[2])-fixlast)))){next}
              vroll=0
              repeat{vroll=vroll+1
              if ((uroll+fixlast+vroll)>nchar(A[2])){break}
              v1 <- as.numeric(substr(A[2],nchar(A[2])-fixlast-uroll-vroll+1,nchar(A[2])-fixlast-uroll))
              if (((v1+v2)==v3)&(!(length(which(cbind(v1,v2,v3)==0))==3))){
                col1[i] <- v1
                col2[i] <- v2
                col3[i] <- v3
                next
              }
              }
              }
              }
            }
          }
          else{#lengthA=1
            fixlast=0
            repeat{fixlast=fixlast+1
            if (fixlast>=nchar(A[1])-Minuses[1]){break}
            v3 <- as.numeric(as.character(substr(A[1],nchar(A[1])-fixlast+1,nchar(A[1]))))
            newlength3=nchar(v3)
            if (!(length(grep("e",v3))==0)){j=0 
            repeat{j=j+1 
            if (substr(v3,j,j)=="e"){newlength3 <- nchar(round(as.numeric(as.character(substr(v3,1,j-1)))))+(as.numeric(substr(v3,j+1,nchar(v3))))
            break}}}
            if (!(newlength3==nchar(substr(A[1],nchar(A[1])-fixlast+1,nchar(A[1]))))){next}
            uroll=0
            repeat{uroll=uroll+1
            if (uroll+fixlast>=nchar(A[1])-Minuses[1]){break}
            v2 <- as.numeric(as.character(substr(A[1],nchar(A[1])-uroll-fixlast+1,nchar(A[1])-fixlast)))
            newlength2=nchar(v2)
            if (!(length(grep("e",v2))==0)){j=0 
            repeat{j=j+1 
            if (substr(v2,j,j)=="e"){newlength2 <- nchar(round(as.numeric(as.character(substr(v2,1,j-1)))))+(as.numeric(substr(v2,j+1,nchar(v2))))
            break}}}
            if (!(newlength2==nchar(substr(A[1],nchar(A[1])-uroll-fixlast+1,nchar(A[1])-fixlast)))){next}
            vroll=0
            repeat{vroll=vroll+1
            if (vroll+uroll+fixlast>nchar(A[1])){break}
            v1 <- as.numeric(as.character(substr(A[1],nchar(A[1])-uroll-vroll-fixlast+1,nchar(A[1])-fixlast-uroll)))
            if (((v1+v2)==v3)&(!(length(which(cbind(v1,v2,v3)==0))==3))){
              col1[i] <- v1
              col2[i] <- v2
              col3[i] <- v3
              next
            }
            }
            }
            }
          }
        }
      }
    }
    
    # using length of indices calculated earlier identify what those indices and description are
    is.space <- function(x) grepl(" ", x)
    numbersnew=rep(0,length(L))
    lettersnew=c()
    h=matrix(0,length(L),3)
    L11=rep(0,length(L))
    L11[Targetrows]=L1
    
    for (i in 1:length(L)){
      numbersnew[i]=as.data.frame(substr(Gen3[i],1,L[i]),stringsAsFactors=FALSE)
      lettersnew[i] <- as.data.frame(substr(Gen3[i], L[i] + 1, nchar(Gen3[i])-L11[i]),stringsAsFactors=FALSE)
      for (j in 1:3){
        if(is.space(substr(lettersnew[i],j,j))==TRUE){
          h[i,j]=1
        }
      }
      lettersnew[i] <- as.data.frame(substr(Gen3[i], L[i]+1+sum(h[i,]),nchar(Gen3[i])-L11[i]),stringsAsFactors=FALSE)
    }
    
    # numbersnew are the indicesm lettersnew are descriptions
    w <- cbind(numbersnew,lettersnew)
    col11 <- rep(NA,length(Gen3))
    col11[Targetrows] <- col1
    col21 <- rep(NA,length(Gen3))
    col21[Targetrows] <- col2
    col31 <- rep(NA,length(Gen3))
    col31[Targetrows] <- col3
    Genfin <- cbind(w, col11, col21, col31)
    
    Y <- as.data.frame(gsub("[^0-9]", "", Genfin[,1]),stringsAsFactors=FALSE)
    Genfin <- cbind (Y, Genfin[,2:length(Genfin[1,])])
    
    # assign numbers to weird lines where TOTAL something have NAs somewhere
    c1 <- is.na(col11)
    c2 <- is.na(col21)
    c3 <- is.na(col31)
    cut <- (substr(Genfin[,2],1,5) %in% "TOTAL")
    Genfin[which(c1&cut),3]=0
    Genfin[which(c2&cut),4]=0
    Genfin[which(c3&cut),5]=0
    
    # just to make sure negative numbers stay negative and decimal separators are treated as expected
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
    
    R1[grep("e",Genfin[,3]),] <- as.numeric(as.character(Genfin[grep("e",Genfin[,3]),3]))
    R2[grep("e",Genfin[,4]),] <- as.numeric(as.character(Genfin[grep("e",Genfin[,4]),4]))
    R3[grep("e",Genfin[,5]),] <- as.numeric(as.character(Genfin[grep("e",Genfin[,5]),5]))
    
    Genfin <- cbind(Genfin[,1:2],R1,R2,R3)
    J <- !((as.data.frame(substr(Genfin[,1],1,1)) == "") & (as.data.frame(substr(Genfin[,3],1,1) == "")))
    Genfin <- as.data.frame(Genfin[J, ], stringsAsFactors=FALSE)
    I <- intersect(which(Genfin[,1]==233),which(Genfin[,2]=="OFFICE O"))
    if (!(length(I)==0)){
      Genfin <- Genfin[-I,]
    }
    
    Genfin <- Genfin[!(is.na(Genfin[,1])),]
    
    # assign names, convert numbers into numeric
    colnames(Genfin) <- c("SubsecID","Description",paste(year1,"original",sep = ""),"Revision",paste(year1,"revised",sep = ""))
    Genfin[,3]   <- type.convert(Genfin[,3], numerals="warn.loss");
    Genfin[,4]   <- type.convert(Genfin[,4], numerals="warn.loss");
    Genfin[,5]   <- type.convert(Genfin[,5], numerals="warn.loss");
  }
  
  #-------------------------------------ODD bugets (for instance, 2003-05)
  else{# everything is pretty much the same as even but much easier (so, see EVEN) 
    # extract tables from the PDF
    Genout1 <- as.data.frame(extract_tables(file, pages = 1, guess = FALSE, method = "data.frame", columns = list(c(320, 420)), stringsAsFactors=FALSE))
    for (i in 2:a) {
      try({
        Out <- as.data.frame(extract_tables(file, pages = i, guess = FALSE, method = "data.frame", columns = list(c(320, 420)), stringsAsFactors=FALSE))
        names(Out) <- names(Genout1)
        if (length(grep("SUMMARY", Out[,1]))==0){
          Genout1 <- rbind(Genout1,Out)
        }
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
    
    c1 <- is.na(Genfin[,3])
    c2 <- is.na(Genfin[,4])
    cut <- (substr(Genfin[,2],1,5) %in% "TOTAL")
    Genfin[which(c1&cut),3]=0
    Genfin[which(c2&cut),4]=0
    
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
    Genfin[,3]   <- type.convert(Genfin[,3], numerals="warn.loss");
    Genfin[,4]   <- type.convert(Genfin[,4], numerals="warn.loss");
  }
  
  #----------------------COMMON part
  
  # find lines with 5 digit indices
  M <- which(is.na(Genfin[,length(Genfin)]))
  f <- which(nchar(Genfin[,1])==5)
  N <- intersect(f,M)
  
  #indices for 2nd NA (4 digit indices => fund code)
  T <- setdiff(M,N)
  
  n <- length(Genfin[,1])
  O1 <- rep(0,n)
  O2 <- rep(0,n)
  
  for (i in 1:length(N)){
    O1[N[i]:n]=Genfin[N[i],1]
  }
  O1 <- O1[-which(is.na(Genfin[,length(Genfin)]))]
  
  for (i in 1:length(T)){
    O2[T[i]:n]=Genfin[T[i],1]
  }
  O2 <- O2[-which(is.na(Genfin[,length(Genfin)]))]
  
  # O is a matrix of budget and fund codes
  O <- cbind(O1,O2)
  Genfin <- as.data.frame(Genfin[-(which(is.na(Genfin[,length(Genfin)]))), ])
  colnames(O) <- c("Supercode1","Supercode2")
  Genfin <- cbind(O,Genfin)
  
  # assign unique account codes to collective lines (not necessary anymore) 
  #--------------------------------------
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
  #------------------------------------
  
  Genfin[which(Genfin$Description==""),4] <- "MISSING"
  Genfin[which(Genfin$Description=="MISSING"),3] <- Genfin[which(Genfin$Description=="MISSING")-1,3]
  
  for (i in 5:ncol(Genfin)){
    Genfin[,i] <- as.numeric(as.character(Genfin[,i]))
  }      
  
  # identify total receipts and requirements and assign negative values to total receipts
  TC <- intersect(agrep("total receipts", Genfin$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(Genfin$Description)-nchar("total receipts"))<2))
  TR <- intersect(agrep("total requirements", Genfin$Description, max=list(cost=1,all=1), ignore.case=TRUE),which(abs(nchar(Genfin$Description)-nchar("total requirements"))<2))
  
  for (i in 1:length(TC)){
    itr <- max(which(TR<TC[i]))
    is <- TC[i]-TR[itr]-1
    if (is > 0){
      Genfin[(TR[itr]+1):(TR[itr]+is), 5:ncol(Genfin)] <- -(Genfin[(TR[itr]+1):(TR[itr]+is), 5:ncol(Genfin)])
    }
  }
  
  Genfin <- Genfin[!duplicated(Genfin),]
  
  # delete all collective lines to avoid double counting
  Genfin <- Genfin[!(substr(Genfin$Description,1,5) %in% "TOTAL"),]
  Genfin <- Genfin[!(substr(Genfin$Description,1,3) %in% "NET"),]
  Genfin <- Genfin[!(Genfin$Description=="HIGHWAY FUND APPROPRIATION"),]
  Genfin <- Genfin[!(Genfin$Description=="HIGHWAY TRUST FUND APPROPRTN"),]
  Genfin <- Genfin[!(Genfin$Description=="CHANGE IN FUND BALANCE"),]
  Genfin <- Genfin[!(Genfin$Description=="ESTIMATED RECEIPTS MISSING"),]
  
  return(Genfin)
}

#vol11999 <- generator("1999_1/vol1.pdf")
#vol21999 <- generator("1999_1/vol2.pdf")
#vol31999 <- generator("1999_1/vol3.pdf")
#vol41999 <- generator("1999_1/vol4.pdf")
#vol51999 <- generator("1999_1/vol5.pdf")
#vol61999 <- generator("1999_1/vol6.pdf")

vol12003 <- generator("2003_5/vol1.pdf")
vol12005 <- generator("2005_7/vol1.pdf")
vol12007 <- generator("2007_9/vol1.pdf")
vol12009 <- generator("2009_11/vol1.pdf")
vol12011 <- generator("2011_13/vol1.pdf")

vol22003 <- generator("2003_5/vol2.pdf")
vol22005 <- generator("2005_7/vol2.pdf")
vol22007 <- generator("2007_9/vol2.pdf")
vol22009 <- generator("2009_11/vol2.pdf")
vol22011 <- generator("2011_13/vol2.pdf")

vol32003 <- generator("2003_5/vol3.pdf")
vol32005 <- generator("2005_7/vol3.pdf")
vol32007 <- generator("2007_9/vol3.pdf")
vol32009 <- generator("2009_11/vol3.pdf")
vol32011 <- generator("2011_13/vol3.pdf")

vol42003 <- generator("2003_5/vol4.pdf")
vol42005 <- generator("2005_7/vol4.pdf")
vol42007 <- generator("2007_9/vol4.pdf")
vol42009 <- generator("2009_11/vol4.pdf")
vol42011 <- generator("2011_13/vol4.pdf")

vol52003 <- generator("2003_5/vol5.pdf")
vol52005 <- generator("2005_7/vol5.pdf")
vol52007 <- generator("2007_9/vol5.pdf")
vol52009 <- generator("2009_11/vol5.pdf")
vol52011 <- generator("2011_13/vol5.pdf")

vol62003 <- generator("2003_5/vol6.pdf")
vol62005 <- generator("2005_7/vol6.pdf")
vol62007 <- generator("2007_9/vol6.pdf")
vol62009 <- generator("2009_11/vol6.pdf")
vol62011 <- generator("2011_13/vol6.pdf")

vol12004 <- generator("2004_5/vol1.pdf")
vol22004 <- generator("2004_5/vol2.pdf")
vol32004 <- generator("2004_5/vol3.pdf")
vol42004 <- generator("2004_5/vol4.pdf")
vol52004 <- generator("2004_5/vol5.pdf")
vol62004 <- generator("2004_5/vol6.pdf")

vol12006 <- generator("2006_7/vol1.pdf")
vol22006 <- generator("2006_7/vol2.pdf")
vol32006 <- generator("2006_7/vol3.pdf")
vol42006 <- generator("2006_7/vol4.pdf")
vol52006 <- generator("2006_7/vol5.pdf")
vol62006 <- generator("2006_7/vol6.pdf")

vol12008 <- generator("2008_9/vol1.pdf")
vol22008 <- generator("2008_9/vol2.pdf")
vol32008 <- generator("2008_9/vol3.pdf")
vol42008 <- generator("2008_9/vol4.pdf")
vol52008 <- generator("2008_9/vol5.pdf")
vol62008 <- generator("2008_9/vol6.pdf")

vol12010 <- generator("2010_11/vol1.pdf")
vol22010 <- generator("2010_11/vol2.pdf")
vol32010 <- generator("2010_11/vol3.pdf")
vol42010 <- generator("2010_11/vol4.pdf")
vol52010 <- generator("2010_11/vol5.pdf")
vol62010 <- generator("2010_11/vol6.pdf")

vol12012 <- generator("2012_13/vol1.pdf")
vol22012 <- generator("2012_13/vol2.pdf")
vol32012 <- generator("2012_13/vol3.pdf")
vol42012 <- generator("2012_13/vol4.pdf")
vol52012 <- generator("2012_13/vol5.pdf")
vol62012 <- generator("2012_13/vol6.pdf")