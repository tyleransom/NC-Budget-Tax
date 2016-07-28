# This script was written by Artem Streltsov of Duke University (artem.streltsov@duke.edu) and
# Vinod Ramakrishnan of IIT Gandhinagar (vinod.ramakrishnan@iitgn.ac.in) for NC Justice Center.
#-------------------------------------------------------------------------------------------------

rm(list = ls())
cat("\014")

# Script to install and run tabulizer to extract tables from PDFs 

# packages necessary to install and run tabulizer. You might have to reinstall Java (see tabulizer package website)
require(devtools)
require(rJava)
require(tabulizer)

# change to your location like: "C:/Users/BLAH/Desktop/Data+/downloader.bat"
dloader <- read.csv("downloader.bat")

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
  if(substr(gsub("[^0-9]", "", file1),1,2)=="19"){
    year1 <- paste(substr(gsub("[^0-9]", "", file1),1,4), substr(as.numeric(substr(gsub("[^0-9]", "", file1),3,4))+1,2,3), sep = "")
    year1 <- paste("y",year1, sep = "")
    if (substr(as.numeric(substr(gsub("[^0-9]", "", file1),3,4))+1,2,3)=="00"){
      a1 <- as.numeric(substr(as.numeric(substr(gsub("[^0-9]", "", file1),3,4))+1,2,3))+1
      year2 <- paste("y2000",a1,sep="0")
    }
    else{
      year2 <- paste(substr(gsub("[^0-9]", "", file1),1,2), as.numeric(substr(gsub("[^0-9]", "", file1),3,4))+1, sep = "")
      year2 <- paste(year2, as.numeric(substr(gsub("[^0-9]", "", file1),3,4))+2, sep = "")
      year2 <- paste("y",year2, sep = "")
    }
  }
  else{
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

vol11999 <- generator("1999_01/vol1.pdf")
vol21999 <- generator("1999_01/vol2.pdf")
vol31999 <- generator("1999_01/vol3.pdf")
vol41999 <- generator("1999_01/vol4.pdf")
vol51999 <- generator("1999_01/vol5.pdf")
vol61999 <- generator("1999_01/vol6.pdf")

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


# prepare data for long panel representation

vol11999p <- unname(cbind(vol11999[,1:5],rep(1999,length(vol11999[,1]))))                 
colnames(vol11999p)[length(vol11999p[1,])] <- "Year" 
vol12003p <- unname(cbind(vol12003[,1:5],rep(2003,length(vol12003[,1]))))                 
colnames(vol12003p)[length(vol12003p[1,])] <- "Year"                    
vol12005p <- unname(cbind(vol12005[,1:5],rep(2005,length(vol12005[,1]))))                   
colnames(vol12005p)[length(vol12005p[1,])] <- "Year"                    
vol12007p <- unname(cbind(vol12007[,1:5],rep(2007,length(vol12007[,1]))))                   
colnames(vol12007p)[length(vol12007p[1,])] <- "Year"                    
vol12009p <- unname(cbind(vol12009[,1:5],rep(2009,length(vol12009[,1]))))                   
colnames(vol12009p)[length(vol12009p[1,])] <- "Year"                    
vol12011p <- unname(cbind(vol12011[,1:5],rep(2011,length(vol12011[,1]))))                   
colnames(vol12011p)[length(vol12011p[1,])] <- "Year"                    

vol12004p <-  unname(cbind(vol12004[,1:4],vol12004[,7],rep(2004,length(vol12004[,1]))))                                     
colnames(vol12004p)[length(vol12004p[1,])] <- "Year"    
vol12006p <-  unname(cbind(vol12006[,1:4],vol12006[,7],rep(2006,length(vol12006[,1]))))                                     
colnames(vol12006p)[length(vol12006p[1,])] <- "Year" 
vol12008p <-  unname(cbind(vol12008[,1:4],vol12008[,7],rep(2008,length(vol12008[,1]))))                                     
colnames(vol12008p)[length(vol12008p[1,])] <- "Year" 
vol12010p <-  unname(cbind(vol12010[,1:4],vol12010[,7],rep(2010,length(vol12010[,1]))))                                     
colnames(vol12010p)[length(vol12010p[1,])] <- "Year" 
vol12012p <-  unname(cbind(vol12012[,1:4],vol12012[,7],rep(2012,length(vol12012[,1]))))                                     
colnames(vol12012p)[length(vol12012p[1,])] <- "Year" 

vol1 <- rbind(vol11999p,vol12003p,vol12004p,vol12005p,vol12006p,vol12007p,vol12008p,vol12009p,vol12010p,vol12011p,vol12012p)
vol1 <- cbind(rep("Education",length(vol1[,1])),vol1)
colnames(vol1)[1:6] <- c("Category","Budget code", "Fund code", "Account code", "Description", "Amount")


vol21999p <- unname(cbind(vol21999[,1:5],rep(1999,length(vol21999[,1]))))                 
colnames(vol21999p)[length(vol21999p[1,])] <- "Year"
vol22003p <- unname(cbind(vol22003[,1:5],rep(2003,length(vol22003[,1]))))                   
colnames(vol22003p)[length(vol22003p[1,])] <- "Year"                    
vol22005p <- unname(cbind(vol22005[,1:5],rep(2005,length(vol22005[,1]))))                   
colnames(vol22005p)[length(vol22005p[1,])] <- "Year"                    
vol22007p <- unname(cbind(vol22007[,1:5],rep(2007,length(vol22007[,1]))))                   
colnames(vol22007p)[length(vol22007p[1,])] <- "Year"                    
vol22009p <- unname(cbind(vol22009[,1:5],rep(2009,length(vol22009[,1]))))                   
colnames(vol22009p)[length(vol22009p[1,])] <- "Year"                    
vol22011p <- unname(cbind(vol22011[,1:5],rep(2011,length(vol22011[,1]))))                   
colnames(vol22011p)[length(vol22011p[1,])] <- "Year"                    

vol22004p <-  unname(cbind(vol22004[,1:4],vol22004[,7],rep(2004,length(vol22004[,1]))))                                     
colnames(vol22004p)[length(vol22004p[1,])] <- "Year"    
vol22006p <-  unname(cbind(vol22006[,1:4],vol22006[,7],rep(2006,length(vol22006[,1]))))                                     
colnames(vol22006p)[length(vol22006p[1,])] <- "Year" 
vol22008p <-  unname(cbind(vol22008[,1:4],vol22008[,7],rep(2008,length(vol22008[,1]))))                                     
colnames(vol22008p)[length(vol22008p[1,])] <- "Year" 
vol22010p <-  unname(cbind(vol22010[,1:4],vol22010[,7],rep(2010,length(vol22010[,1]))))                                     
colnames(vol22010p)[length(vol22010p[1,])] <- "Year" 
vol22012p <-  unname(cbind(vol22012[,1:4],vol22012[,7],rep(2012,length(vol22012[,1]))))                                     
colnames(vol22012p)[length(vol22012p[1,])] <- "Year" 

vol2 <- rbind(vol21999p,vol22003p,vol22004p,vol22005p,vol22006p,vol22007p,vol22008p,vol22009p,vol22010p,vol22011p,vol22012p)
vol2 <- cbind(rep("General Government",length(vol2[,1])),vol2)
colnames(vol2)[1:6] <- c("Category","Budget code", "Fund code", "Account code", "Description", "Amount")


vol31999p <- unname(cbind(vol31999[,1:5],rep(1999,length(vol31999[,1]))))                 
colnames(vol31999p)[length(vol31999p[1,])] <- "Year"
vol32003p <- unname(cbind(vol32003[,1:5],rep(2003,length(vol32003[,1]))))                   
colnames(vol32003p)[length(vol32003p[1,])] <- "Year"                    
vol32005p <- unname(cbind(vol32005[,1:5],rep(2005,length(vol32005[,1]))))                   
colnames(vol32005p)[length(vol32005p[1,])] <- "Year"                    
vol32007p <- unname(cbind(vol32007[,1:5],rep(2007,length(vol32007[,1]))))                   
colnames(vol32007p)[length(vol32007p[1,])] <- "Year"                    
vol32009p <- unname(cbind(vol32009[,1:5],rep(2009,length(vol32009[,1]))))                   
colnames(vol32009p)[length(vol32009p[1,])] <- "Year"                    
vol32011p <- unname(cbind(vol32011[,1:5],rep(2011,length(vol32011[,1]))))                   
colnames(vol32011p)[length(vol32011p[1,])] <- "Year"                    

vol32004p <-  unname(cbind(vol32004[,1:4],vol32004[,7],rep(2004,length(vol32004[,1]))))                                     
colnames(vol32004p)[length(vol32004p[1,])] <- "Year"    
vol32006p <-  unname(cbind(vol32006[,1:4],vol32006[,7],rep(2006,length(vol32006[,1]))))                                     
colnames(vol32006p)[length(vol32006p[1,])] <- "Year" 
vol32008p <-  unname(cbind(vol32008[,1:4],vol32008[,7],rep(2008,length(vol32008[,1]))))                                     
colnames(vol32008p)[length(vol32008p[1,])] <- "Year" 
vol32010p <-  unname(cbind(vol32010[,1:4],vol32010[,7],rep(2010,length(vol32010[,1]))))                                     
colnames(vol32010p)[length(vol32010p[1,])] <- "Year" 
vol32012p <-  unname(cbind(vol32012[,1:4],vol32012[,7],rep(2012,length(vol32012[,1]))))                                     
colnames(vol32012p)[length(vol32012p[1,])] <- "Year" 

vol3 <- rbind(vol31999p,vol32003p,vol32004p,vol32005p,vol32006p,vol32007p,vol32008p,vol32009p,vol32010p,vol32011p,vol32012p)
vol3 <- cbind(rep("Health and Human Services",length(vol3[,1])),vol3)
colnames(vol3)[1:6] <- c("Category","Budget code", "Fund code", "Account code", "Description", "Amount")

vol41999p <- unname(cbind(vol41999[,1:5],rep(1999,length(vol41999[,1]))))                 
colnames(vol41999p)[length(vol41999p[1,])] <- "Year"
vol42003p <- unname(cbind(vol42003[,1:5],rep(2003,length(vol42003[,1]))))                   
colnames(vol42003p)[length(vol42003p[1,])] <- "Year"                    
vol42005p <- unname(cbind(vol42005[,1:5],rep(2005,length(vol42005[,1]))))                   
colnames(vol42005p)[length(vol42005p[1,])] <- "Year"                    
vol42007p <- unname(cbind(vol42007[,1:5],rep(2007,length(vol42007[,1]))))                   
colnames(vol42007p)[length(vol42007p[1,])] <- "Year"                    
vol42009p <- unname(cbind(vol42009[,1:5],rep(2009,length(vol42009[,1]))))                   
colnames(vol42009p)[length(vol42009p[1,])] <- "Year"                    
vol42011p <- unname(cbind(vol42011[,1:5],rep(2011,length(vol42011[,1]))))                   
colnames(vol42011p)[length(vol42011p[1,])] <- "Year"                    

vol42004p <-  unname(cbind(vol42004[,1:4],vol42004[,7],rep(2004,length(vol42004[,1]))))                                     
colnames(vol42004p)[length(vol42004p[1,])] <- "Year"    
vol42006p <-  unname(cbind(vol42006[,1:4],vol42006[,7],rep(2006,length(vol42006[,1]))))                                     
colnames(vol42006p)[length(vol42006p[1,])] <- "Year" 
vol42008p <-  unname(cbind(vol42008[,1:4],vol42008[,7],rep(2008,length(vol42008[,1]))))                                     
colnames(vol42008p)[length(vol42008p[1,])] <- "Year" 
vol42010p <-  unname(cbind(vol42010[,1:4],vol42010[,7],rep(2010,length(vol42010[,1]))))                                     
colnames(vol42010p)[length(vol42010p[1,])] <- "Year" 
vol42012p <-  unname(cbind(vol42012[,1:4],vol42012[,7],rep(2012,length(vol42012[,1]))))                                     
colnames(vol42012p)[length(vol42012p[1,])] <- "Year" 

vol4 <- rbind(vol41999p,vol42003p,vol42004p,vol42005p,vol42006p,vol42007p,vol42008p,vol42009p,vol42010p,vol42011p,vol42012p)
vol4 <- cbind(rep("Justice and Public Safety",length(vol4[,1])),vol4)
colnames(vol4)[1:6] <- c("Category","Budget code", "Fund code", "Account code", "Description", "Amount")

vol51999p <- unname(cbind(vol51999[,1:5],rep(1999,length(vol51999[,1]))))                 
colnames(vol51999p)[length(vol51999p[1,])] <- "Year"
vol52003p <- unname(cbind(vol52003[,1:5],rep(2003,length(vol52003[,1]))))                   
colnames(vol52003p)[length(vol52003p[1,])] <- "Year"                    
vol52005p <- unname(cbind(vol52005[,1:5],rep(2005,length(vol52005[,1]))))                   
colnames(vol52005p)[length(vol52005p[1,])] <- "Year"                    
vol52007p <- unname(cbind(vol52007[,1:5],rep(2007,length(vol52007[,1]))))                   
colnames(vol52007p)[length(vol52007p[1,])] <- "Year"                    
vol52009p <- unname(cbind(vol52009[,1:5],rep(2009,length(vol52009[,1]))))                   
colnames(vol52009p)[length(vol52009p[1,])] <- "Year"                    
vol52011p <- unname(cbind(vol52011[,1:5],rep(2011,length(vol52011[,1]))))                   
colnames(vol52011p)[length(vol52011p[1,])] <- "Year"                    

vol52004p <-  unname(cbind(vol52004[,1:4],vol52004[,7],rep(2004,length(vol52004[,1]))))                                     
colnames(vol52004p)[length(vol52004p[1,])] <- "Year"    
vol52006p <-  unname(cbind(vol52006[,1:4],vol52006[,7],rep(2006,length(vol52006[,1]))))                                     
colnames(vol52006p)[length(vol52006p[1,])] <- "Year" 
vol52008p <-  unname(cbind(vol52008[,1:4],vol52008[,7],rep(2008,length(vol52008[,1]))))                                     
colnames(vol52008p)[length(vol52008p[1,])] <- "Year" 
vol52010p <-  unname(cbind(vol52010[,1:4],vol52010[,7],rep(2010,length(vol52010[,1]))))                                     
colnames(vol52010p)[length(vol52010p[1,])] <- "Year" 
vol52012p <-  unname(cbind(vol52012[,1:4],vol52012[,7],rep(2012,length(vol52012[,1]))))                                     
colnames(vol52012p)[length(vol52012p[1,])] <- "Year" 

vol5 <- rbind(vol51999p,vol52003p,vol52004p,vol52005p,vol52006p,vol52007p,vol52008p,vol52009p,vol52010p,vol52011p,vol52012p)
vol5 <- cbind(rep("Natural and Economic Resources",length(vol5[,1])),vol5)
colnames(vol5)[1:6] <- c("Category","Budget code", "Fund code", "Account code", "Description", "Amount")

vol61999p <- unname(cbind(vol61999[,1:5],rep(1999,length(vol61999[,1]))))                 
colnames(vol61999p)[length(vol61999p[1,])] <- "Year"
vol62003p <- unname(cbind(vol62003[,1:5],rep(2003,length(vol62003[,1]))))                   
colnames(vol62003p)[length(vol62003p[1,])] <- "Year"                    
vol62005p <- unname(cbind(vol62005[,1:5],rep(2005,length(vol62005[,1]))))                   
colnames(vol62005p)[length(vol62005p[1,])] <- "Year"                    
vol62007p <- unname(cbind(vol62007[,1:5],rep(2007,length(vol62007[,1]))))                   
colnames(vol62007p)[length(vol62007p[1,])] <- "Year"                    
vol62009p <- unname(cbind(vol62009[,1:5],rep(2009,length(vol62009[,1]))))                   
colnames(vol62009p)[length(vol62009p[1,])] <- "Year"                    
vol62011p <- unname(cbind(vol62011[,1:5],rep(2011,length(vol62011[,1]))))                   
colnames(vol62011p)[length(vol62011p[1,])] <- "Year"                    

vol62004p <-  unname(cbind(vol62004[,1:4],vol62004[,7],rep(2004,length(vol62004[,1]))))                                     
colnames(vol62004p)[length(vol62004p[1,])] <- "Year"    
vol62006p <-  unname(cbind(vol62006[,1:4],vol62006[,7],rep(2006,length(vol62006[,1]))))                                     
colnames(vol62006p)[length(vol62006p[1,])] <- "Year" 
vol62008p <-  unname(cbind(vol62008[,1:4],vol62008[,7],rep(2008,length(vol62008[,1]))))                                     
colnames(vol62008p)[length(vol62008p[1,])] <- "Year" 
vol62010p <-  unname(cbind(vol62010[,1:4],vol62010[,7],rep(2010,length(vol62010[,1]))))                                     
colnames(vol62010p)[length(vol62010p[1,])] <- "Year" 
vol62012p <-  unname(cbind(vol62012[,1:4],vol62012[,7],rep(2012,length(vol62012[,1]))))                                     
colnames(vol62012p)[length(vol62012p[1,])] <- "Year" 

vol6 <- rbind(vol61999p,vol62003p,vol62004p,vol62005p,vol62006p,vol62007p,vol62008p,vol62009p,vol62010p,vol62011p,vol62012p)
vol6 <- cbind(rep("Transportation",length(vol6[,1])),vol6)
colnames(vol6)[1:6] <- c("Category","Budget code", "Fund code", "Account code", "Description", "Amount")

# call additional volumes for 2013-2015 in csv format 
temporaryFile <- tempfile()
download.file("https://ncosbm.s3.amazonaws.com/s3fs-public/openbudget/NC_Budget_Data_FY2013.csv",destfile=temporaryFile, method="curl")
data2013 <- read.csv(temporaryFile, header=TRUE,stringsAsFactors=FALSE)
temporaryFile <- tempfile()
download.file("https://ncosbm.s3.amazonaws.com/s3fs-public/openbudget/NC_Budget_Data_FY2014.csv",destfile=temporaryFile, method="curl")
data2014 <- read.csv(temporaryFile, header=TRUE,stringsAsFactors=FALSE)
temporaryFile <- tempfile()
download.file("https://ncosbm.s3.amazonaws.com/s3fs-public/openbudget/NC_Budget_Data_FY2015.csv",destfile=temporaryFile, method="curl")
data2015 <- read.csv(temporaryFile, header=TRUE,stringsAsFactors=FALSE)

# alternatively if you have downloaded those manually (MAKE SURE DIRECTORY IS CORRECTLY SPECIFIED)
#data2013 <- read.csv("C:/Users/BLAH/Desktop/Data+/NC_Budget_Data_FY2013.csv", header=TRUE,stringsAsFactors=FALSE)
#data2014 <- read.csv("C:/Users/BLAH/Desktop/Data+/NC_Budget_Data_FY2014.csv", header=TRUE,stringsAsFactors=FALSE)
#data2015 <- read.csv("C:/Users/BLAH/Desktop/Data+/NC_Budget_Data_FY2015.csv", header=TRUE,stringsAsFactors=FALSE)


R1 <-gsub("\\$","",data2013[,length(data2013[1,])])
R1 <-as.numeric(as.character(gsub(",","",R1)))
data2013p <- unname(data.frame(data2013[,2],data2013[,4],data2013[,6],matrix(,length(data2013[,1]),1),data2013[,9],R1,rep("2013",length(data2013[,1]))))
colnames(data2013p)[7] <- "Year"
data2013p <- data2013p[which(data2013$budget_type=="Budget"),]

R2 <-gsub("\\$","",data2014[,length(data2014[1,])])
R2 <-as.numeric(as.character(gsub(",","",R2)))
data2014p <- unname(data.frame(data2014[,2],data2014[,4],data2014[,6],matrix(,length(data2014[,1]),1),data2014[,9],R2,rep("2014",length(data2014[,1]))))
colnames(data2014p)[7] <- "Year"
data2014p <- data2014p[which(data2014$budget_type=="Budget"),]

R3 <-gsub("\\$","",data2015[,length(data2015[1,])])
R3 <-as.numeric(as.character(gsub(",","",R3)))
data2015p <- unname(data.frame(data2015[,2],data2015[,4],data2015[,6],matrix(,length(data2015[,1]),1),data2015[,9],R3,rep("2015",length(data2015[,1]))))
colnames(data2015p)[7] <- "Year"
data2015p <- data2015p[which(data2015$budget_type=="Budget"),]

#[NOTE: 2013 is actually redundant because what is labeled as 2012 in this code is actually 2013]
#as a result, we exclude it from the final product
new <- rbind(data2014p,data2015p)
colnames(new)[1:6] <- c("Category","Budget code", "Fund code", "Account code", "Description", "Amount")

# create a single long panel
Panel <- rbind(vol1,vol2,vol3,vol4,vol5,vol6)
Panel[,2] <- as.character(Panel[,2])
Panel[,5] <- as.character(Panel[,5])
Panel[,c("Year")] <- as.numeric(as.character(Panel[,c("Year")]))+1
Panel <- rbind(Panel,new)

write.csv(Panel,file="Panel.csv")
save(Panel,file="Panel.RDa")
save.image(file="generator.RData")
