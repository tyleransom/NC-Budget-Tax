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
Genout1 <- as.data.frame(extract_tables(file, pages = 1, guess = FALSE, method = "data.frame", columns = list(c(320, 420))),stringsAsFactors=FALSE)
for (i in 2:a) {
	try({
	Out <- as.data.frame(extract_tables(file, pages = i, guess = FALSE, method = "data.frame", columns = list(c(320, 420))),stringsAsFactors=FALSE)
	Genout1 <- rbind(Genout1,Out)
	})
}


Genout1 <- as.data.frame(Genout1[!(as.data.frame(substr(Genout1[,1],1,1)) == "-"), ],stringsAsFactors=FALSE)

# Fix columns that begin with strings that need to begin with numbers
Genout2 <- Genout1
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

# Remove unnecessary lines
Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,3],1,4)) == "PAGE"), ],stringsAsFactors=FALSE)
Gen3 <- as.data.frame(Gen3[!(substr(Gen3[,1],1,3) %in% c("APP", "BUD", "REQ", "DES", "EST", "POS", "SUM")), ],stringsAsFactors=FALSE)

# Remove commas from dollar amounts and any lines with "PAGE" in the final column
Gen2 <- as.data.frame(gsub(",","",Gen3[,2]),stringsAsFactors=FALSE)
Gen42 <- as.data.frame(gsub(",","",Gen3[,3]),stringsAsFactors=FALSE)
Gen3 <- cbind(Gen3[,1], Gen2, Gen42)
Gen3 <- Gen3[!apply(Gen3 == "", 1, all),]
#Gen3 <- Gen3[!grepl("PAGE",Gen4[,3]),]
Gen3 <- Gen3


Gen3 <- cbind(as.data.frame(gsub("  "," ",Gen3[,1]), stringsAsFactors=FALSE),Gen3[,2:3])

#is.letter <- function(x) grepl("[[:alpha:]]", x)
is.number <- function(x) grepl("[[:digit:]]", x)

#track the index of the last digit in first column before letters
L=rep(0,length(Gen3[,1]))
for (j in 1:8){
  k <- as.data.frame(substr(Gen3[,1],j,j),stringsAsFactors=FALSE)
  for (i in 1:length(k[,1])){
    if(is.number(k[i,1])==TRUE){L[i]=j}
  }}

numbersnew=rep(0,length(L))
lettersnew=c()

#map the corresponding figures to a category
for (i in 1:length(L)){
  numbersnew[i]=as.data.frame(substr(Gen3[i,1],1,L[i]),stringsAsFactors=FALSE)
 # numbersnew2 <- rbind(numbersnew2,numprel)
 lettersnew[i] <- as.data.frame(substr(Gen3[i,1],L[i]+1,100),stringsAsFactors=FALSE)
}

w <- cbind(numbersnew,lettersnew)
Genfin <- cbind(w, Gen3[,2:3])


# Start concatenating the vector
#numbers <- as.data.frame(substr(Gen3[,1],1,5),stringsAsFactors=FALSE)
#letters <- as.data.frame(substr(Gen3[,1],6,100),stringsAsFactors=FALSE)

# Aggregate together, rename columns, and convert IDs to numeric
Genfin <- cbind(numbers, letters, Gen3[,2:3])

# Operator to find numbers of unique digits in the years (to name columns) and find these names
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

# Naming columns and converting to numerics
colnames(Genfin) <- c("SubsecID","Description",year1,year2)
Genfin[which(substr(Genfin[,1],5,5) %in% " "),1]=substr(Genfin[which(substr(Genfin[,1],5,5) %in% " "),1],1,4)
Genfin$SubsecID  <- as.numeric(Genfin$SubsecID)
Genfin[,3]   <- type.convert(Genfin[,3], numerals="warn.loss");
Genfin[,4]   <- type.convert(Genfin[,4], numerals="warn.loss");

#Creating two zero vectors and then filling them with values corresponding NAs (with 5 and 4 digits resp.)

#values with 5 digit indeces
#vol[which(!substr(vol[,1],5,5) %in% ""),1]
#indeces with 5 digit indeces
N <- which(!substr(Genfin[,1],5,5) %in% "")
M <- which(is.na(Genfin[,3]))

#indeces for 2nd NA
T <- which(is.na(Genfin[,3]))
T <- T[!(T %in% c(which(!substr(Genfin[,1],5,5) %in% "")))]
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

return(Genfin)
}

#for now the function ends here
vol62013 <-generator("C:/Users/Tom/Desktop/Data+/2003_5/vol6.pdf")














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
