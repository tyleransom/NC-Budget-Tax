############################################################
# Explain here what your script does (2+ lines OK)
# Write your name(s) here
############################################################


#-----------------------------------------------------------
# Load Required Packages and Functions
#-----------------------------------------------------------
source("sampleInitialize.R")


#-----------------------------------------------------------
# Convert PDFs to text files (1 method)
#-----------------------------------------------------------
# Tell R what folder contains your 1000s of PDFs
dest <- "G:/somehere/with/many/PDFs"

# make a vector of PDF file names
myfiles <- list.files(path = dest, pattern = "pdf",  full.names = TRUE)

############### PDF (text format) to TXT ###################

                  ##### Wait! #####
# Before proceeding, make sure you have a copy of pdf2text
# on your computer! Details: https://en.wikipedia.org/wiki/Pdftotext
# Download: http://www.foolabs.com/xpdf/download.html

# If you have a PDF with text, ie you can open the PDF in a 
# PDF viewer and select text with your curser, then use these 
# lines to convert each PDF file that is named in the vector 
# into text file is created in the same directory as the PDFs
# note that my pdftotext.exe is in a different location to yours
lapply(myfiles, function(i) system(paste('"C:/Program Files/xpdf/bin64/pdftotext.exe"', paste0('"', i, '"')), wait = FALSE) )

# where are the txt files you just made?
print(dest) # in this folder

# And now you're ready to do some text mining on the text files

############### PDF to CSV (DfR format) ####################

# or if you want DFR-style csv files...
# read txt files into R
mytxtfiles <- list.files(path = dest, pattern = "txt",  full.names = TRUE)

library(tm)
mycorpus <- Corpus(DirSource(dest, pattern = "txt"))
# warnings may appear after you run the previous line, they
# can be ignored
mycorpus <- tm_map(mycorpus,  removeNumbers)
mycorpus <- tm_map(mycorpus,  removePunctuation)
mycorpus <- tm_map(mycorpus,  stripWhitespace)
mydtm <- DocumentTermMatrix(mycorpus)
# remove some OCR weirdness
# words with more than 2 consecutive characters
mydtm <- mydtm[,!grepl("(.)\\1{2,}", mydtm$dimnames$Terms)]

# get each doc as a csv with words and counts
for(i in 1:nrow(mydtm)){
  # get word counts
  counts <- as.vector(as.matrix(mydtm[1,]))
  # get words
  words <- mydtm$dimnames$Terms
  # combine into data frame
  df <- data.frame(word = words, count = counts,stringsAsFactors = FALSE)
  # exclude words with count of zero
  df <- df[df$count != 0,]
  # write to CSV with original txt filename
  write.csv(df, paste0(mydtm$dimnames$Docs[i],".csv"), row.names = FALSE) 
}

# and now you're ready to work with the csv files

#-----------------------------------------------------------
# Convert PDFs to text files (another method)
#-----------------------------------------------------------
# Use text-mining package to extract text from PDF files    
library(tm)

# Function to read a PDF file and turn it into a data frame
PDFtoDF = function(file) {
  ## Extract PDF text. Each line of PDF becomes one element of the string vector dat.
  dat = readPDF(control=list(text="-layout"))(elem=list(uri=file), 
                                              language="en", id="id1") 
  dat = c(as.character(dat))

  ## Keep only those strings that contain the data we want. 
  ## These are the ones that begin with a number.
  dat = dat[grep("^ {0,2}[0-9]{1,3}", dat)]

  ## Create separators so we can turn strings into a data frame. We'll use the 
  ## pipe "|" as a separator.

  # Add pipe after first number (the row number in the PDF file)
  dat = gsub("^ ?([0-9]{1,3}) ?", "\\1|", dat)

  # Replace each instance of 2 or more spaces in a row with a pipe separator. This 
  # works because the company names have a single space between words, while data
  # fields generally have more than one space between them. 
  # (We just need to first add an extra space in a few cases where there's only one
  # space between two data fields.)
  dat = gsub("(, HVOL )","\\1 ", dat)
  dat = gsub(" {2,100}", "|", dat)

  ## Check for data format problems
  # Identify rows without the right number of fields (there should 
  # be six pipe characters per row) and save them to a file for 
  # later inspection and processing (in this case row 11 of the PDF file is excluded))
  excludeRows = lapply(gregexpr("\\|", dat), function(x) length(x)) != 6
  write(dat[excludeRows], "rowsToCheck.txt", append=TRUE)

  # Remove the excluded rows from the string vector
  dat = dat[!excludeRows]

  ## Convert string vector to data frame 
  dat = read.table(text=dat, sep="|", quote="", stringsAsFactors=FALSE)
  names(dat) = c("RowNum", "Reference Entity", "Sub-Index", "CLIP", 
                  "Reference Obligation", "CUSIP/ISIN", "Weighting")
  return(dat)
}

# Create vector of names of files to read
files = list.files(pattern="CDX.*\\.pdf")

# Read each file, convert it to a data frame, then rbind into single data frame
df = do.call("rbind", lapply(files, PDFtoDF))

# Sample of data frame output from your sample file
df
    # RowNum    Reference Entity    Sub-Index      CLIP           Reference Obligation   CUSIP/ISIN Weighting
# 1        1         ACE Limited          FIN 0A4848AC9     ACE-INAHldgs 8.875 15Aug29    00440EAC1     0.008
# 2        2           Aetna Inc.         FIN 0A8985AC5     AET 6.625 15Jun36 BondCall    00817YAF5     0.008
# 3        3           Alcoa Inc.  INDU, HVOL 014B98AD5                AA 5.72 23Feb19    013817AP6     0.008