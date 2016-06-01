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
file <- "C:/Users/admin/Desktop/DUKE Data+/2003_5/vol6.pdf"
a <- get_n_pages(file=file)
# extract tables from the PDF
genout1 <- extract_tables(file, pages = 1, guess = FALSE, method = "data.frame")
#a <- 
#library(xlsx)
Genout1 <- as.data.frame(genout1)
#write.xlsx(out1, file="filename.xlsx", sheetName="sheet1")

for (i in 2:a)
{try({out <- extract_tables(file, pages = i, guess = FALSE, method = "data.frame")

# make into data frame 
Out <- as.data.frame(out)
#nextsheet <- paste("sheet", i, sep = "")
Genout1 <- rbind(Genout1,Out)
#write.xlsx(out, file="filename.xlsx", sheetName=paste("sheet", i, sep = ""), append=TRUE)
}
)
}
#print(out1)
#write.xlsx(out1, file="File1.xlsx")
firstletter <- as.data.frame(substr(Genout1[,1],1,1))
#write.xlsx(out, file="filename.xlsx", sheetIndex=i, append=TRUE)}
#x <- c()

#for ( b in 1:4741 )
  #if (toString(firstletter[b,], width = NULL) == "-" ){
 #       append( x, b )
#    }
Genout1 = as.data.frame(Genout[!(firstletter == "-"), ])







