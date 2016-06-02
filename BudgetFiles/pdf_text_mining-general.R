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
Genout1 <- as.data.frame(extract_tables(file, pages = 1, guess = FALSE, method = "data.frame", columns = list(c(320, 440))))
for (i in 2:a) {
	try({
	Out <- as.data.frame(extract_tables(file, pages = i, guess = FALSE, method = "data.frame", columns = list(c(320, 440))))
	Genout1 <- rbind(Genout1,Out)
	})
}

firstletter <- as.data.frame(substr(Genout1[,1],1,1))
Genout1 <- as.data.frame(Genout1[!(firstletter == "-"), ])

