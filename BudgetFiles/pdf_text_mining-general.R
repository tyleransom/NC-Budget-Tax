rm(list = ls())

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


Genout1 <- as.data.frame(Genout1[!(as.data.frame(substr(Genout1[,1],1,1)) == "-"), ])

Genout2=Genout1
Gen2 <- as.data.frame(gsub("TOTAL", "00000 TOTAL", Genout2[,1]))
Gen3 <- cbind(Gen2, Genout2[,2:3])

Gen2 <- as.data.frame(gsub("NET", "00001 NET", Gen3[,1]))
Gen3 <- cbind(Gen2, Gen3[,2:3])

Gen2 <- as.data.frame(gsub("HIGHWAY", "00002 HIGHWAY", Gen3[,1]))
Gen3 <- cbind(Gen2, Gen3[,2:3])

Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,2)) == "42"), ])
Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,3)) == "APP"), ])
Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,3)) == "BUD"), ])
Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,3)) == "REQ"), ])
Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,3)) == "142"), ])
Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,3)) == "DES"), ])
Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,3)) == "EST"), ])
Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,3)) == "POS"), ])
Gen3 <- as.data.frame(Gen3[!(as.data.frame(substr(Gen3[,1],1,3)) == "SUM"), ])

Gen4 <- as.data.frame(gsub(",","",Gen3[,2]))
Gen42 <- as.data.frame(gsub(",","",Gen3[,3]))
Gen4 <- cbind(Gen3[,1], Gen4, Gen42)
Gen4 <- Gen4[!apply(Gen4 == "", 1, all),]
Gen4 <- Gen4

numbers <- as.data.frame(substr(Gen4[,1],1,5))
letters <- as.data.frame(substr(Gen4[,1],6,100))

Genfin <- cbind(numbers, letters, Gen4[,2:3])
