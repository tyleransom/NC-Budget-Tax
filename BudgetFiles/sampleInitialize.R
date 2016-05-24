# Remove everything from the environment and clear out memories
rm(list = ls())
gc()

# Print library path
.libPaths()

# Require R version 3.2.2
ver<-R.Version()
if (ver$minor!="2.2") {
	throw("Version incompatibility")
}


# Text mining packages
require(tm)

