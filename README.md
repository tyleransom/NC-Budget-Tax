# NCBTC
Inputs:
◦downloader.bat, which contains links to all PDFs used (update it if the links have been changed)
◦generator.R, which is the R code used to extract the data from the PDFs and create a long panel for Tableau analysis
◦additonal.R, which takes all of the annual budget files and concatenates them (method: outer merge-like) or combines original budgets with revised

•Outputs
From generator.R:
◦Panel.csv, which is the entire budget 1999-2015 in CSV format (long panel with original budget values for odd years and revised values for even years (easily changeable))
◦Panel.Rda, which is the same data in R data format

Tableau workbook which contains analysis of the budget

From additional.R (these are stored in R environment):
◦ outer merged wide panels that can be used to track the codes that have undergone some changes over time (Map_finale1-6)
◦ combined original budgets with the revised ones (for instance, vol12003comb)
