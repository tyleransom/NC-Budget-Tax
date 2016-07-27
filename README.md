# Instructions for reproducing NC Budget Files
## Inputs
* downloader.bat, which contains links to all PDFs used (update it if the links have been changed)
* generator.R, which is the R code used to extract the data from the PDFs and create a long panel for Tableau analysis
* additonal.R, which takes all of the annual budget files and concatenates them (method: outer merge-like) or combines original budgets with revised

## Outputs
### From generator.R
* Panel.csv, which is the entire budget 1999-2015 in CSV format (long panel with original budget values for odd years and revised values for even years (easily changeable: look at two random budgets - even and odd - and tweak columns for lines 912-1076 accordingly))
* Panel.Rda, which is the same data in R data format
* Panel.csv can be read into a Tableau workbook which contains further analysis of the budget

### From additional.R (these are stored in R environment):
* outer merged wide panels that can be used to track the codes that have undergone some changes over time (Map_finale1-6)
* combined original budgets including even-year revisions (for instance, vol12003comb)

# Implementation notes
When it comes to converting pdf files in R, the crux of the matter is to find and install the right packages. In our script we use tabulizer package (https://github.com/leeper/tabulizer) which is not yet on CRAN and hence its installation might be somewhat intricate.

The installation steps are highlighted on the webpage stipulated above. Make sure you have the latest version of Java and the following packages: devtools and rJava. To get those use the following commands in R: install.packages("devtools") and install.packages("rJava"). After that depending on the version of your Windows run:

if(!require("ghit")){
    install.packages("ghit")
}
## on 64-bit Windows
ghit::install_github(c("leeper/tabulizerjars", "leeper/tabulizer"), INSTALL_opts = "--no-multiarch")
## elsewhere
ghit::install_github(c("leeper/tabulizerjars", "leeper/tabulizer"))
