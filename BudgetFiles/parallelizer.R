#-----------------------------------------------------------
# Load Required Packages and Functions
#-----------------------------------------------------------
source("DataGen-linux.R")

# grab the array id value from the environment variable passed from sbatch
n <- as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))
# n <- as.numeric(200306) # (test value)
n <- 200000+n

yr <- floor(n/100)
ch <- n-yr*100
fileList <- list("Edu","GenGov","Health","Justice","NatEcoRe","Trans")

varname <- paste0(fileList[[ch]],yr-2000,"_",yr-1998)
temp <- DataGen(ch,yr)
assign(paste0(fileList[[ch]],yr-2000,"_",yr-1998),temp)
save(list=varname,file=paste0("RdaFiles/",varname,".Rda"))
