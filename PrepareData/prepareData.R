library(arules)
#library(party)
library(LaF)
#library(Cairo)
#library(sqldf)
#library(snowfall)
#library(parallel)
#library(plotrix)
#library(rlist)

##### Settings ######################
# full path to current "PrepareData" directory where file prepareDataFunctions.R is without char "/" at the end
currDir <- "C:/../PrepareData"

# full path to .../allData where transSubgroupsDiagnose will be created without char "/" at the end
# manually create directory "allData" before running
allDataDir <- "I:/allData"

# full path to HCUP NIS DATA directory (where ASC and SPS files are) without char "/" at the end
HCUP_NIS_Dir <- "I:/HCUP NIS"

# years of HCUP NIS transactions you have in directory defined in variable HCUP_NIS_Dir
dataYears <- 2000:2014
#################################### #

##### Optional settings ######################
# you can define additional years and/or ages discretizations
myYears <- list("by months"=1:12,
                "by quarters"=list(1:3, 4:6, 7:9, 10:12)
)
myAges <- list("18-49, 50-64, 65-74, 75-124"=list(18:49, 50:64, 65:74, 75:124),
               "by 5 years, from 18"=list(18:24, 25:29, 30:34, 35:39, 40:44, 45:49, 50:54, 55:59, 60:64, 65:69, 70:74, 75:79, 80:84, 85:124)
)
#################################### #

base::source(paste0(currDir, "/prepareDataFunctions.R"))

# create files data2000.rds, trans2000.rds, data2001.rds, trans2001.rds ...
ascToTransactions(pathToNIS=HCUP_NIS_Dir, years=dataYears)

# create directory if not exists
dir.create(paste0(allDataDir, "/transSubgroupsDiagnose/"), showWarnings = FALSE)
# create groups info for web application
saveRDS(myYears, paste0(allDataDir, "/transSubgroupsDiagnose/myYears.rds"))
saveRDS(myAges, paste0(allDataDir, "/transSubgroupsDiagnose/myAges.rds"))

for (myYear in 1:length(myYears)) {
  for (myAge in 1:length(myAges)) {
    groupFolder <- paste0(myYear,".",myAge)
    myYearName <-  names(myYears)[myYear]
    myAgeName <-  names(myAges)[myAge]
    
    print(paste0(groupFolder, " in progress (part 1)..."))
    transSubGroups(groupFolder, myYears[[myYear]], myAges[[myAge]])

    # crete files with transaction IDs for each diagnose for current group in the loop
    print(paste0(groupFolder, " in progress (part 2)..."))
    diagnoseTransSubGroups(groupFolder, myYearName, myAgeName)
  }
}
