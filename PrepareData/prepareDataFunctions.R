#### Internal functions ####

################################# #
# Load data for selected columns
################################# #
loadBasicHCUP <- function (filename, info) {
  time <- proc.time()
  
  # create laf object for reading data
  colTypes <- info$types 
  colWidths <- info$numChars
  colNames <- info$names
  laf <- laf_open_fwf(filename, colTypes, colWidths, colNames)
  
  # create selColumns vector from column names
  selNames <- c("AGE", "AMONTH", "AWEEKEND", "DIED", paste("DX", 1:30, sep=""), paste("DXCCS", 1:30, sep=""), "FEMALE", "LOS", "RACE",
                paste("PR", 1:15, sep=""), "TOTCHG", "YEAR")
 
  selColumns <- NULL
  for(i in 1:length(selNames)) {
    if(is.null(selColumns)) {
      selColumns <- c(match(selNames[i],info$names))
    } else {
      selColumns <- append(selColumns, match(selNames[i],info$names))
    }
  }
  
  selColumns <- selColumns[!is.na(selColumns)]
  
  begin(laf)
  d <- c(1)
  sum <- 0
  data <- NULL
  while(TRUE) {
    d <- next_block(laf, columns = selColumns, nrows = 1000000)
    
    if(dim(d)[1] == 0) break
    
    if(is.null(data)) {
      data <- d
    } else {
      data <- rbind(data, d)
    }
  }
  
  print(proc.time() - time)
  
  return(data)
}

###################################################################### #
# Function that reads variable information from SPSS load files
# Returns list of vectors with:
# - variable names (names)
# - variable types (types)
# - width of variables in ASCII file (numChars)
# - labels / long names of variables (labels)
# - values that need to be recoded to NA (missVals)
###################################################################### #
readSPSSInfo <- function(filename) {
  # Read Stata load file
  text <- readLines(filename)
  pos <- 5
  
  # locate variable names and fixed width locations
  while(!grepl("DATA LIST FILE", text[pos])) {
    pos <- pos + 1
  }
  pos <- pos + 1
  
  names <- c("")
  numChars <- c(as.integer(0))
  dashPos <- NA
  # process all variable names
  while(nchar(text[pos]) > 10) {
    if(is.na(dashPos)) {
      dashPos <- regexpr("-",text[pos])  
    }
    
    name <- substr(text[pos],0,dashPos-4)
    name <- gsub("(^ +)|( +$)", "", name)
    names <- append(names,name)
    n1 <- as.integer(substr(text[pos],dashPos-3,dashPos-1))
    n2 <- as.integer(substr(text[pos],dashPos+1,dashPos+3))+1
    
    if(is.na(n2)) n2 <- n1 + 1
    
    numChars <- append(numChars, n2-n1)
    pos <- pos + 1
  }
  
  # create data.frame
  df<-data.frame(names, numChars)
  
  df$types <- "categorical"
  
  # locate variable types
  while(!grepl("FORMATS", text[pos])) {
    pos <- pos + 1
  }
  pos <- pos + 1
  
  # process all variable types
  while(nchar(text[pos]) > 10) {
    slashPos <- regexpr("/",text[pos]) 
    
    name <- substr(text[pos],0,slashPos-8)
    # trim leading and trailing spaces
    name <- gsub("(^ +)|( +$)", "", name)
    
    df$types[df$names == name] <- "integer"
    pos <- pos + 1
  }  
  
  
  
  df <- df[-1,]
  return(df)
}

############################ #
# Remove emty values #
############################ #
removeEmptyValues <- function(myFactor) {
  levels(myFactor) <- gsub("[ ]","",levels(myFactor))
  levels(myFactor)[(levels(myFactor)=="")] <- NA
  
  return(myFactor)
}

########################### #
# Edit diagnoses names #
########################### #
editDiagnosesNames <- function(myFactor) {
  levels(myFactor) <- gsub("[-| ]","",levels(myFactor))
  levels(myFactor)[(levels(myFactor)=="")] <- NA
  
  #if code starts with E, then dot is 1 char more right
  boolE <- substr(levels(myFactor),1,1)=="E"
  levels(myFactor)[!boolE] <- paste(substr(levels(myFactor)[!boolE],1,3), substr(levels(myFactor)[!boolE],4,5), sep=".")
  levels(myFactor)[boolE] <- paste(substr(levels(myFactor)[boolE],1,4), substr(levels(myFactor)[boolE],5,5), sep=".")
  
  levels(myFactor) <- gsub("[.]$", "", levels(myFactor))
  
  return(myFactor)
}

########################### #
# Edit procedure names #
########################### #
editProceduresNames <- function(myFactor) {
  levels(myFactor) <- gsub("[-| ]","",levels(myFactor))
  levels(myFactor)[(levels(myFactor)=="")] <- NA
  levels(myFactor) <- paste(substr(levels(myFactor),1,2), substr(levels(myFactor),3,4), sep=".")
  levels(myFactor) <- gsub("[.]$", "", levels(myFactor))
  
  return(myFactor)
}

########################################################### #
# 1. read text data and converting in transactions ####
# transactins is saved on disk in RDS format
########################################################### #
ascToTransactions <- function(pathToNIS, years=2000:2014) {
  for(year in years) {
    myTime <- proc.time()
    print(year)
    filename <- paste(pathToNIS, "/SPSSLoad_NIS_", year, "_Core.SPS", sep="")
    info <- readSPSSInfo(filename)
    
    info$names <- as.character(info$names)
    
    filename <- paste(pathToNIS, "/NIS_", year,"_CORE.asc", sep="")
    print(filename)
    
    data <- loadBasicHCUP(filename, info)
    rm(filename)
    rm(info)
    
    print("end loadHCUP")
    data <- data.frame(lapply(data, as.factor))	
    data[] <- lapply(data[], removeEmptyValues)
    print("end removeEmptyValues")
    
    dxInd <- which(grepl("^DX[1-9]",  colnames(data), perl=TRUE)) # indeces of diagnoses ICD
    prInd <- which(grepl("^PR[1-9]",  colnames(data), perl=TRUE)) # indeces of procedures ICD
    
    data[dxInd] <- lapply(data[dxInd], editDiagnosesNames)
    data[prInd] <- lapply(data[prInd], editProceduresNames)
    print("end editNames")
    
    saveRDS(data, paste(allDataDir, "/data",year,".rds", sep=""))
    
    trans <- as(data, "transactions")
    rm(data)
    
    myTime2 <- proc.time()
    list1 <- as(trans, "list")
    rm(trans)
    print(object.size(list1)/1024/1024)
    print(proc.time() - myTime2)
    
    myTime2 <- proc.time()
    list1 <- lapply(list1, function(x){return(unique(sub("PR[0-9]+=","P_", sub("DX[0-9]+=","D_",x))))})
    print(object.size(list1)/1024/1024)
    print(proc.time() - myTime2)
    
    myTime2 <- proc.time()
    trans <- as(list1, "transactions")
    print(proc.time() - myTime2)
    
    rm(list1)
    print("end transactions")
    
    saveRDS(trans, paste(allDataDir, "/trans",year,".rds", sep=""))
    
    rm(trans)
    
    print(proc.time() - myTime)
  }
  rm(year)
  rm(myTime)
  rm(myTime2)	
}

######################################################################### #
# 2. calculating subgroup tables  #max age is 124 ####
######################################################################### #
transSubGroups <- function(groupFolder, amonths, ageGroups) {
  dir.create(paste0(allDataDir, "/transSubgroups/"), showWarnings = F)
  dir.create(paste0(allDataDir, "/transSubgroups/",groupFolder), showWarnings = F)
  myTime <- proc.time()
  years <- as.integer(gsub(pattern="trans|\\.rds", replacement="", x=list.files(allDataDir, "trans([0-9]+).rds"), perl = T))
  for (year in years) {
    trans <- readRDS(paste0(allDataDir, "/trans", year, ".rds"))
    for (gender in 0:1)	{
      tmpTrans1 <- trans[trans %in% paste0("FEMALE=", gender)]
      for (amonth in amonths) {
        tmpTrans2 <- tmpTrans1[tmpTrans1 %in% paste0("AMONTH=", amonth)]
        for (ageGroup in ageGroups) {
          tmpTrans3 <- tmpTrans2[tmpTrans2 %in% intersect(itemInfo(tmpTrans2)$labels, paste0("AGE=", ageGroup))]
          tmpTrans3 <- as(as(tmpTrans3, "list"), "transactions")
          
          if (length(tmpTrans3) > 0) {
            x <- list(year=year, amonth=max(amonth), age=max(ageGroup), female=gender, trans=tmpTrans3)
            saveRDS(x, paste0(allDataDir, "/transSubgroups/",groupFolder,"/trans",year,"_",gender,"_",max(amonth),"_",max(ageGroup),".rds"))
          }
        }
      }		
    }
  }
  print(proc.time() - myTime)
}

######################################################################### #
# 3 calculating inverted index for 4 (each diagnose has it's own file) + transInfo.rds ####
######################################################################### #
diagnoseTransSubGroups <- function(groupFolder, yearName, ageName) {
  dir.create(paste0(allDataDir, "/transSubgroupsDiagnose/"), showWarnings = FALSE)
  dir.create(paste0(allDataDir, "/transSubgroupsDiagnose/",groupFolder), showWarnings = FALSE)
  saveRDS(list(yearName=yearName, ageName=ageName), file = paste0(allDataDir, "/transSubgroupsDiagnose/",groupFolder,"/GUIselect.rds"))
  allFiles <- list.files(paste0(allDataDir, "/transSubgroups/",groupFolder,"/"), full.names=T)
  # create transInfo.rds file with informations year, amonth, age, female, numTrans
  transInfo <- lapply(1:length(allFiles), function(i){
    myfile <- readRDS(allFiles[i])
    list(year=myfile$year, amonth=myfile$amonth, age=myfile$age, female=myfile$female, numTrans=length(myfile$trans))
  })
  saveRDS(transInfo, paste0(allDataDir, "/transSubgroupsDiagnose/",groupFolder,"/transInfo.rds"))
  
  myTime <- proc.time()
  # create temporal txt files
  for (i in 1:length(allFiles)) {
    myfile <- readRDS(allFiles[i])
    allDiagnoses <- grep("D_", myfile$trans@itemInfo[,1], perl = T)
    for (diagnozaIndex in allDiagnoses) {
      diagnoza <-  myfile$trans@itemInfo[diagnozaIndex, 1]
      resultFile <- paste0(allDataDir, "/transSubgroupsDiagnose/",groupFolder,"/", diagnoza, ".txt")
      mytext <- paste(c(i, which(myfile$trans@data[diagnozaIndex,])), collapse = ",")
      cat(mytext, file = resultFile, append = T, sep = "\n")
    }
  }
  
  allTxtFiles <- list.files(paste0(allDataDir, "/transSubgroupsDiagnose/",groupFolder), pattern=".txt")
  
  # convert txt v rds
  for (j in 1:length(allTxtFiles)) {
    fileName <- paste0(allDataDir, "/transSubgroupsDiagnose/",groupFolder,"/", allTxtFiles[j])
    myfile <- readLines(fileName)
    rdsResult <- list()
    myrecords <- strsplit(myfile, split=",")
    myrecords <- lapply(myrecords, function(t) as.integer(t))
    for (myrecord in myrecords) {
      if (length(myrecord)>1)
        rdsResult[[myrecord[1]]] <- myrecord[-1]
    }
    fileName2 <- paste0(allDataDir, "/transSubgroupsDiagnose/",groupFolder,"/", gsub("\\.txt$", "\\.rds", allTxtFiles[j]))
    saveRDS(rdsResult, fileName2)
    
    # remove txt file
    unlink(fileName)
  }
  print(proc.time() - myTime)
}
