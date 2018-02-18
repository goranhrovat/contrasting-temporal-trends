# faster intersect for two sorted int vectors
cppFunction("
  std::vector<int> vec_intersect(std::vector<int>& a, const std::vector<int>& b) {
    std::vector<int> v_intersection;
    std::set_intersection(a.begin(), a.end(),
                          b.begin(), b.end(),
                          std::back_inserter(v_intersection));
    return v_intersection;
  }
")

readTransInfo <- function(groupFolder) {
  readRDS(paste0(allDataDir,"/", groupFolder,"/transInfo.rds"))
}

readTransInfoM <- memoise(readTransInfo)

readDiagnoze <- function(diagnoze, groupFolder) {
  readRDS(paste0(allDataDir,"/", groupFolder, "/", diagnoze, ".rds"))
}

readDiagnozeM <- memoise(readDiagnoze)

firstDir <- list.dirs(allDataDir, full.names = F, recursive = F)[1]

cacheDiagnoses <- list.files(paste0(allDataDir, "/", firstDir))
cacheDiagnoses <- cacheDiagnoses[sapply(paste0(allDataDir, "/", firstDir, "/", cacheDiagnoses), file.size) > 10E6]
cacheDiagnoses <- gsub(".rds", "", cacheDiagnoses)
getDiagnoze <- function(diagnoze, groupFolder) {
  if (diagnoze %in% cacheDiagnoses) {
    return(readDiagnozeM(diagnoze, groupFolder))
  } else {
    return(readDiagnoze(diagnoze, groupFolder))
  }
}

# function calculates support for item-set for all groups
# itemset is vector of diagnoses e.g. c("D_401.9", "D_250.00")
getItemSetOLAP <- function(itemset, groupFolder, useUnion = F) {
  transData <- lapply(itemset, function(t) getDiagnoze(t, groupFolder))
  
  transDataIndex <- lapply(transData, function(t) which(!sapply(t, is.null)))
  
  if (useUnion) {
    transDataIndex <- Reduce(union, transDataIndex)
    
    transDataCount <- sapply(transDataIndex, function(t) {
      length(Reduce(union, lapply(transData, function(z) {
        if (length(z) < t) return(NULL)
        z[[t]]
      })))
    })
  } else {
    transDataIndex <- Reduce(vec_intersect, transDataIndex)
    
    transDataCount <- sapply(transDataIndex, function(t) {
      length(Reduce(vec_intersect, lapply(transData, function(z) z[[t]])))
    })
  }
  if (length(transDataIndex) < 1) return(data.frame())
  
  transInfo <- readTransInfoM(groupFolder)
  
  finalResult <- lapply(1:length(transDataIndex),function(t) {
    currInd <- transDataIndex[t]
    c(currInd,
      transInfo[[currInd]]$year,
      transInfo[[currInd]]$amonth,
      transInfo[[currInd]]$age,
      transInfo[[currInd]]$female,
      transDataCount[t]/transInfo[[currInd]]$numTrans,
      transDataCount[t],
      transInfo[[currInd]]$numTrans
      )
  })
  
  finalResult <- as.data.frame(do.call(rbind, finalResult))
  colnames(finalResult) <- c("i", "year", "amonth", "age", "female", "support", "CountSupport", "CountTrans")
  finalResult <- finalResult[finalResult$support>0,]
  return(finalResult)
}

getEmptyTransInfo <- function(groupFolder) {
  transInfo <- readTransInfoM(groupFolder)
  
  finalResult <- lapply(1:length(transInfo),function(t) {
    c(transInfo[[t]]$year,
      transInfo[[t]]$amonth,
      transInfo[[t]]$age,
      transInfo[[t]]$female)
  })
  
  finalResult <- as.data.frame(do.call(rbind, finalResult))
  colnames(finalResult) <- c("year", "amonth", "age", "female")
  return(finalResult)
}

getEmptyTransInfoM <- memoise(getEmptyTransInfo)

getOLAP <- function(rule, groupFolder, useUnion=F) {
  
  emptyInfo <- getEmptyTransInfoM(groupFolder)
  
  if (!is.null(rule$items)) { # itemsets
    myOLAP <- getItemSetOLAP(rule$items, groupFolder, useUnion)[,-1]
    
    # add zero values
    finalResult <- sqldf("SELECT E.*, O.support, O.CountSupport, O.CountTrans FROM emptyInfo E
                        LEFT JOIN myOLAP O ON O.year = E.year AND O.amonth = E.amonth AND O.age = E.age AND O.female = E.female")
  } else { # rules
    
    lhsOLAP <- getItemSetOLAP(rule$lhs, groupFolder)
    rhsOLAP <- getItemSetOLAP(rule$rhs, groupFolder)
    allOLAP <- getItemSetOLAP(c(rule$lhs, rule$rhs), groupFolder)
    
    myOLAP <- sqldf('
                    select L.year, L.amonth, L.age, L.female,
                    A.support AS support,
                    A.support/L.support AS confidence,
                    A.support/L.support/R.support AS lift,
                    L.countSupport AS LeftCount,
                    R.countSupport AS RightCount,
                    A.countSupport AS AllCount, A.countTrans AS CountTrans
                    from lhsOLAP L
                    inner join rhsOLAP R ON L.i = R.i
                    inner join allOLAP A ON L.i = A.i
                    ')
    
    # add zero values
    finalResult <- sqldf("SELECT E.*, O.support, O.confidence, O.lift, O.LeftCount,
                          O.RightCount, O.AllCount, O.CountTrans
                          FROM emptyInfo E
                          LEFT JOIN myOLAP O ON O.year = E.year AND O.amonth = E.amonth AND O.age = E.age AND O.female = E.female")
  }
  
  finalResult[is.na(finalResult)] <- 0
  
  return(finalResult)
}
