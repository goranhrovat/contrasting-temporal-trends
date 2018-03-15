library(shinyjs)
library(shiny)
library(arules)
library(party)
library(stringr)
library(memoise)
library(sqldf)
library(DT)
library(Rcpp)

##### Settings ######################
# full path to current "ShinyApp" directory e.g. "C:/ShinyApp" without char "/" at the end
scriptsDir <- "C:/../ShinyApp"

# full path to directoty .../allData without char "/" at the end
# directory should contains files created in "PrepareData" step
allDataDir <- "I:/allData"

# port for web app
PORT <- 6574

# path to file (e.g. requestStats.txt) for logging (or comment the line to prevent logging)
# requestStatsFile <- "C:/../ShinyApp/log/requestStats.txt"
#################################### #

allDataDir <- paste0(allDataDir, "/transSubgroupsDiagnose")

IP <- "0.0.0.0"

options(OutDec= ".", scipen = 5)

source(paste0(scriptsDir,'/node_myPlot2.R'), echo=F)
source(paste0(scriptsDir,'/node_myInner.R'), echo=F)
source(paste0(scriptsDir,'/myEdgeSimple.R'), echo=F)
source(paste0(scriptsDir,'/getOLAP.R'), echo=F)

kode.list <- unlist(readRDS(paste0(scriptsDir,"/codeList.rds")))

#### add diagnoses not on the list ####
# get all available diagnoses in the directory
firstDir <- list.dirs(allDataDir, full.names = F, recursive = F)[1]
ostaleDiagnoze <- gsub("(^D_)|(.rds$)", "", list.files(paste0(allDataDir,"/", firstDir), pattern = "D_"))
# keep only diagnoses where file exists
keep.list <- intersect(names(kode.list), ostaleDiagnoze)
kode.list <- kode.list[keep.list]
# get only diagnoses not on the selectize list
ostaleDiagnoze <- setdiff(ostaleDiagnoze, names(kode.list))
# add them to selectize list and add name missing
kode.list[ostaleDiagnoze] <- paste0("name missing (",ostaleDiagnoze,")")
#################################### #

########## set dropdown box ############
myYears <- readRDS(paste0(allDataDir, "/myYears.rds"))
myAges <- readRDS(paste0(allDataDir, "/myAges.rds"))

yearDrop <- as.character(1:length(myYears))
names(yearDrop) <- names(myYears)

ageDrop <- as.character(1:length(myAges))
names(ageDrop) <- names(myAges)
###################################### #

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Trends of interest of assocation rules and item-sets"),
  wellPanel(
    fluidRow(
      tabsetPanel(
        tabPanel("Item-set", 
                 fluidPage(
                   fluidRow(
                     column(3, style='padding:0px;',
                            selectizeInput('itemSet', label=NULL, choices=NULL, multiple=T)),
                     column(2, style='padding-left:5px;padding-right:5px;min-width:200px;',
                            checkboxInput("useUnion", "Union of diagnoses", value = FALSE, width = NULL)
                     )
                                             ),
                   fluidRow(
                     column(2, offset = 0, style='padding-left:0px;padding-right:5px;min-width:200px;',
                            selectInput("measureS", "Interestingness measure:",
                                        c("support" = "support"))
                     )
                   ))),
        tabPanel("Association rule", 
                 fluidPage(
                   fluidRow(
                     column(3, style='padding:0px;',
                            selectizeInput('lhsRule', label=NULL, choices=NULL, multiple=T)),
                            HTML('<div style="float:left;font-size:x-large;padding-left:5px;padding-right:5px;">&rArr;</div>'),
                     column(2, style='padding:0px;',
                            selectizeInput('rhsRule', label=NULL, choices=NULL, multiple=F))),
                   fluidRow(
                     column(2, offset = 0, style='padding-left:0px;padding-right:5px;min-width:200px;',
                            selectInput("measureR", "Interestingness measure:",
                                        c("confidence" = "confidence",
                                          "lift" = "lift"))
                     )
                   ))),
        id="tabSet", type = "tabs")
                     ),
    fluidRow(
      column(1, offset = 0, style='padding-left:0px;padding-right:5px;min-width:150px;',
             numericInput("alpha", "Significance level:", 0.05, min = 0.001, max = 1.0, step = 0.01)
      ),
      column(2, offset = 0, style='padding-left:0px;padding-right:5px;min-width:250px;',
             numericInput("minSplit", "Min. number of observations in node:", 100, min = 5, max = 500, step = 5)
      ),
      column(2, offset = 0, style='padding-left:0px;padding-right:5px;min-width:250px;',
             selectInput("ageDiscretization", "Discretization of age:",
                         ageDrop
                        )
      ),
      column(2, offset = 0, style='padding-left:0px;padding-right:5px;min-width:200px;',
             selectInput("periodDiscretization", "Discretization of time period:",
                         yearDrop
                        )
      )),
    fluidRow(
      column(2, offset = 0, style='padding-left:0px;padding-right:5px',
             actionButton("btnShow", "Show")
      )
    )
                 ),
  htmlOutput("myOutputInfo"),
  plotOutput('plot', height="550", click="plot_click"),
  fluidRow(DT::dataTableOutput('mytable'))
)

terminalCount <- function(drevo) {
  if (drevo$terminal) return(1)
  else terminalCount(drevo$left)+terminalCount(drevo$right)
}

getTerminals <- function(drevo) {
  if (drevo$terminal) return(list(drevo))
  else c(getTerminals(drevo$left), getTerminals(drevo$right))
}

getItems <- function(input) {
  if (is.null(input)) NULL else paste0("D_", str_match(input, "\\(([a-zA-Z0-9\\.]*)\\)$")[,2])
}

server <- function(input, output, session) {
  session$userData$currentOLAPdata <- NULL
  
  for (selID in c("itemSet", "lhsRule", "rhsRule")) {
    updateSelectizeInput(session, selID, choices = kode.list,
         options=list(
          maxOptions = 30,
          maxItems = 30,
          searchField = c("value", "label"),
          render = I('{
                      option: function(item, escape) {
                      return "<div><strong>" + escape(item.label) + "</strong> (" + escape(item.value) + ")</div>"
                      }
                      }')
          ),
         server = T
        )
  }
  
  getModelreactive <- reactive({
    isRule <- grepl("rule",input$tabSet) # T or F
    measure <- if (isRule) input$measureR else input$measureS # "support", "lift", "confidence"
    alpha <- input$alpha # number
    minSplit <- input$minSplit # number
    
    dat <- getOLAPreactive()
    
    ctrl <- mob_control(alpha = alpha, minsplit = minSplit, objfun = deviance, verbose=F)

    mob(as.formula(paste(measure, "~ mymonth | `Age group` + Gender")), data = dat, control = ctrl, model = linearModel)
  })
  
  # if any of the parameters changes, the function is called again
  getOLAPreactive <- reactive({
    isRule <- grepl("rule",input$tabSet) # T or F
    myitems <- getItems(input$itemSet) # c("D_401.9", ...)
    mylhs <- getItems(input$lhsRule) # c("D_401.9", ...)
    myrhs <- getItems(input$rhsRule) # "D_401.9"
    useUnion <- input$useUnion # bool
    ageDiscretization <- as.numeric(input$ageDiscretization)
    periodDiscretization <- as.numeric(input$periodDiscretization)
    
    groupFolder <- paste0(periodDiscretization,".",ageDiscretization)
    
    if (isRule) {
      if (is.null(mylhs) | is.null(myrhs)) return(NULL)
      if (myrhs %in% mylhs) return(NULL)
      rule <- list(lhs=mylhs, rhs=myrhs)
    } else {
      if (is.null(myitems)) return(NULL)
      rule <- list(items=myitems)
    }
    
    dat <- getOLAP(rule, groupFolder, useUnion)
    
    if (nrow(dat)==0) {
      return(dat)
    }
    
    dat$female <- factor(dat$female)
    dat$age <- factor(dat$age)
    
    levels(dat$age) <- as.character(myAges[[ageDiscretization]])
    
    dat$"mymonth" <- (dat$year-min(dat$year))*length(myYears[[periodDiscretization]])+dat$amonth/(12/length(myYears[[periodDiscretization]]))
    
    colnames(dat)[colnames(dat)=="year"] <- "Year"
    colnames(dat)[colnames(dat)=="amonth"] <- "Month"
    colnames(dat)[colnames(dat)=="age"] <- "Age group"
    colnames(dat)[colnames(dat)=="female"] <- "Gender"
    levels(dat$Gender) <- c("Male", "Female")
    dat
  })
  
  output$plot <- renderPlot({
    if (input$btnShow == 0) return()
    
    shinyjs::disable("btnShow")
    progress <- shiny::Progress$new(min=0, max=100)
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit({
      shinyjs::enable("btnShow")
      progress$close()
    })
    
    progress$set(message = "Calculating interestingness measures for all subgroups (Task 1/2)", value = 30)
    
    isolate({
      ################ code
      isRule <- grepl("rule",input$tabSet) # T or F
      myitems <- getItems(input$itemSet) # c("D_401.9", ...)
      mylhs <- getItems(input$lhsRule) # c("D_401.9", ...)
      myrhs <- getItems(input$rhsRule) # "D_401.9"
      measure <- if (isRule) input$measureR else input$measureS # "support", "lift", "confidence"
      useUnion <- input$useUnion # bool
      alpha <- input$alpha # number
      minSplit <- input$minSplit # number
      ageDiscretization <- as.numeric(input$ageDiscretization)
      periodDiscretization <- as.numeric(input$periodDiscretization)
      
      groupFolder <- paste0(periodDiscretization,".",ageDiscretization)
      
      # write (log) request to file
      if (exists("requestStatsFile")) {
        line <- paste(Sys.time(), session$request$REMOTE_ADDR, "isRule=", isRule,
                      "items=", paste0(myitems, collapse = ","), "lhs=", paste0(mylhs, collapse = ","), "rhs=", paste0(myrhs, collapse = ","), 
                      "union=", useUnion, "measure=", measure, "alpha=", alpha, "minSplit=", minSplit,
                      "groupFolder=", groupFolder,
                      "age=", names(ageDrop)[ageDiscretization],
                      "period=", names(yearDrop)[periodDiscretization],
                      sep = ";")
        write(line, file=requestStatsFile, append=TRUE)
      }
      ################### #
      if (isRule) {
        if (is.null(mylhs) | is.null(myrhs)) return(NULL)
        if (myrhs %in% mylhs) return(NULL)
        rule <- list(lhs=mylhs, rhs=myrhs)
      } else {
        if (is.null(myitems)) return(NULL)
        rule <- list(items=myitems)
      }
      
      dat <- getOLAPreactive()
      
      session$userData$currentOLAPdata <- dat
      
      session$userData$currentindRows <- 0
      if (length(unique(dat[, measure]))==1) {
        stop("Rule or item-set does not exists!")
        return(NULL)
      }
      
      progress$set(message = "Building regression tree (Task 2/2)", value = 60)
      
      model <- getModelreactive()
      
      session$userData$currentModel <- model
      
      ylab <- if(measure=="support") "support" else if (measure=="lift") "lift" else "confidence"
      
      xlab <- if(length(unique(dat$Month))==4) "quarter"
              else if (length(unique(dat$Month))==12) "month"
              else if (length(unique(dat$Month))==2) "half-year"
              else "year"
      
      numLeaves <- terminalCount(model@tree)
      rM <- range(response(model))
      rM2 <- round(range(dat$mymonth))
      if(xlab=="half-year") {
        if (numLeaves > 5) rM2by <- 10 else rM2by <- 5
      } else if (xlab=="quarter") {
        if (numLeaves > 5) rM2by <- 20 else rM2by <- 10
      } else {
        if (numLeaves > 5) rM2by <- 70 else rM2by <- 35
      }
      
      xskala <- unique(c(1,seq(1,rM2[2],by=rM2by),rM2[2]))
      xskala <- xskala[xskala >= rM2[1]]
      xskala <- xskala[xskala <= rM2[2]]
      yskala <- unique(round(seq(rM[1],rM[2],length.out = 4),3))
      if (length(yskala)<2) yskala <- unique(round(seq(round(rM[1],2),rM[2],length.out = 4), 4))
      if (length(yskala)==0) yskala <- c(rM[1]-0.1,rM[2]+0.1)
      
      node <- node_myPlot2(model, which = NULL, id = F, labels = T,
                           linecol = "#0072B2", cex=0.25, col= "red", pch=16,
                           ylines = 3, xlab = xlab, ylab = ylab,
                           xskala=xskala,
                           yskala=yskala )
      
      nodeIn <- node_myInner(model, digits = 4, abbreviate = F, fill = "white", pval = F, id = F)
      
      myedge <- myEdgeSimple(model, numInRow=3)
      
      # png(paste0("image.png"), width = 45, height = 23, units = 'cm', res = 800, bg="white")
      options(OutDec= ",")#, digits = 8, "scipen"=100)
      plot(model, edge_panel=myedge, inner_panel=nodeIn, terminal_panel=node)
      options(OutDec= ".")
      # dev.off()
      
      ################ end code
    })
    
  }, res = 72, height=550 )
  
  output$myOutputInfo <- renderUI({
    if (input$btnShow == 0) return()
    
    isolate({
      isRule <- grepl("rule",input$tabSet) # T or F
      myitems <- getItems(input$itemSet) # c("D_401.9", ...)
      mylhs <- getItems(input$lhsRule) # c("D_401.9", ...)
      myrhs <- getItems(input$rhsRule) # "D_401.9"
      measure <- if (isRule) input$measureR else input$measureS # "support", "lift", "confidence"
      measure <- if (measure=="support") "support" else if (measure=="lift") "lift" else "confidence"
      useUnion <- input$useUnion # T or F
      alpha <- input$alpha # number
      minSplit <- input$minSplit # number
      ageDiscretization <- as.numeric(input$ageDiscretization)
      periodDiscretization <- as.numeric(input$periodDiscretization)
      
      if (isRule) {
        if (is.null(mylhs) | is.null(myrhs)) return("Right or left side of the rule can not be empty!")
        if (myrhs %in% mylhs) return(HTML("Right side of the rule can not appear on the left side."))
      } else {
        if (is.null(myitems)) return("Item-set can not be empty!")
      }
      
      res <- "Regression tree for "
      
      if (isRule) {
        res <- paste0(res, "association rule:<br>")
        res <- paste0(res, "{", paste0(input$lhsRule, collapse = ", "), "} => {", paste0(input$rhsRule, collapse = ", "), "}<br>")
      } else {
        res <- paste0(res, "item-set:<br>")
        if (useUnion) {
          res <- paste0(res, "{", paste0(input$itemSet, collapse = " | "), "} <br>")
        } else {
          res <- paste0(res, "{", paste0(input$itemSet, collapse = ", "), "} <br>")
        }
      }
      
      res <- paste0(res, "Settings: ", "Interestingness measure = ", measure, ", Significance level = ", alpha,
                    ", Min. number of observations in node = ", minSplit,
                    ", Discretization of age = ", names(ageDrop)[ageDiscretization],
                    ", Discretization of time period = ", names(yearDrop)[periodDiscretization])
      if (!isRule) {
        res <- paste0(res, ", Union = ", useUnion)
      }
      
      HTML(res)
    })
  })
  
  output$mytable = DT::renderDataTable({
    isImage <- !is.null(input$plot_click)
    
    isolate({
      tmpdata <- session$userData$currentOLAPdata
      if (is.null(tmpdata)) return() # if has never been loaded
      
      if ("CountSupport" %in% colnames(tmpdata)) {
        tmpdata <- tmpdata[,c("mymonth", "Month", "Year", "Age group", "Gender", "support", "CountSupport", "CountTrans")]
      } else {
        tmpdata <- tmpdata[,c("mymonth", "Month", "Year", "Age group", "Gender", "support", "confidence", "lift",
                              "LeftCount", "RightCount", "AllCount", "CountTrans")]
        tmpdata$confidence <- round(tmpdata$confidence, 8)
        tmpdata$lift <- round(tmpdata$lift, 8)
      }
      
      tmpdata$mymonth <- ordered(tmpdata$mymonth)
      tmpdata$Month <- ordered(as.numeric(ordered(tmpdata$Month)))
      tmpdata$support <- round(tmpdata$support, 8)
      
      colnames(tmpdata)[colnames(tmpdata)=="mymonth"] <- "#"
      colnames(tmpdata)[colnames(tmpdata)=="Month"] <- if(length(unique(tmpdata$Month))==4) "Quarter"
                                                       else if (length(unique(tmpdata$Month))==12) "Month"
                                                       else "Half-year"
      
      if (isImage) {
        model <- session$userData$currentModel
        numTerminals <- terminalCount(model@tree)
        indTerminal <- floor(numTerminals*input$plot_click$x)+1 # which node is clicked
        indTerminal <- min(numTerminals, max(1, indTerminal))
        # get indices of rows for specific terminal node
        indRows <- which(as.logical(getTerminals(model@tree)[[indTerminal]]$model$weights))
        # add it to session for resizing
        session$userData$currentindRows <- indRows
      }
      
      return(DT::datatable(tmpdata[session$userData$currentindRows,], rownames = FALSE, selection = 'none',
                           options=list(pageLength = -1, dom="ti", scrollY = "300px",
                                        autoWidth = F,
                                        language = list(zeroRecords = "Click on a terminal node to display its data"),
                                        order=list(list(0,"asc"),list(1,"asc"),list(2,"asc"))
                           )))
    })
  })
  
}

app <- shinyApp(ui=ui, server=server)
runApp(app, host = getOption("shiny.host", IP), port = getOption("shiny.port", PORT))


