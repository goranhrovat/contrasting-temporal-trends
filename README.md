# contrasting-temporal-trends

################################################################
#                                                              #
# Instructions for preparing data and starting web application #
#                                                              #
################################################################

##############################
# 1. Preparing the data      #
##############################

In that step you firstly need to extract HCUP NIS data (ASC and SPS files) to some directory (see picture "HCUP NIS data.jpg")
Open file "PrepareData/prepareData.R" and check if all used packages are installed.
Set variables in ## Settings ## section at the top of the file (after written packages).
Read comment above each variable.
Run file and wait for files to be created. Be patient, it is a long running task.

##############################
# 2. Running web application #
##############################

Open file "ShinyApp/defaultENG.R" and check if all used packages are installed.
Set variables in ## Settings ## section at the top of the file (after written packages).
Read comment above each variable.
In the ## Optional settings ## section you can define additional years and/or ages discretizations.
Run file to start Shiny web server.
