
load.SAScii <- function(SAScii_Data, SAScii_Setup) {
  SASciiFrame <- read.SAScii(SAScii_Data, SAScii_Setup) #reads in the dataframe minus the labels
  #this part loads in the labels
  lineIncrement <- 1
  SASciiTypeFile <- readLines(con=SAScii_Setup)
  currentLine <- SASciiTypeFile[lineIncrement]
  columnNameList <- c()
  #this part skips through the data section
  while (gsub('\\s', '', (currentLine)) != "LABEL") {
    currentLine <- SASciiTypeFile[lineIncrement]
    lineIncrement <- lineIncrement+1
  } 
  #this part cleans and breaks apart the label section into the actual names
  lineIncrement <- lineIncrement-1
  loopfor <- length(SASciiTypeFile) - lineIncrement-1
  while (!grepl(';', SASciiTypeFile[lineIncrement])) { #looks ahead to check for the ;. bit hacky
    lineIncrement <- lineIncrement+1
    currentLine <- SASciiTypeFile[lineIncrement]
    columnNameList <- append(columnNameList, as.list(strsplit(
      gsub('(?<!\")\"(?!\")', perl=T, '', 
           gsub(' *V\\d+ = ', '', 
                gsub(';', '', str_trim(currentLine)))), "\"\"")[[1]]))
  }
  
  colnames(SASciiFrame) <- columnNameList
  return(SASciiFrame)
}
