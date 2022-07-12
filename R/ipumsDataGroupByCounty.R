library(dplyr)
library(tictoc)
library(data.table)
options(dplyr.summarise.inform = FALSE)

# num of rows <- 256530960
filePath <- "C:/Users/Zagreb/Downloads/Docs/Research/Projects/LateC19/LocalData/fullCountSplit/ipumsFullCountOcc_"
outputFileName <- "C:/Users/Zagreb/Downloads/Docs/Research/Projects/LateC19/LocalData/ipumsFullCountsByCounty.csv"

colnamesDF <- c("year", "stateICP", "countyICP", "countyNHG", "occ1950", "ind1950","histID")

nFiles <- 2565
tic()
for(i in 1:nFiles){

  inFileName <- paste0(filePath,i-1,".csv")
  tmpDFRaw  <- fread(inFileName)
  colnames(tmpDFRaw) <- colnamesDF

  tmpDF <- tmpDFRaw %>%
    group_by(year,stateICP, countyICP, countyNHG, ind1950, occ1950) %>%
    summarize(
      emp = n(),
    )

  if( (1000*round(i/nFiles,3)) %%  100== 0){sprintf("%i %%",100*round(i/nFiles,2)) %>%  print()}
  fwrite(x = tmpDF, file = outputFileName, append = ifelse(i==1, F,T))

}
toc()
