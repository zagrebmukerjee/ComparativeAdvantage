
library(dplyr)
library(data.table)

countyLevelDFRaw <- fread(file = "C:/Users/Zagreb/Downloads/Docs/Research/Projects/LateC19/LocalData/ipumsFullCountsByCounty.csv")


countyLevelDFClean <- countyLevelDFRaw %>% group_by(
  year, stateICP, countyICP, countyNHG, ind1950, occ1950
) %>%
  summarize(emp = sum(emp))

stateNames <-  read.csv("Data/stateFipsCodes.csv", fileEncoding="UTF-8-BOM") %>%  tibble() %>% rename(stateFIPS = FIPS, stateAbb = Postal.Code, stateName = Name)
countyNames <- read.csv("Data/icpCodes.csv") %>%  tibble() %>%  select(countyName = County, countyICP = County.code, stateICP = STATEICP, stateFIPS = STATEFIPS)


countyLevelData <- countyLevelDFClean %>%  left_join(countyNames) %>%  left_join(stateNames)

saveRDS(countyLevelData, "countyLevelData")


