library(tidyverse)
library(readxl)



goodsDFRaw <- readxl::read_excel(path = "Data/lipseyData.xlsx", sheet = "ArrangedForR") %>%
  tibble()

goodsDF <- goodsDFRaw %>% pivot_longer(
  cols = starts_with("1"),
  names_to = "year", values_to = "value", names_transform= as.numeric, values_transform = as.numeric)


saveRDS(goodsDF, "Data/tradeData.rds")
