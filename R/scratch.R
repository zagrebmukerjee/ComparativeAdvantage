countyLevelData %>% ungroup() %>%
# select(year, ind1950, occ1950) %>%
  filter(year==1900, ind1950== 336) %>%   distinct() %>% arrange(desc(emp)) %>%
  View()


countyLevelData %>% ungroup() %>%
  # select(year, ind1950, occ1950) %>%
  filter(year==1900, occ1950== 641) %>%   distinct() %>% arrange(desc(emp)) %>%
  write.csv("test.csv")


