#devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)
library(tidyverse)
library(stringr)

countyLevelData <- readRDS("countyLevelData")

metalsCodes <- read.csv("Data/ind1950Mfg.csv", fileEncoding="UTF-8-BOM")
occCodes <- read.csv("Data/occupationCodes.csv", fileEncoding="UTF-8-BOM")
states_sf <- get_urbn_map(map = "states", sf = TRUE)
counties_sf <- get_urbn_map(map = "counties", sf = TRUE)

empMfgMetals <- countyLevelData %>%
  ungroup() %>%
  select(-countyNHG) %>%
  # filter(ind1950 %in% metalsCodes$Code) %>%
  filter(occ1950 %in% c(524, 641)) %>%
  group_by(across(c(-ind1950, -occ1950, -emp))) %>%
  summarize(emp = sum(emp)) %>%
  filter(!is.na(countyName)) %>%
  mutate(countyName = sub("\\/.*", "", countyName)) %>%
  mutate(stateFIPS =sprintf("%02.0f",stateFIPS)) %>% ungroup()

empMfgMetalsWide <- empMfgMetals %>%
  pivot_wider(id_cols = c(stateICP, stateFIPS,countyICP, countyName, stateName, stateAbb),
              values_from = emp, names_from = year  ) %>%
  rename(emp1910 = `1910`, emp1900 = `1900`, emp1880 = `1880`, emp1870 = `1870`)

countiesClean <- counties_sf %>%
  mutate(countyName = word(county_name,end=-2)) %>%
  rename(stateFIPS = state_fips) %>%
  full_join(empMfgMetals) %>%
  filter(!(stateFIPS %in% c("02", "15"))) %>% # states that didn't exist
  filter(!is.na(year)) %>%
  mutate(emp = ifelse(is.na(emp), 0, emp))

countiesWide <-  counties_sf %>%
  mutate(countyName = word(county_name,end=-2)) %>%
  rename(stateFIPS = state_fips) %>%
  full_join(empMfgMetalsWide) %>%
  filter(!(stateFIPS %in% c("02", "15"))) %>% # states that didn't exist
  mutate(
    emp1910 = ifelse(is.na(emp1910), 0, emp1910),
    emp1900 = ifelse(is.na(emp1900), 0, emp1900),
    emp1880 = ifelse(is.na(emp1880), 0, emp1880),
    emp1870 = ifelse(is.na(emp1870), 0, emp1870)
  ) %>%
  mutate(chg1910 = `emp1910` - `emp1900`, chg1900 = `emp1900` - `emp1880`)



# ggplot(countiesClean ) +
#   geom_sf(aes(fill = emp), color = NA) +
#   scale_fill_distiller(palette = "YlOrRd", direction = 1) +
#   facet_wrap(~year, nrow =2)+
#   theme_minimal() +
#   ggtitle("Metals-Related Manufacturing Workers (Log)") +
#   theme(axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         legend.position= "none")
#
#
# ggplot(countiesClean %>%
#          filter(
#            !(stateFIPS %in% c("06", "53", "41", "32", "16",
#                               "49", "56", "30", "04", "35", "08")))  # focus on East
# ) +
#   geom_sf(aes(fill = log(emp)), color = NA) +
#   scale_fill_distiller(palette = "YlOrRd", direction = 1) +
#   # scale_fill_gradient(low = "white", high = "black") +
#   facet_wrap(~year, nrow =2)+
#   theme_minimal() +
#   ggtitle("Metals-Related Manufacturing Workers (Log)") +
#   theme(axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         legend.position= "none")







ggplot(
  countiesClean  %>%
    filter(
      !(stateFIPS %in% c(
        "06", "53", "41", "32", "16", "49",
        "56", "30", "04", "35", "08"))) %>% # focus on East
    filter(
      !(stateFIPS %in% c(
        "28","05", "13", "01", "45","40","37",
        "12", "48", "22", "31", "38", "46", "31", "20")) # down to NE
    )) +
  geom_sf(aes(fill = log(emp)), color = NA) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  facet_wrap(~year, nrow =2)+
  theme_minimal() +
  ggtitle("Metals-Related Manufacturing Workers (Log)") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position= "none")






ggplot(
  countiesWide  %>%
    filter(
      !(stateFIPS %in% c(
        "06", "53", "41", "32", "16", "49",
        "56", "30", "04", "35", "08"))) %>% # focus on East
    filter(
      !(stateFIPS %in% c(
        "28","05", "13", "01", "45","40","37",
        "12", "48", "22", "31", "38", "46", "31", "20")) # down to NE
    )
  ) +
  geom_sf(aes(fill = chg1900), color = NA) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  theme_minimal() +
  ggtitle("Change in Steel-Making Employment") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  labs(fill = "Change 1880-1900")
        # legend.position= "none")


countiesWide %>% arrange(desc(chg1900))

# test for mistaken county names etc
# countiesClean %>%  filter(is.na(county_name)) %>% arrange(stateFIPS,countyName) %>%  write.csv("test.csv")
# countiesClean %>%  filter(is.na(year)) %>% arrange(stateFIPS,countyName) %>%  View()

# countyLevelData %>% filter(ind1950 %in% metalsCodes$Code) %>%  filter(stateFIPS == "25") %>% View()

