library(tidyverse)
library(viridis)

## Basic descriptive statistics
tradeData <- readRDS("Data/tradeData.rds")

# Breakdown of exports

tableData <- tradeData %>%
  filter(
    direction == "Export",
    categoryNum %in% c(
      220, 301,302, 221, 222, 144, 145, 147, 146, 5,42, 44, 61,66, 70, 71
    ),
    dataType == "$mn",
    year %in% seq(1880, 1910, 5) ) %>%
  select(-categoryNum, -dataType, -aggregationLevel) %>%
  pivot_wider(id_cols = c("year"), names_from = "categoryName") #%>%
# mutate("Ag Share", "Semi Share", "Mfg Share")


tableData %>%  write.csv("export.csv")

ggplot(
  tradeData %>%
    filter(direction == "Export",
           categoryNum %in% c(220, 221, 301),
           year <= 1910) %>% rowwise() %>%
    mutate(categoryName = if(categoryNum == 221){ "Non-Agricultural"}
           else if(categoryNum == 301) {"Agricultural"} else {"Total"}) %>%
    mutate(categoryName = factor(categoryName,
                                 c("Total", "Non-Agricultural", "Agricultural"))),
  aes(x = year, y= value, color = categoryName)) +
  geom_line() +
  ylab("Millions $") +
  xlab(element_blank()) +
  labs(title = "1895-1910 sees rapid export growth")+
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = "bottom")


chartDataAg <- tradeData %>%
  filter(
    direction == "Export",
    dataType == "$mn",
    categoryNum %in% c(5, 108,42, 312)
  ) %>% group_by(year) %>%
  mutate(percentage = value/sum(value)) %>%
  rowwise() %>%
  mutate(categoryName =
           if(categoryNum == 108){"Animal Products"}
         else if(categoryNum == 42){"Raw Cotton"}
         else if(categoryNum == 312){"Other"}
         else categoryName
  ) %>%
  mutate(categoryName = factor(categoryName, levels = c("Animal Products", "Grains", "Raw Cotton", "Other")))

ggplot(chartDataAg, aes(x = year, y = percentage, fill = categoryName )) +
  geom_area(alpha=0.6 , size=1, colour="black") +
  scale_fill_viridis(discrete = T) +
  theme_minimal() +
  ylab(element_blank()) + xlab(element_blank()) +
  labs(title = "Composition of Agricultural Exports") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.title = element_blank(), legend.position = "bottom")



chartDataNAg <- tradeData %>%
  filter(
    direction == "Export",
    dataType == "$mn",
    categoryNum %in% c(146, 61,313, 144,145)
  ) %>% group_by(year) %>%
  mutate(percentage = value/sum(value)) %>%
  rowwise() %>%
  mutate(categoryName =
           if(categoryNum == 146){"Metal Manufactured"}
         else if(categoryNum == 61){"Petroleum Products"}
         else if(categoryNum == 313){"Other Manufactured"}
         else if(categoryNum == 145){"Semimanufactured"}
         else if(categoryNum == 144){"Crude Materials"}
         else categoryName
  ) %>%
  mutate(
    categoryName = factor(
      categoryName,
      levels = c("Metal Manufactured", "Petroleum Products", "Other Manufactured",
                 "Semimanufactured", "Crude Materials")))

ggplot(chartDataNAg, aes(x = year, y = percentage, fill = categoryName )) +
  geom_area(alpha=0.6 , size=1, colour="black") +
  scale_fill_viridis(discrete = T) +
  theme_minimal() +
  ylab(element_blank()) + xlab(element_blank()) +
  labs(title = "Composition of Non-Agricultural Exports") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.title = element_blank(), legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.key.size = unit(.035, "npc")) +
  guides(fill = guide_legend(nrow =2, byrow = T))

