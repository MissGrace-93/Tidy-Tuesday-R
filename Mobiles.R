setwd("D:/R Projects/TidyTuesday")
install.packages("tidytuesdayR")
install.packages("tidyverse")


library(tidytuesdayR)
library(tidyverse)

tuesdata <-tt_load('2020-11-10')

mobile <- tuesdata$mobile
landline <- tuesdata$landline



df <- inner_join(mobile, landline, by=c("code", "year", "entity", "continent", "gdp_per_cap"))


## Divergent bar plot of UK mobile & landline subcriptions

# Diverging bar chart requires x one set by -1
#find the inverse to plot divergent bars


df2 <- df %>%
  filter(entity == "United Kingdom") %>%
  gather(key = "phoneType", value = "subs", -entity, -code, -total_pop.x, -total_pop.y, -gdp_per_cap, -continent, -year)



df2 <- df2 %>%
  mutate(subs = ifelse(phoneType == "landline_subs", subs * -1, subs))

## plot

df2 %>%
  ggplot(aes(x = year, y = subs, fill = phoneType))+
  geom_bar(stat = "identity", position = "identity")+
  geom_hline(yintercept = 0, color = "white")+
  scale_y_continuous(breaks = seq(from = -75, to = 125, by = 15), labels=abs(seq(from = -75, to = 125, by = 15)))+
  scale_x_continuous(breaks = seq(from = 1990, to = 2017, by = 2), expand = c(0,0))+
  scale_fill_manual(name = "Phone Type", labels = c("Landline", "Mobile"), values = c("#DE77AE", "#B8E186"))+
  coord_flip() +
    labs(
    x = "Year",
    y = "Landline/Mobile Phone Subscriptions per 100 People",
    fill = "Phone Type",
    title ="UK Phone Subscriptions Between 1990 and 2017",
    subtitle = "Landline subscriptions have stayed pretty constant, peaking in the year 2000, while mobile subscriptions rapidly grew and then plateaued after 2007",
    caption = "Plot: @missgrace_93 \n Data: ourworldindata.org \n #TidyTuesday") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
        plot.caption = element_text(face = "italic"))
   
install.packages("RColorBrewer")
library(RColorBrewer)

display.brewer.all(type = "div",
                   colorblindFriendly = TRUE)
brewer.pal(n = 11, name = "PiYG")


