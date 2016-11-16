library(dplyr)
library("gapminder")
gapminder
head(gapminder)
##select the different countries
countries <- as.character(unique(unlist(gapminder$country)))

new <- gapminder %>%
  filter(year >= 1952) %>%
  group_by(country)%>%
  summarize(meangdpPercap = mean(gdpPercap),meanlifeExp=mean(lifeExp))

reg <- lm(meanlifeExp~meangdpPercap, data = new)  
summary(reg)