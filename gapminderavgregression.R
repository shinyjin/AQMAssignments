library(dplyr)
library(ggplot2)
library("gapminder")
gapminder
head(gapminder)
##select the different countries
countries <- as.character(unique(unlist(gapminder$country)))

new <- gapminder %>%
  filter(year >= 1952) %>%
  group_by(country)%>%
  summarize(meangdpPercap = mean(gdpPercap),meanlifeExp=mean(lifeExp),country=country)

reg <- lm(meanlifeExp~meangdpPercap, data = new) 
r <- residuals(reg)
plot(r)
g <- ggplot(data = new, aes(x=meangdpPercap, y=meanlifeExp))
l <- ggplot(data=new, aes(x=log(meangdpPercap),y = meanlifeExp))
logreg <- lm(meanlifeExp~log(meangdpPercap), data = new) 
R <- residuals(logreg)
summary(logreg)
g+geom_point(alpha = 1/2)+geom_smooth(method="lm",color="pink")
l+geom_point(alpha = 1/2)+geom_smooth(method="glm",color="blue")

summary(reg)
