library(tidyverse)
GDPPart <- read.csv("C:/Users/marvi/Downloads/SYB64_230_202110_GDP and GDP Per Capita.csv")
EDUPart <- read.csv("C:/Users/marvi/Downloads/SYB64_245_202110_Public expenditure on education.csv")
GDP1 <- GDPPart %>% filter(Year == c(2010,2015,2017)) %>% filter(Series == "GDP in current prices (millions of US dollars)")
ED1 <- EDUPart %>% filter(Series == "Public expenditure on education (% of GDP)") %>% filter(Year == c(2010,2015,2017)) %>%  rename(percent = Value)

All <- merge(GDP1,ED1, by = "X",all.x = T)


as.numeric(p$Value)
is.numeric(p$percent)
c <- p$Value
d <- as.numeric(gsub(",","",c))

p <- All %>% filter(Year.x == Year.y) %>% arrange(X, Year.x) %>% mutate(educost =d * percent / 100 )

f <- p %>% select(X,Year.x,Value,percent,educost) %>% rename(Country = X, Year = Year.x, GDP = Value,edcp = percent, edcd = educost)

write.csv(f,file = "Educational spendint by country -- 2010, 2015, 2017.csv")
library(ggplot2)
s <- f %>% group_by(Country)
ggplot(data = s)+
  geom_bar(aes(Country))

