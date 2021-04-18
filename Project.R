library(tidyverse) # metapackage with lots of helpful functions

library(reshape2)
library(lattice)
library(psych)
library(DataExplorer)
library(viridis)

#reading data
df15 <- read_csv("input/2015.csv")
df16 <- read_csv("input/2016.csv")
df17 <- read_csv("input/2017.csv")
df18 <- read_csv("input/2018.csv")
df19 <- read_csv("input/2019.csv")

df15 <- df15 %>% mutate(Year = 2015)
df16 <- df16 %>% mutate(Year = 2016)
df17 <- df17 %>% mutate(Year = 2017)
df18 <- df18 %>% mutate(Year = 2018)
df19 <- df19 %>% mutate(Year = 2019)




# rename the columns to standardize
df17 <- df17 %>% rename(c("Happiness Rank" = "Happiness.Rank" ,              
                          "Happiness Score" = "Happiness.Score",
                          "Economy (GDP per Capita)" = "Economy..GDP.per.Capita.",  
                          "Health (Life Expectancy)" = "Health..Life.Expectancy.",     
                          "Trust (Government Corruption)" = "Trust..Government.Corruption.",
                          "Dystopia Residual" = "Dystopia.Residual"))#; names(df17)

df18 <- df18 %>% rename(c("Country" = "Country or region",
                          "Happiness Rank" = "Overall rank",
                          "Happiness Score" = "Score",
                          "Economy (GDP per Capita)" = "GDP per capita",              
                          "Health (Life Expectancy)" = "Healthy life expectancy",
                          "Freedom" = "Freedom to make life choices" ,
                          "Trust (Government Corruption)" = "Perceptions of corruption"))#; names(df18)

# change from a character to a numeric data type
df18$`Trust (Government Corruption)` <-  as.numeric(df18$`Trust (Government Corruption)`)

df19 <- df19 %>% rename(c("Country" = "Country or region",
                          "Happiness Rank" = "Overall rank",
                          "Happiness Score" = "Score",
                          "Economy (GDP per Capita)" = "GDP per capita",
                          "Health (Life Expectancy)" = "Healthy life expectancy",    
                          "Freedom" = "Freedom to make life choices",
                          "Trust (Government Corruption)" = "Perceptions of corruption"))#;names(df19)


# bind the sepearte tables
a <- bind_rows(df15, df16)
b <- bind_rows(a, df17)
c <- bind_rows(b, df18)
d <- bind_rows(c, df19)


# drop the columns with too many NA's
n <- c("Family", "Standard Error", "Dystopia Residual", "Region",  
       "Upper Confidence Interval", "Lower Confidence Interval",
       "Whisker.high", "Whisker.low", "Social support")
d <- d[ , !(names(d) %in% n)]


r1 <- df15 %>% select("Country", "Region")
r2 <- df16 %>% select("Country", "Region")
r3 <- full_join(r1, r2, by = c("Country" = "Country","Region" = "Region"))
# join the region data to the data frame
df <- left_join(d, r3, by = "Country"); head(df); tail(df)



# deal with missing data and data types

df <- df %>% drop_na()
# change region to factor
df$Region <- as.factor(df$Region)
df$Year <- as.factor(df$Year)

# combine southern asia and southeastern asia and eastern asia
df <- df %>% mutate(Region = fct_recode(Region,
                                        # new name         old name
                                        "Southeastern Asia" = "Southern Asia", 
                                        "Southeastern Asia" = "Eastern Asia"))



### Number of countries per region ###

options(repr.plot.width=12, repr.plot.height=8)
ggplot(df, aes(Region, fill = Region)) +
  geom_bar(stat = "count", show.legend = F, color = 'gray') + 
  scale_fill_brewer(palette = "Reds") + coord_flip() +
  labs(x = "", fill = "Region", y = "Number of countries", 
       title = "Number of Countries per Region") + 
  theme_light(base_size = 18)


### Median happiness score per region ###


r <- df %>%
  select(Region, `Happiness Score`) %>%
  group_by(Region) %>%
  summarise(n = n(),
            mn = mean(`Happiness Score`),
            md = median(`Happiness Score`),
            std = sd(`Happiness Score`))

ggplot(r, aes(x = fct_reorder(Region, md), y = mn, fill = mn)) +
  geom_bar(stat = "Identity", show.legend = F) +
  scale_fill_gradient(low = "red", high = "green") +
  labs(x = "", y = "Median happiness score", fill = "Score",
       title = "Median happiness score per region") +
  theme_light(base_size = 18) + coord_flip()

### GDP per capita per region ###

e <- df %>%
  select(Region, `Economy (GDP per Capita)`) %>%
  group_by(Region) %>%
  summarise(n = n(),
            md = median(`Economy (GDP per Capita)`))

ggplot(data = e, aes(x = Region, y = md, fill = md)) + 
  geom_bar(stat = "identity", show.legend = F) + 
  scale_fill_gradient(low = "red", high = "green") +
  labs(x= "", y = "Economy (GDP per Capita) Score", 
       title = "GDP per capita per region") + 
  theme_light(base_size = 18) + coord_flip()

### Life expectancy per region ###

h <- df %>% select(Region, `Health (Life Expectancy)`) %>%
  group_by(Region) %>%
  summarise(md = median(`Health (Life Expectancy)`))

ggplot(data = h, aes(x = Region, y = md, fill = md)) +
  geom_bar(stat = "identity", show.legend = F) +
  scale_fill_gradient(low = "red", high = "green") +
  labs(x = "", y = "Life expectancy percentage", 
       title = "Life expectancy per region") + 
  coord_flip() + theme_light(base_size = 18)

### Freedom per region ###

f <- df %>% select(Region, Freedom) %>%
  group_by(Region) %>%
  summarise(md = median(Freedom))

ggplot(data = f, aes(x = Region, y = md, fill = md)) +
  geom_bar(stat = "identity", show.legend = F) + 
  scale_fill_gradient(low = "red", high = "green") + 
  labs(x = "", title = "Freedom per region", y = "Freedom percentage") +
  coord_flip() + theme_light(base_size = 18)

### Trust in government per region ###

t <- df %>%
  select(Region, `Trust (Government Corruption)`) %>%
  group_by(Region) %>%
  summarise(md = median(`Trust (Government Corruption)`))

ggplot(data = t, aes(x = Region, y = md, fill = md)) +
  geom_bar(stat = "identity", show.legend = F) +
  scale_fill_gradient(low = "red", high = "green") +
  labs(x = "", title = "Trust in governmnent per region", 
       y = "Trust Score") +
  coord_flip() + theme_light(base_size = 18)













