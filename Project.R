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

## Importing packages

library(tidyverse) # metapackage with lots of helpful functions

library(reshape2)
library(lattice)
library(psych)
library(DataExplorer)
library(viridis)
#library(viridisLite)


# read in the datasets
df15 <- read_csv("/kaggle/input/world-happiness/2015.csv")
df16 <- read_csv("/kaggle/input/world-happiness/2016.csv")
df17 <- read_csv("/kaggle/input/world-happiness/2017.csv")
df18 <- read_csv("/kaggle/input/world-happiness/2018.csv")
df19 <- read_csv("/kaggle/input/world-happiness/2019.csv")

# add a year column to each data set
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
  scale_fill_gradient(low = "red4", high = "green") +
  labs(x = "", y = "Median Happiness Score", fill = "Score",
       title = "Median happiness score per region") +
  theme_light(base_size = 18) + coord_flip()


















