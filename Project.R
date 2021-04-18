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

pairs(
  formula = `Happiness Score` ~ `Economy (GDP per Capita)` +
    `Health (Life Expectancy)` + Freedom +
    Generosity + `Trust (Government Corruption)`,
  data = df
)

install.packages("ggcorrplot")
library(ggcorrplot)
# Compute a correlation matrix
cor_df <- subset(df, select = -c(1, 2, 9, 10))

corr <- round(cor(cor_df), 1)

# Visualize the correlation matrix
ggcorrplot(corr, 
           # hc.order = TRUE, #for ordering; using hierarchical clustering
           #type = "lower", #for lower triangle
           lab = TRUE,
           lab_size = 4,
           title = "Correlation of Variables",
           ggtheme = theme_bw)

#Average value of happiness variables for different regions

df.Region <- df %>%
  select(-3) %>%
  group_by(Region) %>%
  summarise_at(vars(-Country, -`Happiness Rank`, -Year), funs(mean(.)))

df.Region.melt <- melt(df.Region)

ggplot(df.Region.melt, aes(y=value, x=Region, color=Region, fill=Region)) + 
  geom_bar( stat="identity") +    
  facet_wrap(~variable) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Average value of happiness variables for different regions", 
       y = "Average value") 

#Scatter plot with regression line

ggplot(df, aes(x = `Health (Life Expectancy)`, y = `Happiness Score`)) + 
  geom_point(aes(color=Region), size = 3, alpha = 0.8) +  
  geom_smooth(aes(color = Region, fill = Region), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~Region) +
  theme_bw() + labs(title = "Scatter plot with regression line")


# Happiness score comparison on different regions 
# boxplot

ggplot(df , aes(x = Region, y = `Happiness Score`)) +
  geom_boxplot(aes(fill=Region)) + theme_bw() +
  theme(axis.title = element_text(size = (8)))

# violin plot
ggplot(df, aes(x=Region, y=`Happiness Score`))+
  geom_violin(aes(fill=Region),alpha=0.7)+ theme_bw() +
  theme(axis.title = element_text(size = (8)))

# Compute descriptive statistics by groups
install.packages("ggpubr")
library(ggpubr)
stable <- desc_statby(df, measure.var = "Happiness Score",
                      grps = "Region")
stable <- stable[, c("Region","mean","median")]
names(stable) <- c("Region", "Mean of happiness score","Median of happiness score")
# Summary table plot
stable.p <- ggtexttable(stable,rows = NULL, 
                        theme = ttheme("classic"))

stable.p


# Linear Regression for predicting the happines of a country judging by the
# health/life expectancy


Linear_Model_1 = lm(df$`Happiness Score`~df$`Health (Life Expectancy)`, data = df)
Linear_Model_1
#hapiness_score = 3.303+3.382*health


# The correlation between the Health Expectancy and the Happiness score
cor(df$`Happiness Score`, df$`Health (Life Expectancy)`)

summary(Linear_Model_1)
summary(Linear_Model_1)$coefficients
standard_residuals_1 = rstandard(Linear_Model_1) #Standardized residuals"
resid_1 = Linear_Model_1$residuals
# predict_1 = predict(Linear_Model_1)

# standard_residuals_1
# resid_1


ggplot(df, aes(x=`Health (Life Expectancy)`,y=`Happiness Score`)) + 
  geom_point() + 
  geom_smooth(method="lm") +
  labs(x="Health Expectancy",y="Happiness Score") + 
  geom_hline(yintercept=mean(df$`Happiness Score`),size=0.5) +
  geom_vline(xintercept=mean(df$`Health (Life Expectancy)`),size=0.5) +
  ggtitle("Linear Regression predicting hapiness score using the health/life expentacy")


# Residuals
ggplot(data = Linear_Model_1, aes(x=.fitted, y = .resid)) + 
  geom_point()+
  labs(x="Health Expectancy Predictions",y="Residuals") 
  

install.packages("ggfortify")
library(ggfortify)
autoplot(Linear_Model_1)


### Multiple linear regression model with all parameters included.


Linear_Model_2 <- lm(
  formula = df$`Happiness Score` ~ df$`Economy (GDP per Capita)` + df$`Health (Life Expectancy)` +
    df$Freedom + df$`Trust (Government Corruption)` + df$Generosity,
  data = df
)
Linear_Model_2
summary(Linear_Model_2)

Linear_Model_3 <- lm(
  formula = df$`Happiness Score` ~ df$`Economy (GDP per Capita)` + df$`Health (Life Expectancy)`,
  data = df
)
Linear_Model_3
summary(Linear_Model_3)


# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
dataset <- df[2:10]
split = sample.split(dataset$`Happiness Score`, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

Linear_Model_4 <- lm(formula = `Happiness Score` ~ .,
                     data = training_set)
summary(Linear_Model_4)

predict_4 = predict(Linear_Model_4, newdata = test_set)

predict_actual_4 <- as.data.frame(cbind(Prediction = predict_4, Actual = test_set$`Happiness Score`))

ggplot(predict_actual_4, aes(Actual, Prediction )) +
  geom_point() + geom_abline() +
  labs(title = "Multiple Linear Regression", x = "Actual happiness score",
       y = "Predicted happiness score") 

### Random Forest Regression

# install.packages("randomForest")
library(randomForest)

rf <- randomForest(x = dataset[-1],
                            y = dataset$`Happiness Score`,
                            ntree = 500)

predict_rf = predict(rf, newdata = test_set)

predict_actual_rf <- as.data.frame(cbind(Prediction = predict_rf, Actual = test_set$`Happiness Score`))


ggplot(predict_actual_rf, aes(Actual, Prediction )) +
  geom_point() + theme_bw() + geom_abline() +
  labs(title = "Random Forest Regression", x = "Actual happiness score",
       y = "Predicted happiness score")

###TODO
### Neural Net
# install.packages("neuralnet")
# library(neuralnet)
# 
# nn <- neuralnet(formula=Happiness Score ~ Economy (GDP per Capita) + Health (Life Expectancy) +
#                   Freedom + Trust (Government Corruption) + Generosity,
#                 data=as.data.frame(training_set),hidden=10,linear.output=TRUE)
# plot(nn)
  