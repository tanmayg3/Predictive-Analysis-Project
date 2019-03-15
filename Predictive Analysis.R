library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)
library(corrplot)
library(Hmisc)
library(caTools)

WorldHappinessData_2016 <- read_excel("C:/Users/Tanmay/Desktop/HBU/ANLY 502 - Analytical Methods I/Project/WorldHappinessData_2016.xlsx")
View(WorldHappinessData_2016)

str(WorldHappinessData_2016)

# Data clean-up and preparation :
# Remove columns 5 and 6 as they are just CI values
Rev_WorldHappinessData_2016<- WorldHappinessData_2016[, -c(5,6)]
View(Rev_WorldHappinessData_2016)

str(Rev_WorldHappinessData_2016)

par(mfrow = c(1,1))
### Creating a Histogram for the Happiness_score to depict the Normality of distribution
hist(Rev_WorldHappinessData_2016$Happiness_Score,xlab = "Happiness_Score",col = "blue",main = "Histogram of Happiness Score")


### Scatter plot to determine the relation between Happines Rank and Happiness Score
plot(Rev_WorldHappinessData_2016$Happiness_Score,Rev_WorldHappinessData_2016$Happiness_Rank,xlab = "Happiness Score",
     ylab = "Happiness Rank",main ="Scatterplot to determine Happiness Rank by Happiness Score",
     abline(lm(Rev_WorldHappinessData_2016$Happiness_Rank ~ Rev_WorldHappinessData_2016$Happiness_Score),col = "Red"))

### Correlation matrix for the Happiness score and other variables
corrdata <- Rev_WorldHappinessData_2016[,-1:-2]
corrdata

res <- rcorr(as.matrix(corrdata))
res
# Extract the correlation coefficients
res$r
# Extract p-values
res$P
## Plot Correlation plot
res2<-rcorr(as.matrix(corrdata[,1:8]))
res2

corrplot(res$r,type = "full",order = "hclust",tl.col = "black", tl.srt = 45)


## Distribution of Happiness Score by Region
p<- Rev_WorldHappinessData_2016 %>%
  dplyr::mutate(Region=as.factor(Region)) %>%
  dplyr::select(Happiness_Score, Region) %>%
  ggplot2::ggplot(ggplot2::aes(x=Happiness_Score)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=Region, colour=Region), alpha=0.55) +
  ggplot2::xlab("Happiness_Score") +
  ggplot2::ggtitle("Distribution of Happiness_Score by Region") +
  ggplot2::labs(fill="Region", y="Density")

# Display the plot
p

## Distribution of Economy GDP by Region
q <- Rev_WorldHappinessData_2016 %>%
  dplyr::mutate(Region=as.factor(Region)) %>%
  dplyr::select(Economy_GDP_Capita, Region) %>%
  ggplot2::ggplot(ggplot2::aes(x=Economy_GDP_Capita)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=Region, colour=Region), alpha=0.55) +
  ggplot2::xlab("Economy_GDP") +
  ggplot2::ggtitle("Distribution of Economy_GDP by Region") +
  ggplot2::labs(fill="Region", y="Density")
# Display the plot
q

### Distribution of Family by Region
r <- Rev_WorldHappinessData_2016 %>%
  dplyr::mutate(Region=as.factor(Region)) %>%
  dplyr::select(Family, Region) %>%
  ggplot2::ggplot(ggplot2::aes(x=Family)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=Region, colour=Region), alpha=0.55) +
  ggplot2::xlab("Family") +
  ggplot2::ggtitle("Distribution of Family by Region") +
  ggplot2::labs(fill="Region", y="Density")

# Display the plot
r

### Distribution of Health_Life_expectancy by Region
s<- Rev_WorldHappinessData_2016 %>%
  dplyr::mutate(Region=as.factor(Region)) %>%
  dplyr::select(Health_Life_Expectancy, Region) %>%
  ggplot2::ggplot(ggplot2::aes(x=Health_Life_Expectancy)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=Region, colour=Region), alpha=0.55) +
  ggplot2::xlab("Health_Life_Expectancy") +
  ggplot2::ggtitle("Distribution of Health_Life_Expectancy by Region") +
  ggplot2::labs(fill="Region", y="Density")

# Display the plot
s



########################
# train

# Remove Country and Region from data as as they are Categorical values
df = Rev_WorldHappinessData_2016[, -c(1,2)]
str(df)

## Training and Testing the processed data-set:
set.seed(101) 

# Randomly Splitting the sample, basically randomly assigns a booleans to a new column "sample"
sample <- sample.split(df$Happiness_Score, SplitRatio = 0.70) # SplitRatio = percent of sample==TRUE


# Creating a train data
train = subset(df, sample == TRUE)

# Testing the Data
test = subset(df, sample == FALSE)

#############
# Model selection using Multiple Linear Regression:

# 1. Consider 'Happinesss_score' against 'Economy GDP per Capita' for Multiple Linear Regression model; given by model1
model1 <- lm(Rev_WorldHappinessData_2016$Happiness_Score ~  Rev_WorldHappinessData_2016$Economy_GDP_Capita, train)
summary(model1)
# 62% variance is observed in the model1


# 2. Consider 'Happinesss_score' against 'Economy GDP per Capita' & 'Family' for Multiple Linear Regression model; given by model2
model2 <- lm(Rev_WorldHappinessData_2016$Happiness_Score ~  Rev_WorldHappinessData_2016$Economy_GDP_Capita 
             + Rev_WorldHappinessData_2016$Family, train)
summary(model2)
# 70% variance is observed in the model2


# 3. Consider 'Happinesss_score' against 'Economy GDP per Capita', 'Family' & 'Health_Life_Expectancy' for Multiple Regression model; given by model3
model3 <- lm(Rev_WorldHappinessData_2016$Happiness_Score ~  Rev_WorldHappinessData_2016$Economy_GDP_Capita 
             + Rev_WorldHappinessData_2016$Family + Rev_WorldHappinessData_2016$Health_Life_Expectancy, train)
summary(model3)
# 72% variance is observed in the model3


# 4. Consider 'Happinesss_score' against 'Economy GDP per Capita', 'Family', 'Health_Life_Expectancy' & 'Freedom' for Multiple Regression model; given by model4
model4 <- lm(Rev_WorldHappinessData_2016$Happiness_Score ~  Rev_WorldHappinessData_2016$Economy_GDP_Capita 
             + Rev_WorldHappinessData_2016$Family + Rev_WorldHappinessData_2016$Health_Life_Expectancy
             + Rev_WorldHappinessData_2016$Freedom, train)
summary(model4)
# 77% variance is observed in the model4


# 5. Consider 'Happinesss_score' against 'Economy GDP per Capita', 'Family', 'Health_Life_Expectancy', 'Freedom' & 'Trust_Government_Corruption' for Multiple Regression model; given by model5
model5 <- lm(Rev_WorldHappinessData_2016$Happiness_Score ~  Rev_WorldHappinessData_2016$Economy_GDP_Capita 
             + Rev_WorldHappinessData_2016$Family + Rev_WorldHappinessData_2016$Health_Life_Expectancy
             + Rev_WorldHappinessData_2016$Freedom + Rev_WorldHappinessData_2016$Trust_Government_Corruption, train)
summary(model5)
# 78% variance is observed in the model5


# 6. Consider 'Happinesss_score' against 'Economy GDP per Capita', 'Family', 'Health_Life_Expectancy', 'Freedom', 'Trust_Government_Corruption' & 'Generosity' for Multiple Regression model; given by model6
model6 <- lm(Rev_WorldHappinessData_2016$Happiness_Score ~  Rev_WorldHappinessData_2016$Economy_GDP_Capita 
             + Rev_WorldHappinessData_2016$Family + Rev_WorldHappinessData_2016$Health_Life_Expectancy
             + Rev_WorldHappinessData_2016$Freedom + Rev_WorldHappinessData_2016$Trust_Government_Corruption
             + Rev_WorldHappinessData_2016$Generosity, train)
summary(model6)
# 77% variance is observed in the model6

# Since, Generosity variable in 'model6' decreases the variance with respect to model5; 
# thus, 'model5' is the Best Fit model

# plotting model5
plot(model5)


# Grab residuals
res <- residuals(model5)
# Convert to DataFrame for gglpot
res <- as.data.frame(res)
head(res)

# Plotting the residuals result
ggplot(res,aes(res)) +  geom_histogram(fill='blue',alpha=0.5)


# Creating a Prediction model
predictions <- predict(model5,test)
results <- cbind(predictions,test$Happiness_Score) 
colnames(results) <- c('pred','real')
results <- as.data.frame(results)
head(results)

plot(results$real, ylab = 'Observed Values', xlab = 'Predicted Values', main="Predicted vs Observed Values", type="b")
par(new=TRUE)
plot( results$pred, ylab = 'Observed Values', xlab = 'Predicted Values', main="Predicted vs Observed Values", type="p", col = "red")


mse <- mean((results$real-results$pred)^2)
print(mse)


###################################



