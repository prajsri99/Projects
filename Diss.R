dev.off()
setwd("C:/Users/True Gamer/OneDrive/Desktop/Dissertation")
options(scipen = 999)
library(ggplot2)
library(corrplot)
library(ellipse)
data <- read.csv("clean_missing_aviation.csv")
names(data)
data$Educational.Attainment <- as.numeric(data$Educational.Attainment)
data$Aircraft.Movements <- as.numeric(data$Aircraft.Movements)
data$Economic <- as.numeric(data$Economic)
data$Official.Language <- as.character(data$Official.Language)
data$Pop.in.1972 <- as.numeric(data$Pop.in.1972)
data$Pop.in.2022 <- as.numeric(data$Pop.in.2022)
data$Continent <- as.factor(data$Continent)
summary(data)
for(i in 2:10){
  data[is.na(data[,i]),i] <- median(data[,i],na.rm=TRUE)
}
str(data)
col <- colnames(data)[2:9]
par(mfrow = c(4,4))
for (i in 1:length(col)){
  sub_data = data[col[i]][,1]
  hist(sub_data, main = paste("Hist. of",col[i], sep = " "),xlab = col[i])
  qqnorm(sub_data, main = paste("QQ plot of", col[i], sep = " "))
  qqline(sub_data)

}

number_continent <- ggplot(data, aes(x = Continent)) + geom_bar(fill = "lightblue") + ggtitle("Number of states in each Continent") + xlab("Continent") + ylab("Number of States")
pop_state <- ggplot(data, aes(x=Pop.in.1972,y=Pop.in.2022)) + geom_col(aes(color="States"),position= "dodge")  + ggtitle("Population in 1972 Vs Population in 2022") + xlab("Population in 1972") + ylab("Population in 2022") + facet_wrap(.~States,scales="free_y",nrow=6) + scale_x_log10()
pop_state
land_state <- ggplot(data, aes(x=Landmass,y=Pop.in.2022)) + geom_col(aes(color="States"),position= "dodge")  + ggtitle("Landmass Vs Population for each state") + xlab("Landmass") + ylab("Population in 2022") + facet_wrap(.~States,scales="free_y",nrow=6)
land_state
num_col <- data[, 2:9]
corMatrix <- cor(as.matrix(num_col))
col <- colorRampPalette(c("red","green","blue"))
corrplot.mixed(corMatrix,order="AOE",tl.pos = "lt", lower="number",lower.col="black", upper="ellipse",upper.col=col(10),number.cex=0.8,tl.col="black")
model <- aov(data$Landmass ~ data$Pop.in.2022, data)
summary(model)
data$pop.density <- data$Pop.in.2022/data$Landmass
ggplot(data,aes(x=Continent,y=pop.density)) + geom_boxplot() + xlab("States") + ylab("Population Density")
lm.model1 <- lm(data$Continent ~ data$pop.density)
summary(lm.model1)
life_infant <- ggplot(data, aes(x=Infant.Mortality,y=Life.Expectancy)) + geom_point()  + ggtitle("Infant Mortality Vs Life Expectancy for each State") + xlab("Infant Mortality Rate") + ylab("Life Expectancy") + facet_wrap(.~States,scales="free_y",nrow=6)
life_infant
ggplot(data, aes(x=State.Abbreviations,y=Continent)) + geom_col(fill="lightblue")  + ggtitle("States in Each Continent") + xlab("States Abbreviations") + ylab("Continents")
ggplot(data, aes(x=Continent, y=Life.Expectancy,fill=Continent)) + geom_violin(trim=FALSE) + geom_boxplot(width=0.1)
sub_data <- subset(data,Continent == "Africa" | Continent=="Asia"|Continent=="Oceania", select=c(State.Abbreviations, Life.Expectancy,Infant.Mortality, Continent))
sub_data$Continent <- as.factor(sub_data$Continent)
str(sub_data)
ggplot(sub_data,aes(x=State.Abbreviations,y=Life.Expectancy)) + geom_point(aes(color=Continent,size=8))
model2 <- aov(data$Life.Expectancy ~ data$Continent, data)
summary(model2)
model3 <- aov(sub_data$Life.Expectancy ~ sub_data$Continent, sub_data)
summary(model3)
model4 <- aov(data$Infant.Mortality ~ data$Continent, data)
summary(model4)
ggplot(data, aes(x=Continent, y=Infant.Mortality,fill=Continent)) + geom_violin(trim=FALSE) + geom_boxplot(width=0.1)
ggplot(sub_data,aes(x=State.Abbreviations,y=Infant.Mortality)) + geom_point(aes(color=Continent,size=8))
