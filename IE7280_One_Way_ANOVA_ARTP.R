library(magrittr)
library(plotly)

diet <- read.csv("Diet.csv")
diet1<-read.csv("Diet.csv")

#Displaying first 10 records
head(diet, 10)

#Getting the Summary of the data
summary(diet)

#Counting number of people taking different diets
str(diet)
diet_counts <- table(diet$Diet)
print(diet_counts)

#Plotting a boxplot for comparing Age, Height, Start Weight and End Weight
fig <- plot_ly()
fig <- fig %>% add_boxplot(y = ~diet$Age, name = "Age")
fig <- fig %>% add_boxplot(y = ~diet$Height, name = "Height")
fig <- fig %>% add_boxplot(y = ~diet$Before_Weight, name = "Start weight")
fig <- fig %>% add_boxplot(y = ~diet$Weight_6Weeks, name = "Finish weight")
fig

#Unique Genders
unique_genders <- unique(diet$Gender)
print(unique_genders)

#Plotting a scatter plot for before weight and height with respect to gender
fig1 <- plot_ly(diet, x = ~Height, y = ~Before_Weight, color = ~Gender, type = "scatter", mode = "markers") %>%
  layout(title = "Height to Weight ratio by Gender")

fig1

#Calculating Starting BMI, Ending BMI and Change in BMI
diet1$BMI_Start <- diet1$Before_Weight / (diet1$Height / 100)^2
diet1$BMI_End <- diet1$Weight_6Weeks / (diet1$Height / 100)^2
diet1$BMI_Change <- diet1$BMI_Start - diet1$BMI_End
print(diet1)

#Plotting a scatter plot for before weight and height with respect to diet
fig2 <- plot_ly(diet, x = ~Height, y = ~Before_Weight, color = ~Diet, type = "scatter", mode = "markers") %>%
  layout(title = "Range of people with their before Weight and Height for diets")

fig2

#Plotting a boxplot for comparing 3 different diets
fig3 <- plot_ly()
fig3 <- fig3 %>% add_boxplot(y = diet1[diet1$Diet == 1, "BMI_Change"], name = "Diet 1")
fig3 <- fig3 %>% add_boxplot(y = diet1[diet1$Diet == 2, "BMI_Change"], name = "Diet 2")
fig3 <- fig3 %>% add_boxplot(y = diet1[diet1$Diet == 3, "BMI_Change"], name = "Diet 3")
fig3 <- fig3 %>% layout(title = 'BMI Change results for different diets')
fig3

#Calculating BMI change for each diet
diet_1 <- diet1[diet1$Diet == 1, "BMI_Change"]
diet_2 <- diet1[diet1$Diet == 2, "BMI_Change"]
diet_3 <- diet1[diet1$Diet == 3, "BMI_Change"]

print(diet_1)
print(diet_2)
print(diet_3)

#Calculating mean for BMI change
Mean <- mean(diet1$BMI_Change)
Mean


#Calculating Sum of Squares Total for 3 different diets 
diets <- list(diet_1, diet_2, diet_3)
SST <- 0
for (diet in diets) {
  for (change in diet) {
    SST <- SST + ((change - mean(unlist(diets)))^2)
  }
}
print(SST)

#Degrees of freedom 1
diet1_df=3-1

#Calculating Sum of Squares Within for 3 different diets and in total
SSW <- function(diet1) {
  Mean1 <- mean(diet1)
  SSW <- sum((diet1-Mean1)^2)
  return(c(Mean1, SSW))
}

SSW_1 = SSW(diet_1)
SSW_2 = SSW(diet_2)
SSW_3 = SSW(diet_3)
SSW_full <- SSW_1[2] + SSW_2[2] + SSW_3[2]
print(SSW_full)

#Degrees of Freedom 2
diet1_df2=90-1

#Calculating Sum of Squares Between for 3 different diets
SSB <- 30*(abs(SSW_1[1] - Mean)^2 + abs(SSW_2[1] - Mean)^2 + abs(SSW_3[1] - Mean)^2)
print(SSB)

#Calculating F-score
fscore<-SSB/diet1_df/(SSW_full/diet1_df2)
print(fscore)


#Performing One-Way ANOVA on the diet
alldiet <- c(diet_1, diet_2, diet_3)
group <- rep(1:3, each = length(diet_1))
model <- aov(alldiet ~ group, data = diet1)
summary(model)

#As we get the p-value i.e 0.000548 which is less than the level of significance i.e 0.05,
#thus we will be rejecting the H0 , null hypothesis.


