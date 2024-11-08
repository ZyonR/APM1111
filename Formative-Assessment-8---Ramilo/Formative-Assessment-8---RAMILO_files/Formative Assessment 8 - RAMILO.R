library(datasets)
library(tidyverse)
library(car)
library(Hmisc)

head(PlantGrowth)
unique(PlantGrowth$group)
boxplot(weight ~ group, data = PlantGrowth,
        main = "Box Plot of the plant weight for each group",
        xlab = "Group",
        ylab = "Weight",
        col = "lightblue")

PlantGrowth[17,"weight"] <- 5.87
boxplot(weight ~ group, data = PlantGrowth,
        main = "Box Plot of the plant weight for each group",
        xlab = "Group",
        ylab = "Weight",
        col = "lightblue")

ShapiroWilkTest <- PlantGrowth %>%
  group_by(group) %>%
  summarise(
    shapiro_statistic = shapiro.test(weight)$statistic,
    shapiro_pValue = shapiro.test(weight)$p.value
  )
ShapiroWilkTest

LeveneTest <- PlantGrowth %>%
  group_by(group) %>%
  reframe(
    leveneStatistic = leveneTest(weight ~ group, data = .)$`F value`,
    levene_pValue = leveneTest(weight ~ group, data = .)$`Pr(>F)`
  )
LeveneTest

describe(PlantGrowth$weight)

anova_result <- aov(weight ~ group, data = PlantGrowth)
summary(anova_result)

anova_result <- aov(weight ~ group, data = PlantGrowth)
tukey <- TukeyHSD(anova_result)
tukey

