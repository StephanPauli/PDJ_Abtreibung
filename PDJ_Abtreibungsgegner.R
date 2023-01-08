#Mac
setwd("/Users/stephanpauli/Downloads")
#wiwndows
#setwd("C:/Users/steph/Downloads")
library(haven)
library(dplyr)
library(forcats)
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(shiny)
library(ggExtra)


ALLBUS <- read_dta("Allbus21Stata.dta")
typeof(ALLBUS)

#Selecting Variables from Dataframe, Recoding and omiting Variables
Allbus_PDJ <- select(ALLBUS,c(respid,age,sex,rb07,rd01,vm15))

Allbus_PDJ$age <- na_if(Allbus_PDJ$age, -32)

Allbus_PDJ$sex <- as.integer(Allbus_PDJ$sex)
Allbus_PDJ$sex  <- recode(Allbus_PDJ$sex, `1` = 0, `2` = 1, `3` = NA_real_, `-9` = NA_real_)

Allbus_PDJ$rb07 <- as.integer(Allbus_PDJ$rb07)
Allbus_PDJ$rb07 <- recode(Allbus_PDJ$rb07, `1` = 1, `2` = 2, `3` = 3, `4` = 4,  `5` = 5, `6` = 6, `7` = 7, `8` = 8, `9` = 9, `10` = 10, `-42` = NA_real_, `-11` = NA_real_, `-9` = NA_real_)

Allbus_PDJ$rd01 <- as.integer(Allbus_PDJ$rd01)
Allbus_PDJ$rd01 <- recode(Allbus_PDJ$rd01, `1` = 1, `2` = 2, `3` = 3, `4` = 4,  `5` = 5, `6` = 6, `-42` = NA_real_, `-9` = NA_real_, `-7` = NA_real_)

Allbus_PDJ$vm15 <- as.integer(Allbus_PDJ$vm15)
Allbus_PDJ$vm15 <- recode(Allbus_PDJ$vm15, `1` = 1, `2` = 2, `3` = 3, `-42` = NA_real_, `-9` = NA_real_, `-8` = NA_real_)

Allbus_PDJ <- na.omit(Allbus_PDJ)

summary(Allbus_PDJ)

#Regression to test assumption
#Model1 with gender, confession, age
#Model2 with gender, religiosity, age

m1 <- lm(vm15 ~ sex + rd01 + age, data = Allbus_PDJ)
m2 <- lm(vm15 ~ sex + rb07 + age + rb07*age, data = Allbus_PDJ)
summary(m1)
summary(m2)


#Preparations for Stacked Bar Plot
gender_names <- c(`0` = "Männer", `1` = "Frauen")

#Plotting the Data
#1: Plotting the DV(VM15) over IV1 (Age) with Facetswap Gender

p1 <- ggplot(data=Allbus_PDJ, aes(x=age, group=factor(vm15), fill=factor(vm15))) +
  geom_density(adjust=1, position="fill")+ 
  labs(title = "Abtreibungseinstellung in Abhängigkeit des Alters", 
       subtitle = "Deutschland 2021",
       x = "Alter in Jahren", 
       y = "Einstellung gegenüber Abtreibung (Anteil)") +
  guides(fill = guide_legend(title = "Einstellung zu Abtreibung")) +
  theme(legend.position = "bottom") + 
  scale_y_continuous(labels = scales::percent) + 
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white")) +
  scale_fill_manual(values=c('#FBF5EF', '#F8ECE0', '#FF8000'), labels = c("Ja, Jederzeit", "Ja, bis drei Monate", "Nein, gar nicht")) +
  facet_grid(~sex, labeller = as_labeller(gender_names))
p1

#2: Plotting the DV(VM15) over IV2 (Religiosity)  with Facetswap Gender

p2 <- ggplot(data=Allbus_PDJ, aes(x=rb07, group=factor(vm15), fill=factor(vm15))) +
  geom_bar(position="fill")+ 
  labs(title = "Abtreibungseinstellung in Abhängigkeit der Religiosität", 
       subtitle = "Deutschland 2021",
       x = "Religiosität - Selbsteinschätzung von 1 (Nicht Religiös) bis 10 (Religiös)", 
       y = "Einstellung gegenüber Abtreibung (Anteil)") +
  guides(fill = guide_legend(title = "Einstellung zu Abtreibung")) +
  theme(legend.position = "bottom") + 
  scale_y_continuous(labels = scales::percent) + scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white")) +
  scale_fill_manual(values=c('#FBEFF5', '#F8E0EC', '#FF8000'), labels = c("Ja, Jederzeit", "Ja, bis drei Monate", "Nein, Gar nicht")) +
  facet_grid(~sex, labeller = as_labeller(gender_names))
p2


p3 <- ggplot(data = Allbus_PDJ, aes(x = age))+
  geom_histogram()+
  labs(title = "A) Default histogram (30 bins)")
p3
p4 <- ggplot(data = Allbus_PDJ, aes(x = rb07))+
  geom_histogram()+
  labs(title = "A) Default histogram (30 bins)")
p4

