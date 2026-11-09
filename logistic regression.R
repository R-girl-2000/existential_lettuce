library(tidyverse)
library(knitr)
library(broom)
library(pROC) #ROC curves
library(psych)
#set working dir and read in data
data_in<- "C:/Users/pvand/OneDrive/Documents/existential_lettuce"
framingham<- read.csv(file = file.path(data_in, "framingham.csv"))
#descriptives with the pysch package
descriptives<- describe(framingham, na.rm= TRUE)
framingham<-na.omit(framingham)
#mean center age and tot chol to have a meaningful reference value. 
framingham$ageCent<- framingham$age- mean(framingham$age)
framingham$totCholCent <- framingham$totChol-mean(framingham$totChol, na.rm = TRUE) 
#save smoking stat as a factor
framingham$currentSmokerFactor <- as.factor(framingham$currentSmoker)

#run model
output<- glm(TenYearCHD ~ ageCent + totCholCent + currentSmokerFactor, family = binomial,  data = framingham)
#stadnardized influential points. https://library.virginia.edu/data/articles/detecting-influential-points-in-regression-with-dfbetas
#save to df for ease of viewing
dfbetas_vals<-data.frame(dfbetas(output))
#plot code taken from the above website
# Plot
library(ggplot2)
ggplot(dfbetas_vals) +
  geom_point(aes(x = 1:nrow(dfbetas_vals), y = ageCent)) +
  geom_segment(aes(x = 1:nrow(dfbetas_vals), xend = 1:nrow(dfbetas_vals), y = 0, yend =ageCent),
               color = 'cornflowerblue') +
  geom_hline(yintercept = c(2, -2) / sqrt(nrow(dfbetas_vals)), color = 'salmon') +
  labs(x = 'Observation index', y = 'DFBETAS',
       title = paste0('DFBETAS values for coefficient of ', colnames(dfbetas_vals)[2]),
       subtitle = 'Thresholds are at \u00B1(2\u00F7\u221An)') +
  geom_text(aes(x = 1:nrow(dfbetas_vals), y = ifelse(ageCent > 0, ageCent + .05, ageCent - .05),
                label = ifelse(abs(ageCent) > (2/sqrt(nrow(dfbetas_vals))),
                               paste0(round(ageCent, digits = 2), ' (', 1:nrow(dfbetas_vals), ')'), '')))