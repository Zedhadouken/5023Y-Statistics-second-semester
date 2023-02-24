library(tidyverse)
library(GGally)
library(emmeans)
library(performance)

darwin <- read_csv(here("data", "darwin.csv"))

#least-squares model 0----
lsmodel0 <- lm(formula = height ~ 1, data = darwin)

#broom----
broom::tidy(lsmodel0)

#proving that the intercept is the same as the overall mean
mean(darwin$height)
