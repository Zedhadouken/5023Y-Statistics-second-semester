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

#compare means----
lsmodel1 <- lm(height ~ type, data=darwin) # note that the following is identical
                                           # lsmodel1 <- lm(height ~ 1 + type, data=darwin)
broom::tidy(lsmodel1)

summary(lsmodel1) #a look at the full summary

darwin %>%                                 # superimpose the calculated means onto a plot.
  ggplot(aes(x=type,
             y=height,
             colour=type))+
  geom_jitter(alpha=0.5,
              width=0.1)+
  stat_summary(fun=mean,
               size=1.2)+
  theme_bw()

#standard error of the difference----
## second row gives the mean difference in this column it gives the standard error of the difference between the two means (SED)

#confidence intervals----
broom::tidy(lsmodel1, conf.int=T)

#answering the question----
GGally::ggcoef_model(lsmodel1,
                     show_p_values=FALSE, 
                     conf.level=0.95)

broom::tidy(lsmodel1, conf.int=T, conf.level=0.99) # changing [increasing] the confidence level will mean you no longer are able to reject the null hypthesis

