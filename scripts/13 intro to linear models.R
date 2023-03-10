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

broom::tidy(lsmodel1, conf.int=T, conf.level=0.99) # changing [increasing] the confidence level will mean you no longer are able to reject the null hypothesis

##getting the other treatment mean & standard error----
darwin %>% 
  mutate(type=factor(type)) %>% 
  mutate(type=fct_relevel(type, c("Self", "Cross"))) %>% 
  lm(height~type, data=.) %>% 
  broom::tidy()

##emmeans
means <- emmeans::emmeans(lsmodel1, specs = ~ type)

means


means %>% 
  as_tibble() %>% 
  ggplot(aes(x=type, 
             y=emmean))+
  geom_pointrange(aes(
    ymin=lower.CL, 
    ymax=upper.CL))

#13.5 assumption checking----
plot(lsmodel1) #Tidyverse

performance::check_model(lsmodel1) #Base R

##13.5.1 normal distribution----
performance::check_model(lsmodel1, check=c("normality","qq"))

plot(lsmodel1, which=c(2,2))
###QQ "quantile-quantile" plot = a way to check whether a sample distribution is the same as another [or theoretical] distribution. If the residuals FOLLOW A NORMAL DISTRIBUTION, they should follow a PERFECT DIAGONAL LINE across the plot.

##13.5.2 equal variance----
performance::check_model(lsmodel1, check="homogeneity")

plot(lsmodel1, which=c(1,3))

##13.5.3 outliers----
performance::check_model(lsmodel1, check="outliers")

plot(lsmodel1, which=c(4,4))

#13.6 Summary----
darwin %>% 
  ggplot(aes(x=type, 
             y=height))+
  geom_jitter(width=0.1, 
              pch=21, 
              aes(fill=type))+
  theme_classic()+
  geom_segment(aes(x=1, xend=2, y=20.192, yend=20.192-2.617), linetype="dashed")+
  stat_summary(fun.y=mean, geom="crossbar", width=0.2)
