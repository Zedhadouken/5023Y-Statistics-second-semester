library(tidyverse)
library(here)
library(kableExtra)

darwin <- read_csv(here("data", "darwin.csv"))

#Check the structure of the data----
glimpse(darwin)

# check data is in a tidy format
head(darwin)

# clean up column names
darwin <- janitor::clean_names(darwin)

# check for duplication
darwin %>% 
  duplicated() %>% 
  sum()

# check for typos - by looking at impossible values
darwin %>% 
  summarise(min=min(height, na.rm=TRUE), 
            max=max(height, na.rm=TRUE))

# check for typos by looking at distinct characters/values
darwin %>% 
  distinct(pair)

darwin %>% 
  distinct(type)

# missing values
darwin %>% 
  is.na() %>% 
  sum()

# quick summary
summary(darwin)

#____________----
#Visualisation----
##boxplot----
darwin %>% 
  ggplot(aes(x=type,
                   y=height))+
  geom_boxplot()

##histogram----
darwin %>% 
  ggplot()+
  geom_histogram(aes(x=height, fill=type))

#____________----
#Comparing groups----
# use tidy functions to determine mean and SD
darwin %>%
  group_by(type) %>%
  summarise(mean=mean(height),
            sd=sd(height))
# present simple data (mean and sd) in a table
darwin_summary <- darwin %>%
  group_by(type) %>%
  summarise(mean=mean(height),
            sd=sd(height))

# make a summary plot
darwin_summary %>% 
  ggplot(aes(x=type,
             y=mean))+
  geom_boxplot(aes(ymin=mean-sd, ymax=mean+sd))+
  theme_bw() #boxplot isn't the plot for this

darwin_summary %>% 
  ggplot(aes(x=type,
             y=mean))+
  geom_pointrange(aes(ymin=mean-sd, ymax=mean+sd))+
  theme_bw()

# using Library(kableExtra) functions, make a nice table (could be replaced with kable() if needed)
darwin_summary %>% 
  kbl(caption="Summary statistics of crossed and selfed maize plants") %>% 
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")



#____________----
#Estimation----
# In order to find the statistically significant difference between, do some data wrangling "tidyr::pivot_wider()" Chapter 2 and calculations (mutate)
# create new colmn with height difference
# pivot data to wide format then subtract Selfed plant heights from Crossed plant heights

darwin_wide <- darwin %>% 
  pivot_wider(names_from = type, values_from = height) %>% 
  mutate(difference = Cross - Self)

# now calculate the mean difference in height between paired plants and the amount of variance (as standard deviation)
difference_summary <- darwin_wide %>% 
  summarise(mean=mean(difference),
            sd=sd(difference),
            observations=n())

difference_summary

# now calculate standard error 

difference_summary %>% 
  mutate(se= sd/sqrt(observations))

#communicate: "...the average difference in height was 2.62 ± 1.22 inches (mean ± SE)."


##Wrangling data to calculate differene----
#"In order to calculate the differences in height between each pair we need to do some data wrangling with tidyr::pivot_wider() Chapter 2 and calculations with mutate"
# pivot data to wide format then subtract Selfed plant heights from Crossed plant heights

darwin_wide <- darwin %>% 
  pivot_wider(names_from = type, values_from = height) %>% 
  mutate(difference = Cross - Self)

##calculate the difference between mean heights----
difference_summary <- darwin_wide %>% 
  summarise(mean=mean(difference),
            sd=sd(difference),
            n=n())

difference_summary

##calculating standard error----
difference_summary %>% 
  mutate(se= sd/sqrt(n))
##Communicate the findings----
#"... the average difference in height was 2.62 ± 1.22 inches (mean ± SE)."

#____________----

#Uncertainty----
##Normal distribution----
#read ☑
##convidence intervals----
lowerCI <- 2.62-(2*1.22)

upperCI <- 2.62+(2*1.22)

lowerCI
upperCI

#"The maize plants that have been cross pollinated were taller on average than the self-pollinated plants, with a mean difference in height of 2.62 [0.18, 5.06] inches (mean [95% CI])."

#____________----
#____________----
#MOVE ON TO NEXT CHAPTER 13----

