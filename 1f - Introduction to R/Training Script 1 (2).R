# This is a comment

# this loads in tidyverse 
library(tidyverse)

# load in the data to capacity_ae
library(readr)
capacity_ae <- read_csv("capacity_ae.csv")

# view the data in a tab
# can also double click on environment
# you also hold ctrl and click on a data object
View(capacity_ae)

# exploreatory data analysis
summary(capacity_ae)

capacity_ae

# lets gets plotting

ggplot(data = capacity_ae) + 
  geom_point(aes(x = dcubicles,
                 y = dwait))
  

# add smoothing line
ggplot(data = capacity_ae) + 
  geom_point(aes(x = dcubicles,
                 y = dwait)) +
  geom_smooth(aes(x = dcubicles,
                  y = dwait))

# add lm smoothing line
ggplot(data = capacity_ae) + 
  geom_point(aes(x = dcubicles,
                 y = dwait)) +
  geom_smooth(aes(x = dcubicles,
                  y = dwait), 
              method = "lm")

# describe linear model
summary(lm(dcubicles ~ dwait, data = capacity_ae))

# add colour to graph
ggplot(data = capacity_ae) + 
  geom_point(aes(x = dcubicles,
                 y = dwait,
                 colour = staff_increase)) +
  geom_smooth(aes(x = dcubicles,
                  y = dwait), 
              method = "lm")


# add colour to graph
ggplot(data = capacity_ae) + 
  geom_point(aes(x = dcubicles,
                 y = dwait,
                 size = dcubicles),
             colour = "red") +
  geom_smooth(aes(x = dcubicles,
                  y = dwait), 
              method = "lm")


# remove duplication
ggplot(data = capacity_ae,
       aes(x = dcubicles,
           y = dwait)) + 
  geom_point() +
  geom_smooth()


# facet magic
ggplot(data = capacity_ae,
       aes(x = dcubicles,
           y = dwait)) + 
  geom_point() +
  geom_smooth() +
  facet_wrap(~ staff_increase,
             scales = "free")

# make it pretty
ggplot(data = capacity_ae,
       aes(x = dcubicles,
           y = dwait)) + 
  geom_point() +
  geom_smooth() +
  theme_minimal()

# historgram
ggplot(data = capacity_ae) +
  geom_histogram(aes(dwait),
                 binwidth = 10)

# bar plot (ugly version)
ggplot(data = capacity_ae) +
  geom_col(aes(x = site,
               y = attendance2018))

# bar plot (ordered version)
ggplot(data = capacity_ae) +
  geom_col(aes(x = reorder(site, attendance2018),
               y = attendance2018)) +
  theme_light()

# boxplot
ggplot(data = capacity_ae) +
  geom_boxplot(aes(staff_increase, dwait))

# violin
ggplot(data = capacity_ae) +
  geom_violin(aes(staff_increase, dwait))

# violin + jitter
ggplot(data = capacity_ae) +
  geom_violin(aes(staff_increase, dwait)) +
  geom_jitter(aes(staff_increase, dwait))

# violin + jitter
ggplot(data = capacity_ae) +
  geom_violin(aes(staff_increase, dwait)) +
  geom_jitter(aes(staff_increase, dwait)) +
  labs(title = "Change in staffing vs waits",
       y = "Average waiting time")


# save plot
ggplot(data = capacity_ae) +
  geom_violin(aes(staff_increase, dwait)) +
  geom_jitter(aes(staff_increase, dwait)) +
  labs(title = "Change in staffing vs waits",
       y = "Average waiting time") 
