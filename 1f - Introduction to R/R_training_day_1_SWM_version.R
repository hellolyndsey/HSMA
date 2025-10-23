
# This is a comment and it shows how to add some numbers together
2+2

# call the tidyverse


library(tidyverse)

# code to import data
library(readr)
capacity_ae <- read_csv("capacity_ae.csv")

capacity_ae

# code to make basic plot
# with data capscity ae, 
# plotting x as cubicles, y as dwait

ggplot(data = capacity_ae) + 
  geom_point(aes(x = dcubicles, 
                 y = dwait))

ggplot(capacity_ae) + 
  geom_point(aes(dcubicles, dwait))

# same plot but with smoothing line

ggplot(data = capacity_ae) + 
  geom_point(aes(x = dcubicles, 
                 y = dwait)) +
  geom_smooth(aes(x = dcubicles, 
                  y = dwait))

# same plot but with linear trend line

ggplot(data = capacity_ae) + 
  geom_point(aes(x = dcubicles, 
                 y = dwait)) +
  geom_smooth(aes(x = dcubicles, 
                  y = dwait),
              method = 'lm')

summary(lm(dcubicles ~ dwait, data = capacity_ae))

# add colour by staff increase
ggplot(data = capacity_ae) + 
  geom_point(aes(x = dcubicles, 
                 y = dwait,
                 colour = staff_increase)) +
  geom_smooth(aes(x = dcubicles, 
                  y = dwait),
              method = 'lm')

# add colour 
ggplot(data = capacity_ae, 
       aes(x = dcubicles, 
          y = dwait)
           ) + 
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_grey()


# code to create a facet chart

ggplot(data = capacity_ae) + 
  geom_point(aes(x = dcubicles, 
                 y = dwait, 
                 colour = staff_increase)) +
  facet_wrap(~ staff_increase, 
             #scales = "free",
             ncol = 1)

# basic histogram of waits
ggplot(data = capacity_ae) + 
  geom_histogram(aes(dwait), bins = 25)

# bar plot of site and attendance
ggplot(data = capacity_ae) +
  geom_col(aes(x = reorder(site, -attendance2018),
               y = attendance2018))

# boxplot to show distribution difference by staff increase of wait
ggplot(data = capacity_ae) +
  geom_boxplot(aes(staff_increase, 
                   dwait))

ggplot(data = capacity_ae) +
  geom_violin(aes(staff_increase, 
                   dwait,
                   fill = staff_increase)) +
  geom_jitter(aes(staff_increase, 
                  dwait)) +
  theme_replace() 


  ggplot(data = capacity_ae) +
  geom_boxplot(aes(staff_increase, dwait)) +
  labs(title = "Do changes in staffing affect waiting times",
       y = "Waiting",
       x = "Staff Increase")

### load in beds data
  
  beds_data <- read_csv("beds_data.csv", 
                        col_types = cols(date = col_date(format = "%d/%m/%Y")), 
                        skip = 3)
  View(beds_data)
  
  beds_data_not_null <- beds_data |>
    filter (is.na(beds_av))
  
  beds_data_not_null <- beds_data |>
    filter (!is.na(beds_av))
  
  # beds data then arrange in order by beds_av
  beds_data |> 
    arrange(desc(beds_av))

  filter_top_two <- beds_data |> 
    filter(date == "2018-09-01")|> 
    arrange(desc(beds_av)) |>
    slice_head(n = 2)
  
  # create a percentage occupoancy variable
  beds_data |>
    mutate(perc_occ = occ_av / beds_av) |>
    filter(date == "2018-09-01")|> 
    arrange(desc(perc_occ))
  
  beds_data |>
    group_by(date) |>
    summarise(mean_beds = mean(beds_av,
                               na.rm = TRUE),
              median_beds = median(beds_av,
                               na.rm = TRUE)
              ) |> 
    ungroup()

  #Q5. Which organisations have the highest mean % bed occupancy?
  
  library(tidylog)
    
  summary_data <- beds_data |>
    group_by(org_code) |>
    summarise(total_beds = sum(beds_av,
                               na.rm=TRUE),
              total_occ = sum(occ_av,
                              na.rm= TRUE)) |>
    mutate(perc_occ = total_occ / total_beds,
           double_perc_occ = perc_occ * 2)  |>
    arrange(desc(perc_occ)) |>
    ungroup()
  
  # import data for join
  
  tb_cases <- read_csv("tb_cases.csv")
  tb_new_table <- read_csv("tb_new_table.csv")
  tb_pop <- read <- read_csv("tb_pop.csv")
  
  
  join_cases <- tb_cases |>
    left_join(tb_pop, by = c("country", "year"))
  
  
    
    