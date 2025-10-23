
# load in initial libraries
library(tidyverse)
library(NHSRdatasets)

# make a dataframe
data <- ae_attendances

# get list of column names
colnames(data)

# get list of unique data items in a variable
unique(data$type)

# get the range of a variable (useful for dates)
range(data$period)

# see structure of data to check data types
str(data)

# see summary statistics of a dataframe
summary(data)

# dealing with spaces in variables (looking at you excel!)
# or if you want to rename a variable to a readable format for table

# lets rename our 'org_code' to 'Organisation Code'
data <- data |>
  rename(`Organisation Code` = org_code)

#lets also rename some more variables to something horrible
data <- data |>
  rename(Breaches = breaches,
         PERIOD = period,
         ` type` = type)

# we now have the look of a typical NHS table
# we could clean all those names manually
# or we could call in a janitor to use a function to do this for us

library(janitor)

data <- clean_names(data)

# phew! all better.

###### have a quick try at renaming breaches to 'number of breaches'

###### then put it back again















###### HINT - check the order or your rename!
###### ensure we have 'org_name' as we use it alot!



################
# conditionals #
################

# basic two part conditional - if_else

# lets flag all instances where attendances were above 20,000

data <- data |>
  mutate(above_20000 = if_else (attendances >= 20000,                              'Y',
                                'N'))

# multi part conditional

# lets create a grouping column for our attendances in 5000s

data <-data |>
  mutate(attendance_grouping = case_when(attendances < 5000 ~ 'Less than 5,000',
                                         attendances < 10000 ~ '5,000 to 9,999',
                                         attendances < 15000 ~ '10,000 to 14,999',
                                         attendances < 20000 ~ '15,000 to 19,999',
                                         attendances < 25000 ~ '20,000 to 24,999',
                                         TRUE ~ 'Over 25,0000'))

# note the 'true' is the else statement 
# personally I find it best to use that as an error catch 

data <-data |>
  mutate(attendance_grouping = case_when(attendances < 5000 ~ 'Less than 5,000',
                                         attendances < 10000 ~ '5,000 to 9,999',
                                         attendances < 15000 ~ '10,000 to 14,999',
                                         attendances < 20000 ~ '15,000 to 19,999',
                                         attendances < 25000 ~ '20,000 to 24,999',
                                         attendances > 25000 ~ 'Over 25,0000',
                                         TRUE ~ 'ERROR - does not compute'))

# for example this case statement fails if we have a value of exactly 25,000
data$attendances[1] <- 25000

# also it will catch a null value which the original statement will not
data$attendances[1] <- NA

####### add a column that if type 1 doubles the attendances
#######                   if type 2 triples the attendances
#######                   if type other quads the attendances
#######                   if error returns 999999









#######  HINT:  for your returns ensure they are all of the same data type

#####################
# group and mutate  #
# to make sub total #
#####################

# grouping and mutating rather than summarising
# calc total and percentages by month and org

# lets create a total number of attendances across all types by org and month

data_tot_perc <- data |>
  group_by (org_code,
            period) |>
  mutate (total_attend = sum(attendances),
          perc_attend = (attendances / total_attend) * 100) |>
  ungroup()

####### that creates a pretty long decimal as a percentage
####### can you round it to 1 decimal place?










####### hint - be very mindful of your comma!

##########################
# pivot wider and longer #
##########################

# pivoting data to longer or wider formats

# often we want long data for charts and wide data for tables and often have to
# convert from one to another

# lets go wide, lets look at a handful of sites and attendances and pivot wider on date

# ie convert

#  org   period  attendances
#  abd   jan     100
#  abd   feb     200
#  abd   mar     300

# to

# org   jan   feb   mar
# abd   100   200   300

# lets start with filtering our data and selecting only a few columns

data_wide <- data |>
  filter(org_code == 'RQM',
         type == '1',
         period >= '2018-08-01') |>
  select (org_code,
          period,
          attendances)

# lets pivot
data_wide <- data_wide |>
  pivot_wider(names_from = period,
              values_from = attendances)



#########  do the same but for breaches










#######



# lets do a complex version
data_wide <- data |>
  filter(org_code == 'RQM',
         #type == '1',                     # have not removed the type
         period >= '2018-08-01') |>
  select (org_code,
          period,
          attendances,
          type)                            # have included type back in 

# lets pivot
data_wide <- data_wide |>
  pivot_wider(names_from = period,
              values_from = attendances)


# lets do a complex version with more sites
data_wide <- data |>
  filter(org_code %in% c('RQM','RJ1', 'RDD'), 
         #type == '1',                     # have not removed the type
         period >= '2018-08-01') |>
  select (org_code,
          period,
          attendances,
          type)                            # have included type back in 

# lets pivot
data_wide <- data_wide |>
  pivot_wider(names_from = period,
              values_from = attendances)



# lets do a complex version with breaches as well - another period dependant variable
data_wide <- data |>
  filter(org_code %in% c('RQM','RJ1', 'RDD'), 
         period >= '2018-08-01') |>
  select (org_code,
          period,
          attendances,
          breaches,                      # have breaches type back in 
          type)                           


# lets pivot 
data_wide <- data_wide |>
  pivot_wider(names_from = period,
              values_from = attendances)

# yuck not what we want - reset the data and lets try again
data_wide <- data |>
  filter(org_code %in% c('RQM','RJ1', 'RDD'), 
         period >= '2018-08-01') |>
  select (org_code,
          period,
          attendances,
          breaches,                      # have breaches type back in 
          type)                           

# lets pivot 
data_wide <- data_wide |>
  pivot_wider(names_from = period,
              values_from = c(attendances, breaches))

# r has given us automatically generated column names
# (there are options to change how that is handled, but not going to go into that now)

# lets make our wide data long

# start with a basic wide dataset again

data_wide <- data |>
  filter(org_code == 'RQM',
         type == '1',
         period >= '2018-08-01') |>
  select (org_code,
          period,
          attendances)

# lets pivot
data_wide <- data_wide |>
  pivot_wider(names_from = period,
              values_from = attendances)

# and make it long
data_long <- data_wide |>
  pivot_longer(cols = starts_with('20'),
               names_to = 'period',
               values_to = 'attendances')

# not going to lie, pivoting wide data to long is harder and requires much more
# wrangling, however the principle is the same and so going to move on 

#####################
# rolling functions #
#####################

# lets just do some other bits of wrangling
# say we wanted a 6 month rolling mean of attendances by each of the sites in
# our data...

library(zoo)

# zoo has a lovely function for rolling windows

data_roll <- data |>
  filter(org_code %in% c('RQM',
                         'RJ1', 
                         'RDD')
         ) |>
  group_by(org_code,
           type) |>
  arrange(org_code,
          type,
          period) |>
  mutate(rolling = rollapply(attendances,
                             6,
                             mean,
                             align= 'right',
                             fill = NA)
         ) |>
  ungroup()

########### see if you can adapt the code to bring back the max value in that rolling window
###########     then adjust the window to 3 months
###########         then add an additional new column with a median over 3 months












###########



# adding a row number - useful calculating times between things
# or identifying things like referral time to second contact etc

data_row <- data |>
  filter(org_code %in% c('RQM',
                         'RJ1', 
                         'RDD')) |>
  arrange(org_code,
          type,
          period) |>
  group_by(org_code,
           type) |>
  mutate(row_number = 1:n()) 

# now lets remove 2018 and check the row numbers still work on the new dataset

data_row <- data |>
  filter(org_code %in% c('RQM',
                         'RJ1', 
                         'RDD'),
         !between (period,                      # note the ! in front for the between to convert it into a not between
                   as.Date('2018-01-01') , 
                   as.Date('2018-12-31'))
         ) |>  
  arrange(org_code,
          type,
          period) |>
  group_by(org_code,
           type) |>
  mutate(row_number = 1:n()) |>
  ungroup()

# the ! is awesome - for example could add if in front of the org code 
# to filter everything except for those org codes

##########  adjust the code above to remove the financial year 2017 and
##########  return only the first 3 rows for only type 1 for each organisation
##########          *bonus points* to return the last 3 rows  for only type 1 for each organisation












#####################
# grouping by dates #
#####################

# grouping our data by year and getting totals

library(lubridate)

data_year <- data |>
  filter(org_code == 'RQM',
         type == '1') |>
  group_by(year_total = floor_date(period, 'year')) |>
  summarise (total_attendances = sum(attendances),
             total_breaches = sum(breaches),
             median_admissions = median(admissions, na.rm = TRUE))

# calculating a financial year
data_finance <- data |>
  filter(org_code == 'RQM',
         type == '1') |>
  mutate(finance_year = ifelse(month(period) >= 4, 
                               year(period) + 1, 
                               year(period)))

######### create a dataframe that contains a summary by sites RF4, Y02696 and RQ3
######### and returns the average maximum number of attendances across those sites by finical year 












#########

######################
# cutting up strings #
######################

# lets see if we can extract some more info from our data

# our org_code contains numbers and letters  
# what if we want to pull out just the numbers from those org_codes?

data_org_code_numbers <- data |>
  mutate(org_code_number = parse_number(as.character(org_code), 
                                        na = c("NA", 
                                               "Nothing")))

# this does throw up a warning as a number of org_codes do not have numbers in them
# it also is a little messy, but gives and idea of what can be done

#maybe we want to filter our data to any org code that contains a 'R' or a 'P' anywhere

data_filt <- data |>
  filter (str_detect (org_code,'R') |      # note the use of | to denote 'or'
            str_detect (org_code,'P'))

########## what organisations have a number in them over 50?
##########       can you return a dataframe with just the org_codes of those with a number over 50













# sometimes we want to shorten strings 
# we can do this by words or characters

example <- 'This is an example of a long sentance that I would like to shorten as it is far too long'

# by characters

substr(example, start = 1, stop = 15) 

# by words 

word(example, start = 1, end = 4, sep = fixed(' ')) # note uses end and not stop

# removing words 
# useful for handling really long hospital names

################################
# make hospital names readable #
################################


long_hospital_name <- 'Boggins University HosPital NHS Trust'

#lets change this to 'Boggins HosPital'

library(tm)
# tm is a text mining library but has some nice text features

short_hospital_name <- removeWords(long_hospital_name, c('University', 'NHS', 'Trust'))

short_hospital_name

# close but now we have odd spaces between our words and at the end

# nice little function call  string squish

short_hospital_name <- str_squish(short_hospital_name)

short_hospital_name

# this all will work within a dataframe as well

###### we still have an annoying rogue capital P in our hospital
######  use your google foo to find a str_ function to fix it
######    we want 'Boggins Hospital'







#####################
# intro to  factors #
#####################

# a quick note on factors
# factors are a datatype that converts a character into a categorical datatype
# think low medium and high

# lets have a look at our attendance groupings
data_fact <- data |>
  filter (period == '2016-04-01',
          type == '1') |>
  arrange (attendance_grouping)

# press ctrl and click on data_fact

# lets do a super quick plot
data_fact |> ggplot(aes(x=attendance_grouping)) +
  geom_histogram(stat="count")

# lets change our character data type to a factor data type
data_fact <- data_fact |>
  mutate(attendance_grouping = factor(
    attendance_grouping,
    levels = c(
      "Less than 5,000",
      "5,000 to 9,999",
      "10,000 to 14,999",
      "15,000 to 19,999",
      "20,000 to 24,999",
      "Over 25,0000")
      )
    )
  
# lets do our plot again
data_fact |> ggplot(aes(x=attendance_grouping)) +
  geom_histogram(stat="count")

# also if we look at our dataframe again, we can sort it by that factor too

#  this can be really useful to show groupings of providers by systems etc
# we can also reorder factors based on a different variable, for instance you may
# want to order your providers b y number of attendances or highest performance

#########################
# intro to dynamic text #
#########################


# A quick note on combining text and variables - useful for writting commentary or dynamic labels

#  Say we want to create a sentence that says 'The maximum number of attendances was 20000' (or whatever it is)

# we can create join a string together with the paste0 command 

text <- paste0('The maximum number of attendances was ', max(data$attendances))

# you can make longer strings and switch between text and non text with commas

text <- paste0('The maximum number of attendances was ', max(data$attendances), ' and the lowest was ', min(data$attendances))

############################################
# SPC - statistical process control graphs #
############################################

# Plot the dots - SPC charts
library(NHSRplotthedots)

data_spc <- data |>
  filter(org_code == 'RVR',
         type == '1')

data_spc |>
  ptd_spc(value_field = attendances,
          date_field = period)

#########  add in a target of 17500
#########     we actually want to show improvement as a reduction 











# we now want to create a for each attendance type
data_spc <- data |>
  filter(org_code == 'RVR')

# simply add a facet field
data_spc |>
  ptd_spc(value_field = attendances,
          date_field = period,
          improvement_direction = 'decrease',
          facet_field = type)

# not bad but looks a bit clumpy and we need to tweak a few things to make this 
# a pretty ggplot 

# turn our ptd_spc into an object
plot_spc <- data_spc |>
  ptd_spc(value_field = attendances,
          date_field = period,
          improvement_direction = 'decrease',
          facet_field = type)

# feed that object into ptd_create_ggplot

plot_spc |> 
  ptd_create_ggplot(fixed_y_axis_multiple = FALSE,
                    x_axis_date_format = "%b %y",
                    x_axis_breaks = "2 months")


######  Create a faceted plot for type 1 attendances RQM, RJ1 and RDD
######   Change the point size to look nicer
######    Change the x axis label to date rather than period
######      Make any other changes you feel would be useful to describe chart













###### HINT - think about using paste to make nice dynamic titles

#######################
# cheating with plots #
#######################

# esquisse is a really great packahe to help you learn GGplot syntax and
# play with plots - 
library(esquisse)







################################
# Basic functional programming #
################################

# say we want to do something many times,  we could just copy and past our code, 
# but that gets really dull really quickly
# it also means if we update something, we have to update it in all the 
# duplicate instances of that code
# this gets super dull super quickly

# so what we can do is set up a function do repeat a number of steps

# lets start simple

x <-  5
y <- 10
z <- 15

# we want to triple each of our variables and then 
# we could do x*3, y*3, z*3 

# then we want to check if our result is odd or even
# (we can do this with the %% operator - this is a modulous operator that returns the reminder of a division)
# ie 4/2 = 2 with no remainder   4%%2 = 0
# ie 5/2 = 2 with 1 remainder    5%%2 = 1
# therefore if a number %%2 == 0 then it is an even number else it is odd

# so in long hand we could do
x_mult <- x * 3

x_mod <- x_mult %% 2

# then
if_else(x_mod == 0, 'Even', 'Odd')

# but we would have to repeat all of that again if we wanted to check y & z and
# change all the varibles

# so lets build a function
# we start with naming our function
# we then do all the calculations
# we then return the result
# NOTE - a function can only return one result, but later we will be clever in how 
#        this 'one result' is built

is_odd_or_even <- function (input) {
  mult <- input * 3
  mod <- mult %% 2
  return (if_else(mod == 0, 'Even', 'Odd'))
}

# we can now test it with our variables
is_odd_or_even(x)

is_odd_or_even(y)

# or put in a new number completely
is_odd_or_even(4256)

# NOTE the variables mult and mod within our function only exist within the function

# you can add more than one varible into a function seperated by commas

###########  tweak the above function so that instead of times by 3 
###########  you specify the number it multiples by
###########  for bonus points return a string that says "<input> times by <whatever> is an <odd/even> number"








####  HINT paste0 is your friend to 'return' to


# we can apply our made up new functions to a dataframe

############ filter the data down to RQM, RJ1 and RDD
############ lets convert our type to a full numeric - so lets change 'other' to '3'
############ lets ensure that type is as a numeric datatype
############ make a new column that uses your function to multiple attendances by type 
############ and returns the result 

















# this is a bit of a silly example but shows what potentially can be done.
# can be great for caculating rates per 10,000 

# is not just data functions, we can set up a plot to be a function
# say we had our lovely SPC chart set up the way we want it and we just want 
# to tweak the site 

plot_site <- function (site) {
  
  # we now want to create a for each attendance type
  data_spc <- data |>
    filter(org_code == site)      # note this is where we are using our 'site' variable
  
  # turn our ptd_spc into an object
  plot_spc <- data_spc |>
    ptd_spc(value_field = attendances,
            date_field = period,
            improvement_direction = 'decrease',
            facet_field = type)
  
  # feed that object into ptd_create_ggplot
  
  return (plot_spc |> 
    ptd_create_ggplot(fixed_y_axis_multiple = FALSE,
                      x_axis_date_format = "%b %y",
                      x_axis_breaks = "2 months"))
}


plot_site('RJ1')

# it is good practice for a function not to call on anything outside of the function
# in this instance we are calling on our 'data' which does not exist within the function
# this can cause issues, especially if we later change something in the data
# so best practice is to feed the data into function.
# We can set it to a default, but this allows us to change it later without issue
# or to point it at another dataset if we want to without having to copy the function


plot_site <- function (site, df=data) {     # this is where we set a default
  
  # we now want to create a for each attendance type
  data_spc <- df |>                        # note this is where pulling in our data
    filter(org_code == site)      
  
  # turn our ptd_spc into an object
  plot_spc <- data_spc |>
    ptd_spc(value_field = attendances,
            date_field = period,
            improvement_direction = 'decrease',
            facet_field = type)
  
  # feed that object into ptd_create_ggplot
  
  return (plot_spc |> 
            ptd_create_ggplot(fixed_y_axis_multiple = FALSE,
                              x_axis_date_format = "%b %y",
                              x_axis_breaks = "2 months"))
}

# works just the same 
plot_site('RJ1')

# ok so we have a nice facet plot for a single site, but now I want to create the
# same plot but over a number of sites, or even all the sites
# of course I could just call the function for each and every site, however
# what if there is a new site or a site is removed.  I then have to keep a curated
# site list.  Sounds like hard work.  Yerk!

# simple loops

# a loop is a element of that repeats a portion of code a desired number of times

# lets look at a simple sequence

seq(1:10)

# this gives us a simple sequence from one to ten
# we want to print out the sequence number plus 5

# we can make a loop to do this

for (i in seq(1,10)) {                 # traditionally variables in a loop start at i (they just do!)
  # what will happen is R will run the loop and for the first iteration 
  # i will be 1 and then 2 and then 3 etc 
  i_plus_five <- i+5
  result <- paste0(i,' plus 5 equals ', i_plus_five)
  print(result)
  }

# we can mess about with seq to come up with all sorts of number patterns

seq(50,nrow(data), 500)

# this makes a number sequence that starts at 50, ends at the number of rows in 
# our data (12765) and steps in incraments of 500

# however the real magic starts is when we feed in a vector of data

vector <- c('Bob', 'Pete', 'Mary')

for (i in vector) {                 
  result <- paste0('Hello ',i)
  print(result)
}

# so now we can look at running our function but within a loop 
# almost that simple but we need to recall that a function can only return one result
# we would really like to collate all of those results into one object
# we do this by utilising a list object
# a list object is a super special multi dimensional thing that can contain
# a mix of varibles, dataframes, plot objects and any combination 

# we can not create a list from within our loop, otherwise we would be creating it
# blank on each iteration
# so lets make a blank list outside of our loop

plot_list <- list ()

# this is the vecor we will feed to our loop
vector <- c('RQM', 'RJ1', 'RDD')

for (i in vector) {
  plot_list[i] <- plot_site(i)
}

# lets call some of our plits from the list
plot_list['RQM']

plot_list['RJ1']

# lets just say for example we wanted to run across all sites in a dynamic way
# we could create a vector of all the site names from the data

vector <- unique(data$org_code)

vector

# this does create a list 274 long and so lets not run it now, 
# but we could feed that into the loop

######################################### 
#creating paramerised reports in a loop #
#########################################

# lets say we want to create a seperate report for each site
vector <- c('RQM', 'RJ1', 'RDD')

# we can call the data and wrangle it in a separate file and 
# then call quart to run the data based on a parameter or paremters that we specify


for (site in vector) {
  
  filename <- paste0('output_',site, '.html')
  
  quarto::quarto_render(
    input = 'param_quarto.qmd',
    output_format = 'html',
    execute_params = list(site = site),
    output_file =  filename 
        )
}







