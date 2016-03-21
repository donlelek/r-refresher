# R script for the second session of the R refresher course
# by Dr. Raphael Vanderstichel
# March 23rd, 2016
# Second part; The dplyr/tidyr way
# Derek Price - dprice@upei.ca

# For this workshop we will be using mainly the packages 
# `dplyr` and `tidyr`, both written by Hadley Wickham

# First let's load them, this is a good time to install them if you 
# haven't already
library(dplyr) 
library(tidyr) 
library(readr)

# Raph covered how to select variables and observations in a number
# of different ways using a number of approaches in 'base' R. In contrast,
# dplyr and tidyr offer a limited number of functions, also called 'Verbs' 
# that were designed to be easy to remember and to work in a consistent way.
# Each of these verbs have different versions and/or some helper functions.   
# 
# dplyr main verbs: 
# 'select' 'filter' 'group_by' 'arrange' 'mutate' 'summarize' 
# also 'bind_rows' 'bind_cols' and the 'join' family of functions
# 
# tidyr main verbs:
# 'gather' 'spread' 'separate' 'unite'

# for the next examples I'll be using VER's daisy2 dataset
src <- "https://raw.githubusercontent.com/donlelek/r-refresher/master/data/daisy2.csv"
daisy2 <- read_csv(src)

# | Variable   | Description |
# |------------|-------------|
# | region     | Region |
# | herd       | Herd number |
# | cow        | Cow number |
# | study_lact | Study lactation number |
# | herd_size  | Herd size |
# | mwp        | Minimum wait period for herd |
# | parity     | Lactation number |
# | milk120    | Milk volume in first 120 days of lactation |
# | calv_dt    | Calving date |
# | cf         | Calving to first service interval |
# | fs         | Conception at first service |
# | cc         | Calving to conception interval |
# | wpc        | Interval from wait period to conception |
# | spc        | Services to conception |
# | twin       | Twins born |
# | dyst       | Dystocia at calving |
# | rp         | Retained placenta at calving |
# | vag_disch  | Vaginal discharge observed |
# | h7         | Indicator for 7 herd subset |

daisy2

str(daisy2)
glimpse(daisy2) # also from dplyr, adapts to your screen

#### Select variables (columns) ####
# the main verb here is select
# basic usage:
select(daisy2, cow, dyst) # include by name
select(daisy2, -h7, -wpc) # exclude by name
select(daisy2, mwp:twin)    # include by range
select(daisy2, -mwp:twin)   # oops!
select(daisy2, -(mwp:twin)) # exclude by range

# usign helper fuctions
select(daisy2, everything()) # what?, we'll use it later...

# helpers for character strings
select(daisy2, contains("a"))      
select(daisy2, -contains("a"))      
select(daisy2, starts_with("c")) 
select(daisy2, ends_with("t")) 
select(daisy2, one_of(c("calv_dt", "vag_disch")))
select(daisy2, one_of(c("calv", "vag"))) # careful, only accepts complete names

# regular expressions
select(daisy2, matches("._."))

# numerical range
# let's make a small dataset to try this
df <- as.data.frame(matrix(runif(100), nrow = 10))
df
select(df, num_range("V", 1:5))

# reorder your dataset using select
select(daisy2, cow, h7, everything())

# or rename
select(daisy2, cow_id = cow, everything())
rename(daisy2, cow_id = cow) # similar result but keeps original order
select(daisy2, a_ = contains("a"), everything()) # batch rename

#### Select observations (rows) ####
# the main verb is `filter`
# basic usage:
# filter using logical operators, need help?, run this: ?base::Logic
filter(daisy2, cow == 1)
filter(daisy2, is.na(fs)) # how to deal with NAs
filter(daisy2, !is.na(fs))
filter(daisy2, cow != 1)
filter(daisy2, cow <= 4)
filter(daisy2, cow %in% 1:4) # this works even with ordered factors

# multiple arguments
filter(daisy2, herd == 1 & parity < 2)
filter(daisy2, herd == 1, parity < 2) # comma interpreted as 'AND'
filter(daisy2, herd == 1, parity < 2 | parity > 6) # '|' means 'OR'

# using helper fuctions
sample_frac(daisy2, 0.05) # random sample 5% without replacement (the default)
sample_n(daisy2, 20, replace = TRUE) # random sample of exact number of obs
distinct(sample_n(daisy2, 20, replace = TRUE)) # excludes duplicates
slice(daisy2, 5:15)
slice(daisy2, which(herd == 6)) # `which` returns an index

#### Selecting Variables AND Observations: ####
#### using pipe operator to produce chains

# I have introduced the pipe operator ( %>% ) in a previous talk but, I'll make
# a super quick intro, just read %>% as 'then'

# non-piped version to select rows and columns
select(filter(daisy2, herd == 4), contains("a"))

# the pipe version, the result of each step is passed to the right
daisy2 %>% filter(herd == 4) %>%  select(contains("a"))

# or even nicer, each line is a step
daisy2 %>% 
  filter(herd == 4) %>% 
  select(contains("a"))

# you can pipe chains as long as you want, and you don't have to learn
# another syntax, just omit the data argument

#### Data mangement ####
#### Transforming Variables ####
# the main verb for variable creation is `mutate`, there is also a 
# `mutate_each` version that applies the tranformation to all variables

daisy2 %>% 
  select(calv_dt) %>% 
  mutate(c_date = lubridate::dmy(calv_dt))

# up to this point we haven't modified the original datasets, to do so
# we have to assign the result of our chain to the object that represents our
# dataset, or assign it to a new object
daisy2 <- daisy2 %>% 
  mutate(c_date = lubridate::dmy(calv_dt))

daisy2

#### Apply new variables over groups (a la `bysort: egen`) ####
# the main verb for applying functions by group is unsurprisingly `group_by`
# but, because usually the order here might be of importance we usually use it 
# combined with `arrange`

daisy2 %>% 
  group_by(herd) %>% 
  arrange(cow) %>%
  mutate(avg_milk = mean(milk120, na.rm = TRUE)) %>%
  select(avg_milk, everything()) # puts the resulting variable as first column

# there is a couple more useful helper functions to generate new variables,
# most notables are lag/lead percent_rank, row_number, ntile, cumsum

daisy2 %>% 
  group_by(herd) %>% 
  mutate(last_cow  = lag(cow),
         next_cow = lead(cow),
         skip_next_cow = lead(cow, n = 2L),
         cum_milk = cumsum(milk120),
         row = row_number(), 
         rank = dense_rank(milk120),
         percentile = percent_rank(milk120),
         decile = ntile(milk120, 10)) %>% 
  select(row, cow, last_cow, next_cow, skip_next_cow, 
         cum_milk, rank, percentile, decile)

#### Summarize your data ####
# the main verb for summarizing data in dplyr is `summmarise` but, as in other
# verbs, there is a `summarise_each` version that applies to all selected vars.
# The summary will be applied to the groups specified in the `group_by` 
# statement. 
# Note: While dplyr accepts also the spelling `summarize` in my case this
# calls a homonym function from the `Hmisc` package, if this happens to you 
# make it work by using this syntax `dplyr::summarize`

daisy2 %>% 
  group_by(region) %>% 
  summarise(avg_milk120 = mean(milk120, na.rm = TRUE),
            sd_milk120  = sd(milk120, na.rm = TRUE),
            tot_milk120 = sum(milk120, na.rm = TRUE))

# remember df? we'll use it to show summarise_each
df_sum <- df %>%
  summarise_each(funs(sum, mean, sd))

df_sum  

#### Reshaping your data using `tidyr` ####
# I assume you're somehow familiar with the difference between wide and long 
# format of a dataset, if you're not read http://r4ds.had.co.nz/tidy.html
# in a nutshell tidy data is when:
# - each variable is placed in its own column,
# - each observation in its own row,
# - and each value in its own cell
# 
# The main verbs for reshaping are `gather` to reshape from wide to long and
# `spread` to reshape from long to wide format. If you have more than one
# variable inside a column/cell you need to use `separate`. There is also a 
# `unite` function for some specific ocasions when you need to concatenate the
#  content of several columns

# reshape df to long
df_sum %>% 
  gather(var, value, 1:30)

# We can separate the information contained in `var` using `separate`
df_sum %>% 
  gather(var, value, 1:30) %>%
  separate(var, c("var", "statistic"), sep = "_")

# Well, now we should have each statistic in its own column...
df_sum %>% 
  gather(var, value, 1:30) %>%
  separate(var, c("var", "statistic"), sep = "_") %>%
  spread(statistic, value)

#### Merging datasets and joining tables ####
# merging datasets is done using `bind_cols` and more commonly `bind_rows`
# dplyr has a family of `join` functions that are designed after SQL
# join commands, `left_join`, `right_join`, `inner_join`, 
# and `full_join` are the most commonly used. In base R you can use 
# `rbind`, `cbind` and `merge` but dplyr versions are faster and easier to use.
# If you want to learn more about this I recommend reading:
# http://r4ds.had.co.nz/relational-data.html

# let's add some new observations to the first herd
new_cows <- data.frame(region  = c(1, 1),
                       herd    = c(1, 1),
                       cow     = c(0, 0),
                       parity  = c(3, 4),
                       milk120 = c(3000, 3500))

# and now put them together, of course if you want to make this permanent 
# you have to reassign the result to daisy2 or a new dataset
bind_rows(new_cows, daisy2)

# what about columns? lets add a random number (can be used to subset)
rnd <- data.frame(random = runif(9383)) # match the number of observations

# add the new column
bind_cols(rnd, daisy2)

# let's say we have a dataset with cow treatments
cow_treatments <- data.frame (cow = 0:5, 
                              treatment = LETTERS[1:6])
cow_treatments

# left_join, search the key in the first dataset and pulls in all the matching
# info from dataset 2
daisy2 %>%
  filter(cow < 10) %>%
  left_join(cow_treatments, by = "cow") %>%
  select(treatment, everything())

# right_join is the opposite of left_join also, if `by` is not specified
# all the common columns will be used and a message will be printed 
daisy2 %>%
  filter(cow < 10) %>%
  right_join(cow_treatments) %>%
  select(treatment, everything())

# only observations in both datasets
daisy2 %>%
  filter(cow < 10) %>%
  inner_join(cow_treatments, by = "cow") %>%
  select(treatment, everything())

# all observations from both datasets
daisy2 %>%
  filter(cow < 10) %>%
  full_join(cow_treatments,  by = "cow") %>%
  select(treatment, everything())

# what if theres more than one treatment? 
cow_treatments <- data.frame (cow = c(1, 2, 3, 4, 4, 5, 5, 6, 7), 
                              treatment = LETTERS[1:9])
cow_treatments

# right_join didn't led to any duplication
daisy2 %>%
  filter(cow < 10) %>%
  right_join(cow_treatments, by = "cow") %>%
  select(cow, treatment, everything())

# but left_join did because there are multiple matches
daisy2 %>%
  filter(cow < 10) %>%
  left_join(cow_treatments, by = "cow") %>%
  select(cow, treatment, everything())

# there are tw more joins, these are special joins called filtering joins
# `semi_join` keeps all observations in the first dataset that have a match 
# in the second while `anti_join` drops all observations
daisy2 %>%
  filter(cow < 10) %>%
  semi_join(cow_treatments, by = "cow") # no duplication!

daisy2 %>%
  filter(cow < 10) %>%
  anti_join(cow_treatments, by = "cow")

#### Questions? ####