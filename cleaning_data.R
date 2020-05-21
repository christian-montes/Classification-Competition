

# Loading packages --------------------------------------------------------

library(tidyverse)
library(skimr)
library(janitor)



# Loading data ------------------------------------------------------------

train_data <- read_csv(file = 'data/unprocessed/train.csv') %>% 
  clean_names()

train_data %>% 
  skim_without_charts()


test_data <- read_csv(file = 'data/unprocessed/test.csv') %>% 
  clean_names()


# Cleaning data -----------------------------------------------------------



# Categorical variables
# there are some variables that have a lot of classes; can possibly replace with a cluster assignment
####
# addr_state
# emp_title
# purpose (possibly)

train_data %>% 
  group_by(purpose) %>% 
  count()

# has 12 levels 


train_data %>% 
  group_by(emp_title) %>% 
  count()


# variables that are incorrectly typed
# earliest credit line is a date ______ kept only the year, turned to numeric
# emp length should be numeric _____ extracted years, 1 NA; fill with cluster assignment
# last credit pull date is date instead of character _____ took out the year, most important; no NAs
# term of loan should be numeric ___ took out the months ; no NAs; maybe convert to years for consistency

# other
# verification status should be binary; has redundant level
# changed; 1 for verified 0 for not verified; factor

test_data %>% 
  group_by(application_type) %>% 
  count()


# changing tyoe of variables that are incorrectly typed
clean_testing_data <- test_data %>% 
  mutate(earliest_cr_line = sub('^.*(\\d{4})$', '\\1' , earliest_cr_line),
         earliest_cr_line = as.numeric(earliest_cr_line),
         emp_length = sub('(<*)\\s*([0-9]+)(\\+*).*$', '\\2', emp_length),
         emp_length = as.numeric(emp_length),
         last_credit_pull_d = sub('^([a-zA-z]+)-(\\d{4})$', '\\2', last_credit_pull_d),
         last_credit_pull_d = as.numeric(last_credit_pull_d),
         term = sub('^(\\d+)\\s([a-z]+).*', '\\1', term),
         term = as.numeric(term),
         verification_status = fct_recode(verification_status,
                                          '1' = 'Verified',
                                          '0' = 'Not Verified',
                                          '1' = 'Source Verified'),
         verification_status = as_factor(verification_status))

# pipeline works on the test data


# writing csv for work in python
write_csv(clean_testing_data, path = 'data/processed/test.csv')

clean_training_data %>% skim_without_charts()



