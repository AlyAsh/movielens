##############################
#
# GENERATE TRAINING AND TEST SETS, CLEAN/PROCESS DATA. 
#
# We can add to this script in conjunction with future analyses.
#
##############################

if(!exists("edx")) load(file = "rda/edx.rda")

# Load packages
library(tidyverse)
library(caret)

# Let's first look at what the data contains
names(edx)
head(edx)

####################
#
# CONVERTING TIMESTAMP
#
###################

# In its current format, the 'timestamp' variable is not easily interpretable
# so we will convert it to a more readable format using the lubridate package.
library(lubridate)
edx <- edx %>% mutate(timestamp = as_datetime(timestamp))
head(edx)

#####################
#
# EXTRACTING YEAR (MOVIE RELEASE YEAR)
#
####################

# We can see that the 'title' column contains the year the movie was released,
# which may be useful in later analysis. So let's split that into its own column

# But make sure that we don't include movies that have numbers in their title
# that can be mistaken for years. The year is formatted such that:
#   - it is a 4-digit number
#   - beginning with either 19 or 20
#   - encased in parentheses
#   - and found at the end of the string
# (Also remove the trailing spaces)

edx <- edx %>%
  mutate(year = str_extract(title, "\\s*\\((19|20)\\d{2}\\)$"),
         title = str_remove(title, "\\s*\\((19|20)\\d{2}\\)$")) %>%
  mutate(year = str_extract(year, "(19|20)\\d{2}"))

head(edx)

##################
#
# 'PULL MY DAISY' GENRE
#
##################

# Looking at all the unique genres, we see a genre called "(no genres listed)"
edx %>% filter(!str_detect(genres, "\\|")) %>%
  select(genres) %>% distinct()

edx %>% filter(genres == "(no genres listed)")
  # And discover that only 1 movie, 'Pull My Daisy', classifies as having no genre.
  # After doing some research, we can say that its genre is "Comedy",
  # so rather than have that one outlier, we will change its genre.

edx$genres[edx$genres == ("(no genres listed)")] <- "Comedy"
edx %>% filter(title == "Pull My Daisy")


####################
#
# GENERATE TRAIN AND TEST SETS
#
###################

# Finally, we can generate the training and test sets we will use to
# create our model
library(caret)
set.seed(1994, sample.kind = "Rounding")
test_index <- createDataPartition(y = edx$rating,
                                  times = 1,
                                  p = 0.20,
                                  list = FALSE)
train <- edx[-test_index, ]
temp <- edx[test_index, ]

# We have to make sure that the movie-user combinations in the test set
# exist in the train set:
test <- temp %>% 
  semi_join(train, by = "movieId") %>%
  semi_join(train, by = "userId")

# And move the movie-user combinations that weren't back into the training
# set
removed <- anti_join(temp, test)
train <- rbind(train, removed)

rm(temp, removed, test_index)
