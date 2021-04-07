##########################################################################
#
# THIS CODE DOWNLOADS THE MOVIELENS DATA TO THE data/ FOLDER
# AND DIVIDES IT INTO THE EDX AND VALIDATION SETS.
#
# THIS CODE WAS PROVIDED BY THE 'DATA SCIENCE: CAPSTONE' COURSE
# (HARVARDX PH159.9X) AT EDX.ORG 
# 
##########################################################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# Downloading the Movielens 10M dataset from
# https://grouplens.org/datasets/movielens/10m/
# https://files.grouplens.org/datasets/movielens/ml-10m.zip

# Create a temporary path for the file
dl <- tempfile()

# Download file from the internet and set destination to dl
download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

# Read the ratings file and assign column names
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

# Read the movies file and assign column names
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# Convert movies to a data frame and convert to appropriate classes
movies <- as.data.frame(movies) %>% 
  mutate(movieId = as.numeric(movieId),
         title = as.character(title),
         genres = as.character(genres))

# Combine ratings and movies into one data set
movielens <- left_join(ratings, movies, by = "movieId")




# Create Validation set (10% of movielens data)
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = movielens$rating,
                                  times = 1,
                                  p = 0.1,
                                  list = FALSE)
edx <- movielens[-test_index, ]
temp <- movielens[test_index, ]


# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>%
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")
# semi_join() returns all rows in edx with a match in temp
# first by movieId, then by userId

# Note that 8 observations from temp were excluded, meaning
# those 8 movie-user rating combinations did not exist in 
# the training (edx) set

# Move those rows from temp back into the edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

# Clean up the workspace
rm(dl, ratings, movies, test_index, temp, movielens, removed)


# Save RData objects to the rda folder
save(edx, file = "rda/edx.rda") # Note that edx exceeds GitHub's file size limit
save(validation, file = "rda/validation.rda")


    
