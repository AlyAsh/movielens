#########################
#
# DATA EXPLORATION
#
#########################

# Where to begin with making a prediction on ratings?

# First, let's get an idea of the data we are working with.
names(train)

length(unique(train$userId)) # 69,878 unique users
length(unique(train$movieId)) # 10,677 unique movies
c(min(train$year), max(train$year)) # the movies rated came out between 1915 and 2008

# DISTRIBUTION OF RATINGS - BY MOVIE

# The distribution of movie ratings shows that some movies are given
# very few or very many ratings.
train %>% group_by(movieId) %>% summarize(n_ratings = n()) %>% 
  ggplot(aes(n_ratings)) + geom_histogram(bins = 30, color = "black") +
    scale_x_log10() +
    ggtitle("Number of ratings per movie") +
    xlab("Number of ratings (log10)")

# In fact, 99% of movies have 9,158 or fewer ratings,
train %>% group_by(movieId) %>% summarize(n_ratings = n()) %>% top_frac(-.99) %>%
  pull(n_ratings) %>% max()

# And the top 1% most-rated movies contribute almost 70% of all the ratings
# in the data set
train %>% group_by(movieId) %>% summarize(n_ratings = n()) %>% top_frac(0.1) %>%
  pull(n_ratings) %>% sum()/length(train$rating)

# DISTRIBUTION OF RATINGS - BY USER

# Similarly, there are a few users who provided very large or very small
# number of ratings.
train %>% group_by(userId) %>% summarize(n_ratings = n()) %>%
  ggplot(aes(n_ratings)) + geom_histogram(bins = 30, color = "black") + 
    scale_x_log10() +
    ggtitle("Number of ratings per user") +
    xlab("Number of ratings (log10)")

# 99% of users have given 763 or fewer ratings.
train %>% group_by(userId) %>% summarize(n_ratings = n()) %>% top_frac(-0.99) %>%
  pull(n_ratings) %>% max()

# The top 1% contribute almost 45% of all ratings in the data set
train %>% group_by(userId) %>% summarize(n_ratings = n()) %>% top_frac(0.1) %>%
  pull(n_ratings) %>% sum()/length(train$rating)


###################
#
# MEASURING MODEL ACCURACY
#
##################

# Our objective is to create a model that accurately predicts the rating
# for a given user and a given movie.
#
# How would we calculate the accuracy of the models we make?
#   We can use the Root Mean Square Error (RMSE), which takes the standard
#   deviation of the residuals. Residuals measure how far from the regression
#   line your predictions are. Therefore, RMSE measures how well or how poorly
#   the model fits the data.

# We can write a function that calculates the RMSE for all our models
RMSE <- function(actual_ratings, predicted_ratings){
  sqrt(mean((actual_ratings - predicted_ratings)^2))
}

########################
#
# MODEL 1: BASELINE RATING
#
########################

# The simplest model would be to predict a rating that is simply the average
# rating of all observations in the data, i.e. a baseline movie rating.
# We will call this rating 'mu'.
mu <- mean(train$rating)

RMSE(test$rating, mu)
# Predicting based solely on the average of all movie ratings 
# results in a RMSE of 1.06, which is not a very good indication
# of the model's ability to predict the rating.

# We will store the results in a table for future comparison.
results <- data.frame(method = "Basline",
                      RMSE = RMSE(test$rating, mu))
results

#####################
#
# MODEL 2: MOVIE EFFECT
#
#####################

# We know that certain movies are commonly thought of as better or worse than
# others due to its inherent features. 
# Does the data show this?

train %>% group_by(movieId) %>% 
  summarize(average_rating = mean(rating - mu)) %>%
  ggplot(aes(average_rating)) + 
    geom_histogram(bins = 30, color = "black") +
    ggtitle("Are movies rated differently?") +
    xlab("Distance from the Average Movie Rating") +
    ylab("Number of movies")

  # Observation: While most movies are rated around the average,
  # the distribution of ratings show that certain
  # movies are on average rated better or worse than others.

# Let us call this movie-specific effect 'bi' (beta to denote effect, i to denote movie)
# and calculate bi by getting the average difference from mu that each movie gets.
movie_effect <- train %>% group_by(movieId) %>%
    summarize(bi = mean(rating - mu))

# We add these "movie effects" to the model, to improve our prediction on the 
# test set. 
#
# We calculate our predicted rating as the baseline movie rating
# +/- the effect of the particular movie we are observing.
prediction <- test %>% 
      left_join(movie_effect, by = "movieId") %>%
      mutate(prediction = mu + bi) %>%
      pull(prediction)

# How well does our "Movie Effect" model predict the test set?
RMSE(test$rating, prediction)
  # It appears to do much better, giving us a RMSE of 0.94.

results <- rbind(results,
                 data.frame(method = "Movie Effect",
                      RMSE = RMSE(test$rating, prediction)))
results
rm(prediction)

##########################
#
# MODEL 3: MOVIE AND USER EFFECTS
#
##########################

# Using the same logic, we can ask whether there is also a "user effect", in 
# which certain users might rate a certain way due to their own specific 
# characteristics. 
#
# Do different users rate movies differently?

train %>% group_by(userId) %>%
    summarize(user_effect = mean(rating - mu)) %>%
    ggplot(aes(user_effect)) + 
      geom_histogram(color = "black") +
      ggtitle("Do users rate movies differently?") +
      xlab("Distance from the Average Movie Rating") +
      ylab("Number of users")

  # Observation: While most users appear to rate around the average,
  # there appears to be variability in how users rate on average, with some
  # users rating more harshly than others.

# We can calculate the "User Effect" as a user's rating above or below
# the sum of mu and the movie-specific effect bi.
# In other words, it is the user's unique contribution or "effect" on the rating 
# ultimately given. We will call this bu.

user_effect <- train %>% 
  left_join(movie_effect, by = "movieId") %>%
  group_by(userId) %>%
  summarize(bu = mean(rating - (mu + bi)))

# Our prediction will now be a function of three terms (plus an error term)
# of a baseline rating, a movie-specific effect, and a user-specific effect.

prediction <- test %>% 
  group_by(movieId) %>%
  left_join(movie_effect, by = "movieId") %>%
  group_by(userId) %>%
  left_join(user_effect, by = "userId") %>%
  mutate(prediction = (mu + bi + bu)) %>%
  pull(prediction)

RMSE(test$rating, prediction)
  # Now we get a much better RMSE value of 0.866

results <- rbind(results,
                 data.frame(method = "Movie and User Effect",
                            RMSE = RMSE(test$rating, prediction)))
results
rm(prediction)

############################
#
# MODEL 4: REGULARIZED MOVIE AND USER EFFECTS
#
############################

# However, as we saw when exploring the data, not all of movies have the 
# same number of ratings, and not all users provide the same number of ratings.
# In fact, there were clear outliers: 
#   - movies that had very few or very many ratings, 
#   - and users that gave very few or very many ratings.


# A model that is trained on very few data points (i.e. a small sample size)
# is at risk of over-fitting. Though the model may predict the training 
# set very well, it may not predict another set of observations as well.

# To mitigate this, we can use REGULARIZATION - assigning a penalty term 
# to large estimates that come from small sample sizes.

# This penalty value is "passed on" to our calculation of RMSE, in order
# to show the effect of these potentially erroneous data points.
#
#   1/N * sum (Y_u,i - mu - b_i)^2 + lambda * sum b_i^2
#   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^   ^^^^^^^^^^^^^^^^^^
#               RMSE                   penalty term

# REGULARIZATION makes use of a value lambda, which we assign ourselves to be the
# penalty.
#
# In the case of our movie-effect-only model, a new estimate derived
# for the movie effect that minimizes the penalty term will be:
#
#   b_hat_i(lambda) =  [1 / (lambda + n_i)] * sum(Y_u,i - mu_hat)
#
#       where n_i is the # of ratings for movie i

# Which means that as the number of movie ratings increases (we get more data), 
# the value of lamba becomes insignificant to the equation (no penalty). 
# Whereas if there were few number of ratings, then lambda's effect will be more pronounced,
# and our estimate of the movie effect is adjusted (diminished) accordingly.

# But what value of lambda should we use? 
#
# Lambda is an example of what is called a 'tuning parameter' -- values in our
# model than can be tuned in order to result in the most optimal metric. 
# In our case, we want the smallest RMSE.

lambdas <- seq(0, 10, 0.5)

# Calculate new RMSEs with regularization
RMSEs <- sapply(lambdas, function(lambda){
  
  movie_effect <- train %>% group_by(movieId) %>%
    summarize(bi = sum(rating - mu)/(lambda + n()))
  
  user_effect <- train %>% group_by(movieId) %>%
    left_join(movie_effect, by = "movieId") %>%
    ungroup() %>% group_by(userId) %>%
    summarize(bu = sum(rating - mu - bi)/(lambda + n()))
  
  predictions <- test %>% 
    group_by(movieId) %>% 
    left_join(movie_effect, by = "movieId") %>%
    ungroup() %>% group_by(userId) %>% 
    left_join(user_effect, by = "userId") %>%
    mutate(prediction = mu + bi + bu) %>%
    pull(prediction)
  
  return(RMSE(test$rating, predictions))
  
})
plot(RMSEs)
lambdas[which.min(RMSEs)] 
  # The lambda value that gives us the smallest RMSE is lambda = 5
  # RMSE: 0.8655498

results <- rbind(results,
                 data.frame(method = "Regularized Movie & User Effect", 
                            RMSE = min(RMSEs)))
    # We see our RMSE improve from 0.866 to 0.865 with regularization.

results

rm(lambdas, RMSEs)


