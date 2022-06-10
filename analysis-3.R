#######################
#
# MOVIE-USER SPECIFIC INTERACTION
#
######################

# We have so far determined 3 factors that influence a given rating:
#   - a baseline value shared by all movie-user combinations in the data
#   - a movie-specific effect (regularized)
#   - a user-specific effect (regularized)

# There may also be effects caused by the interaction between a specific movie
# and a specific user, we will call them movie-user effects.

# These effects may include:
#   (1) The number of days since the user's first rating.
#         To indicate whether a user becomes a harsher or more forgiving 
#         critic over time..
#         This can be calculated as a factor that allows the user's 
#         rating to (linearly) depend on the (square root of the) number of 
#         days since the user's first rating.
#
#   (2) The number of days since the movie's first rating by anyone.
#         To indicate whether the user saw the movie early (showing interest)
#         or later (purchasing a DVD or catching it on TV)
#
#   (3) The number of people who have already rated the movie.
#         To indicate whether the user is a trend-setter (rating movies that
#         many haven't seen) or likes to follow the crowd.
#
#   (4) The movie's overall rating at the time their rating was given.


######################
#
# (1) DAYS SINCE USER'S FIRST RATING
#
######################

#   The movie-user-specific interaction occurring here is the user is rating
#   their nth movie; the number of 'days_since' the user's first rating will
#   always be unique to the specific movie being rated.

# We will need to extract date information from the data set
library(lubridate)

# Keep track of the first day of rating for each user in a variable called
# 'first_day'
user_first <- train %>% arrange(timestamp) %>% 
  group_by(userId) %>% 
    filter(seq(n()) == 1) %>%
    mutate(user_first_day = timestamp) %>%
  select(userId, user_first_day)

# And calculate the running total number of days since the user's first rating
# for each movie a user rates in a variable called 'days_since'. 
# We then calculate y as the residual movie-user-specific effect.
dat <- train %>% arrange(timestamp) %>%
  group_by(movieId) %>% 
    left_join(movie_effect, by = "movieId") %>% ungroup() %>%
  group_by(userId) %>% 
    left_join(user_effect, by = "userId") %>%
    left_join(user_first, by = "userId") %>%
    mutate(days_since = time_length(timestamp - user_first_day, "days"),
         y = rating - mu - bi - bu) %>% 
    ungroup() %>%
  select(userId, movieId, days_since, y)

dat %>% mutate(days_since = round(days_since, digits = 0)) %>%
  filter(days_since != 0) %>%
  ggplot(aes(days_since)) +
    geom_histogram()

  # We see that very few ratings were given long after the user's first rating,
  # and therefore it would be helpful to transform the 'days_since' variable
  # in order to make calculations easier.


# We want to know if there is a linear relationship between the residual
# y and the square-root of days_since for each user.
fit_ur <- dat %>% group_by(userId) %>%
    summarize(intercept = lm(y ~ sqrt(days_since))$coefficients[1],
              slope = lm(y ~ sqrt(days_since))$coefficients[2])

# Rounded days_since version:
fit_ur2 <- dat %>% group_by(userId) %>%
    mutate(days_since = round(days_since)) %>%
    summarize(intercept = lm(y ~ sqrt(days_since))$coefficients[1],
              slope = lm(y ~ sqrt(days_since))$coefficients[2])

# Each user has a slope and intercept that represent the linear relationship between
# their (residual) rating and the number of days since their first rating.

sum(is.na(fit_ur))
sum(is.na(fit_ur2))
# We see that some have 'NA' slopes. 

fit_ur2 %>% filter(is.na(slope)) %>%
  left_join(train, by = "userId")
  # Upon inspecting these users, we notice that they rated all movies at the
  # same time, and never rated again. This is likely caused by a bulk data collection
  # process. In these cases, we will choose to ignore the model completely,
  # since it doesn't provide any useful information on the user's behavior.


# Test the model's prediction and RMSE:
test %>% arrange(timestamp) %>%
  group_by(movieId) %>% 
    left_join(movie_effect, by = "movieId") %>% ungroup() %>%
  group_by(userId) %>% 
    left_join(user_effect, by = "userId") %>% 
    left_join(fit_ur2, by = "userId") %>%
    left_join(user_first, by = "userId") %>% ungroup() %>%
  mutate(days_since = time_length(timestamp - user_first_day, "days"),
         beta = ifelse(is.na(slope),
                       0,
                       ifelse(days_since < 0,
                              intercept - slope*sqrt(abs(days_since)),
                              intercept + slope*sqrt(abs(days_since)))),
         prediction = mu + bi + bu + beta) %>%
  pull(prediction) %>%
  RMSE(test$rating)

# The RMSE is 1.587049, which is much worse than the baseline.
#   This means that adding the (intercept + slope*sqrt(days_since)) variable,
#   i.e. the effect of the number of days since the user's first rating,
#   greatly increases the error. 
#
# Note that using the rounded version (whole days_since) results in a smaller
# RMSE of 1.229355, which is still worse than any previous model.


# What does this mean?
#   The number of days since a user's first rating does not predict
#   how the user will rate a certain movie.
#
#   The effect we are trying to capture is whether a user rates their earlier
#   movies differently than their later movies; i.e., we are trying to capture
#   user behavior over time.

train %>% arrange(timestamp) %>%
  group_by(movieId) %>%
    left_join(movie_effect, by = "movieId") %>% ungroup() %>%
  group_by(userId) %>%
    left_join(user_effect, by = "userId") %>%
    left_join(user_first, by = "userId") %>%
    mutate(days_since = round(time_length(timestamp - user_first_day, "days")),
           y = rating - mu - bi - bu) %>% ungroup() %>%
  group_by(days_since) %>%
    summarize(average = mean(y)) %>%
  ggplot(aes(days_since, average)) + geom_line() +
    geom_smooth()

# Taking the average across all users, we see that there isn't a significant
# trend between the (residual) ratings and 'days_since', besides the fact
# that variability increases greatly the more days pass/time goes on.

# Can we determine a cut-off point to tweak our model that would improve
# the prediction up to (or after) a certain number of 'days_since'?

cutoffs <- seq(1000, 4000, 250)

RMSEs <- sapply(cutoffs, function(cutoff){
  
  test %>% arrange(timestamp) %>%
  group_by(movieId) %>% 
  left_join(movie_effect, by = "movieId") %>% ungroup() %>%
  group_by(userId) %>% 
  left_join(user_effect, by = "userId") %>% 
  left_join(fit_ur, by = "userId") %>%
  left_join(user_first, by = "userId") %>% ungroup() %>%
  mutate(days_since = time_length(timestamp - user_first_day, "days"),
         beta = ifelse(days_since >= cutoff,
                       ifelse(is.na(slope),
                              0,
                              ifelse(days_since < 0,
                                     intercept - slope*sqrt(abs(days_since)),
                                     intercept + slope*sqrt(abs(days_since)))),
                       0),
         prediction = mu + bi + bu + beta) %>% 
  pull(prediction) %>%
  RMSE(test$rating)

})

# The RMSE still does not improve if we exclude shorter days.


# POST-ANALYSIS: Even with cut-offs on both ends, the best RMSE is still
#   worse than not having one at all.

rm(user_first, dat, cutoffs, RMSEs, fit_ur, fit_ur2)


##########################
#
# (2) DAYS SINCE MOVIE'S FIRST RATING
#
#########################

# We now know that a user does not rate a movie based on the number of days
# since their first rating.
#
# Another movie-user-specific interaction we can look at is whether the user's
# rating is related to the number of days since a particular movie's first rating.
# This models the behavior of users who may rate the movie along with most other
# users (new releases) or after a certain period.

movie_first <- train %>% arrange(timestamp) %>%
  group_by(movieId) %>% filter(seq(n()) == 1) %>%
  mutate(movie_first_day = timestamp) %>%
  select(movieId, movie_first_day)

dat <- train %>% 
  arrange(timestamp) %>%
  group_by(movieId) %>% 
    left_join(movie_effect, by = "movieId") %>% 
    left_join(movie_first, by = "movieId") %>% ungroup() %>%
  group_by(userId) %>% 
    left_join(user_effect, by = "userId") %>%
    ungroup() %>%
  mutate(days_since = round(time_length(timestamp - movie_first_day, "days")),
         y = rating - mu - bi - bu)

# Use a simple linear regression model to find a linear relationship
# between a user's (residual) rating and the square-root of the number of days 
# since the movie's first given rating

fit <- dat %>% group_by(userId) %>%
  summarize(intercept = lm(y ~ sqrt(days_since))$coefficients[1],
            slope = lm(y ~ sqrt(days_since))$coefficients[2])
  
# In this instance, 'NA' slopes occur when a user had rated everything all at once.
fit %>% filter(is.na(slope))
train %>% filter(userId == 40516)
  # Once again, we will ignore the effects of users with NA slopes, since
  # it does not provide meaningful information


# Testing the model
test %>% 
  group_by(movieId) %>% 
    left_join(movie_effect, by = "movieId") %>%
    left_join(movie_first, by = "movieId") %>% ungroup() %>%
  group_by(userId) %>% 
    left_join(user_effect, by = "userId") %>% 
    left_join(fit, by = "userId") %>% ungroup() %>%
  mutate(days_since = time_length(timestamp - movie_first_day, "days"),
         beta = ifelse(is.na(slope),
                     0,
                     ifelse(days_since < 0, # It is possible for a test-set observation to occur
                            # prior to the training set movie's 'first_day', in which case the sign
                            # would be reversed.
                            intercept - slope*sqrt(abs(days_since)),
                            intercept + slope*sqrt(abs(days_since)))),
         prediction = mu + bi + bu + beta) %>%
  pull(prediction) %>%
  RMSE(test$rating)

# RMSE 0.8716986, which is better than the baseline guess but still worse than 
# the Regularized Movie & User Effects model, which means that we are causing
# more errors to occur by including this movie-user-specific interaction.

# Can we improve this model?

dat %>%
  mutate(days_since = round(days_since)) %>%
  group_by(days_since) %>%
    summarize(average = mean(y),
              sd = sd(y)) %>%
  gather(key = "type", value = "value", c(average, sd)) %>%
  ggplot(aes(days_since, value)) +
    geom_point() +
    geom_smooth() +
  facet_wrap( ~ type) 

  # Looking at a plot of the data, we notice that there doesn't seem to be a relationship
  # between a user's (residual) ratings and the number of days since the particular
  # movie's first rating until that 'days_since' value reaches around 4,000.
  #
  # After the 4,000th day, the average tends to be higher. Can we set a cut-off
  # value to further minimize the RMSE?

cutoffs <- seq(1000, 5000, 500)

RMSEs <- sapply(cutoffs, function(cutoff){
  
  test %>% 
    group_by(movieId) %>% 
      left_join(movie_effect, by = "movieId") %>%
      left_join(movie_first, by = "movieId") %>% ungroup() %>%
    group_by(userId) %>% 
      left_join(user_effect, by = "userId") %>% 
      left_join(fit, by = "userId") %>% ungroup() %>%
    mutate(days_since = time_length(timestamp - movie_first_day, "days"),
           beta = ifelse(days_since >= cutoff,
                         ifelse(is.na(slope),
                                0,
                                ifelse(days_since < 0,
                                       intercept - slope*sqrt(abs(days_since)),
                                       intercept + slope*sqrt(abs(days_since)))),
                         0),
           prediction = mu + bi + bu + beta) %>%
    pull(prediction) %>%
    RMSE(test$rating)
  
})

plot(RMSEs)
cutoffs[which.min(RMSEs)]
min(RMSEs)

# It appears that, with a cut-off of 2,500 days since a movie's first rating,
# we can achieve a better RMSE of 0.8560951.

# How is this interpreted?
#
#   The relationship between a user's rating and the number of days since
#   the movie's first rating only matters when the user is rating a movie
#   > 2,500 days (6.8 years) after the movie was first rated.


cutoff <- cutoffs[which.min(RMSEs)]

prediction <- test %>% 
  group_by(movieId) %>% 
  left_join(movie_effect, by = "movieId") %>%
  left_join(movie_first, by = "movieId") %>% ungroup() %>%
  group_by(userId) %>% 
  left_join(user_effect, by = "userId") %>% 
  left_join(fit, by = "userId") %>% ungroup() %>%
  mutate(days_since = time_length(timestamp - movie_first_day, "days"),
         beta = ifelse(days_since >= cutoff,
                       ifelse(is.na(slope),
                              0,
                              ifelse(days_since < 0, # It is possible for a test-set observation to occur
                                     # prior to the training set movie's 'first_day', in which case the sign
                                     # would be reversed.
                                     intercept - slope*sqrt(abs(days_since)),
                                     intercept + slope*sqrt(abs(days_since)))),
                       0),
         prediction = mu + bi + bu + beta) %>%
  pull(prediction)


RMSE(prediction, test$rating)

results <- rbind(results,
                 data.frame(method = "Movie's First Day",
                            RMSE = RMSE(prediction, test$rating)))

results %>%
  mutate(improvement = c(0, as.numeric(sprintf("%.7f", diff(as.matrix(results$RMSE))))))

rm(dat, cutoffs, prediction)



# ************* 
# DAYS SINCE (CAPPED)
# ************

# As we are continuously adding terms to the equation, it is likely that our
# prediction value will reach > 5, which is the maximum rating possible.
#
# To reduce the error, we will assume that if the prediction hits the cap,
# the rating given is 5.

prediction <- test %>% 
  group_by(movieId) %>% 
  left_join(movie_effect, by = "movieId") %>%
  left_join(movie_first, by = "movieId") %>% ungroup() %>%
  group_by(userId) %>% 
  left_join(user_effect, by = "userId") %>% 
  left_join(fit, by = "userId") %>% ungroup() %>%
  mutate(days_since = time_length(timestamp - movie_first_day, "days"),
         beta = ifelse(days_since >= cutoff,
                       ifelse(is.na(slope),
                              0,
                              intercept + slope*sqrt(abs(days_since))),
                       0),
         prediction = mu + bi + bu + beta) %>%
  mutate(prediction = ifelse(prediction > 5, 5, prediction)) %>%
  pull(prediction)

RMSE(prediction, test$rating)

results <- rbind(results,
                 data.frame(method = "Movie's First Day (Capped)",
                            RMSE = RMSE(prediction, test$rating)))

results %>%
  mutate(improvement = c(0, as.numeric(sprintf("%.7f", diff(as.matrix(results$RMSE))))))

#######################
#
# (3) MOVIE'S NUMBER OF RATINGS
#
#######################

# We saw an improvement in the RMSE when modelling the relationship between
# a user's rating and whether it has been 2,500 or more days since the movie's
# first rating.

# Another movie-user-specific interaction is whether a user's rating is affected 
# by the number of users who have already rated a particular movie. Each 
# number of user-ratings is unique to the movie and user combination.

#   This models the behavior of users who either set or follow trends.

# The variable n_ratings tallies how many users have already rated movie i 
# at the time of rating, excluding the user themselves.

#   If we can assume that every user only rated a movie once and did not
#   repeat any ratings, then we can say that the number of ratings is
#   equal to the number of users who have rated the movie.

train %>% group_by(userId) %>%
  summarize(dupes = sum(duplicated(movieId))) %>%
  pull(dupes) %>% sum()
  # There are 0 duplicated movieIds, meaning every user rated their movies
  # only once.

dat <- train %>% arrange(timestamp) %>%
  group_by(movieId) %>%
    mutate(n_userratings = seq(n()) - 1) %>% 
    left_join(movie_effect, by = "movieId") %>% 
    left_join(movie_first, by = "movieId") %>%
  group_by(userId) %>% 
    left_join(user_effect, by = "userId") %>% 
    left_join(fit, by = "userId") %>% ungroup() %>%
  mutate(days_since = time_length(timestamp - movie_first_day, "days"),
         bdi = ifelse(days_since >= cutoff,
                      intercept + slope*sqrt(days_since),
                      0),
         y = rating - mu - bi - bu - bdi) %>%
  select(-genres, -year, -movie_first_day, -intercept, -slope, -days_since)
  
# We group the data by user and use a simple regression model to find a
# linear relationship between a user's rating and the number of users who
# have already rated the movie they are rating.

fit_nu <- dat %>% group_by(userId) %>%
  summarize(intercept_nu = lm(y ~ sqrt(n_userratings))$coefficients[1],
            slope_nu = lm(y ~ sqrt(n_userratings))$coefficients[2])

sum(is.na(fit_nu)) # There are no NAs in the fit

# Testing the model

test %>% arrange(timestamp) %>%
  group_by(movieId) %>% 
    left_join(movie_effect, by = "movieId") %>% 
    left_join(movie_first, by = "movieId") %>% 
      mutate(days_since = time_length(timestamp - movie_first_day, "days"),
             n_userratings = seq(n()) - 1) %>% 
    ungroup() %>%
  group_by(userId) %>% 
    left_join(user_effect, by = "userId") %>%
    left_join(fit, by = "userId") %>%
      mutate(bdi = ifelse(is.na(slope),
                          0,
                          ifelse(days_since < 0,
                                 intercept + slope*(-sqrt(abs(days_since))),
                                 intercept + slope*sqrt(abs(days_since))))) %>%
    left_join(fit_nu, by = "userId") %>% 
      mutate(bnu = intercept_nu + slope_nu*sqrt(n_userratings)) %>%
    ungroup() %>%
  mutate(prediction = mu + bi + bu + bdi + bnu) %>%
  mutate(prediction = ifelse(prediction >= 5, 5, prediction)) %>%
  select(-genres, -year, -movie_first_day, -intercept, -slope, -days_since, -intercept_nu, -slope_nu) %>%
  pull(prediction) %>%
  RMSE(test$rating)
  # RMSE = 1.27498 (RMSE capped = 1.261212) 

# The resulting RMSE is once again worse than the baseline guessin model.

# Can we tune the n_userratings model? In the previous days_since
# model, we determined a cut-off point. 
#
#   Intuitively, we would think that a user would be influenced by the 
#   number of ratings a movie already has in certain occasions:
#   (1) if that number is very high, meaning the user likes to put their 2 cents in on a popular topic, or 
#   (2) if that number if very low, meaning the user tends to be the first to make a review. 

dat %>% group_by(n_userratings) %>%
  summarize(average = mean(y)) %>%
  ggplot(aes(n_userratings, average)) + geom_smooth(method = "lm")

  # Generally (across all movie-user combinations) we see a positive
  # relationship between the number of days and the average rating,
  # but a NEGATIVE relationship when compared to the residuals.

dat %>% group_by(n_userratings) %>%
  summarize(average = mean(y),
            sd = sd(y)) %>%
  gather(key = "type", value = "value", c(average, sd)) %>%
  ggplot(aes(n_userratings, value)) + 
  geom_point() + 
  facet_wrap(~type)

  # As the number of ratings a movie has increases, the rating given becomes
  # more spread out. 
  #
  # Can we determine a value up to which fit_nu makes a good prediction?

test %>% group_by(movieId) %>%
  mutate(n_userratings = seq(n())) %>%
  pull(n_userratings) %>% max()

  # The max n_ratings in the test set is 6331.

cutoffs_nu <- seq(0, 7000, 500) 

RMSEs <- sapply(cutoffs_nu, function(cutoff_nu){
  
  test %>% 
    group_by(movieId) %>% 
      left_join(movie_effect, by = "movieId") %>%
      left_join(movie_first, by = "movieId") %>% 
      mutate(n_userratings = seq(n()) - 1,
             days_since = time_length(timestamp - movie_first_day, "days")) %>%
    ungroup() %>%
    group_by(userId) %>% 
      left_join(user_effect, by = "userId") %>% 
      left_join(fit, by = "userId") %>% 
        mutate(beta = ifelse(days_since >= cutoff,
                             ifelse(is.na(slope),
                                    0,
                                    intercept + slope*sqrt(abs(days_since))),
                             0)) %>%
        select(-intercept, -slope) %>%
      left_join(fit_nu, by = "userId") %>%
        mutate(beta2 = ifelse(n_userratings <= cutoff_nu,
                              intercept_nu + slope_nu*sqrt(n_userratings),
                              0)) %>%
        select(-intercept_nu, -slope_nu) %>%
    ungroup() %>%
      mutate(prediction = mu + bi + bu + beta + beta2) %>%
      mutate(prediction = ifelse(prediction >= 5, 5, prediction)) %>% 
    #select(-genres, -year, -movie_first_day, -days_since) %>% arrange(desc(beta2))
    pull(prediction) %>%
    RMSE(test$rating)
  
})


# However, even with changing the limits, introducing the model only makes the RMSE worse.

rm(dat, cutoffs_nu, fit_nu, RMSEs)

######################
#
# (4) MOVIE'S CURRENT AVERAGE
#
######################

# Another user-movie-interaction we could look at is whether the user
# is affected by the current overall rating of the movie they are about
# to review.

# How do we calculate this variable?
dat <- train %>% 
  group_by(movieId) %>%
    mutate(n = seq(n()),
           cum_avg = cumsum(rating)/n,
           curr_avg = ifelse(n > 1, 
                             ((n*cum_avg) - rating)/(n -1),
                             NA)) %>%
    left_join(movie_effect, by = "movieId") %>% 
    left_join(movie_first, by = "movieId") %>%
      mutate(days_since = time_length(timestamp - movie_first_day, "days")) %>%
    select(-n, -cum_avg, -movie_first_day) %>%
  ungroup() %>%
  group_by(userId) %>%
    left_join(user_effect, by = "userId") %>%
    left_join(fit, by = "userId") %>%
    mutate(beta = ifelse(days_since >= cutoff,
                         intercept + slope*sqrt(abs(days_since)),
                         0)) %>%
    select(-intercept, -slope) %>%
  ungroup() %>%
  mutate(y = rating - mu - bi - bu - beta) %>%
  select(-genres, -year)

# Should we ignore the first rating given?
#   For the moment, yes, since we are concerned about whether the user
#   is affected by the current rating.
#
#   Whether the user rates a certain way if they are the first is more of
#   a user-specific interaction.
      
fit_ca <- dat %>% group_by(userId) %>% filter(!is.na(curr_avg)) %>%
  summarize(intercept = lm(y ~ curr_avg)$coefficients[1],
            slope = lm(y ~ curr_avg)$coefficients[2])

# Only one has 'NA' slope, userId == 3
fit_ca %>% filter(is.na(slope))
dat %>% filter(userId == 3)
  # who was often the first person to rate certain movies. (There may also be 
  # a relationship between a user's rating and being the first to rate.)
  # In which case, we can ignore the single data point.

fit_ca <- fit_ca %>% filter(!is.na(slope))

# Test the model and RMSE

test %>% 
  group_by(movieId) %>% 
    left_join(movie_effect, by = "movieId") %>%
    left_join(movie_first, by = "movieId") %>% 
      mutate(n = seq(n()),
             cum_avg = cumsum(rating)/n,
             curr_avg = ifelse(n > 1,
                               ((n*cum_avg) - rating)/(n - 1),
                               NA),
             days_since = time_length(timestamp - movie_first_day, "days")) %>%
      select(-n, -cum_avg, -movie_first_day) %>%
  ungroup() %>%
  group_by(userId) %>% 
    left_join(user_effect, by = "userId") %>% 
    left_join(fit, by = "userId") %>% 
      mutate(beta = ifelse(days_since >= cutoff,
                           intercept + slope*sqrt(abs(days_since)),
                           0)) %>%
      select(-intercept, -slope) %>%
    left_join(fit_ca, by = "userId") %>%
      mutate(beta2 = ifelse(is.na(curr_avg),
                            0,
                            intercept + slope*curr_avg)) %>%
      select(-intercept, -slope) %>%
  ungroup() %>%
  mutate(prediction = mu + bi + bu + beta + beta2) %>%
  #mutate(prediction = ifelse(prediction >= 5, 5, prediction)) %>%
  pull(prediction) %>%
  RMSE(test$rating)

# The resulting RMSE is 0.8539223, which is an improvement on the previous
# model without capping the prediction at 5. 
#
# Does this mean we can see further improvement by tuning the parameters?

dat %>% filter(!is.na(curr_avg)) %>% 
  group_by(curr_avg) %>%
    summarize(y_hat = mean(y)) %>%
  ggplot(aes(curr_avg, y_hat)) + geom_smooth(method = "lm")

  # We see a positive relationship between the current averages and the
  # effect on the rating.
  #
  # What parameters are there to tune?

# There are users who have are 'first-raters' and have many 'NA' curr_avg.
dat %>% group_by(userId) %>%
  filter(is.na(curr_avg)) %>% 
  summarize(n = n()) %>%
  arrange(n) %>%
  filter(n >= 25)

dat %>% filter(is.na(curr_avg)) %>% group_by(userId) %>%
  summarize(y_hat = mean(y),
            n = n()) %>% # n is the number of times the user has first-rated
  filter(n < 250 & n >= 25) %>% # filter out the outliers
  ggplot(aes(n, y_hat)) + geom_point() + 
    geom_smooth(method = "lm")

  # It appears as if there is no relationship between how often a user is 
  # a first-rater and the (residual) rating they give on average.

  # In which case, we are correct in setting beta2 = 0 for test-set users
  # giving their first rating.

# What other parameter can we look at?
#   Does the actual current average value matter?
#   i.e. is the user influenced by the curr_avg only if it is high/low?

dat %>% 
  ggplot(aes(curr_avg, rating)) + geom_point() +
    geom_hline(yintercept = 0, color = "red") + 
    geom_vline(xintercept = 0, color = "red")

cutoffs_ca <- seq(0, 200, 25)

RMSEs <- sapply(cutoffs_ca, function(cutoff_ca){
  
  test %>% 
    group_by(movieId) %>% 
      left_join(movie_effect, by = "movieId") %>%
      left_join(movie_first, by = "movieId") %>% 
        mutate(n = seq(n()),
               cum_avg = cumsum(rating)/n,
               curr_avg = ifelse(n > 1,
                                 ((n*cum_avg) - rating)/(n - 1),
                                 NA),
               days_since = time_length(timestamp - movie_first_day, "days")) %>%
        select(-n, -cum_avg, -movie_first_day) %>%
    ungroup() %>%
    group_by(userId) %>% 
      left_join(user_effect, by = "userId") %>% 
      left_join(fit, by = "userId") %>% 
        mutate(beta = ifelse(days_since >= cutoff,
                             intercept + slope*sqrt(abs(days_since)),
                             0)) %>%
        select(-intercept, -slope) %>%
      left_join(fit_ca, by = "userId") %>%
        mutate(beta2 = ifelse(is.na(curr_avg),
                              0,
                              intercept + slope*curr_avg)) %>%
        select(-intercept, -slope) %>%
    ungroup() %>%
    mutate(prediction = mu + bi + bu + beta + beta2) %>%
    #mutate(prediction = ifelse(prediction >= 5, 5, prediction)) %>%
    pull(prediction) %>%
    RMSE(test$rating)
  
})
