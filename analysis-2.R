##################
#
# OTHER SOURCES OF VARIATION FROM THE BASELINE
#
#################

# We have isolated movie-specific effects and user-specific effects,
# taking into account the distribution of ratings and penalizing
# estimates that come from small sample sizes.

# What else can we do to improve our model?
#
# Currently, we predict a rating as a function of:
#   (1) a baseline rating
#   (2) a movie-specific effect
#   (3) a user-specific effect

# But in reality, movies are not rated in a vacuum. Their ratings are
# implicitly affected by a variety of factors, for instance:
#
#   (1) Similar groups of movies may receive similar ratings;  
#
#   (2) Similar groups of users  may tend to rate movies in a similar way;
#   

# How can we prove these implicit effects?

###################
#
# EXPLORING GENRES
#
###################

# For (1), a "particular type of movie" can mean its genre. In other words, are
# movies of a certain genre rated similarly AND significantly different
# (lower or higher) than other genres?
#
# Does the data support this hypothesis?

# First, let's get an idea of what the genre data look like:

length(unique(train$genres))
# There are a total of 796 genre combinations

genre_list <- train %>% filter(!str_detect(genres, "\\|")) %>% select(genres) %>% unique() %>%
  pull(genres)
# made up of 19 distinct genre classifications

# Are certain genres rated more than others?
genre_counts <- sapply(genre_list, function(genre){
  train %>% filter(str_detect(genres, genre)) %>%
    summarize(n = n()) %>%
    pull(n)
})

dat <- data.frame(genre = genre_list,
                  n = genre_counts,
                  row.names = NULL)

dat %>% ggplot(aes(reorder(genre, n), n)) + geom_col(color = "black") +
  scale_y_continuous(labels = scales::comma) + 
  theme(axis.text.x = element_text(angle = 45)) +
  geom_text(label = dat$n,
            position = position_stack(vjust = 1),
            vjust = -0.5,
            size = 2.5) +
  ggtitle("Number of Ratings per Genre (Total)") +
  xlab("Genre")

#
# DOES THE NUMBER OF GENRES MATTER?
#
# We notice that a lot of movies are classified as combinations of genres.
train %>% filter(str_detect(genres, "\\|")) %>% nrow() / nrow(train)
# 81% of the ratings are of movies with more than 1 genre.

train %>% mutate(n_genres = str_count(genres, "\\|") + 1) %>%
  group_by(n_genres) %>% summarize(n = n()) %>% mutate(prop = n/sum(n))
# Almost 95% of the ratings are of movies with 4 or fewer genres,
# with 5% having 5 genres, and less than 1% have 6 or more.

# Can we ignore the ratings of 5% of the data without significantly
# affecting our analysis?
#
# Let's see whether having more or fewer genres affects the ratings of movies.

train %>% mutate(n_genres = str_count(genres, "\\|") + 1) %>%
  group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
  group_by(userId) %>% left_join(user_effect, by = "userId") %>% ungroup() %>%
  group_by(n_genres) %>% mutate(n_genre_effect = rating - movie_effect - user_effect) %>%
  summarize(n_genre_effect = mean(n_genre_effect - mu))
# It doesn't seem as though the number of genres affects the rating, after
# taking into account the movie-specific and user-specific effects.

# Therefore, we can safely ignore the effects of movies with 5 or more genres,
# as they represent a small subset of our data.


# Now, let's take a look at the genres and genre combinations.
dat <- train %>% filter(str_count(genres, "\\|") + 1 <= 4) %>%
  group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
  group_by(userId) %>% left_join(user_effect, by = "userId") %>% ungroup() %>%
  mutate(genre_effect = rating - movie_effect - user_effect) %>% 
  group_by(genres) %>%
  summarize(n = n(), genre_effect = mean(genre_effect - mu)) %>% mutate(n_genres = str_count(genres, "\\|") + 1)

p <- dat %>% ggplot(aes(n, genre_effect, col = factor(n_genres))) + 
  geom_point(size = 2) +
  scale_x_log10(labels = scales::comma) + 
  scale_y_continuous() +
  ggtitle("Are Genres Rated Differently?") +
  xlab("Number of ratings (log10)") +
  ylab("Average Distance from Baseline") +
  labs(col = "# of Genres") +
  scale_color_brewer(palette = "Set1")

# Where do the individual genres stand?
p + geom_label_repel(data = dat %>% filter(n_genres == 1), 
                     aes(label = genres),
                     color = "black",
                     show.legend = FALSE,
                     max.overlaps = 20,
                     box.padding = 1,
                     point.padding = unit(0.5, "lines"),
                     nudge_y = 0.1)

# Which genres negatively affect ratings the most?
p + geom_label_repel(data = dat %>% filter(abs(genre_effect) >= 0.5),
                     aes(label = genres),
                     color = "black",
                     show.legend = FALSE,
                     box.padding = 1,
                     point.padding = 1,
                     direction = "both")

# Some genres with relatively few number of ratings contribute some effect
# on ratings, though most don't. However, the number of genres definitely
# does not seem to matter.


# What if we don't consider the specific movie and user biases, and just
# focus on the genre's effect above the baseline?
# train %>% filter(str_count(genres, "\\|") + 1 <= 4) %>%
#   group_by(genres) %>%
#   summarize(n = n(), genre_effect = mean(rating - mu)) %>%
#   mutate(n_genres = str_count(genres, "\\|") + 1) %>%
#   ggplot(aes(n, genre_effect, col = factor(n_genres))) + geom_point(size = 2) +
#     scale_x_log10(labels = scales::comma) + 
#     scale_y_continuous() +
#     ggtitle("Are Genres Rated Differently?") +
#     xlab("Number of ratings (log10)") +
#     ylab("Average Distance from Baseline") +
#     labs(col = "# of Genres") +
#     scale_color_brewer(palette = "Set1") +
#   geom_label_repel(data = . %>% filter(n_genres == 1), 
#                    aes(label = genres),
#                    color = "black",
#                    show.legend = FALSE,
#                    max.overlaps = 20,
#                    box.padding = 1,
#                    point.padding = unit(0.5, "lines"),
#                    nudge_y = 0.1)
# Based on genre alone, there doesn't seem to be that much difference.
rm(dat, p, shortlist)


###########################
#
# MODEL 5: GENRE EFFECTS MODEL
#
###########################

# Does incorporating a simple genre bias (regularized) improve the model?

lambdas <- seq(1, 10, 0.5)

RMSEs <- sapply(lambdas, function(lambda){
  
  genre_effect <- train %>%
    group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
    group_by(userId) %>% left_join(user_effect, by = "userId") %>% ungroup() %>%
    group_by(genres) %>%
    summarize(genre_effect = sum(rating - (mu + movie_effect + user_effect))/(lambda + n()))
  
  prediction <- test %>%
    group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
    group_by(userId) %>% left_join(user_effect, by = "userId") %>% ungroup() %>%
    group_by(genres) %>% left_join(genre_effect, by = "genres") %>%
    mutate(prediction = mu + movie_effect + user_effect + genre_effect) %>%
    pull(prediction) 
  
  return(RMSE(test$rating, prediction))
  
})

lambdas[which.min(RMSEs)]
# The lambda value that gives us the minimal RMSE is lambda = 1, which is
# at risk of over-fitting. Regardless, we will use it.

genre_effect <- train %>%
  group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
  group_by(userId) %>% left_join(user_effect, by = "userId") %>% ungroup() %>%
  group_by(genres) %>%
  summarize(genre_effect = sum(rating - (mu + movie_effect + user_effect))/(1 + n()))

prediction <- test %>%
  group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
  group_by(userId) %>% left_join(user_effect, by = "userId") %>% ungroup() %>%
  group_by(genres) %>% left_join(genre_effect, by = "genres") %>%
  mutate(prediction = mu + movie_effect + user_effect + genre_effect) %>%
  pull(prediction)

results <- rbind(results, data.frame(method = "Regularized Genre Effect",
                                     RMSE = RMSE(test$rating, prediction)))
results

rm(prediction, lambdas, genre_counts, genre_list, RMSEs)



#####################
#
# EXPLORING TIME
#
####################

names(train)

# Looking at our data, we find that we have temporal characteristics that
# may implicitly affect how ratings are given --
#
#   (1) the date/time of the rating, and
#   (2) the year a movie was released


# Let's explore (1): information regarding when the ratings were made.
#
#   Question: Does the data show a significant pattern in ratings over a period
#             of time?

# We make use of the lubridate package to parse the timestamp variable
library(lubridate)

# According to the documentation, 'timestamp' represents the seconds since
# midnight on January 1, 1970 (UTC). Lubridate has a function called
# as_datetime() that can convert the variable to a more readable format.

# Question: Are movies rated differently over time?
train %>% mutate(y_rated = year(as_datetime(timestamp))) %>% 
  group_by(y_rated) %>%
  summarize(n = n(), average_rating = mean(rating)) %>% 
  ggplot(aes(y_rated, average_rating)) + geom_line() +
  ggtitle("Are movies rated differently over the years?") +
  xlab("Year Rated") +
  ylab("Average Rating") +
  geom_hline(yintercept = mu, color = "red") +
  geom_text(aes(x = 1995, y = mu, label = "Baseline"), vjust = 1) +
  scale_x_continuous(breaks = seq(1995, 2008, 1))

# Observation: Movies rated between 2002 and 2007 were rated lower than
# the baseline average, while movies rated earlier were rated higher more
# often than not.


# Question: Are movies rated differently within the year?
train %>% mutate(m_rated = month(as_datetime(timestamp))) %>% 
  group_by(m_rated) %>%
  summarize(n = n(), average_rating = mean(rating)) %>%
  ggplot(aes(m_rated, average_rating)) + geom_line() +
  ggtitle("Are movies rated differently within the year?") +
  xlab("Month Rated") +
  ylab("Average Rating") +
  scale_size_continuous(labels = scales::comma) +
  geom_hline(yintercept = mu, color = "red") +
  geom_text(aes(x = 1, y = mu, label = "Baseline"), vjust = 1) +
  scale_x_continuous(breaks = seq(1, 12, 1))

# Observation 1: It seems like movies rated toward the end of the year are rated VERY SLIGHTLY higher
# on average, while movies rated between May to August (roughly summer season) 
# are rated lower on average.
#
# Note that the range on these values is from about 3.48 to 3.56, which is 
# not a huge variation. 

  ##################
  #
  # OSCAR BAIT VS SUMMER BLOCKBUSTERS?
  #   (movie-related seasonal effects)
  #
  ##################

# There is a phenomenon in the film industry of movies called "Oscar Bait"
# which are produced for the purpose of being nominated for the Academy Award.
# So that they are fresh in the voters' minds, these movies are usually released 
# just before awards season -- late in the calendar year.

# Could this phenomenon be driving the higher-than-average ratings during
# the last few months of the year?

# Question: What month are movies most often rated? 
library(lubridate)
train %>% mutate(m_rated = month(as_datetime(timestamp))) %>%
  ggplot(aes(factor(m_rated))) + geom_histogram(stat = "count", bins = 30, color = "black") +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("What month are movies most often rated?") +
  ylab("Number of ratings") +
  xlab("Month Rated")

# Observation: Oct-Dec do get the most ratings, while Feb & Sep get the fewest. 
#   The differences, however, are not that large.

# Question: Have the number of ratings increased over the years?

train %>% mutate(y_rated = year(as_datetime(timestamp))) %>%
  group_by(y_rated) %>%
  summarize(n = n()) %>%
  ggplot(aes(factor(y_rated), n)) +
  geom_bar(stat = "identity", bins = 30, color = "black") +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Number of Ratings Over Time") +
  xlab("Year Rated") +
  ylab("Number of Ratings")

# Observation 1: There does not seem to be a pattern in the number of 
# ratings made over the years.
#
# However, There were fewer ratings than average in 1998, and very few rated 
# in 1995 and 2009, while a lot rated in 1996, 2000, and 2005.

# Side-Question: Could this be user-driven? Compared to the overall number of ratings
# made, how has the number of unique users giving these ratings changed over time?

train %>% mutate(y_rated = year(as_datetime(timestamp))) %>%
  group_by(y_rated) %>%
  summarize(n_users = length(unique(userId))) %>%
  ggplot(aes(factor(y_rated), n_users)) + geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Number of Unique Users per Year") +
  xlab("Year Rated") +
  ylab("Number of Unique Users")

# Observation: There also does not seem to be a significant trend in the
# number of users over time.
#
# The year with the most number of unique users was 1996, possibly
# due to the launch of the program. After that, a sharp decline, and
# then a relatively stable number averaging around 6-7,000 users.


# Side-Question 2: How about the number of ratings per user? Has it changed
#   over time?

train %>% mutate(y_rated = year(as_datetime(timestamp))) %>%
  group_by(userId, y_rated) %>%
  summarize(n_ratings = n()) %>% ungroup() %>%
  group_by(y_rated) %>%
  summarize(avg_rpu = mean(n_ratings)) %>%
  ggplot(aes(factor(y_rated), avg_rpu)) + geom_bar(stat = "identity") +
  ggtitle("Average Number of Ratings per User per Year") +
  xlab("Year Rated") +
  ylab("Average Number of Ratings")

# Observation: The average number of ratings per user increased after 1998
# and remained relatively stable throughout. Data for 1995 and 2009 are
# expected to be small.
#
# 2005 had the highest # of ratings per user, which contributed to the year
# having the 2nd highest number of ratings.
#
# 2000 had the highest number of ratings, due to a larger number of unique
# users (2nd highest since 1996) making, on average, a lot of ratings as well.

###
#
# Conclusion: While the average number of ratings the average user makes increases
#     over time, the overall number of ratings does not increase over time. 
#
##

# Returning to the "Oscar Bait" and "Summer Blockbuster" phenomenon, 
# is it the case that movies rated in Oct-Dec are rated higher than average, and
# movies rated in May-Aug rated lower than average?

# Question; In general, are movies rated differently within the year for all years?
train %>% mutate(y_rated = year(as_datetime(timestamp)),
                 m_rated = month(as_datetime(timestamp))) %>% 
  group_by(m_rated, y_rated) %>%
  summarize(average_rating = mean(rating)) %>%
  ggplot(aes(m_rated, average_rating)) +
  geom_point() +
  geom_line(alpha = 0.5) + 
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  scale_y_continuous(breaks = seq(3, 4.5, 0.25)) +
  geom_hline(yintercept = mu, color = "Red") +
  ggtitle("Average Rating per Month across the Years") +
  xlab("Month Rated") +
  ylab("Average Rating per Month") +
  
  annotate(geom = "rect",
           xmin = 5,
           xmax = 8,
           ymin = -Inf,
           ymax = Inf,
           color = "transparent",
           fill = "yellow",
           alpha = 0.2) +
  
  annotate(geom = "rect",
           xmin = 10,
           xmax = 12,
           ymin = -Inf,
           ymax = Inf,
           fill = "blue",
           alpha = 0.2) +
  
  facet_wrap(~y_rated, nrow = 3)


##
#
# CONCLUSION: The overall trend of movies getting rated lower between 2002-2007
#         persists, but there does not seem to be a strong enough pattern 
#         showing that movies are rated differently within the year.
#
#      However, more often than not "blockbuster" movies are
#       rated lower than "Oscar bait" movies; though individually they do not
#       deviate significantly from the baseline.
#
#
#     But once again, the margins aren't that wide.
##


# Question: If a movie was rated between May to August, what is it's likely
# effect on the rating?

train %>% mutate(m_rated = month(as_datetime(timestamp))) %>%
  group_by(m_rated) %>%
  summarize(season_effect = sum(rating - mu)/(n())) %>%
  # regularization does improve results that much because there are 
  # no large estimates made from these effects
  filter(m_rated %in% seq(5,8,1)) %>%
  summarize(season_effect = mean(season_effect))

# It's a decrease of 0.02 on the rating, which is practically negligible.

# For argument's sake, let's say that the month the rating was given does
# affect the rating. Will it improve our RMSE?

RMSEs <- sapply(lambdas, function(lambda){
  
  movie_effect <- train %>%
    group_by(movieId) %>% 
    summarize(bi = sum(rating - mu)/(n() + lambda))
  
  user_effect <- train %>%
    group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
    group_by(userId) %>% 
    summarize(bu = sum(rating - mu - bi)/(n() + lambda))
  
  genre_effect <- train %>%
    group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
    group_by(userId) %>% left_join(user_effect, by = "userId") %>% ungroup() %>%
    group_by(genres) %>%
    summarize(bg = sum(rating - mu - bi - bu)/(n() + lambda))
  
  year_effect <- train %>%
    group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
    group_by(userId) %>% left_join(user_effect, by = "userId") %>% ungroup() %>%
    group_by(genres) %>% left_join(genre_effect, by = "genres") %>% ungroup() %>%
    mutate(m_rated = month(as_datetime(timestamp)),
           y_rated = year(as_datetime(timestamp))) %>% 
    group_by(m_rated) %>% left_join(month_effect, by = "m_rated") %>% ungroup() %>%
    group_by(y_rated) %>% 
    summarize(by = sum(rating - mu - bi - bu - bg)/(n() + lambda))
  
  test %>% 
    group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
    group_by(userId) %>% left_join(user_effect, by = "userId") %>% ungroup() %>%
    group_by(genres) %>% left_join(genre_effect, by = "genres") %>% ungroup() %>%
    mutate(m_rated = month(as_datetime(timestamp)),
           y_rated = year(as_datetime(timestamp))) %>%
    group_by(y_rated) %>% left_join(year_effect, by = "y_rated") %>%
    mutate(prediction = mu + bi + bu + bg + by) %>%
    pull(prediction) %>%
    RMSE(test$rating)
  
})

# A lambda  = 4.5 value results in the minimal RMSE of 0.8652323.


# It did not. In fact, it made it worse with an RMSE of 1.50.

##
#
# CONCLUSION: There is no reason to believe that there are seasonal effects on
#             ratings; i.e., ratings do not significantly differ across months.
#       
#   However, we do no know that ratings made between 2002 and 2007 tend to fall
#   below the baseline. This does not help in making future predictions,
#   but it may help in other analysis.
#
##

  #######
  #
  # OLD VS. NEW MOVIES
  #   (movie-related year effects)
  #
  #######

# Another phenomenon we can try to guess is whether older "classic" movies are rated
# better or worse than "modern" movies.
#   In this case, we will be using the release year column 'year'

train %>% group_by(year) %>%
  summarize(average_rating = sum(rating)/(n() + 5), n = n()) %>%
  ggplot(aes(year, average_rating)) +
  geom_point(aes(size = n)) +
  scale_x_discrete(breaks = seq(1915, 2008, 5)) +
  scale_size(labels = scales::comma,
             breaks = c(10000, 100000, 200000, 600000)) +
  geom_hline(yintercept = mu,
             color = "red") +
  geom_text(aes(x = min(year), y = mu, label = "Baseline"), vjust = 1, hjust = 0) +
  ggtitle("Are older movies rated differently?") +
  xlab("Release Year") +
  ylab("Average Rating of all Movies")

# Observation: The number of ratings increases the more recent a movie was
#   released. And there does seem to be a trend where older movies, up to 
#   the early 1980's, were rated above the baseline.
#
#   The exception are some of the oldest movies ever rated in the data set.

train %>% filter(year <= 1920) %>%
  group_by(title) %>%
  summarize(n = n(), average = mean(rating - mu)) %>% arrange(desc(average))

# Is there a relationship between the year released and rating?
#
#   What is the effect on RMSE if we include a "year effect"?
#   We can tune the best lambda, assuming the other effects already have
#   the optimal lambda values.

RMSEs <- sapply(lambdas, function(lambda){
  
  movie_effect <- train %>% group_by(movieId) %>% 
    summarize(bi = sum(rating - mu)/(n() + lambda))
  
  user_effect <- train %>% group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
    group_by(userId) %>%
    summarize(bu = sum(rating - mu - bi)/(n() + lambda))
  
  genre_effect <- train %>% group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
    group_by(userId) %>% left_join(user_effect, by = "userId") %>% ungroup() %>%
    group_by(genres) %>% 
    summarize(bg = sum(rating - mu - bi - bu)/(n() + lambda))
  
  year_effect <- train %>% group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
    group_by(userId) %>% left_join(user_effect, by = "userId") %>% ungroup() %>%
    group_by(genres) %>% left_join(genre_effect, by = "genres") %>% ungroup() %>%
    group_by(year) %>% 
    summarize(by = sum(rating - mu - bi - bu - bg)/(n() + lambda))
  
  test %>%
    group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
    group_by(userId) %>% left_join(user_effect, by = "userId") %>% ungroup() %>%
    group_by(genres) %>% left_join(genre_effect, by = "genres") %>% ungroup() %>%
    group_by(year) %>% left_join(year_effect, by = "year") %>%
    mutate(prediction = mu + bi + bu + bg + by) %>%
    pull(prediction) %>%
    RMSE(test$rating)
  
})

###########
#
# MODEL 6: RELEASE YEAR EFFECT
#
##########

# Regularizing all of them together, the new lambda value is 4.5, resulting
# in an RMSE of 0.8651156.

lambda <- 4.5

movie_effect <- train %>% group_by(movieId) %>% 
  summarize(bi = sum(rating - mu)/(n() + lambda))

user_effect <- train %>% group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
  group_by(userId) %>%
  summarize(bu = sum(rating - mu - bi)/(n() + lambda))

genre_effect <- train %>% group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
  group_by(userId) %>% left_join(user_effect, by = "userId") %>% ungroup() %>%
  group_by(genres) %>% 
  summarize(bg = sum(rating - mu - bi - bu)/(n() + lambda))

year_effect <- train %>% group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
  group_by(userId) %>% left_join(user_effect, by = "userId") %>% ungroup() %>%
  group_by(genres) %>% left_join(genre_effect, by = "genres") %>% ungroup() %>%
  group_by(year) %>% 
  summarize(by = sum(rating - mu - bi - bu - bg)/(n() + lambda))

results <- rbind(results,
                 data.frame(method = "Regularized Year Effect",
                            RMSE = min(RMSEs)))
results

# Our RMSE on the training set has improved by 0.0001.  


###############
#
# EXPLORING RATING BEHAVIOR OVER TIME
#     (user-specific temporal effects)
#
###############

# So far we've looked at each observation on its own, as a single rating given
# at a single point in time. 
#
# What if users form a pattern in their rating behavior that affected their
# previous ratings and will affect their future ratings?


# Let's define a variable to describe a given user's rating behavior.

# One such variable would be the number of ratings already given at the time
# of the new rating.

train %>% group_by(userId) %>% 
  mutate(count = seq(n())) %>% # the count represents the n-th rating given by the user
  ungroup() %>%
  group_by(count) %>%
  summarize(average_rating = mean(rating)) %>%
  ggplot(aes(count, average_rating)) +
  geom_line() +
  geom_smooth(method = "lm") + 
  xlab("Cumulative Number of Ratings") +
  ylab("Average Rating") +
  geom_hline(yintercept = mu, color = "red") +
  scale_x_continuous(breaks = seq(0, 5000, 500))

# Observation: The early reviews start out relatively high, and begin 
#   a descent until they stabilize at the 500th-1500th review.
#   The ratings then get more and more variable after the 2000th review.

# What does this tell us?
#
#   Maybe we can group the cumulative number of ratings into bins, OR use a 
#   regression model to determine the effect.

dat <- train %>% 
  group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
  group_by(userId) %>% left_join(user_effect, by = "userId") %>% ungroup() %>%
  group_by(genres) %>% left_join(genre_effect, by = "genres") %>% ungroup() %>%
  group_by(year) %>% left_join(year_effect, by = "year") %>% ungroup() %>%
  group_by(userId) %>% 
  mutate(count = seq(n())) %>% ungroup() %>%
  group_by(count) %>%
  summarize(count_effect = sum(rating - mu - bi - bu - bg - by)/(n() + lambda))

# Let's try using a regression model to fit the count effect to the count.
library(caret)
fit <- lm(data = dat,
          count_effect ~ count)

# Let's see if the prediction improves our RMSE

test %>% 
  group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
  group_by(userId) %>% left_join(user_effect, by = "userId") %>% ungroup() %>%
  group_by(genres) %>% left_join(genre_effect, by = "genres") %>% ungroup() %>%
  group_by(year) %>% left_join(year_effect, by = "year") %>% ungroup() %>%
  group_by(userId) %>%
  mutate(count = seq(n())) %>% ungroup() %>%
  mutate(bc = fit$coefficients[1] + fit$coefficients[2]*count) %>%
  mutate(prediction = mu + bi + bu + bg + by + bc) %>%
  pull(prediction) %>%
  RMSE(test$rating)

# It does not improve the model using lm, but maybe another regression method would.
# Neither does glm


# What about using each individual count effect, without grouping?

count_effect <- train %>% 
  group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
  group_by(userId) %>% left_join(user_effect, by = "userId") %>% ungroup() %>%
  group_by(genres) %>% left_join(genre_effect, by = "genres") %>% ungroup() %>%
  group_by(year) %>% left_join(year_effect, by = "year") %>% ungroup() %>%
  group_by(userId) %>%
  mutate(count = seq(n())) %>% ungroup() %>%
  group_by(count) %>%
  summarize(bc = sum(rating - mu - bi - bu - bg - by)/(n() + lambda))

test %>% 
  group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
  group_by(userId) %>% left_join(user_effect, by = "userId") %>% ungroup() %>%
  group_by(genres) %>% left_join(genre_effect, by = "genres") %>% ungroup() %>%
  group_by(year) %>% left_join(year_effect, by = "year") %>% ungroup() %>%
  group_by(userId) %>%
  mutate(count = seq(n())) %>% ungroup() %>%
  group_by(count) %>% left_join(count_effect, by = "count") %>% ungroup() %>%
  mutate(prediction = mu + bi + bu + bg + by + bc) %>%
  pull(prediction) %>%
  RMSE(test$rating)

# Even with using each count's individual effect, the RMSE does not improve.
#   RMSE = 0.8651442

# What if we only include that early stage, i.e. if the user has reviewed 
# fewer than X number of times, there is a bias toward a higher rating?
#     Is this true?

train %>%
  group_by(userId) %>%
  mutate(count = seq(n())) %>% ungroup() %>%
  group_by(count) %>%
  summarize(n_ratings = n(), average_rating = mean(rating), sd = sd(rating)) %>%
  filter(n_ratings >= 5) %>%
  ggplot(aes(count, sd)) + 
  geom_line() +
  geom_smooth()

# Observation: The deviation remains stable until around the 1000th rating given.
# They may be a case for only accounting for the first X number of ratings.

  # Note that we are calculating the effect of being the nth rating.
  # Does the effect of being the nth rating affect the (n+1)th rating?

count_effect <- train %>%
  group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
  group_by(userId) %>% left_join(user_effect, by = "userId") %>% ungroup() %>%
  group_by(genres) %>% left_join(genre_effect, by = "genres") %>% ungroup() %>%
  group_by(year) %>% left_join(year_effect, by = "year") %>% ungroup() %>%
  group_by(userId) %>%
  mutate(count = seq(n())) %>% ungroup() %>%
  group_by(count) %>%
  filter(count <= 1000) %>%
  summarize(bc = sum(rating - mu - bi - bu - bg - by)/(n() + lambda)) %>%
  pull(bc) %>% mean()


test %>% 
  group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
  group_by(userId) %>% left_join(user_effect, by = "userId") %>% ungroup() %>%
  group_by(genres) %>% left_join(genre_effect, by = "genres") %>% ungroup() %>%
  group_by(year) %>% left_join(year_effect, by = "year") %>% ungroup() %>%
  group_by(userId) %>%
  mutate(count = seq(n())) %>% ungroup() %>%
  group_by(count) %>% 
  mutate(bc = ifelse(count <= 1000, count_effect, 0)) %>%
  mutate(prediction = mu + bi + bu + bg + by + bc) %>%
  pull(prediction) %>%
  RMSE(test$rating)

rm(count_effect)

# There is a marginal improvement over using all the counts, but still not
# better than the model without it, which tells us that there may be a subset 
# that can improve the RMSE.
#   RMSE = 0.8651167

# What if we tune that cutoff number?

cutoffs <- c(5, 25, 50, 100, 250)

RMSEs <- sapply(cutoffs, function(cutoff){
  
  count_effect <- train %>%
    group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
    group_by(userId) %>% left_join(user_effect, by = "userId") %>% ungroup() %>%
    group_by(genres) %>% left_join(genre_effect, by = "genres") %>% ungroup() %>%
    group_by(year) %>% left_join(year_effect, by = "year") %>% ungroup() %>%
    group_by(userId) %>%
    mutate(count = seq(n())) %>% ungroup() %>%
    group_by(count) %>%
    filter(count <= cutoff) %>%
    summarize(bc = sum(rating - mu - bi - bu - bg - by)/(n() + lambda)) %>%
    pull(bc) %>% mean()
  
  
  test %>% 
    group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
    group_by(userId) %>% left_join(user_effect, by = "userId") %>% ungroup() %>%
    group_by(genres) %>% left_join(genre_effect, by = "genres") %>% ungroup() %>%
    group_by(year) %>% left_join(year_effect, by = "year") %>% ungroup() %>%
    group_by(userId) %>%
    mutate(count = seq(n())) %>% ungroup() %>%
    group_by(count) %>% 
    mutate(bc = ifelse(count <= cutoff, count_effect, 0)) %>%
    mutate(prediction = mu + bi + bu + bg + by + bc) %>%
    pull(prediction) %>%
    RMSE(test$rating)
  
})

# It appears that a cutoff of 250 will results in an RMSE that improves
# by 0.0000002. 

# Is it worth it to go to such lengths, or are we at risk of over-fitting?
# Let's see the combined model, with recalculated lambdas.

cutoff <- 250

lambdas <- seq(0, 10, 0.5)

RMSEs <- sapply(lambdas, function(lambda){
  
  movie_effect <- train %>%
    group_by(movieId) %>%
    summarize(bi = sum(rating - mu)/(n() + lambda))
  
  user_effect <- train %>%
    group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
    group_by(userId) %>%
    summarize(bu = sum(rating - mu - bi)/(n() + lambda))
  
  genre_effect <- train %>%
    group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
    group_by(userId) %>% left_join(user_effect, by = "userId") %>% ungroup() %>%
    group_by(genres) %>%
    summarize(bg = sum(rating - mu - bi - bu)/(n() + lambda))
  
  year_effect <- train %>%
    group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
    group_by(userId) %>% left_join(user_effect, by = "userId") %>% ungroup() %>%
    group_by(genres) %>% left_join(genre_effect, by = "genres") %>% ungroup() %>%
    group_by(year) %>%
    summarize(by = sum(rating - mu - bi - bu - bg)/(n() + lambda))
  
  count_effect <- train %>%
    group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
    group_by(userId) %>% left_join(user_effect, by = "userId") %>% ungroup() %>%
    group_by(genres) %>% left_join(genre_effect, by = "genres") %>% ungroup() %>%
    group_by(year) %>% left_join(year_effect, by = "year") %>% ungroup() %>%
    group_by(userId) %>%
    mutate(count = seq(n())) %>% ungroup() %>%
    group_by(count) %>%
    filter(count <= cutoff) %>%
    summarize(bc = sum(rating - mu - bi - bu - bg - by)/(n() + lambda)) %>%
    pull(bc) %>% mean()
  
  test %>% 
    group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
    group_by(userId) %>% left_join(user_effect, by = "userId") %>% ungroup() %>%
    group_by(genres) %>% left_join(genre_effect, by = "genres") %>% ungroup() %>%
    group_by(year) %>% left_join(year_effect, by = "year") %>% ungroup() %>%
    group_by(userId) %>%
    mutate(count = seq(n())) %>% ungroup() %>%
    group_by(count) %>% 
    mutate(bc = ifelse(count <= cutoff, count_effect, 0)) %>%
    mutate(prediction = mu + bi + bu + bg + by + bc) %>%
    pull(prediction) %>%
    RMSE(test$rating)
  
})


# With a cutoff at the 250th rating and lambda = 4.5, we get an RMSE of
# 0.8561153, which is 0.0000003 better than the previous model.

results <- rbind(results,
                 data.frame(method = "Reg. First 250 Effect",
                            RMSE = min(RMSEs)))
results

# We are noticing that our improvements are getting smaller and smaller.

results %>%
  mutate(improvement = c(0, as.numeric(sprintf("%.7f", diff(as.matrix(results$RMSE))))))

rm(cutoff, cutoffs, lambda, lambdas, RMSEs)

# Which may point to the fact that variation has already been
# accounted for in the first few terms.

# Does this mean we look further into the movie and user effects?



#####################
#
# OTHER MOVIE-SPECIFIC BIASES
#
######################

# We have explored whether the number of ratings a user has made had an effect on their
# next rating, and determined that there is an effect (very small) for only
# the first 250 ratings given.

# What if we explore this on the side of the movie?

train %>% group_by(movieId) %>%
  mutate(r_count = seq(n())) %>% ungroup() %>%
  group_by(r_count) %>%
    summarize(n_ratings = n(), average_rating = mean(rating)) %>%
  ggplot(aes(n_ratings, average_rating)) +
    geom_line() + 
    geom_hline(yintercept = mu, color = "red") +
  scale_x_continuous(breaks = seq(0, 10000, 1000))

  # Observation, we see that the average_rating varies more greatly for movies
  # with fewer than 500 ratings, and as the number of ratings increases, the 
  # average rating stabilizes.

train %>% group_by(movieId) %>%
  mutate(r_count = seq(n())) %>% ungroup() %>%
  group_by(r_count) %>%
  summarize(n_ratings = n(), sd_rating = sd(rating)) %>%
  ggplot(aes(n_ratings, sd_rating)) +
  geom_line() + 
  scale_x_continuous(breaks = seq(0, 10000, 1000))

  # Observation: There is a similar trend with the deviations. The first 
  # couple of hundred ratings have varying standard deviations, but
  # as the number gets higher the deviations stabilize.

# What does this tell us?
#
#   Intuitively, it makes sense. Movies with few ratings can cause significant
#   variation since we have a small sample size of observations.
#   This issue was rectified by regularizing movie-effects.
#
#   But let's see if we can find a similar "cutoff" value, like we did for
#   the users' number of ratings, to improve our RMSE.


  ######
  #
  # FIRST n MOVIE RATINGS
  #
  #####

cutoffs <- seq(0, 1000, 125)

lambda <- 4.5

RMSEs <- sapply(cutoffs, function(cutoff){
  
  rcount_effect <- train %>%
    group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
    group_by(userId) %>% left_join(user_effect, by = "userId") %>% ungroup() %>%
    group_by(genres) %>% left_join(genre_effect, by = "genres") %>% ungroup() %>%
    group_by(year) %>% left_join(year_effect, by = "year") %>% ungroup() %>%
    group_by(userId) %>% 
      mutate(count = seq(n())) %>%
      mutate(bc = ifelse(count <= 250, count_250, 0)) %>% ungroup() %>%
    group_by(movieId) %>%
      mutate(r_count = seq(n())) %>% ungroup() %>%
    group_by(r_count) %>%
    filter(r_count <= cutoff) %>%
    summarize(brc = sum(rating - mu - bi - bu - bg - by - bc)/(n() + lambda)) %>%
    pull(brc) %>% mean()
  
  test %>% 
    group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
    group_by(userId) %>% left_join(user_effect, by = "userId") %>% ungroup() %>%
    group_by(genres) %>% left_join(genre_effect, by = "genres") %>% ungroup() %>%
    group_by(year) %>% left_join(year_effect, by = "year") %>% ungroup() %>%
    group_by(userId) %>%
      mutate(count = seq(n()), bc = ifelse(count <= 250, count_250, 0)) %>% ungroup() %>%
    group_by(movieId) %>%
      mutate(r_count = seq(n())) %>%
    group_by(r_count) %>% 
      mutate(brc = ifelse(r_count <= cutoff, rcount_effect, 0)) %>% ungroup() %>%
    mutate(prediction = mu + bi + bu + bg + by + bc + brc) %>%
    pull(prediction) %>%
    RMSE(test$rating)
  
})

# A cutoff of 125 is deemed optimal, yielding an RMSE of 0.8651142, which is
# an improvement of 0.0000011. It's more of an improvement over the 
# user-side # of ratings effect.

nu_cutoff <- 250
ni_cutoff <- 125

# We may be onto something here. Let's regularize:

lambdas <- seq(0, 10, 0.5)

RMSEs <- sapply(lambdas, function(lambda){
  
  movie_effect <- train %>%
    group_by(movieId) %>%
    summarize(bi = sum(rating - mu)/(n() + lambda))
  
  user_effect <- train %>%
    group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
    group_by(userId) %>%
    summarize(bu = sum(rating - mu - bi)/(n() + lambda))
  
  genre_effect <- train %>%
    group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
    group_by(userId) %>% left_join(user_effect, by = "userId") %>% ungroup() %>%
    group_by(genres) %>%
    summarize(bg = sum(rating - mu - bi - bu)/(n() + lambda))
  
  year_effect <- train %>%
    group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
    group_by(userId) %>% left_join(user_effect, by = "userId") %>% ungroup() %>%
    group_by(genres) %>% left_join(genre_effect, by = "genres") %>% ungroup() %>%
    group_by(year) %>%
    summarize(by = sum(rating - mu - bi - bu - bg)/(n() + lambda))
  
  usercount_effect <- train %>%
    group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
    group_by(userId) %>% left_join(user_effect, by = "userId") %>% ungroup() %>%
    group_by(genres) %>% left_join(genre_effect, by = "genres") %>% ungroup() %>%
    group_by(year) %>% left_join(year_effect, by = "year") %>% ungroup() %>%
    group_by(userId) %>%
    mutate(count = seq(n())) %>% ungroup() %>%
    group_by(count) %>%
    filter(count <= nu_cutoff) %>%
    summarize(bnu = sum(rating - mu - bi - bu - bg - by)/(n() + lambda)) %>%
    pull(bnu) %>% mean()
  
  moviecount_effect <- train %>%
    group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
    group_by(userId) %>% left_join(user_effect, by = "userId") %>% ungroup() %>%
    group_by(genres) %>% left_join(genre_effect, by = "genres") %>% ungroup() %>%
    group_by(year) %>% left_join(year_effect, by = "year") %>% ungroup() %>%
    group_by(userId) %>% 
      mutate(count = seq(n())) %>%
      mutate(bnu = ifelse(count <= nu_cutoff, usercount_effect, 0)) %>% ungroup() %>%
    group_by(movieId) %>%
      mutate(r_count = seq(n())) %>% ungroup() %>%
    group_by(r_count) %>%
    filter(r_count <= ni_cutoff) %>%
    summarize(bni = sum(rating - mu - bi - bu - bg - by - bnu)/(n() + lambda)) %>%
    pull(bni) %>% mean()
  
  test %>% 
    group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
    group_by(userId) %>% left_join(user_effect, by = "userId") %>% ungroup() %>%
    group_by(genres) %>% left_join(genre_effect, by = "genres") %>% ungroup() %>%
    group_by(year) %>% left_join(year_effect, by = "year") %>% ungroup() %>%
    group_by(userId) %>%
      mutate(count = seq(n())) %>% ungroup() %>%
    group_by(count) %>% 
      mutate(bnu = ifelse(count <= nu_cutoff, usercount_effect, 0)) %>% ungroup() %>%
    group_by(movieId) %>%
      mutate(r_count = seq(n())) %>%
      mutate(bni = ifelse(r_count <= ni_cutoff, moviecount_effect, 0)) %>%
    mutate(prediction = mu + bi + bu + bg + by + bnu + bni) %>%
    pull(prediction) %>%
    RMSE(test$rating)
  
})

moviecount_effect <- train %>%
  group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
  group_by(userId) %>% left_join(user_effect, by = "userId") %>% ungroup() %>%
  group_by(genres) %>% left_join(genre_effect, by = "genres") %>% ungroup() %>%
  group_by(year) %>% left_join(year_effect, by = "year") %>% ungroup() %>%
  group_by(userId) %>% 
  mutate(count = seq(n())) %>%
  mutate(bnu = ifelse(count <= nu_cutoff, usercount_effect, 0)) %>% ungroup() %>%
  group_by(movieId) %>%
  mutate(r_count = seq(n())) %>% ungroup() %>%
  group_by(r_count) %>%
  filter(r_count <= ni_cutoff) %>%
  summarize(bni = sum(rating - mu - bi - bu - bg - by - bnu)/(n() + lambda)) %>%
  pull(bni) %>% mean()

  # The lambda remains at 4.5, with cutoffs at the first 250 user ratings
  # and first 125 movie ratings given.

results <- rbind(results,
                 data.frame(method = "Reg. First 125 Movies Effect",
                            RMSE = min(RMSEs)))
results %>%
  mutate(improvement = c(0, as.numeric(sprintf("%.7f", diff(as.matrix(results$RMSE))))))

  ######
  #
  # CURRENT AVERAGE RATING
  # 
  ######

# What other movie-specific bias could there be?
# 
# We have considered the number of ratings given -- what about the quality of 
# the ratings? In other words, does the current average rating at the time of
# rating affect the rating of a movie? (Note that the user is excluded in this.)


# How do we compute the average rating of a movie at the time of rating?

train %>% group_by(movieId) %>%
  mutate(n = seq(n()),
         cum_avg = (cumsum(rating))/seq_along(rating),
         curr_avg = ifelse(n > 1, ((n*cum_avg)-rating)/(n-1), NA),
         diff = rating - curr_avg) %>%
# What is the mean and sd of the difference in ratings?
  group_by(movieId) %>%
  summarize(mean_diff = mean(diff, na.rm = TRUE),
            sd_diff = sd(diff, na.rm = TRUE)) %>%
  ggplot(aes(mean_diff, sd_diff)) + 
      geom_point(alpha = 0.5) +
  xlab("Average Difference") +
  ylab("Standard Deviation of Difference") +
  ggtitle("Exploring Differences Between Current Movie Average and Rating Given")

  # Observation: There is a clear "center" where the average difference
  # is small, between 0 to 1 absolute units, and the standard deviation 
  # is between 0.5 and 1.5. 
  #
  # The spread is a bit high, since a standard deviation of 1 means that
  # most of the ratings could differ by 1 whole unit.


# Still, can we assume that there is a difference in the rating?
train %>% group_by(movieId) %>%
  mutate(n = seq(n()),
         cum_avg = cumsum(rating)/seq_along(rating),
         current_avg = ifelse(n > 1, ((n*cum_avg) - rating)/(n - 1), NA),
         diff = rating - current_avg) %>%
  summarize(mean_current = mean(current_avg, na.rm = TRUE),
            mean_rating = mean(rating),
            mean_diff = mean(diff, na.rm = TRUE)) %>%
  filter(movieId %in% seq(1, 500)) %>%
  ggplot(aes(reorder(movieId, mean_diff), mean_diff)) +
    geom_point() +
    geom_hline(yintercept = 0, color = "red") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_y_continuous(breaks = seq(-4, 1, 0.5)) +
  ggtitle("Difference in Movie's Mean Rating vs. Mean Current Rating")

    # Observation: It seems like there are some movies with significant 
    #   differences between the rating given and the current rating it has.
  
# Let's look at those movies in particular:

movie_titles <- train %>% select(movieId, title) %>% distinct()
  
train %>% group_by(movieId) %>%
  mutate(n = seq(n()),
         cum_avg = cumsum(rating)/seq_along(rating),
         current_avg = ifelse(n > 1, ((n*cum_avg) - rating)/(n - 1), NA),
         diff = rating - current_avg) %>%
  summarize(mean_current = mean(current_avg, na.rm = TRUE),
            mean_rating = mean(rating),
            mean_diff = mean(diff, na.rm = TRUE)) %>%
  left_join(movie_titles, by = "movieId") %>%
  arrange(desc(abs(mean_diff)))

  # These movies with large average differences seem obscure. 
  # Do they have a lot of ratings? Perhaps the large difference is due to 
  # there being only few ratings given.

train %>% group_by(movieId) %>%
  mutate(n = seq(n()),
         cum_avg = cumsum(rating)/seq_along(rating),
         current_avg = ifelse(n > 1, ((n*cum_avg) - rating)/(n - 1), NA),
         diff = rating - current_avg) %>%
  summarize(n = n(),
            mean_current = mean(current_avg, na.rm = TRUE),
            mean_rating = mean(rating),
            mean_diff = mean(diff, na.rm = TRUE)) %>%
  left_join(movie_titles, by = "movieId") %>%
  arrange(desc(abs(mean_diff)))
  
  # It does seem like these high-variance movies have only 2 ratings.
  # If we filter out n < 50, are there still standouts?

train %>% group_by(movieId) %>%
  mutate(n = seq(n()),
         cum_avg = cumsum(rating)/seq_along(rating),
         current_avg = ifelse(n > 1, ((n*cum_avg) - rating)/(n - 1), NA),
         diff = rating - current_avg) %>%
  summarize(n = n(),
            mean_current = mean(current_avg, na.rm = TRUE),
            mean_rating = mean(rating),
            mean_diff = mean(diff, na.rm = TRUE)) %>%
  filter(n >= 50) %>%
  ggplot(aes(reorder(movieId, mean_diff), mean_diff)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_y_continuous(breaks = seq(-1, 1, 0.25)) +
  ggtitle("Difference in Movie's Mean Rating vs. Mean Current Rating")
  
  # Observation: The degree of difference gets smaller, to up to 0.5.
  # We can look at these movies now:

train %>% group_by(movieId) %>%
  mutate(n = seq(n()),
         cum_avg = cumsum(rating)/seq_along(rating),
         current_avg = ifelse(n > 1, ((n*cum_avg) - rating)/(n - 1), NA),
         diff = rating - current_avg) %>%
  summarize(n = n(),
            mean_current = mean(current_avg, na.rm = TRUE),
            mean_rating = mean(rating),
            mean_diff = mean(diff, na.rm = TRUE)) %>%
  filter(n >= 50) %>%
  left_join(movie_titles, by = "movieId") %>%
  arrange(desc(abs(mean_diff)))

# Let us look at the behaviour of their ratings

train %>% filter(movieId == 3245) %>% # "I Am Cuba (Soy Cuba/Ya Kuba)"
  mutate(n = seq(n()),
         cum_avg = cumsum(rating)/seq_along(rating),
         current_avg = ifelse(n > 1, ((n*cum_avg) - rating)/(n - 1), NA),
         diff = rating - current_avg) %>%
  ggplot(aes(as_datetime(timestamp), diff)) +
    geom_line() +
    geom_hline(yintercept = 0, color = "red")


  ################
  # 
  # CURRENT AVERAGE (USER-SPECIFIC)
  #
  ###############

# It doesn't make sense for the movie to have an inherent effect on itself.
# Maybe the current average is more important to the user rating the movie.

# But how do we calculate the current average of the movie at the time of 
# a user's rating?

train %>% group_by(movieId) %>% arrange(timestamp) %>%
  mutate(n_i = seq(n()),
         cum_avg = cumsum(rating)/seq_along(rating),
         current_avg = ifelse(n_i > 1, ((n_i*cum_avg) - rating)/(n_i - 1), NA)) %>%
  ungroup() %>%
  group_by(userId) %>%
    mutate(diff = ifelse(n_i > 1, rating - current_avg, 0)) %>%
    summarize(n_ratings = n(),
              mean_diff = mean(diff, na.rm = TRUE)) %>%
  filter(mean_diff != 0) %>%
  ggplot(aes(reorder(userId,mean_diff), mean_diff)) +
    geom_point()

  # Once again, it seems like there are users with high
  # mean differences. Let's look at them:

train %>% group_by(movieId) %>% arrange(timestamp) %>%
  mutate(n_i = seq(n()),
         cum_avg = cumsum(rating)/seq_along(rating),
         current_avg = ifelse(n_i > 1, ((n_i*cum_avg) - rating)/(n_i - 1), NA)) %>%
  ungroup() %>%
  group_by(userId) %>%
  mutate(n_u = seq(n()),
         diff = ifelse(n_i > 1, rating - current_avg, 0)) %>%
  summarize(n_ratings = n(),
            mean_diff = mean(diff, na.rm = TRUE)) %>%
  filter(mean_diff != 0) %>% # users with mean_diff = 0 are the first to rate,
    # so there won't be much information to get from that.
  arrange(desc((mean_diff))) %>%
  # We see that the large variances come from users who have made between 13 to 24
  # ratings. Let's filter that more to see if the variances change.
  filter(n_ratings >= 50)

  # Okay, now we are seeing still large variances from users who have made
  # multiple ratings. Let's compare the individual rating differences to 
  # see if there is a pattern

train %>% group_by(movieId) %>% arrange(timestamp) %>%
  mutate(n_i = seq(n()),
         cum_avg = cumsum(rating)/seq_along(rating),
         current_avg = ifelse(n_i > 1, ((n_i*cum_avg) - rating)/(n_i - 1), NA)) %>%
  ungroup() %>% 
  group_by(userId) %>%
  mutate(n_u = seq(n()),
         diff = ifelse(n_i > 1, rating - current_avg , 0)) %>%
  filter(userId == 59342) %>%
  ggplot(aes(n_u, diff)) +
    geom_line() +
    geom_hline(yintercept = 0, color = "red")
  
  # For this user, who has a mean_diff of -2.38 (which means, on average,
  # he rates movies 2.38 units lower than what the current rating is), we can
  # definitely see that rates movies lower than its current average.
  #
  # This "negative" behavior does not have a trend in itself (the user does not
  # rate lower and lower as the number of ratings increases or vice-versa).


train %>% group_by(movieId) %>% arrange(timestamp) %>% arrange(timestamp) %>%
  mutate(n_i = seq(n()),
         cum_avg = cumsum(rating)/seq_along(rating),
         current_avg = ifelse(n_i > 1, ((n_i*cum_avg) - rating)/(n_i - 1), NA)) %>%
  ungroup() %>% 
  group_by(userId) %>%
  mutate(n_u = seq(n()),
         diff = ifelse(n_i > 1, rating - current_avg , 0)) %>%
  filter(userId == 2) %>%
  ggplot(aes(n_u, diff)) +
  geom_line() +
  geom_hline(yintercept = 0, color = "red")

  # In contrast, we see this user who always rates higher than the average,
  # again with no pattern in their overall rating over time. 


# Can we make a conclusion that they are being affected by the current average?

# How do we incorporate this into the model?
#   Is it merely the difference between the rating?

train %>% group_by(movieId) %>%
  mutate(n_i = seq(n()),
         cum_avg = cumsum(rating)/seq_along(rating),
         current_avg = ifelse(n_i > 1, ((n_i*cum_avg) - rating)/(n_i - 1), NA)) %>%
  ungroup() %>% 
  group_by(userId) %>%
  mutate(n_u = seq(n()),
         diff = ifelse(n_i > 1, (rating - current_avg), NA)) %>% arrange(desc(diff)) %>%
  filter(n_i >= 10) %>% # make sure the movie has at least 10% ratings to reduce variability
  summarize(n_ratings = n(),
            mean_diff = ifelse(!is.na(diff),
                               sum(diff, na.rm = TRUE)/(n_ratings + 4.5),
                               NA)) %>%
  filter(!is.na(mean_diff), n_ratings >= 25)
  
  # Users with NaN were always the first to review the movies they rated and
  # can be ignored in this case.

test %>% group_by(userId) %>%
  left_join(user_behavior, by = "userId") %>%
  mutate(bub = ifelse(is.na(mean_diff), 0, mean_diff)) %>%
  left_join(user_effect, by = "userId") %>% ungroup() %>% 
  group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
  group_by(genres) %>% left_join(genre_effect, by = "genres") %>% ungroup() %>%
  group_by(year) %>% left_join(year_effect, by = "year") %>% ungroup() %>%
  mutate(prediction = mu + bi + bu + bg + by + bub) %>%
  pull(prediction) %>%
  RMSE(test$rating)

  # The RMSE worsens to 0.9510223. Maybe we should remove those effects that
  # have small variances, and users who have rated more than 25 times.


# How many ratings should a movie get before it converges to its average?
train %>% group_by(movieId) %>% arrange(timestamp) %>%
  mutate(n_i = seq(n()),
         movie_average = mean(rating),
         dist = rating - movie_average,
         cum_avg = cumsum(rating)/seq_along(rating),
         date = as_datetime(timestamp)) %>%
  filter(movieId == 185)
user_behavior <- user_behavior %>% filter(abs(mean_diff) >= 1)

#
# EXPLORING SOURCES OF VARIATION (CONTINUED)
#
##############################

# If the similarity in ratings of "particular types of movies" has been
# accurately captured by our Genre Effects model, that would mean that
# there would be no more movie-to-movie interaction left in the 
# residual -- the difference between the actual rating and the sum of all
# our model's effects.

# Is this the case?
#
#   Consider a matrix of size (m x n) where the rows are movies and the columns
#   are users. The values are the ratings given to movie m by user n.
#
#   If we subtract the movie effect from the rows and the user effect 
#   from the columns, we arrive at their residuals. 

# Because we are potentially dealing with a matrix of 10,677 movies by
# 69,878 users, it would be helpful to filter data that would not be useful.
#
#   We know that 99% of the movies have 9,158 or fewer ratings, and 99% of users
#   have rated 763 or fewer times.
#
# Let's take the middle 75% of the data

train %>% group_by(movieId) %>% summarize(n = n()) %>% summary()
# movies with between 24 and 453 ratings
train %>% group_by(userId) %>% summarize(n = n()) %>% summary()
# and users with between 26 and 103 ratings

x <- train %>% 
  group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% filter(n() >= 24 & n() <= 453) %>% ungroup() %>%
  group_by(userId) %>% left_join(user_effect, by = "userId") %>% filter(n() >= 26 & n() <= 103) %>%
  mutate(resid = rating - mu - movie_effect - user_effect) %>%
  select(userId, movieId, resid) %>%
  spread(userId, resid) %>% 
  as.matrix()

# Rename columns and rows for easier analysis
rownames(x) <- x[ , 1] # the first column contains all the movieIds
x <- x[ , -1] # we can now remove that first column

movie_titles <- train %>% select(movieId, title) %>% distinct()

rownames(x) <- movie_titles$title[match(rownames(x), movie_titles$movieId)]

# Decide whether to treat NAs as 0s
x_0 <- x
x_0[is.na(x_0)] <- 0

# We want to know the distance between each movie pair's residuals to find out
# whether they can be grouped further

dist <- dist(x)

h <- hclust(dist)

# To generate actual groups, we can do one of two things:
#
#   1) Decide on a minimum distance required for observations to be
#      in the same group.
#
#   2) Decide on the number of groups you want and then find the minimum 
#      distance that achieves this.
#
# The function cutree() can be applied to the output of hclust() to perform
# either of the two operations and generate groups.

groups <- cutree(h, h = 10)

# K-means
k <- kmeans(x_0, centers = 10)
groups <- k$cluster

# After taking away the movie- and user-specific effects, these movies form
# certain groups based on their residuals from the baseline.
#
# Without knowing what ties these groups together, how can we improve our model?

# First thing to ask is, do these groups have an effect on the model?
#   Our matrix x has 5,412 movies and 4,759 users

group_tbl <- train %>% group_by(title) %>% 
  mutate(group = groups[match(title, names(groups))]) %>% 
  filter(!is.na(group)) %>% 
  group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% ungroup() %>%
  group_by(userId) %>% left_join(user_effect, by = "userId") %>% ungroup() %>%
  group_by(group) %>%
  summarize(group_effect = sum(rating - mu)/(n()))


# Apart from groups 4 and 9, there doesn't seem to be a significant enough
# effect on the ratings. And even then, the effect of groups 4 and 9 are 
# small.

# Looking at the movies in those groups, is there a discernible pattern?
grp_4 <- train %>% group_by(title) %>%
  mutate(group = groups[match(title, names(groups))]) %>% ungroup() %>%
  filter(group == 4) %>%
  group_by(title) %>% 
  summarize(n = n()) %>% # Group 4 comprises 58 genres
  pull(title)

grp_9 <- train %>% group_by(title) %>%
  mutate(group = groups[match(title, names(groups))]) %>% ungroup() %>%
  filter(group == 9) %>%
  group_by(title) %>%
  summarize(n = n()) %>% # Group 9 comprises only 10 genres, most around the
  # Horror Thriller Mystery Crime genres
  pull(title)

# Let's see if a group classification would help.
# Group 4's effect is 0.108 and group 9's is 0.109

group_effect <- train %>% group_by(title) %>%
  mutate(group = groups[match(title, names(groups))],
         group_effect = group_tbl$group_effect[group]) %>% ungroup() %>%
  group_by(movieId) %>% summarize(group_effect = max(group_effect)) %>%
  select(movieId, group_effect)

group_effect$group_effect[is.na(group_effect$group_effect)] <- 0
# 5,143 movies/titles are not part of a group because we used only a subset
# of the data to generate the groupings

test %>%
  group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>% 
  left_join(group_effect, by = "movieId") %>% ungroup() %>%
  group_by(userId) %>% left_join(user_effect, by = "userId") %>% ungroup() %>%
  #group_by(genres) %>% left_join(genre_effect, by = "genres") %>% ungroup() %>%
  mutate(prediction = mu + movie_effect + user_effect + group_effect) %>%
  pull(prediction) %>%
  RMSE(test$rating)

# There is no improvement when using a group effect for only Group 4 and 9,
# whose effects were determined after removing mu, bi, bu, and bg(i).

# Does it do better than a genre effect model? (i.e. without accounting for bg(i))
#   Answer: No.


# What about without accounting for bi, bu, and bg(i) altogether?
#   In which case, more groups would have more significant effects


############
#
# CLUSTERING
#
###########

# It would be difficult to know which genre/genre combinations are being
# formed in the data.
#
# We need to turn to UNSUPERVISED LEARNING and clustering algorithms to find
# which groups of genres are contributing similar effects to ratings.
#
# We want to find out the relationships between movies and whether they
# seem to form groups based on the same genre classifications.

# Recall that there are 796 unique genre combinations, 
shortlist <- train %>% filter(str_count(genres, "\\|") + 1 <= 4) %>% 
  pull(genres) %>% unique()
# 639 of which have 4 or fewer genre combinations

# We can look at the correlation between each of their average ratings
dat <- train %>% filter(genres %in% shortlist) %>% 
  group_by(movieId) %>% 
  
  
  
  
  
  
  
  
  
  
  




###########
#
# [[[SCRATCH]]]
#
###########



# However, there could be an interaction between movies themselves
# and between users themselves, i.e. that certain movies are
# rated similarly and that certain users rate movies similarly.
#
# How would we check if this is true?

# We can create a matrix with users (69,878) on each row and movies (10,677)
# on each column, depicting the rating given by user u for movie i.
# Then we subtract the movie-specific effect from the columns and
# the user-specific effect from the rows to get a matrix of residuals.

# This residual matrix can be analyzed to see if there are correlations
# among certain movies or among certain users.

# Step 0: Since we are potentially dealing with a matrix of over 746 million
# data points, we can reduce the computing effort by filtering out
# movies and users with few ratings.

# Say we want a movie to have at least 50 ratings, and for a user to have
# rated at least 50 times, which trims the data down to 
# 6,628 movies rated by 34,960 users = 231 million data points.

y <- train %>% group_by(movieId) %>% filter(n() >= 50) %>% ungroup() %>%
  group_by(userId) %>% filter(n() >= 50) %>% ungroup()

# Step 1: Transform the data into a matrix
y <- y %>% select(userId, movieId, rating) %>%
  spread(movieId, rating) %>%
  as.matrix()

# Step 2: Renaming
# Note that userId is found in the first column, rather than as row names.
rownames(y) <- y[ , 1]
y <- y[ , -1]

# We can also convert the column names into the respective movie titles
# to make it easier to understand
movie_titles <- train %>% select(movieId, title) %>% distinct()
colnames(y) <- movie_titles$title[match(colnames(y), movie_titles$movieId)]


# Step 3: Subtract the colMeans and rowMeans to arrive at the residuals
library(matrixStats)

# Note that there are a lot of NAs, so be sure to ignore them
r <- sweep(y, 2, colMeans(y, na.rm = TRUE))
r <- sweep(r, 1, rowMeans(r, na.rm = TRUE))
rm(y)


# So, to test our hypothesis that there ARE variations caused by groups of
# movies and groups of users, we can look at the patterns of the residuals.
#
# If our current model completely captures all the effects of movies and users, 
# then there should be no pattern (i.e. the residuals are independent from each other)

# Which movies are likely to be correlated? 
# 
# Movies with sequels, for instance, the Star Wars or Lord of the Rings series.
cor(r[ , movie_titles$title[str_detect(movie_titles$title, "Lord of the Rings")]], use = "pairwise.complete") %>% knitr::kable()
# Note that there is a film titled "Lord of the Rings, The" that is not part of the
# trilogy by Peter Jackson.

# Are movies of a certain genre rated similarly?
# In other words, can a movie's genre affect how it would be rated?

# Observation:
train %>% count(genres) %>%
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") +
  scale_x_sqrt(labels = scales::comma) +
  ggrepel::geom_text_repel(data = train %>% count(genres) %>% filter(n > 250000),
                           segment.color = NA,
                           inherit.aes = FALSE,
                           aes(label = genres,
                               x = n,
                               y = 25),
                           direction = "both",
                           position = position_fill(vjust = -1)) +
  xlab("Number of ratings (sqrt-transformed)") +
  ylab("Frequency of occurence") +
  labs(title = "Frequency of Genres Rated n times")

# Very few genres dominate the number of ratings, meaning a handful of genres
# get rated more than others. There is an extremely large variance when
# it comes to how many ratings a genre gets --
train %>% count(genres) %>% summarize(mean = mean(n), sd = sd(n))

# Which genres are the most rated?
train %>% group_by(genres) %>% summarize(n = n()) %>%
  top_n(10, n) %>% arrange(desc(n))
# The usual suspects are Drama, Comedy, Romance, Action, Adventure, Sci-Fi
# Thriller, and Crime


# Looking at the genre classifications, we see that there are various
# combinations of genres. The fact that "Drama" and "Comedy" have the most
# number of ratings does not take into account those movies that are
# classified as "Drama" among other genres, like "Comedy|Drama" or "Drama|Romance".
#
# If we look at the average distance from the mean of single genre movies,
# will we find the same or similar effects in multi-genre movies?

# List of individual genres
genres <- train %>% filter(!str_detect(genres, "\\|")) %>% 
  select(genres) %>% distinct() %>% pull(genres)
# We see that 1 movie, rated by 5 people, is classified as having
# "(no genre listed)" 
# The movie, 'Pull My Daisy' is a 27-minute short film adapted from
# the third act of the play 'Beat Generation' by Jack Kerouac. 
# From what has been said about the short (https://www.newyorker.com/goings-on-about-town/movies/pull-my-daisy)
# the genre is reasonably "Comedy"

# Change 'Pull My Daisy' movieId = 8606 genre to "Comedy"


# However, a lot of these most-rated genres are combinations of others
# (Comedy|Romance, Comedy|Drama, Comedy|Drama|Romance, etc.)

# How much of the movies have more than 1 genres?
movie_ids <- train %>% select(movieId) %>% unique()

genre_counts <- train %>% left_join(movie_ids, by = "movieId") %>%
  select(movieId, genres) %>% mutate(genre_count = str_count(genres, "\\|") + 1)

genre_counts %>% group_by(genre_count) %>% summarize(n = n()) %>% mutate(prop = n/sum(n)) %>%
  ggplot(aes(genre_count, n, fill = as.factor(genre_count))) +
  geom_col() +
  xlab("Number of genres") +
  ylab("# of ratings (000's)") +
  scale_x_discrete(limits = seq(1, 8, 1)) +
  scale_y_continuous(n.breaks = 10) +
  labs(title = "Observations Based on Number of Genres", fill = "# of genres") +
  theme(legend.position = "none") +
  geom_text(aes(label = paste(round(prop * 100, 3),"%"),
                y = n,
                vjust = -.5)) 

# So we know that most of the ratings have between 1 to 4 genres.
# Does the average rating of movies with multiple genres change?

train %>% mutate(genre_count = str_count(genres, "\\|") + 1) %>%
  group_by(genre_count) %>% summarize(n = n(), average_rating = mean(rating), multigenre_effect = sum(rating - mu)/(5 + n))
# it doesn't appear that the number of genres affects the ratings
# since they are all still around the average of 3.51

# So having multiple genres doesn't seem to matter.
# We can ignore that and ask whether specific genres have an effect


# What are the most common genres?
train %>% group_by(genres) %>% summarize(n = n()) %>%
  top_n(10, n) %>% arrange(desc(n))

# What are their (Regularized) effects?
genre_effects <- sapply(genre_list, function(genre){
  train %>% filter(str_detect(genres, genre)) %>%
    summarize(effect = sum(rating - mu)/(5 + n())) %>%
    pull(effect)
})
genre_effects_list <- data.frame(genre = genre_list,
                                 effect = genre_effects,
                                 row.names = NULL)

# Let's try to include genre effects in our model
# by taking the total for each observation's genre_effect

train_2 <- train %>% # let's consider only 4 genres, since most of the observations have 4 or fewer
  separate(col = genres, into = c("genre_1", "genre_2", "genre_3", "genre_4"), sep = "\\|")

train_2 <- train_2 %>% group_by(movieId) %>%
  mutate(bg1 = genre_effects[genre_1],
         bg2 = genre_effects[genre_2],
         bg3 = genre_effects[genre_3],
         bg4 = genre_effects[genre_4]) 

train_2 %>% filter(movieId == 2)

# Store genre bias values
bg <- train_2 %>%
  select(movieId, bg1, bg2, bg3, bg4)

# Convert NAs to 0s
bg[is.na(bg)] <- 0
bg <- bg %>% summarize(total_genre_effect = bg1 + bg2 + bg3 + bg4)
bg <- distinct(bg)


# Now we have a vector of movie effects, user effects, and total genre effects
prediction <- test %>%
  group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>%
  left_join(bg, by = "movieId") %>%
  group_by(userId) %>% left_join(user_effect, by = "userId") %>%
  mutate(prediction = mu + movie_effect + user_effect + total) %>%
  pull(prediction)

# NOTE that some predictions go over the maximum rating of 5
RMSE(test$rating, prediction)
# Including a genre-specific effect for up to 4 genres actually results
# in a higher RMSE of 0.8872917, which means the model does WORSE.

# Why is that the case?
#
#
# Let's look at the genre-specific biases again (the effect on rating if a movie
# is classified as being at least one of the genres listed):
genre_effects

# Firstly, note how large/small the effects are
data.frame(genre = genre_list,
           effect = genre_effects) %>%
  arrange(desc(abs(effect)))

# About half of the genres don't contribute a significant effect -- and
# these are the popular ones. 

# What does the distribution of ratings look like for genres?
genre_counts


train_2 %>% filter(movieId == 2)

total_genre_effects <- train_2 %>% group_by(movieId) %>% summarize(total = b_g1 + b_g2 + b_g3 + b_g4)
total_genre_effects[is.na(total_genre_effects)] <- 0
total_genre_effects %>% mutate(total = b_g1 + b_g2 + b_g3 + b_g4) %>%
  group_by(movieId)

train %>% group_by(movieId)
length(unique(train_2$movieId))

total_genre_effects

train_2 <- cbind(train_2, total_genre_effects)

total_genre_effects <- train_2 %>% group_by(movieId) %>% select(movieId, total_genre_effects)
total_genre_effects
movie_effect

prediction <- test %>% 
  group_by(movieId) %>% left_join(movie_effect, by = "movieId") %>%
  group_by(userId) %>% left_join(user_effect, by = "userId") %>%
  group_by(movieId) %>% left_join(total_genre_effects, by = "movieId") %>%
  mutate(prediction = mu + movie_effect + user_effect + total_genre_effects) %>%
  pull(prediction)

# Does this make sense? To test, let us look at the effects of movies classified
# as two genres with opposing effects (Drama and Action) 

train %>% filter(str_detect(genres, "Action") & str_detect(genres, "Drama")) %>%
  summarize(effect = sum(rating - mu)/(5 + n()))
# Action + Drama movies have an effect of 0.06
# which is roughly the sum of Drama and Action effects

# Test with Comedy and Romance
train %>% filter(str_detect(genres, "Comedy") & str_detect(genres, "Romance")) %>%
  summarize(effect = sum(rating - mu)/(5 + n()))
# 0.04, which is not the difference between the two genres


# Let us test (1) a multi-genre effect
train %>%  
  mutate(genre_count = str_count(genres, "\\|") + 1) %>% # since genres are divided by "|"
  group_by(genre_count) %>%
  summarize(average_rating = mean(rating)) %>%
  ggplot(aes(x = genre_count, y = average_rating)) + geom_point(size = 5)

# There doesn't seem to be a meaningful pattern with the # of genres a movie has


# Let us test (2), a single genre effect
# To narrow down the search, let's look at the most frequent genres
train %>% group_by(genres) %>% summarize(n = n()) %>%
  top_n(10, n) %>% arrange(desc(n))

# They appear to be Drama, Comedy, Romance, Action, Adventure, Sci-Fi,
# Thriller, and Crime (8 genres total)



genres_8 <- c("Drama", "Comedy", "Romance", "Action", "Adventure", "Sci-Fi",
              "Thriller", "Crime")

genre_effects <- sapply(genres_8, function(genre){
  genre_effect <- train %>% group_by(genres) %>%
    filter(str_detect(genres, genre)) %>%
    summarize(genre_effect = rating - mu) %>%
    pull(genre_effect) %>%
    mean()
})


# Just for the sake of argument, let's look at the effect on movies NOT
# classified as these genres:

non_genre_effects <- sapply(genres_8, function(genre){
  genre_effect <- train %>% group_by(genres) %>%
    filter(!str_detect(genres, genre)) %>%
    summarize(genre_effect = mean(rating) - mu) %>%
    pull(genre_effect) %>%
    mean()
})

# Their signs are opposite, which makes sense. 

# How strong are their effects?
genre_effects %>% as.data.frame() %>% arrange(desc(abs(.)))


# Let's take a look at movies classified as "Comedy"
train %>% group_by(genres) %>%
  filter(str_detect(genres, "Drama")) %>%
  summarize(average_rating = mean(rating), sd = sd(rating)) %>%
  ggplot(aes(average_rating, sd)) + geom_point()

# While there seems to be a group forming around the average rating 3.5-3.7,
# the standard deviation is close to 1, meaning the rating would differ by 
# a whole "star". 

# Let's look at those values that seem to cause variability in the group:
train %>% group_by(genres) %>%
  filter(str_detect(genres, "Drama")) %>%
  summarize(average_rating = mean(rating), sd = sd(rating)) %>%
  filter(sd >= 1) %>%
  pull(genres)

# Intuitively, we know that "Comedy" is quite the opposite of "Drama" -- maybe
# there is an effect of movies that are classified as both?
# Let's exclude "Drama" from any "Comedy" movie that we analyze:
train %>% group_by(genres) %>%
  filter(str_detect(genres, "Drama")) %>%
  filter(!str_detect(genres, "Comedy")) %>%
  summarize(average_rating = mean(rating), sd = sd(rating)) %>%
  ggplot(aes(average_rating, sd)) + geom_point()

# It doesn't seem like there was an effect. So let us now look at what's 
# causing the variability:
train %>% group_by(genres) %>%
  filter(str_detect(genres, "Drama")) %>%
  filter(!str_detect(genres, "Comedy")) %>%
  summarize(average_rating = mean(rating), sd = sd(rating)) %>%
  arrange(desc(sd))

# Multiple genres seem to affect "Drama" when combined together:
# "Crime", "Action", "Romance", etc.


# It does not seem like we can intuit a pattern with genres. In cases where
# we do not or cannot know the groupings of variables, we can use
# clustering algorithms to explore the data.


# Since it is too complex to zero in on a genre effect, let's see if the
# data itself shows the groups. This is done through UNSUPERVISED LEARNING.

# The way we do this is by first calculating the model's residuals 
# defined as r_u,i = Y_u,i - b_i - b_u
# where Y_u,i is a matrix with users on each row and movies on each column

# Looking at our data, we see that:
train %>% group_by(movieId) %>% summarize(n = n()) %>% summary()
# Average number of ratings per movie is 676. Half of movies have fewer than
# 100 ratings.
train %>% group_by(userId) %>% summarize(n = n()) %>% summary()
# The average user rates 103 times. Half of the users rate 50 or fewer times.


# Now begin creating the matrix
temp <- train_small %>%
  select(movieId, userId, rating) %>%
  spread(movieId, rating) %>%
  as.matrix()

# Note that this creates 'NA' values, since some users may not have rated
# some movies.

# The first column contains 'userId', so we will remove that and instead use
# rownames()
rownames(temp) <- temp[, 1]
y <- temp[ , -1]

# Next, to make our analysis easier, let's convert column names to the 
# actual movie titles

titles <- train_small %>% select(movieId, title) %>% distinct()

colnames(y) <- titles$title[match(colnames(y), titles$movieId)]

library(matrixStats)

# Now, recall that r_u,i = Y_u,i - b_i - b_u,
# which means we have to subtract the column (movie) averages and
# row (user) averages
y <- sweep(y, 2, colMeans(y, na.rm = TRUE))
y <- sweep(y, 1, rowMeans(y, na.rm = TRUE))
# We also exclude the NAs


# Now, given our matrix of residuals, we can test whether our intuition
# that certain genre-movies are rated differently from others.
#
# In our earlier analysis, we found that Dramas we the most frequently rated
# genre, and that variations within movies classified as "Drama" came mostly 
# when combined with the "Comedy" genre -- which makes sense since they are
# quite opposites. 
#
# Let's look at movies that are dramatic versus movies that are comedic.

top_dramas <- train_small %>% filter(str_detect(genres, "Drama")) %>%
  group_by(title) %>%
  summarize(average = mean(rating)) %>%
  top_n(3, average) %>%
  arrange(desc(average)) %>%
  pull(title)

top_comedies <- train_small %>% filter(str_detect(genres, "Comedy")) %>%
  group_by(title) %>%
  summarize(average = mean(rating)) %>%
  top_n(3, average) %>%
  arrange(desc(average)) %>%
  pull(title)

# Let's look at the correlations to first see if movies in the same genre
# are rated similarly

cor(y[ , top_dramas], use = "pairwise.complete") %>% knitr::kable()
# Among the top dramas, it doesn't seem like there is a strong enough correlation

cor(y[ , top_comedies], use = "pairwise.complete") %>% knitr::kable()
# The same is true for comedies


# What about other genre combinations that intuitively seem opposite to each
# other? For instance, Crime and Romance --

top_crime <- train_small %>% 
  filter(str_detect(genres, "Crime")) %>%
  group_by(title) %>%
  summarize(average = mean(rating)) %>%
  top_n(3, average) %>%
  arrange(desc(average)) %>%
  pull(title)

top_romance <- train_small %>%
  filter(str_detect(genres, "Romance")) %>%
  group_by(title) %>%
  summarize(average = mean(rating)) %>%
  top_n(3, average) %>%
  arrange(desc(average)) %>%
  pull(title)

cor(y[, c(top_crime, top_romance)], use = "pairwise.complete") %>% knitr::kable()
