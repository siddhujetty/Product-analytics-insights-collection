Song Challenge
================
Siddhartha Jetti
8/4/2019

# Goal

Company XYZ is a very early stage startup. They allow people to stream
music from their mobile for free. Right now, they still only have songs
from the Beatles in their music collection, but they are planning to
expand soon.

They still have all their data in json files and they are interested in
getting some basic info about their users as well as building a very
preliminary song recommendation model in order to increase user
engagement.

# Challenge Description

You are the fifth employee at company XYZ. The good news is that if the
company becomes big, you will become very rich with the stocks. The bad
news is that at such an early stage the data is usually very messy. All
their data is stored in json format.

The company CEO asked you for very specific questions:

  - What are the top 3 and the bottom 3 states in terms number of users?

  - What are the top 3 and the bottom 3 states in terms of user
    engagement? You can choose how to mathematically define user
    engagement. What the CEO cares about here is in which states users
    are using the product a lot/very little.

  - The CEO wants to send a gift to the first user who signed-up for
    each state. That is, the first user who signed-up from California,
    from Oregon, etc. Can you give him a list of those users?

  - Build a function that takes as an input any of the songs in the data
    and returns the most likely song to be listened next. That is, if,
    for instance, a user is currently listening to “Eight Days A Week“,
    which song has the highest probability of being played right after
    it by the same user? This is going to be V1 of a song recommendation
    model.

  - How would you set up a test to check whether your model works well?

# Data

The json is: data - Each row represents a song that was listened by a
user.

## Fields:

id : it is unique. user\_id : user id who listened to a given song.
user\_state : where the user is based. user\_sign\_up\_date : when the
user signed-up. song\_played : the song that was listened. time\_played
: at which time the user started listening to the song (local time).

# Problem Setup

``` r
# Load required libraries
library(tidyverse)
```

    ## Registered S3 methods overwritten by 'ggplot2':
    ##   method         from 
    ##   [.quosures     rlang
    ##   c.quosures     rlang
    ##   print.quosures rlang

    ## ── Attaching packages ──────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.1.1     ✔ purrr   0.3.2
    ## ✔ tibble  2.1.1     ✔ dplyr   0.8.1
    ## ✔ tidyr   0.8.3     ✔ stringr 1.4.0
    ## ✔ readr   1.3.1     ✔ forcats 0.4.0

    ## ── Conflicts ─────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(jsonlite)
```

    ## 
    ## Attaching package: 'jsonlite'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     flatten

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
# Read in the input data into a dataframe
songs <- fromJSON("song.json")
```

# Data Exploration and checks

Check data types of columns in songs dataset

``` r
# Check data types of each of the columns
str(songs)
```

    ## 'data.frame':    4000 obs. of  6 variables:
    ##  $ id               : chr  "GOQMMKSQQH" "HWKKBQKNWI" "DKQSXVNJDH" "HLHRIDQTUW" ...
    ##  $ user_id          : int  122 3 35 126 6 147 155 171 174 170 ...
    ##  $ user_state       : chr  "Louisiana" "Ohio" "New Jersey" "Illinois" ...
    ##  $ user_sign_up_date: chr  "2015-05-16" "2015-05-01" "2015-05-04" "2015-05-16" ...
    ##  $ song_played      : chr  "Hey Jude" "We Can Work It Out" "Back In the U.S.S.R." "P.s. I Love You" ...
    ##  $ time_played      : chr  "2015-06-11 21:51:35" "2015-06-06 16:49:19" "2015-06-14 02:11:29" "2015-06-08 12:26:10" ...

``` r
# take a peek at the data
summary(songs)
```

    ##       id               user_id       user_state        user_sign_up_date 
    ##  Length:4000        Min.   :  1.0   Length:4000        Length:4000       
    ##  Class :character   1st Qu.: 48.0   Class :character   Class :character  
    ##  Mode  :character   Median :102.0   Mode  :character   Mode  :character  
    ##                     Mean   :101.6                                        
    ##                     3rd Qu.:155.0                                        
    ##                     Max.   :200.0                                        
    ##  song_played        time_played       
    ##  Length:4000        Length:4000       
    ##  Class :character   Class :character  
    ##  Mode  :character   Mode  :character  
    ##                                       
    ##                                       
    ## 

Check for missing values in the data

``` r
# Check if any missing values exist
colSums(is.na(songs))
```

    ##                id           user_id        user_state user_sign_up_date 
    ##                 0                 0                 0                 0 
    ##       song_played       time_played 
    ##                 0                 0

Check for duplicates in the data

``` r
# check if any duplicate id exist
length(songs$id) == length(unique(songs$id))
```

    ## [1] TRUE

``` r
# check if any duplicate user id exist in the data
length(songs$user_id) == length(unique(songs$user_id))
```

    ## [1] FALSE

Clearly, there are duplicate user ids in dataset. This is OK because
single user can listen to multiple songs. However, id appears to be
unique.

Check if dates make sense. The time played for all the entries should
NOT be before the sign-up date

``` r
all(as.Date(songs$user_sign_up_date) <= as.Date(songs$time_played))
```

    ## [1] TRUE

Clearly, All the entries have sign-up dates before time played. Overall,
the data looks OK.

# Question 1

Summarize the data by user state

``` r
top3_states <- songs %>%
  group_by(user_state) %>%
  summarise(user_count = n_distinct(user_id)) %>%
  ungroup() %>%
  arrange(desc(user_count), user_state) %>%
  filter(row_number() <= 3)

bottom3_states <- songs %>%
  group_by(user_state) %>%
  summarise(user_count = n_distinct(user_id)) %>%
  ungroup() %>%
  arrange(user_count, user_state) %>%
  filter(row_number() <= 3)

top3_states
```

    ## # A tibble: 3 x 2
    ##   user_state user_count
    ##   <chr>           <int>
    ## 1 New York           23
    ## 2 California         21
    ## 3 Texas              15

``` r
bottom3_states
```

    ## # A tibble: 3 x 2
    ##   user_state  user_count
    ##   <chr>            <int>
    ## 1 Arizona              1
    ## 2 Connecticut          1
    ## 3 Idaho                1

# Question 2

Based on the given data and problem description, the only way users
engage with the service is by playing songs. I define user engagement as
number of play events per user in a given period of time. I plan to use
average daily user engagement, which is average number of play events
per day per user, as the metric to decide the top and bottom states for
product usage.

If the users use the product a lot then number of play events per day
per user would go up and hence would drive the metric up. Also, The
daily user engagement rates can be used to visualize the trends over
time.

The user engagement should be calculated using the number of user
signups prior to the play event.

The number of user sign-ups by state and date.

``` r
total_signups_by_date <- songs %>%
  arrange(user_sign_up_date) %>%
  group_by(user_sign_up_date, user_state) %>%
  summarize(counts = n_distinct(user_id)) %>%
  ungroup() %>%
  arrange(user_state, user_sign_up_date)

# Unique states
unique_states <- songs %>% 
  select(user_state) %>%
  distinct() %>% 
  arrange(user_state)
```

The dates for which the daily engagement rate needs to be
computed

``` r
required_dates <- substring(songs$time_played, 1, 10) %>% unique() %>% sort()
```

Initialize a place holder to hold daily engagement
rate.

``` r
engagement_state_date <- data.frame(date = required_dates, stringsAsFactors = F) %>%
  merge(unique_states)

# Merge with other dataset to get the number of play events
daily_engagement_by_state_date <- songs %>%
  mutate(date_played = substring(time_played, 1, 10)) %>%
  group_by(user_state, date_played) %>%
  summarise(plays = n()) %>%
  ungroup() %>%
  right_join(engagement_state_date, by = c("user_state" = "user_state", "date_played" = "date")) %>%
  mutate(plays = ifelse(is.na(plays), 0, plays), signups_till_date = NA) 

head(daily_engagement_by_state_date)
```

    ## # A tibble: 6 x 4
    ##   user_state date_played plays signups_till_date
    ##   <chr>      <chr>       <dbl> <lgl>            
    ## 1 Alabama    2015-06-01      4 NA               
    ## 2 Alabama    2015-06-02      4 NA               
    ## 3 Alabama    2015-06-03      4 NA               
    ## 4 Alabama    2015-06-04      2 NA               
    ## 5 Alabama    2015-06-05      6 NA               
    ## 6 Alabama    2015-06-06      2 NA

Compute daily user engagement by state as “number of play events/ number
of user sign ups till date”

``` r
# Loop through the each of the entries
for(i in 1:nrow(daily_engagement_by_state_date)){
tmp <- total_signups_by_date %>%
  filter(user_state == daily_engagement_by_state_date$user_state[i], 
         as.Date(user_sign_up_date) <= as.Date(daily_engagement_by_state_date$date_played[i]))

daily_engagement_by_state_date$signups_till_date[i] <- sum(tmp$counts)
}

daily_engagement_by_state_date <- daily_engagement_by_state_date %>%
  mutate(daily_engagement = plays/signups_till_date) 

head(daily_engagement_by_state_date)
```

    ## # A tibble: 6 x 5
    ##   user_state date_played plays signups_till_date daily_engagement
    ##   <chr>      <chr>       <dbl>             <int>            <dbl>
    ## 1 Alabama    2015-06-01      4                 4              1  
    ## 2 Alabama    2015-06-02      4                 4              1  
    ## 3 Alabama    2015-06-03      4                 4              1  
    ## 4 Alabama    2015-06-04      2                 4              0.5
    ## 5 Alabama    2015-06-05      6                 4              1.5
    ## 6 Alabama    2015-06-06      2                 4              0.5

``` r
daily_engagement_summary <- daily_engagement_by_state_date %>%
  group_by(user_state) %>%
  summarise(avg_daily_engagement = round(mean(daily_engagement), digits = 2)) 

daily_engagement_summary
```

    ## # A tibble: 41 x 2
    ##    user_state  avg_daily_engagement
    ##    <chr>                      <dbl>
    ##  1 Alabama                    0.93 
    ##  2 Alaska                     1.04 
    ##  3 Arizona                    0.79 
    ##  4 Arkansas                   0.61 
    ##  5 California                 0.72 
    ##  6 Colorado                   0.64 
    ##  7 Connecticut                0.570
    ##  8 Florida                    0.92 
    ##  9 Georgia                    0.8  
    ## 10 Idaho                      0.93 
    ## # … with 31 more rows

Top and bottom 3 states by user daily user engagement

``` r
top3_states_engagement <- daily_engagement_summary %>%
  arrange(desc(avg_daily_engagement)) %>%
  filter(row_number() <= 3)

bottom3_states_engagement <- daily_engagement_summary %>%
  arrange(avg_daily_engagement) %>%
  filter(row_number() <= 3)

top3_states_engagement
```

    ## # A tibble: 3 x 2
    ##   user_state  avg_daily_engagement
    ##   <chr>                      <dbl>
    ## 1 Nebraska                    1.29
    ## 2 Alaska                      1.04
    ## 3 Mississippi                 1.01

``` r
bottom3_states_engagement
```

    ## # A tibble: 3 x 2
    ##   user_state avg_daily_engagement
    ##   <chr>                     <dbl>
    ## 1 Kansas                    0.290
    ## 2 Virginia                  0.3  
    ## 3 Minnesota                 0.38

# Question 3

First users by state

``` r
first_users_by_state <- songs %>%
  group_by(user_state) %>%
  arrange(user_sign_up_date) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(user_state, user_id) %>%
  arrange(user_state)

first_users_by_state
```

    ## # A tibble: 41 x 2
    ##    user_state  user_id
    ##    <chr>         <int>
    ##  1 Alabama           5
    ##  2 Alaska          106
    ##  3 Arizona         105
    ##  4 Arkansas         78
    ##  5 California       39
    ##  6 Colorado        173
    ##  7 Connecticut     127
    ##  8 Florida          41
    ##  9 Georgia          20
    ## 10 Idaho           165
    ## # … with 31 more rows

# Question 4

The approach to build song recommendation system is to use first order
Markov chain where for each song, we predict the most likely next song
without looking at user history, but only taking into account the
current song. The Markov chain approach is combined with similarity
score obtained from Collaborative filtering to break any ties or for
cases of cold start.

The algorithm is to build a data set where for each user and song, it
gives the very next song listened to. We can then group by each song
across all users and find the next song with the highest count in a
given time window. Here, I choose the time window as one day. For every
song, We are interested in finding the counterpart that is played
consecutively the most number of times but on the same day across all
the users. In the cases where there is a tie or missing data, the
similarity using collaborative filtering is used to give the prediction.

## Markov Chain

``` r
songs <- songs %>%
  mutate(k = 1)

# Cartesian join with the same table and apply appropriate filter
songs_joined <- songs %>%
  select(user_id1 = user_id, song = song_played, time_played_song1 = time_played, k) %>%
  full_join(songs, by = "k") %>%
  # Only interested in next song played most times by that user for that day
  filter(user_id1 == user_id, date(ymd_hms(time_played_song1)) == date(ymd_hms(time_played)),  
         ymd_hms(time_played_song1) < ymd_hms(time_played), song != song_played) %>%
  select(user_id, song, next_song = song_played)

# Most likely next song based on Markov chain
song_pairs <- songs_joined %>%
  mutate(song = toupper(song), next_song = toupper(next_song)) %>%
  group_by(song, next_song) %>%
  summarise(counts = n()) %>%
  ungroup() %>%
  arrange(song, desc(counts)) 
```

Clearly, ties exist in the data.

## Collaborative Filtering

Using coll.filtering to break ties. Each song can be imagined as a point
in the n-dimensional user space. Each coordinate of the
point(n-dimensional) would be the number of times the song is played by
the particular user.

``` r
# Build user song matrix
user_song_matrix <- songs %>%
  group_by(user_id, song_played) %>%
  summarise(nplays = n()) %>%
  ungroup() %>%
  spread(song_played, nplays) %>%
  mutate_all(list(~replace_na(., 0))) %>%
  select(-user_id)

unique_songs <- colnames(user_song_matrix)
```

Cosine similarity is used to compute similarity between two songs. The
idea here is if two songs are played by the same set of users, then they
must be similar and have high cosine similarity value.

``` r
# Define a function to compute the cosine similarity between two songs
cosine_similarity <- function(x, y) { 
  sum(x * y) / (sqrt(sum(x * x)) * sqrt(sum(y * y)))
}

# Define a place holder to hold similarity between each pair of songs
# similarity between a song and itself is 1
song_similarity  <- diag(1, nrow = ncol(user_song_matrix), ncol = ncol(user_song_matrix))
rownames(song_similarity) <- toupper(unique_songs)
colnames(song_similarity) <- toupper(unique_songs)
nsongs <- ncol(user_song_matrix)
```

Generate song similarity matrix

``` r
# Loop through the columns
for(i in 1:nsongs) {
  # Loop through the columns for each column
  for(j in 1:nsongs) {
    # Fill in placeholder with cosine similarities
    song_similarity[i, j] <- cosine_similarity(user_song_matrix[i], user_song_matrix[j])
  }
}

# Process song pairs
song_similarity_df <- song_similarity %>%
  as.data.frame() 
row.names(song_similarity_df) <- c()

song_similarity_df$song1 <- row.names(song_similarity)
song_similarity_df <- song_similarity_df %>%
  select(song1, 1:100) %>%
  gather(key = "song2", value = "similarity", -song1) %>%
  filter(song1 != song2)

# Take a peek at the song pair similarity scores
head(song_similarity_df)
```

    ##                                 song1             song2 similarity
    ## 1                  A HARD DAY'S NIGHT A DAY IN THE LIFE  0.2357023
    ## 2 A SATURDAY CLUB XMAS/CRIMBLE MEDLEY A DAY IN THE LIFE  0.0745356
    ## 3                 ACROSS THE UNIVERSE A DAY IN THE LIFE  0.2121320
    ## 4                       ALL MY LOVING A DAY IN THE LIFE  0.3550235
    ## 5                ALL YOU NEED IS LOVE A DAY IN THE LIFE  0.3294039
    ## 6              AND YOUR BIRD CAN SING A DAY IN THE LIFE  0.1521452

For every song, get the song with most counts and if multiple songs have
the most counts then use highest similarity score.

``` r
# summarize
next_song <- song_pairs %>%
  left_join(song_similarity_df, by = c("song" = "song1", "next_song" = "song2")) %>%
  arrange(song, desc(counts), desc(similarity)) %>%
  group_by(song) %>%
  filter(row_number() == 1)
```

Based on the number of songs, Not all songs got a prediction on the next
song to be played. For those cases, choose the song with highest
similarity as the next likely song.

``` r
# Get the missing songs similarity
missing_songs <- song_similarity_df %>%
  filter(!song1 %in% next_song$song) %>%
  arrange(song1, desc(similarity)) %>%
  group_by(song1) %>%
  filter(row_number() == 1)
```

Combine all the predictions.

``` r
# Combining  
next_song_final <- missing_songs %>%
  select(song = song1, next_song = song2) %>%
  bind_rows(next_song) %>%
  arrange(song) %>%
  select(song, next_song)

next_song_final
```

    ## # A tibble: 100 x 2
    ## # Groups:   song [100]
    ##    song                                next_song                
    ##    <chr>                               <chr>                    
    ##  1 A DAY IN THE LIFE                   COME TOGETHER            
    ##  2 A HARD DAY'S NIGHT                  LET IT BE                
    ##  3 A SATURDAY CLUB XMAS/CRIMBLE MEDLEY GIRL                     
    ##  4 ACROSS THE UNIVERSE                 WE CAN WORK IT OUT       
    ##  5 ALL MY LOVING                       HEY JUDE                 
    ##  6 ALL YOU NEED IS LOVE                LET IT BE                
    ##  7 AND YOUR BIRD CAN SING              COME TOGETHER            
    ##  8 ANYTIME AT ALL                      LET IT BE                
    ##  9 BABY YOU'RE A RICH MAN              REPRISE / DAY IN THE LIFE
    ## 10 BACK IN THE U.S.S.R.                REVOLUTION               
    ## # … with 90 more rows

Now, Define the function to get the mostr likely next song using the
above data set.

``` r
# Function to get the next song
get_next_song <- function(song){
  if(!toupper(song) %in% next_song_final$song){
    return("Song not found in database!")
  }
  return(next_song_final$next_song[next_song_final$song == toupper(song)])
}

# Test cases
get_next_song("Eight Days A Week")
```

    ## [1] "COME TOGETHER"

``` r
get_next_song("XXXXXXX")
```

    ## [1] "Song not found in database!"

# Question 5

Launching song recommendation system to an existing product is a major
change and is likely to introduce lot of UI changes. When testing the
song recommendation system, It is important to isolate the effect of UI
changes from the overall change in metric before and after introducing
the recommender, to know the goodness of recommender algorithm. To
accomplish this we test 3 versions.

V1 - original product with out recommendation. V2 - with recommendation
and associated UI changes (recommendation based on random guess or a
very rudimentary model). V3 - With recommendation based on the built
algorithm and associated UI changes.

Perform multiple A/B testing on three versions as follows.

  - First estimate the number days the test needs to be run for the
    desired effect size, p-value and statistical power. P-value should
    be corrected using Boniferroni correction as multiple tests are
    involved.
  - Randomly split users into three groups, Each group is shown one of
    the three versions to be tested.
  - Collect data on “average number of play events per user per day” for
    all three groups.
  - Here are the hypotheses to be tested.
      - H0 : No difference in the metric across the groups.
      - H1 : There is a difference in the metric between the groups.
  - After test period, perform T-test on each pair of the groups and
    check if you can reject or fail to reject H0 at adjusted p-value
    (employing Boniferroni correction) and judge the effect of
    recommendation algorithm.
