library(statsr)
library(tidyverse)
library(ggplot2)
library(GGally)


load("movies.Rdata")

head(movies)
str(movies)
names(movies)

summary(movies)

There are some missing values in runtime, dvd_rel_year, dvd_rel_month, dvd_rel_day        

movies_set <- movies %>% 
  filter(title != "Godzilla" & !is.na(runtime) & !is.na(dvd_rel_year)) %>% 
  mutate(thtr_rel_day_period = as.factor(if_else(thtr_rel_day >= 1 & thtr_rel_day <= 10, "early",
                              if_else(thtr_rel_day >=11 & thtr_rel_day <= 20, "mid", "late"))),
         dvd_rel_day_period  = as.factor(if_else(dvd_rel_day >= 1 & dvd_rel_day <= 10, "early",
                              if_else(dvd_rel_day >=11 & dvd_rel_day <= 20, "mid", "late"))),
         thtr_rel_month = as.factor(as.character(thtr_rel_month)),
         dvd_rel_month  = as.factor(as.character(dvd_rel_month))
         ) %>% 
  select(audience_score, runtime, imdb_rating, imdb_num_votes, critics_score, 
         thtr_rel_month, thtr_rel_day_period, dvd_rel_month, dvd_rel_day_period,
         title_type, genre, mpaa_rating, critics_rating, 
         best_pic_nom, best_pic_win, best_actor_win, 
         best_actress_win, best_dir_win, top200_box)

dim(movies_set)
summary(movies_set)
str(movies_set)
head(movies_set)

hist(movies_set$audience_score)
qqnorm(movies_set$audience_score)
qqline(movies_set$audience_score)

ggpairs(movies_set, columns = c(1:5))

There are strong correleation between audience_score and imdb_rating.
And critics_score also show strong correlation with audience_score.
However There are strong correleation between imdb_rating and critics_score.
Therefore, I just pick imdb_rating which shows stronger correlation with audience_score and drop the critics_score due to collinearity.

ggpairs(movies_set, columns = c(1,6:9))

ggpairs(movies_set, columns = c(1,10:13))

ggpairs(movies_set, columns = c(1,14:16))

ggpairs(movies_set, columns = c(1,17:19))


movies_set %>% ggplot(aes(audience_score))+
  geom_histogram()

movies_set %>% ggplot(aes(imdb_rating, audience_score))+
  geom_point()+
  geom_smooth(method = lm, se=FALSE)

movies_set %>% ggplot(aes(critics_score, audience_score))+
  geom_point()+
  geom_smooth(method = lm, se=FALSE)

movies_set %>% ggplot(aes(imdb_rating, critics_score))+
  geom_point()+
  geom_smooth(method = lm, se=FALSE)


#modeling

movies_score_1 <- lm(audience_score ~ runtime + imdb_rating + imdb_num_votes + critics_score +
                   thtr_rel_month + thtr_rel_day_period + dvd_rel_month + dvd_rel_day_period +
                   title_type + genre + mpaa_rating + critics_rating +
                   best_pic_nom + best_pic_win + best_actor_win +
                   best_actress_win + best_dir_win + top200_box, data = movies_set)
summary(movies_score_1)

For model selection, I will choose backwards elimination using the adjusted R squared method.


movies_score_2 <- lm(audience_score ~ runtime + imdb_rating + imdb_num_votes + 
                       thtr_rel_month + thtr_rel_day_period + dvd_rel_month + dvd_rel_day_period +
                       title_type + genre + mpaa_rating + critics_rating +
                       best_pic_nom + best_pic_win + best_actor_win +
                       best_actress_win + best_dir_win + top200_box, data = movies_set)
summary(movies_score_1)

# ...

movies_score_12 <- lm(audience_score ~ runtime + imdb_rating + 
                       dvd_rel_month + 
                       genre + critics_rating +
                       best_pic_nom  + 
                       best_actress_win, data = movies_set)
summary(movies_score_12)

movies_score_13 <- lm(audience_score ~ runtime + imdb_rating + 
                        dvd_rel_month + 
                        genre + critics_rating +
                        best_pic_nom, data = movies_set)
summary(movies_score_13)


# movies_score_12 = movies_model
movies_model <- lm(audience_score ~ runtime + imdb_rating + 
                     dvd_rel_month + 
                     genre +  critics_rating +
                     best_pic_nom + best_actress_win , data = movies_set)
summary(movies_model)

movies_model_set <- movies %>% select(runtime, imdb_rating,
                                        dvd_rel_month, 
                                        genre, critics_rating,
                                        best_pic_nom, best_actress_win)

ggpairs(movies_model_set, columns = c(1:7))

head(movies_model_set)
summary(movies_model_set)


summary(movies$audience_score)
str(movies_set)
str(newprof)




movies_predict <- movies %>% 
  filter(title == "Godzilla") %>% 
  mutate(dvd_rel_month  = as.factor(as.character(dvd_rel_month))) %>% 
  select(runtime, imdb_rating, dvd_rel_month, genre, critics_rating, best_pic_nom, best_actress_win)

predict(movies_model, movies_predict)

predict(movies_model, movies_predict, interval = "prediction", level = 0.95)

movies %>% filter(title == "Godzilla") %>% select(audience_score)

plot(movies_model$residuals ~ movies_set$runtime)
plot(movies_model$residuals ~ movies_set$imdb_rating)

hist(movies_model$residuals)
qqnorm(movies_model$residuals)
qqline(movies_model$residuals)

plot(movies_model$residuals ~ movies_model$fitted)
plot(abs(movies_model$residuals) ~ movies_model$fitted)


