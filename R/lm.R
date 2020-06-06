library(dplyr)
library(caret)
library(ggplot2)
library(lubridate)
library(readxl)

zip_daily = read_excel("data_updated/zipcode_daily_cases_social-distance_population_income.xlsx")

# Exploratory data analysis
zip_daily$date = as_date(zip_daily$date) 
zip_daily = zip_daily %>% group_by(ZIP) %>%
  arrange(date) %>%
  mutate(
    first = dplyr::first(date),
    last = dplyr::last(date)
  )

#ggplot2::ggplot(data = zip_daily, aes(x=first))+geom_bar()
#ggplot2::ggplot(data = zip_daily, aes(x=last))+geom_bar()

#ggplot2::ggplot(data = zip_daily, aes(x=date,y=confirmed_cases, color=ZIP))+geom_line()

# making new income dummy features
zip_daily$low_income = 0
zip_daily$low_income = (zip_daily$Income_level==1)
zip_daily$middle_income = 0
zip_daily$middle_income = (zip_daily$Income_level==2)
zip_daily$high_income = 0
zip_daily$high_income = (zip_daily$Income_level==3)
# add stay_home_ratio as new feature
zip_daily$stay_home_ratio = zip_daily$completely_home_device_count / zip_daily$device_count
# no slash / in feature name pls!
colnames(zip_daily)[14:15] = c("cases_adjusted_by_pop","new_cases_adjusted_by_pop")
# get rid of tibble to run faster and avoid session abortion
zip_daily1 = as.data.frame(zip_daily)

# Model1: previous 5 days + today's mobility to predict today's Risk(new cases_pop_adjusted）

# making previous new_confirmed_cases features
n = 5
for(j in 1:n) {
  zip_daily1[[paste0("new",j,"day_ago")]] = 0
}
# write new_n_day_ago features
for(i in 1: nrow(zip_daily1)) {
  if(as.numeric(zip_daily1[i,2]) < as.numeric(as.Date("2020-03-21")))
    next
  
  zip = as.numeric(zip_daily1[i,1])
  print(i)
  for(j in 1:n) {
    pre_date = zip_daily1[i,2] - ddays(j)
    pre_rec = zip_daily1 %>%
      filter(date == pre_date,
             ZIP == zip)
    if(nrow(pre_rec) == 0) next
    zip_daily1[[paste0("new",j,"day_ago")]][i] = pre_rec$new_confirmed_cases
  }
}

zip_daily1 = zip_daily1 %>% filter(as.numeric(date) > as.numeric(as.Date("2020-03-21")))
zip_daily_scaled1 = scale(zip_daily1[,sapply(zip_daily1, is.numeric)])
zip_daily_scaled1[,3] = zip_daily1$new_confirmed_cases
zip_daily_scaled1 = as.data.frame(zip_daily_scaled1[,-1])

set.seed(7) # set for reproducibility
train.control = trainControl(method = "cv", number = 10) # 10-fold cross validation
lm.model1 = train(data = zip_daily_scaled1, 
                  new_confirmed_cases ~.-new_cases_adjusted_by_pop,
                  method = "leapSeq",
                  tuneGrid = data.frame(nvmax = 5:15),
                  trControl = train.control
                  )

## model1 performace
lm.model1$bestTune
lm.model1$results
model1.coef = data.frame(coef = coef(lm.model1$finalModel,6),
                         features = names(coef(lm.model1$finalModel,6)))
#summary(lm.model1$finalModel)
ggplot(model1.coef, aes(x=coef, y = features)) +
  geom_point() + geom_vline(xintercept = 0, colour="red") +
  theme_minimal()


# add potential outcomes (other definitions of Infectious Risk)
# Model2, predict new cases b/ 7+day and 11+day
zip_daily2 = as.data.frame(zip_daily)
# write new_n_day_after features
n = 11
for(j in 1:n) {
  zip_daily2[[paste0("new",j,"day_after")]] = 0
}

for(i in 1: nrow(zip_daily)) { # to row 10180
  if(as.numeric(zip_daily2[i,2]) > as.numeric(as.Date("2020-05-16")))
    next
  
  zip = as.numeric(zip_daily2[i,1])
  print(i)
  for(j in 1:n) {
    fut_date = zip_daily2[i,2] + ddays(j)
    fut_rec = zip_daily2 %>%
      filter(date == fut_date,
             ZIP == zip)
    if(nrow(fut_rec) == 0) next
    zip_daily2[[paste0("new",j,"day_after")]][i] = fut_rec$new_confirmed_cases
  }
}

zip_daily2 = zip_daily2 %>% filter(as.numeric(date) < as.numeric(as.Date("2020-05-16")))
zip_daily2$ave_new6_9after = apply(zip_daily2[,29:32],1,mean)
zip_daily2$ave_new7_10after = apply(zip_daily2[,30:33],1,mean)
zip_daily2$ave_new8_11after = apply(zip_daily2[,31:34],1,mean)

# save and load the dataset with mobility data padded
zip_daily2 = read.csv("~/Desktop/zipcode_daily_with_1_11_future_day_case_count.csv")
# predict 7-10
zip_daily2 = zip_daily2 %>%
  filter(as.numeric(as_date(zip_daily2$date)) < as.numeric(as_date("2020-05-16")))
zip_daily_scaled2 = scale(zip_daily2[,sapply(zip_daily2, is.numeric)])
zip_daily_scaled2[,38] = zip_daily2$ave_new7_10after
zip_daily_scaled2 = as.data.frame(zip_daily_scaled2)
zip_daily_scaled2 = zip_daily_scaled2 %>% 
  select(-c("X","ZIP", "new7day_after","new8day_after","new9day_after",
            "new10day_after","new11day_after","ave_new6_9after","ave_new8_11after"))
  


set.seed(1) # set for reproducibility
train.control = trainControl(method = "cv", number = 10) # 10-fold cross validation
lm.model2 = train(data = zip_daily_scaled2, 
                  ave_new7_10after ~.,
                  method = "leapSeq",
                  tuneGrid = data.frame(nvmax = 3:30),
                  trControl = train.control
)

## model2 performace
lm.model2$bestTune
lm.model2$results # MSE 3.337, R-sq 0.6414457
model2.coef = data.frame(coef = coef(lm.model2$finalModel,25),
                         features = names(coef(lm.model2$finalModel,25)))

ggplot(model2.coef, aes(x=coef, y = features)) +
  geom_point() + geom_vline(xintercept = 0, colour="red") +
  theme_minimal()

y_yhat = data.frame(predict_case=predict(lm.model2), true_case=zip_daily_scaled2$ave_new7_10after)
ggplot(y_yhat, aes(x=predict_case,y=true_case)) +
  geom_point()
predict(lm.model2)
# write csv file for other model use
write.csv(zip_daily2,"zipcode_daily_with_1_11_future_day_case_count.csv")
