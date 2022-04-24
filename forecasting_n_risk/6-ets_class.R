library(fpp3)

#use below dataset
aus_holidays <- tourism %>% 
  filter(Purpose == "Holiday") %>% 
  summarise(Trips = sum(Trips)/1000)

autoplot(aus_holidays)

#fitting models
fit <- aus_holidays %>% 
  model(SES = ETS(Trips ~ error("A") + trend("N") + season("N")),
        Holt = ETS(Trips ~ error("A") + trend("A") + season("N")),
        Damped = ETS(Trips ~ error("A") + trend("Ad") + season("N")))

#review fitted models (AICc)
glance(fit)

#forecast 3 years out and plot
fc <- fit %>% forecast(h = "3 years")

fc %>% autoplot(aus_holidays, level = NULL)

#cross-validation for accuracy
fit_cv <- aus_holidays %>% 
  stretch_tsibble(.init = 10) %>% 
  model(SES = ETS(Trips ~ error("A") + trend("N") + season("N")),
        Holt = ETS(Trips ~ error("A") + trend("A") + season("N")),
        Damped = ETS(Trips ~ error("A") + trend("Ad") + season("N")),
        HW = ETS(Trips ~ error("A") + trend("A") + season("A")))

fit_cv %>% forecast(h = 2) %>% accuracy(aus_holidays)

#create training and test set
train <- aus_holidays %>% 
  filter(year(Quarter) < '2016')

test <- aus_holidays %>% 
  filter(year(Quarter) >= '2016')

#Holt-Winters' model
hw <- train %>% 
  model(
        HW = ETS(Trips ~ error("A") + trend("A") + season("A")))

#review estimated parameters
report(hw)

fc_hw <- hw %>% forecast(h = "2 years")

fc_hw %>% autoplot(aus_holidays, level = NULL)

#accuracy of test set
fc_hw %>% accuracy(aus_holidays)

#use AICc to find me a model and also check out some other models
models <- train %>% 
  model(
    AAA = ETS(Trips ~ error("A") + trend("A") + season("A")),
    AAM = ETS(Trips ~ error("A") + trend("A") + season("M")),
    MAA = ETS(Trips ~ error("M") + trend("A") + season("A")),
    aicc = ETS(Trips))

glance(models)

fc<- models %>% forecast(h = "2 years")

fc %>% autoplot(aus_holidays, level = NULL)

fc %>% accuracy(aus_holidays)

#AAM model performs best on test data
final_model <- train %>% 
  model(AAM = ETS(Trips ~ error("A") + trend("A") + season("M")))

report(final_model)

fc<- final_model %>% forecast(h = "2 years")

fc %>% autoplot(aus_holidays, level = NULL)

fc %>% accuracy(aus_holidays)
