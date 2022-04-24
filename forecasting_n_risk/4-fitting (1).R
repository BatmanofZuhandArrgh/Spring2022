#required libraries
library(fpp3)

#to read in data
setwd("~/Documents/BANA4090/R Scripts")
wm <- readRDS("weekly_walmart_sales.rds")

#to save
#saveRDS(wm, "test_object.rds")

#explore full dataset
wm %>% autoplot(Weekly_Sales)

wm %>% 
  group_by(id, Store, Dept) %>% 
  tally()

wm_93 <- wm %>% 
  filter(id == '1_93') %>% 
  select(id, Weekly_Sales) #will retain index even if not specified

#visualize dataset
wm_93 %>% autoplot(Weekly_Sales)

wm_93 %>% gg_season(Weekly_Sales)

wm_93 %>% ACF(Weekly_Sales)# %>% autoplot()

wm_93 %>% 
  model(stl = STL()) %>% 
  components() %>% 
  autoplot()

#fit model

tsm_fit <- wm_93 %>% 
  model(trend_model = TSLM(Weekly_Sales ~ trend()))

report(tsm_fit)

tsm_fit %>% 
  forecast(h = "21 weeks") %>% 
  autoplot(wm_93)

#residuals
augment(tsm_fit)
aug <- augment(tsm_fit)

#visualize fitted values
aug %>% 
  ggplot(aes(x= Date)) +
  geom_line(aes(y = Weekly_Sales, color = "Data")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  scale_color_manual(values = c(Data = "Black", Fitted = "Blue"))

tsm_fit %>% gg_tsresiduals()

sum(aug$.innov) #mean of residuals

#ljung-box for autocorrelation of residuals
glance(tsm_fit) #for degrees of freedom of model for Ljung-Box test

aug %>% features(.innov, ljung_box, lag = 10, dof = 2)
#p-value here shows that residuals are autocorrelated

