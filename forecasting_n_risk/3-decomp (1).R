x <- seq(1:100)
y <- log(x) #tells you simply what exponent is needed to make a certain #

exp(1)^y

plot(x)
plot(y)

#log transform with dataset
library(fpp3)

food <- aus_retail %>%
  filter(Industry == "Food retailing") %>%
  summarise(Turnover = sum(Turnover))

food

food %>% autoplot(log(Turnover))

a10 <- PBS %>% 
  filter(ATC2 == "A10") %>% 
  select(Month, Concession, Type, Cost) %>% 
  summarise(Total_Cost = sum(Cost))

#plot Total_Cost. What do you notice about the variability?
a10 %>% autoplot()

#use the Guerrero feature from Features() to estimate lambda for a Box-Cox transform
lambda <- a10 %>% 
  features(Total_Cost, features = guerrero) %>% 
  pull(lambda_guerrero)

lambda

#plot the Box-Cox transformation with the lambda value retrieved. Does it
#stablize variation?
a10 %>% 
  model(
  STL(Total_Cost)) %>%
  components() %>%
  autoplot()

a10 %>% 
  model(
    STL(box_cox(Total_Cost, lambda))) %>% 
  components() %>% 
  autoplot()
  

#moving averages
aus_exports <- global_economy %>%
  filter(Country == "Australia") %>%
  mutate(
    `5-MA` = slider::slide_dbl(Exports, mean,
                               .before = 2, .after = 2, .complete = TRUE)
  )

aus_exports %>%
  autoplot(Exports) +
  geom_line(aes(y = `5-MA`), colour = "#D55E00") +
  labs(y = "% of GDP",
       title = "Total Australian exports") +
  guides(colour = guide_legend(title = "series"))

#coding a 3x3-MA of above and show table
aus_exports <- global_economy %>%
  filter(Country == "Australia") %>%
  mutate(
    `3-MA` = slider::slide_dbl(Exports, mean,
                               .before = 1, .after = 1, .complete = TRUE),
    `3x3-MA` = slider::slide_dbl(`3-MA`, mean,
                                  .before = 1, .after = 1, .complete = TRUE)
  )

aus_exports

#classical decomposition
us_retail_employment <- us_employment %>%
  filter(year(Month) >= 1990, Title == "Retail Trade") %>%
  select(-Series_ID)

us_retail_employment %>% autoplot()

us_retail_employment %>%
  model(
    classical_decomposition(Employed, type = "additive")
  ) %>%
  components() %>%
  autoplot() +
  labs(title = "Classical additive decomposition of total
                  US retail employment")


#STL
#discussion - how do the arguments for STL impact the plots
us_retail_employment %>%
  model(
    STL(Employed ~ trend(window = 7) +
          season(window = "periodic"),
        robust = TRUE)) %>%
  components() %>%
  autoplot()



#features

tourism %>%
  features(Trips, feat_stl)

tourism %>%
  features(Trips, feat_stl) %>%
  ggplot(aes(x = trend_strength, y = seasonal_strength_year,
             col = Purpose)) +
  geom_point() +
  facet_wrap(vars(State))

tourism %>% 
  features(Trips, list(mean = mean)) %>% 
  arrange(mean)

#lab 3
aus_retail
supermkt <- aus_retail %>% 
  filter(Industry == "Supermarket and grocery stores") %>% 
  summarise(Turnover = sum(Turnover))

supermkt %>% autoplot()

lambda <- supermkt %>% 
  features(Turnover, guerrero) %>% 
  pull(lambda_guerrero)

supermkt %>% 
  autoplot(box_cox(Turnover, lambda))

supermkt %>%
  model(
    STL(Turnover)) %>%
  components() %>%
  autoplot()

supermkt %>%
  model(
    STL(box_cox(Turnover, lambda))) %>%
  components() %>%
  autoplot()

supermkt %>%
  model(
    STL(box_cox(Turnover, lambda) ~ trend(window = 30),
        robust = TRUE)
        ) %>%
  components() %>%
  autoplot()


# Question
# What is the impact the box_cox transformation makes on the supermkt
# STL decomposition

#bonus - our first forecast

fit_dcmp <- supermkt %>%
  model(stlf = decomposition_model(
    STL(box_cox(Turnover, lambda) ~ trend(window = 7), robust = TRUE),
    NAIVE(season_adjust)
  ))

fit_dcmp %>%
  forecast() %>%
  autoplot(supermkt)+
  labs(y = "Number of people",
       title = "US retail employment")
  
