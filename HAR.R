rm(list=ls())
library(plm)
library(tidyverse)
library(lubridate)
library(broom)
library(xts)

load("panel.Rdata")

################################################################################
# HAR-Models
################################################################################

# 1. Choose Variables
panel_har <- panel %>% 
                select(t, Index, rv_lr, rv_20ahead, har_1, har_5, har_20)
                

panel_har_c <- panel %>% 
        select(t, Index, rv_lr, rv_20ahead, rv_20ahead_c, har_1_c, har_5_c, har_20_c)
        




# 2. In-Sample Forecasting
# 2.1 Asset-by-Asset


har_model_is <- panel_har %>% 
                group_by(Index) %>% 
                do(model = lm(rv_20ahead ~ har_1 + har_5 + har_20, data=.))

har_model_is_output <- tidy(har_model_is,model)
har_model_is_output


panel_har <- panel_har %>%
        group_by(Index) %>% 
        nest %>% 
        inner_join(har_model_is) %>% 
        mutate(har_is = map2(model, data, predict)) %>% 
        select(-model) %>% 
        unnest(c(data, har_is))
                
panel_har_r2 <- panel_har %>% 
        drop_na() %>% 
        group_by(Index) %>%
        mutate(y2=(rv_20ahead-mean(rv_20ahead))^2,
               e2=(rv_20ahead-har_is)^2) %>%
        summarize(tss=sum(y2),
                  rss=sum(e2),
                  r2=1-(rss/tss))

har_r2 <- mean(panel_har_r2$r2)



# 2.2 Panel Estimation
har_model_c <- plm(rv_20ahead_c ~ har_1_c + har_5_c + har_20_c,
                   data = panel_har_c,
                   index = c("Index","t"),
                   model = "within")

# Extract Fixed Effects & Coefficients
summary(har_model_c)
hexp_c_r2 <- summary(har_model_c)
hexp_c_r2 <- hexp_c_r2$r.squared[1]

summary(har_model_c)
fixed <- tibble(Index=names(fixef(har_model_c)), fixef = as.numeric(fixef(har_model_c)))

# Comparison gives same coefficients
har_model_c_1 <- lm(rv_20ahead_c ~ har_1_c + har_5_c + har_20_c +factor(Index),
                   data = panel_har_c)

summary(har_model_c_1)


# Prediction Strategy: Match differing Intercepts to Index, Multiply factors with coefficients, add Intercepts
panel_har_c <- panel_har_c %>%
        left_join(fixed, by=c("Index"="Index")) %>% 
        mutate(har_panel_c = fixef+har_1_c*har_model_c$coefficients[1]+har_5_c*har_model_c$coefficients[2]+har_20_c*har_model_c$coefficients[3],
               har_panel = har_panel_c + rv_lr)


# Test
fitted <- har_model_c$model[ , 1] - har_model_c$residuals
fitted

