rm(list=ls())
library(plm)
library(tidyverse)
library(lubridate)
library(broom)
library(xts)

load("panel.Rdata")

################################################################################
# HExp-Models
################################################################################

# 1. Choose Variables
panel_hexp <- panel %>% 
        select(t, Index, rv_lr, rv_20ahead, exp_1, exp_5, exp_25, exp_125)

panel_hexp_c <- panel %>% 
        select(t, Index, rv_lr, rv_20ahead, rv_20ahead_c, exp1_c, exp5_c, exp25_c, exp125_c)



# 2. In-Sample Forecasting
# 2.1 Asset-by-Asset


hexp_model_is <- panel_hexp %>% 
        group_by(Index) %>% 
        do(model = lm(rv_20ahead ~ exp_1 + exp_5 + exp_25 + exp_125, data=.))


hexp_model_is_output <- tidy(hexp_model_is, model)
hexp_model_is_output


panel_hexp <- panel_hexp %>%
        group_by(Index) %>% 
        nest %>% 
        inner_join(hexp_model_is) %>% 
        mutate(hexp_is = map2(model, data, predict)) %>% 
        select(-model) %>% 
        unnest(c(data, hexp_is))


panel_hexp_r2 <- panel_hexp %>% 
        drop_na() %>% 
        group_by(Index) %>%
        mutate(y2=(rv_20ahead-mean(rv_20ahead))^2,
               e2=(rv_20ahead-hexp_is)^2) %>%
        summarize(tss=sum(y2),
                  rss=sum(e2),
                  r2=1-(rss/tss))

hexp_r2 <- mean(panel_hexp_r2$r2)
        
       


# 2.2 Panel Estimation

hexp_model_c <- plm(rv_20ahead_c ~ exp1_c + exp5_c + exp25_c + exp125_c,
                   data = panel_hexp_c,
                   index = c("Index","t"),
                   model = "within")

# Extract Fixed Effects & Coefficients
summary(hexp_model_c)
hexp_c_r2 <- summary(hexp_model_c)
hexp_c_r2 <- hexp_c_r2$r.squared[1]

fixed <- tibble(Index=names(fixef(hexp_model_c)), fixef = as.numeric(fixef(hexp_model_c)))

# Comparison gives same coefficients
hexp_model_c_1 <- lm(rv_20ahead_c ~ exp1_c + exp5_c + exp25_c + exp125_c + factor(Index),
                    data = panel_hexp_c)

summary(hexp_model_c_1)


# Prediction Strategy: Match differing Intercepts to Index, Multiply factors with coefficients, add Intercepts
panel_hexp_c <- panel_hexp_c %>%
        left_join(fixed, by=c("Index"="Index")) %>% 
        mutate(hexp_panel_c = fixef+exp1_c*hexp_model_c$coefficients[1]+exp5_c*hexp_model_c$coefficients[2]+exp25_c*hexp_model_c$coefficients[3]+exp125_c*hexp_model_c$coefficients[4],
               hexp_panel = hexp_panel_c + rv_lr)


# Test
fitted <- hexp_model_c$model[ , 1] - hexp_model_c$residuals
fitted




