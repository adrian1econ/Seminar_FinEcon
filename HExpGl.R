rm(list=ls())
library(plm)
library(tidyverse)
library(lubridate)
library(broom)
library(xts)

load("panel.Rdata")

################################################################################
# HExpGl - Models
################################################################################

# 1. Choose Variables
panel_hexpgl <- panel %>% 
        select(t, Index, rv_lr,rv_20ahead, exp_1, exp_5, exp_25, exp_125, expgl_5)

panel_hexpgl_c <- panel %>% 
        select(t, Index, rv_lr,rv_20ahead, rv_20ahead_c, exp1_c, exp5_c, exp25_c, exp125_c, expgl5_c)


# 2. In-Sample Forecasting
# 2.1 Asset-by-Asset


hexpgl_model_is <- panel_hexpgl %>% 
        group_by(Index) %>% 
        do(model = lm(rv_20ahead ~ exp_1 + exp_5 + exp_25 + exp_125 + expgl_5, data=.))

hexpgl_model_is_output <- tidy(hexpgl_model_is, model)
hexpgl_model_is_output


panel_hexpgl <- panel_hexpgl %>%
        group_by(Index) %>% 
        nest %>% 
        inner_join(hexpgl_model_is) %>% 
        mutate(hexpgl_is = map2(model, data, predict)) %>% 
        select(-model) %>% 
        unnest(c(data, hexpgl_is)) 

panel_hexpgl_r2 <- panel_hexpgl %>% 
        drop_na() %>% 
        group_by(Index) %>%
        mutate(y2=(rv_20ahead-mean(rv_20ahead))^2,
               e2=(rv_20ahead-hexpgl_is)^2) %>%
        summarize(tss=sum(y2),
                  rss=sum(e2),
                  r2=1-(rss/tss))

hexpgl_r2 <- mean(panel_hexpgl_r2$r2)

summary(hexpgl_model_is$model[[10]])



# 2.2 Panel Estimation

hexpgl_model_c <- plm(rv_20ahead_c ~ exp1_c + exp5_c + exp25_c + exp125_c + expgl5_c,
                    data = panel_hexpgl_c,
                    index = c("Index","t"),
                    model = "within")

# Extract Fixed Effects & Coefficients
summary(hexpgl_model_c)
hexpgl_c_r2 <- summary(hexpgl_model_c)
hexpgl_c_r2 <- hexpgl_c_r2$r.squared[1]


fixed <- tibble(Index=names(fixef(hexpgl_model_c)), fixef = as.numeric(fixef(hexpgl_model_c)))

# Comparison gives same coefficients
hexpgl_model_c_1 <- lm(rv_20ahead_c ~ exp1_c + exp5_c + exp25_c + exp125_c + expgl_5_c + factor(Index),
                     data = panel_hexpgl_c)

summary(hexpgl_model_c_1)


# Prediction Strategy: Match differing Intercepts to Index, Multiply factors with coefficients, add Intercepts
panel_hexpgl_c <- panel_hexpgl_c %>%
        left_join(fixed, by=c("Index"="Index")) %>% 
        mutate(hexpgl_panel_c = fixef+exp1_c*hexpgl_model_c$coefficients[1]+exp5_c*hexpgl_model_c$coefficients[2]+exp25_c*hexpgl_model_c$coefficients[3]+exp125_c*hexpgl_model_c$coefficients[4]+expgl_5_c*hexpgl_model_c$coefficients[5],
               hexpgl_panel = hexpgl_panel_c + rv_lr)


# Test
fitted <- hexpgl_model_c$model[ , 1] - hexpgl_model_c$residuals




