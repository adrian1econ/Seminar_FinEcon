rm(list=ls())
library(plm)
library(tidyverse)
library(lubridate)
library(broom)
library(xts)

load("panel.Rdata")

################################################################################
# Static & Moving Average - Models
################################################################################

# 1. Choose Variables
panel_ma <- panel %>% 
        select(t, Index,rv_20ahead, rv, rv_lr)


# 2. Static Model (expanding sample average of realized RV)
panel_ma <- panel_ma %>% 
        group_by(Index) %>% 
        mutate(static=lag(rv_lr,1))



# 3. Calculate Moving Average (21-Day RV) 
panel_ma <- panel_ma %>% 
        group_by(Index) %>%
        mutate(ma_21 = rollapply(data = rv,
                                      width=list(c(-1:-21)),
                                      FUN=mean,
                                      align="right",
                                      na.rm=TRUE,
                                      fill=NA))

panel_ma_r2 <- panel_ma %>% 
        drop_na() %>% 
        group_by(Index) %>%
        mutate(y2=(rv_20ahead-mean(rv_20ahead))^2,
               e2=(rv_20ahead-ma_21)^2) %>% 
        summarize(tss=sum(y2),
                  rss=sum(e2),
                  r2=1-(rss/tss))

ma_r2 <- mean(panel_ma_r2$r2)

# Test
mean(panel_ma[1:21,]$rv)
