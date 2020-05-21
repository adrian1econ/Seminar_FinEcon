rm(list=ls())
library(tidyverse)
library(lubridate)
library(xts)


# Import:
data <- read_csv('oxfordmanrealizedvolatilityindices.csv')
data <- data %>% 
        mutate(t=as.Date(time)) %>%
        select(t, time, everything())
      


# Index Series
index_names <- unique(data$Symbol)

for(i in index_names){
        name <- paste('index',i,sep = "")
        assign(name, data %>% filter(Symbol==i)) 
}



# Final Data ###########################################################

# 1. Choose necessary variables
panel <- data %>% 
        select(t, Symbol, rv5_ss) %>% 
        rename(Index=Symbol,rv=rv5_ss) %>% 
        arrange(Index, t)

# 2. Calculate expanding means by Index
panel <- panel %>% 
        group_by(Index) %>% 
        mutate(rv_lr=cummean(rv))

# 3. Calculate (Realized Volatility)/(Expanding Mean)
panel <- panel %>% 
        mutate(rv_sc=rv/rv_lr)


# 4. Create Global Factor
panel <- panel %>% 
        group_by(t) %>% 
        mutate(Global=mean(rv_sc))

# 5. Scale back Global Factor to asset's own level of volatility
panel <- panel %>% 
        mutate(GlRV=Global*rv_lr)

# Test ##############################################################


test <- panel %>% 
        filter(t<='2000-01-07')





