rm(list=ls())
library(tidyverse)
library(lubridate)
library(xts)


# Import:
data <- read_csv('oxfordmanrealizedvolatilityindices.csv')
data <- data %>% 
        mutate(t=as.Date(time)) %>%
        select(t, time, everything())
      

# Final Data ###########################################################

# 1. Choose necessary variables
panel <- data %>% 
        select(t, Symbol, rv5_ss) %>% 
        rename(Index=Symbol,rv=rv5_ss) %>% 
        arrange(Index, t)

# 2. Calculate expanding means by Index (R_lr)
panel <- panel %>% 
        group_by(Index) %>% 
        mutate(rv_lr=cummean(rv))

# 3. Calculate (Realized Volatility)/(Expanding Mean)
panel <- panel %>% 
        mutate(rv_sc=rv/rv_lr)


# 4. Create global Factor
panel <- panel %>% 
        group_by(t) %>% 
        mutate(global=mean(rv_sc))

# 5. Scale back global Factor to asset's own level of volatility
panel <- panel %>% 
        mutate(glrv=global*rv_lr)


# 6. Create dependent variable: Realized Variance over 20-days (uncentered & centered)
panel <- panel %>% 
        group_by(Index) %>%
        mutate(rv_20ahead = rollapply(data = rv,
                                 width=list(c(1:20)),
                                 FUN=mean,
                                 align="left",
                                 na.rm=TRUE,
                                 fill=NA))

panel <- panel %>% 
        mutate(rv_20ahead_c = rv_20ahead - rv_lr)

# 7. HAR Model
# 7.1 One Lag of Realized Volatility
panel <- panel %>% 
        group_by(Index) %>% 
        mutate(har_1=lag(rv))


# 7.2 Weekly (5-day) Average Realized Volatility
panel <- panel %>% 
        group_by(Index) %>% 
        mutate(har_5 = rollapply(data = rv,
                                      width=list(c(-1:-5)),
                                      FUN=mean,
                                      align="right",
                                      na.rm=TRUE,
                                      fill=NA))


# 7.3 Monthly (20-day) Average Realized Volatility
panel <- panel %>% 
        group_by(Index) %>% 
        mutate(har_20 = rollapply(data = rv,
                                 width=list(c(-1:-20)),
                                 FUN=mean,
                                 align="right",
                                 na.rm=TRUE,
                                 fill=NA))

# 8. Centered HAR Model
panel <- panel %>% 
        mutate(har_1_c = har_1 - rv_lr,
               har_5_c = har_5 - rv_lr,
               har_20_c = har_20 - rv_lr)



# 9. HExp Models: Create Factors
# 9.1 Rates of Decay
lambda_1 <- log(1+1/1)
lambda_5 <- log(1+1/5)
lambda_25 <- log(1+1/25)
lambda_125 <- log(1+1/125)





# 9.2 Create ExpRV
ExpRV_fun <- function(x,lambda,iter){
        sum_w <- sum(exp(-iter*lambda))
        w <- exp(-iter*lambda)/sum_w
        
        sum(w*x)
}

# 9.2.1 ExpRV (1)
panel <- panel %>% 
        group_by(Index) %>% 
        mutate(exp_1 = rollapply(data = rv,
                                  width=list(c(0:-499)),
                                  FUN=ExpRV_fun,
                                  iter=-500:-1,
                                  lambda=lambda_1,
                                  align="right",
                                  fill=NA))

# 9.2.2 ExpRV (5)
panel <- panel %>% 
        group_by(Index) %>% 
        mutate(exp_5 = rollapply(data = rv,
                                 width=list(c(0:-499)),
                                 FUN=ExpRV_fun,
                                 iter=-500:-1,
                                 lambda=lambda_5,
                                 align="right",
                                 fill=NA))

# 9.2.3 ExpRV (25)
panel <- panel %>% 
        group_by(Index) %>% 
        mutate(exp_25 = rollapply(data = rv,
                                 width=list(c(0:-499)),
                                 FUN=ExpRV_fun,
                                 iter=-500:-1,
                                 lambda=lambda_25,
                                 align="right",
                                 fill=NA))

# 9.2.4 ExpRV (125)
panel <- panel %>% 
        group_by(Index) %>% 
        mutate(exp_125 = rollapply(data = rv,
                                  width=list(c(0:-499)),
                                  FUN=ExpRV_fun,
                                  iter=-1:-500,
                                  lambda=lambda_125,
                                  align="right",
                                  fill=NA))

# 9.3 Expglrv: Exponentially Weighted global Realised Volatility Factor (5-day Center-of-Mass)
# 9.2.2 ExpRV (5)
panel <- panel %>% 
        group_by(Index) %>% 
        mutate(expgl_5 = rollapply(data = glrv,
                                 width=list(c(0:-499)),
                                 FUN=ExpRV_fun,
                                 iter=-500:-1,
                                 lambda=lambda_5,
                                 align="right",
                                 fill=NA))


# 9.4 Centered HExp & HExpGl Models
panel <- panel %>% 
        mutate(exp1_c = exp_1 - rv_lr,
               exp5_c = exp_5 - rv_lr,
               exp25_c = exp_25 - rv_lr,
               exp125_c = exp_125 - rv_lr,
               expgl5_c = expgl_5 - rv_lr)


# 10. Check by index
index_names <- unique(panel$Index) 

for(i in index_names){
        name <- paste('index',i,sep = "")
        assign(name, panel %>% filter(Index==i)) 
}

# 11. Ungroup & Save
panel <- panel %>% 
    ungroup()

save(panel, file="panel.Rdata")

