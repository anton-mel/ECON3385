# Set up packages
setwd('C:/Users/ch2229/CH_Research Dropbox/Charles Hodgson/Teaching/MMP/lectures/5')
install.packages("stargazer") 
library(stargazer)
install.packages("AER") 
library(AER)
install.packages("tidyr") 
library(tidyr)
library(dplyr)
install.packages("broom")
library(broom)
library(ggplot2)


# Load and set up data
data_good <- read.csv("hausman_data.csv")
data_bad <- read.csv("hausman_data_bad.csv")


# First we need to reshape the data
data_good_wide <- data_good |>
  pivot_wider(
    id_cols    = c(city, time),     # invariant columns included here
    names_from = prod,
    values_from = c(price, qty),
    names_glue = "{.value}{prod}"
  )

data_bad_wide <- data_bad |>
  pivot_wider(
    id_cols    = c(city, time),     # invariant columns included here
    names_from = prod,
    values_from = c(price, qty),
    names_glue = "{.value}{prod}"
  )


# Now we generate the Hausman instruments
# Good data
data_good_wide <- data_good_wide %>%
  group_by(time) %>%
  mutate(otherprice1 = sum(price1) - price1) %>%
  ungroup()
data_good_wide$otherprice1 <- data_good_wide$otherprice1/19

data_good_wide <- data_good_wide %>%
  group_by(time) %>%
  mutate(otherprice2 = sum(price2) - price2) %>%
  ungroup()
data_good_wide$otherprice2 <- data_good_wide$otherprice2/19

data_good_wide <- data_good_wide %>%
  group_by(time) %>%
  mutate(otherprice3 = sum(price3) - price3) %>%
  ungroup()
data_good_wide$otherprice3 <- data_good_wide$otherprice3/19

data_good_wide <- data_good_wide %>%
  group_by(time) %>%
  mutate(otherprice4 = sum(price4) - price4) %>%
  ungroup()
data_good_wide$otherprice4 <- data_good_wide$otherprice4/19

data_good_wide <- data_good_wide %>%
  group_by(time) %>%
  mutate(otherprice5 = sum(price5) - price5) %>%
  ungroup()
data_good_wide$otherprice5 <- data_good_wide$otherprice5/19

# Bad data
data_bad_wide <- data_bad_wide %>%
  group_by(time) %>%
  mutate(otherprice1 = sum(price1) - price1) %>%
  ungroup()
data_bad_wide$otherprice1 <- data_bad_wide$otherprice1/19

data_bad_wide <- data_bad_wide %>%
  group_by(time) %>%
  mutate(otherprice2 = sum(price2) - price2) %>%
  ungroup()
data_bad_wide$otherprice2 <- data_bad_wide$otherprice2/19

data_bad_wide <- data_bad_wide %>%
  group_by(time) %>%
  mutate(otherprice3 = sum(price3) - price3) %>%
  ungroup()
data_bad_wide$otherprice3 <- data_bad_wide$otherprice3/19

data_bad_wide <- data_bad_wide %>%
  group_by(time) %>%
  mutate(otherprice4 = sum(price4) - price4) %>%
  ungroup()
data_bad_wide$otherprice4 <- data_bad_wide$otherprice4/19

data_bad_wide <- data_bad_wide %>%
  group_by(time) %>%
  mutate(otherprice5 = sum(price5) - price5) %>%
  ungroup()
data_bad_wide$otherprice5 <- data_bad_wide$otherprice5/19


# Run 2SLS Demand for Product 1
# Good Data
ivgood <- ivreg(qty1 ~ price1 + price2 + price3 + price4 + price5 | otherprice1 + otherprice2 + otherprice3 + otherprice4 + otherprice5, data=data_good_wide)

# Bad Data
ivbad <- ivreg(qty1 ~ price1 + price2 + price3 + price4 + price5 | otherprice1 + otherprice2 + otherprice3 + otherprice4 + otherprice5, data=data_bad_wide)

stargazer(ivgood,ivbad,type="text")

# Q: how many first stage regression are there for each equation?
# Check the 1st stage regressions for price1

fsgood <- lm(price1 ~ otherprice1 + otherprice2 + otherprice3 + otherprice4 + otherprice5, data=data_good_wide)
fsbad <- lm(price1 ~ otherprice1 + otherprice2 + otherprice3 + otherprice4 + otherprice5, data=data_bad_wide)

stargazer(fsgood,fsbad,type="text")
plot(data_good_wide$price1, data_good_wide$otherprice1 ,  col = "red")
plot(data_bad_wide$price1, data_bad_wide$otherprice1 ,  col = "blue")

##############################
# Backing out marginal costs #
##############################

# Save 2SLS coefficients
tidy_coefficients <- tidy(ivgood)
print(tidy_coefficients)
# Own price coefficient
print(tidy_coefficients$estimate[2])

# Generate markups
data_good_wide$markup1 <- -1*data_good_wide$qty1/tidy_coefficients$estimate[2]
hist(data_good_wide$markup1)
# Generate MC
data_good_wide$mc1 <- data_good_wide$price1 - data_good_wide$markup1  
hist(data_good_wide$mc1)
plot(data_good_wide$mc1, data_good_wide$price1 ,  col = "blue")

# Decompose national vs local variation
decomp <- lm(mc1 ~ factor(time), data=data_good_wide)
stargazer(decomp,type="text")
# compare residual SE to overall SD
sd(data_good_wide$mc1)

# Get MC for the "bad" data using the "good" data demand estimates
data_bad_wide$markup1 <- -1*data_bad_wide$qty1/tidy_coefficients$estimate[2]
data_bad_wide$mc1 <- data_bad_wide$price1 - data_bad_wide$markup1

# Decomposition for "bad" data
decomp <- lm(mc1 ~ factor(time), data=data_bad_wide)
stargazer(decomp,type="text")
# compare residual SE to overall SD
sd(data_bad_wide$mc1)


#######################
# Get all demand eqns #
#######################

ivgood1 <- ivreg(qty1 ~ price1 + price2 + price3 + price4 + price5 | otherprice1 + otherprice2 + otherprice3 + otherprice4 + otherprice5, data=data_good_wide)
tidy_coefficients1 <- tidy(ivgood1)
data_good_wide$resid1 <- residuals(ivgood1)

ivgood2 <- ivreg(qty2 ~ price1 + price2 + price3 + price4 + price5 | otherprice1 + otherprice2 + otherprice3 + otherprice4 + otherprice5, data=data_good_wide)
tidy_coefficients2 <- tidy(ivgood2)
data_good_wide$resid2 <- residuals(ivgood2)

ivgood3 <- ivreg(qty3 ~ price1 + price2 + price3 + price4 + price5 | otherprice1 + otherprice2 + otherprice3 + otherprice4 + otherprice5, data=data_good_wide)
tidy_coefficients3 <- tidy(ivgood3)
data_good_wide$resid3 <- residuals(ivgood3)

ivgood4 <- ivreg(qty4 ~ price1 + price2 + price3 + price4 + price5 | otherprice1 + otherprice2 + otherprice3 + otherprice4 + otherprice5, data=data_good_wide)
tidy_coefficients4 <- tidy(ivgood4)
data_good_wide$resid4 <- residuals(ivgood4)

ivgood5 <- ivreg(qty5 ~ price1 + price2 + price3 + price4 + price5 | otherprice1 + otherprice2 + otherprice3 + otherprice4 + otherprice5, data=data_good_wide)
tidy_coefficients5 <- tidy(ivgood5)
data_good_wide$resid5 <- residuals(ivgood5)

##############
# Get all mc #
##############

data_good_wide$markup1 <- -1*data_good_wide$qty1/tidy_coefficients1$estimate[2]
data_good_wide$mc1 <- data_good_wide$price1 - data_good_wide$markup1 

data_good_wide$markup2 <- -1*data_good_wide$qty2/tidy_coefficients2$estimate[3]
data_good_wide$mc2 <- data_good_wide$price2 - data_good_wide$markup2 

data_good_wide$markup3 <- -1*data_good_wide$qty3/tidy_coefficients3$estimate[4]
data_good_wide$mc3 <- data_good_wide$price3 - data_good_wide$markup3

data_good_wide$markup4 <- -1*data_good_wide$qty4/tidy_coefficients4$estimate[5]
data_good_wide$mc4 <- data_good_wide$price4 - data_good_wide$markup4

data_good_wide$markup5 <- -1*data_good_wide$qty5/tidy_coefficients5$estimate[6]
data_good_wide$mc5 <- data_good_wide$price5 - data_good_wide$markup5

# Make a copy of the dataset to do the cf on
data_orig <- data_good_wide
data_orig <- data_orig[1:10, ]
data_cf <- data_orig

############################
# Solving for Pass-thru CF #
############################

# Iterate prices until convergence (fixed point)
tol      <- 0.01          # convergence tolerance
max_iter <- 500           # safety cap
iter     <- 0
diff     <- Inf

while (iter < max_iter && diff > tol) {
  iter <- iter + 1
  
  # Update Q
  data_cf$qty1 <- predict(ivgood1, newdata=data_cf) + data_cf$resid1
  data_cf$qty2 <- predict(ivgood2, newdata=data_cf) + data_cf$resid2
  data_cf$qty3 <- predict(ivgood3, newdata=data_cf) + data_cf$resid3
  data_cf$qty4 <- predict(ivgood4, newdata=data_cf) + data_cf$resid4
  data_cf$qty5 <- predict(ivgood5, newdata=data_cf) + data_cf$resid5
  
  data_cf$price_new1 <- data_cf$mc1*1.1 -1*data_cf$qty1/tidy_coefficients1$estimate[2]
  data_cf$price_new2 <- data_cf$mc2*1 -1*data_cf$qty2/tidy_coefficients2$estimate[3]
  data_cf$price_new3 <- data_cf$mc3*1 -1*data_cf$qty3/tidy_coefficients3$estimate[4]
  data_cf$price_new4 <- data_cf$mc4*1 -1*data_cf$qty4/tidy_coefficients4$estimate[5]
  data_cf$price_new5 <- data_cf$mc5*1 -1*data_cf$qty5/tidy_coefficients5$estimate[6]
  
  # check convergence
  diff <- max(
    max(abs(data_cf$price_new1 - data_cf$price1), na.rm = TRUE),
    max(abs(data_cf$price_new2 - data_cf$price2), na.rm = TRUE),
    max(abs(data_cf$price_new3 - data_cf$price3), na.rm = TRUE),
    max(abs(data_cf$price_new4 - data_cf$price4), na.rm = TRUE),
    max(abs(data_cf$price_new5 - data_cf$price5), na.rm = TRUE),
    na.rm = TRUE
  )  

  
  # if not converged
  data_cf$price1 <- 0.1*data_cf$price_new1 + 0.9*data_cf$price1
  data_cf$price2 <- 0.1*data_cf$price_new2 + 0.9*data_cf$price2
  data_cf$price3 <- 0.1*data_cf$price_new3 + 0.9*data_cf$price3
  data_cf$price4 <- 0.1*data_cf$price_new4 + 0.9*data_cf$price4
  data_cf$price5 <- 0.1*data_cf$price_new5 + 0.9*data_cf$price5
}

data_cf$qty1 <- predict(ivgood1, newdata=data_cf) + data_cf$resid1
data_cf$qty2 <- predict(ivgood2, newdata=data_cf) + data_cf$resid2
data_cf$qty3 <- predict(ivgood3, newdata=data_cf) + data_cf$resid3
data_cf$qty4 <- predict(ivgood4, newdata=data_cf) + data_cf$resid4
data_cf$qty5 <- predict(ivgood5, newdata=data_cf) + data_cf$resid5

iter
diff

# Look at price changes
chg <- sapply(1:5, function(j) {
  mean((data_cf[[paste0("price", j)]] - data_orig[[paste0("price", j)]]) /
         data_orig[[paste0("price", j)]], na.rm = TRUE)
})

changes <- tibble(product = paste0("price", 1:5),
             mean_change = as.numeric(chg))

ggplot(changes, aes(x = product, y = mean_change)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  labs(title = "Mean % change in price",
       y = "Mean ( (cf - orig) / orig )",
       x = NULL)




###############################
# Solving for Merger CF 1 + 2 #
###############################

# Make a copy of the dataset to do the cf on
dtata_passthru <- data_cf
data_orig <- data_good_wide
data_orig <- data_orig[1:10, ]
data_cf <- data_orig


# Iterate prices until convergence (fixed point)
tol      <- 0.01          # convergence tolerance
max_iter <- 500           # safety cap
iter     <- 0
diff     <- Inf

while (iter < max_iter && diff > tol) {
  iter <- iter + 1
  
  # Update Q
  data_cf$qty1 <- predict(ivgood1, newdata=data_cf) + data_cf$resid1
  data_cf$qty2 <- predict(ivgood2, newdata=data_cf) + data_cf$resid2
  data_cf$qty3 <- predict(ivgood3, newdata=data_cf) + data_cf$resid3
  data_cf$qty4 <- predict(ivgood4, newdata=data_cf) + data_cf$resid4
  data_cf$qty5 <- predict(ivgood5, newdata=data_cf) + data_cf$resid5
  
  data_cf$price_new1 <- data_cf$mc1 -1*data_cf$qty1/tidy_coefficients1$estimate[2] - (data_cf$price2 - data_cf$mc2)*tidy_coefficients2$estimate[2]/tidy_coefficients1$estimate[2]
  data_cf$price_new2 <- data_cf$mc2 -1*data_cf$qty2/tidy_coefficients2$estimate[3] - (data_cf$price1 - data_cf$mc1)*tidy_coefficients1$estimate[3]/tidy_coefficients2$estimate[3]
  data_cf$price_new3 <- data_cf$mc3 -1*data_cf$qty3/tidy_coefficients3$estimate[4]
  data_cf$price_new4 <- data_cf$mc4 -1*data_cf$qty4/tidy_coefficients4$estimate[5]
  data_cf$price_new5 <- data_cf$mc5 -1*data_cf$qty5/tidy_coefficients5$estimate[6]
  
  # check convergence
  diff <- max(
    max(abs(data_cf$price_new1 - data_cf$price1), na.rm = TRUE),
    max(abs(data_cf$price_new2 - data_cf$price2), na.rm = TRUE),
    max(abs(data_cf$price_new3 - data_cf$price3), na.rm = TRUE),
    max(abs(data_cf$price_new4 - data_cf$price4), na.rm = TRUE),
    max(abs(data_cf$price_new5 - data_cf$price5), na.rm = TRUE),
    na.rm = TRUE
  )  
  
  
  # if not converged
  data_cf$price1 <- 0.1*data_cf$price_new1 + 0.9*data_cf$price1
  data_cf$price2 <- 0.1*data_cf$price_new2 + 0.9*data_cf$price2
  data_cf$price3 <- 0.1*data_cf$price_new3 + 0.9*data_cf$price3
  data_cf$price4 <- 0.1*data_cf$price_new4 + 0.9*data_cf$price4
  data_cf$price5 <- 0.1*data_cf$price_new5 + 0.9*data_cf$price5
}


data_cf$qty1 <- predict(ivgood1, newdata=data_cf) + data_cf$resid1
data_cf$qty2 <- predict(ivgood2, newdata=data_cf) + data_cf$resid2
data_cf$qty3 <- predict(ivgood3, newdata=data_cf) + data_cf$resid3
data_cf$qty4 <- predict(ivgood4, newdata=data_cf) + data_cf$resid4
data_cf$qty5 <- predict(ivgood5, newdata=data_cf) + data_cf$resid5

iter
diff


# Look at price changes
chg <- sapply(1:5, function(j) {
  mean((data_cf[[paste0("price", j)]] - data_orig[[paste0("price", j)]]) /
         data_orig[[paste0("price", j)]], na.rm = TRUE)
})

changes <- tibble(product = paste0("price", 1:5),
                  mean_change = as.numeric(chg))

ggplot(changes, aes(x = product, y = mean_change)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  labs(title = "Mean % change in price",
       y = "Mean ( (cf - orig) / orig )",
       x = NULL)

# Loot at profit changes
data_orig$profit1 = (data_orig$price1 - data_orig$mc1)*data_orig$qty1
data_orig$profit2 = (data_orig$price2 - data_orig$mc2)*data_orig$qty2
data_orig$profit3 = (data_orig$price3 - data_orig$mc3)*data_orig$qty3
data_orig$profit4 = (data_orig$price4 - data_orig$mc4)*data_orig$qty4
data_orig$profit5 = (data_orig$price5 - data_orig$mc5)*data_orig$qty5

data_cf$profit1 = (data_cf$price1 - data_cf$mc1)*data_cf$qty1
data_cf$profit2 = (data_cf$price2 - data_cf$mc2)*data_cf$qty2
data_cf$profit3 = (data_cf$price3 - data_cf$mc3)*data_cf$qty3
data_cf$profit4 = (data_cf$price4 - data_cf$mc4)*data_cf$qty4
data_cf$profit5 = (data_cf$price5 - data_cf$mc5)*data_cf$qty5

chg <- sapply(1:5, function(j) {
  mean((data_cf[[paste0("profit", j)]] - data_orig[[paste0("profit", j)]]) , na.rm = TRUE)
})

changes_pi <- tibble(product = paste0("profit", 1:5),
                  mean_change = as.numeric(chg))

ggplot(changes_pi, aes(x = product, y = mean_change)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  labs(title = "Mean $ change in profit",
       y = "Mean ( (cf - orig) )",
       x = NULL)






###############################
# Solving for Merger CF 2 + 4 #
###############################

# Make a copy of the dataset to do the cf on
dtata_merger1 <- data_cf
data_orig <- data_good_wide
data_orig <- data_orig[1:10, ]
data_cf <- data_orig


# Iterate prices until convergence (fixed point)
tol      <- 0.01          # convergence tolerance
max_iter <- 500           # safety cap
iter     <- 0
diff     <- Inf

while (iter < max_iter && diff > tol) {
  iter <- iter + 1
  
  # Update Q
  data_cf$qty1 <- predict(ivgood1, newdata=data_cf) + data_cf$resid1
  data_cf$qty2 <- predict(ivgood2, newdata=data_cf) + data_cf$resid2
  data_cf$qty3 <- predict(ivgood3, newdata=data_cf) + data_cf$resid3
  data_cf$qty4 <- predict(ivgood4, newdata=data_cf) + data_cf$resid4
  data_cf$qty5 <- predict(ivgood5, newdata=data_cf) + data_cf$resid5
  
  data_cf$price_new1 <- data_cf$mc1 -1*data_cf$qty1/tidy_coefficients1$estimate[2] 
  data_cf$price_new2 <- data_cf$mc2 -1*data_cf$qty2/tidy_coefficients2$estimate[3] - (data_cf$price4 - data_cf$mc4)*tidy_coefficients4$estimate[3]/tidy_coefficients2$estimate[3]
  data_cf$price_new3 <- data_cf$mc3 -1*data_cf$qty3/tidy_coefficients3$estimate[4]
  data_cf$price_new4 <- data_cf$mc4 -1*data_cf$qty4/tidy_coefficients4$estimate[5] - (data_cf$price2 - data_cf$mc2)*tidy_coefficients2$estimate[5]/tidy_coefficients4$estimate[5]
  data_cf$price_new5 <- data_cf$mc5 -1*data_cf$qty5/tidy_coefficients5$estimate[6]
  
  # check convergence
  diff <- max(
    max(abs(data_cf$price_new1 - data_cf$price1), na.rm = TRUE),
    max(abs(data_cf$price_new2 - data_cf$price2), na.rm = TRUE),
    max(abs(data_cf$price_new3 - data_cf$price3), na.rm = TRUE),
    max(abs(data_cf$price_new4 - data_cf$price4), na.rm = TRUE),
    max(abs(data_cf$price_new5 - data_cf$price5), na.rm = TRUE),
    na.rm = TRUE
  )  
  
  
  # if not converged
  data_cf$price1 <- 0.1*data_cf$price_new1 + 0.9*data_cf$price1
  data_cf$price2 <- 0.1*data_cf$price_new2 + 0.9*data_cf$price2
  data_cf$price3 <- 0.1*data_cf$price_new3 + 0.9*data_cf$price3
  data_cf$price4 <- 0.1*data_cf$price_new4 + 0.9*data_cf$price4
  data_cf$price5 <- 0.1*data_cf$price_new5 + 0.9*data_cf$price5
}


data_cf$qty1 <- predict(ivgood1, newdata=data_cf) + data_cf$resid1
data_cf$qty2 <- predict(ivgood2, newdata=data_cf) + data_cf$resid2
data_cf$qty3 <- predict(ivgood3, newdata=data_cf) + data_cf$resid3
data_cf$qty4 <- predict(ivgood4, newdata=data_cf) + data_cf$resid4
data_cf$qty5 <- predict(ivgood5, newdata=data_cf) + data_cf$resid5

iter
diff


# Look at price changes
chg <- sapply(1:5, function(j) {
  mean((data_cf[[paste0("price", j)]] - data_orig[[paste0("price", j)]]) /
         data_orig[[paste0("price", j)]], na.rm = TRUE)
})

changes <- tibble(product = paste0("price", 1:5),
                  mean_change = as.numeric(chg))

ggplot(changes, aes(x = product, y = mean_change)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  labs(title = "Mean % change in price",
       y = "Mean ( (cf - orig) / orig )",
       x = NULL)

# Loot at profit changes
data_orig$profit1 = (data_orig$price1 - data_orig$mc1)*data_orig$qty1
data_orig$profit2 = (data_orig$price2 - data_orig$mc2)*data_orig$qty2
data_orig$profit3 = (data_orig$price3 - data_orig$mc3)*data_orig$qty3
data_orig$profit4 = (data_orig$price4 - data_orig$mc4)*data_orig$qty4
data_orig$profit5 = (data_orig$price5 - data_orig$mc5)*data_orig$qty5

data_cf$profit1 = (data_cf$price1 - data_cf$mc1)*data_cf$qty1
data_cf$profit2 = (data_cf$price2 - data_cf$mc2)*data_cf$qty2
data_cf$profit3 = (data_cf$price3 - data_cf$mc3)*data_cf$qty3
data_cf$profit4 = (data_cf$price4 - data_cf$mc4)*data_cf$qty4
data_cf$profit5 = (data_cf$price5 - data_cf$mc5)*data_cf$qty5

chg <- sapply(1:5, function(j) {
  mean((data_cf[[paste0("profit", j)]] - data_orig[[paste0("profit", j)]]), na.rm = TRUE)
})

changes_pi <- tibble(product = paste0("profit", 1:5),
                     mean_change = as.numeric(chg))

ggplot(changes_pi, aes(x = product, y = mean_change)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  labs(title = "Mean $ change in profit",
       y = "Mean ( (cf - orig) )",
       x = NULL)



