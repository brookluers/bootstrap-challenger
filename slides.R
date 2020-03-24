## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
knitr::opts_chunk$set(echo=TRUE, fig.width=4, fig.pos='H',size = "tiny",
                      fig.height=3, fig.align='center',
                      out.width='0.75\\textwidth',
                      message=FALSE, warning=FALSE)
library(tidyverse)
library(knitr)
mytheme <- theme(panel.background = element_rect(fill=NA, color='grey'),
          plot.background = element_rect(fill=NA),
          text = element_text(color='#0d0d0d'))



## ----load-challenger, echo=FALSE-----------------------------------------
library(faraway)
# The original data reports the number of damaged o-rings out of six 
# see ?orings
# Convert to one-zero format
orings$ones <- orings$damage
orings$zeroes <- 6 - orings$damage
data_original <- 
  data.frame(
    do.call('rbind',
          lapply(1:nrow(orings), function(i) 
          return(cbind(fail = c(rep(1, orings[i,'ones']), rep(0, orings[i, 'zeroes'])),
               temp = orings[i, 'temp']))))
  )
# Now each row is a single o-ring with a binary response


## ----plot-challenger, echo=FALSE-----------------------------------------
# Plot damage (Y) as a function of temperature
ggplot(data_original, aes(x=temp, y=fail))+
  geom_point(position=position_jitter(width=0, height=0.01),
             shape=1) +
  mytheme + 
  xlab("Temperature (deg. F)") + 
  ylab("") +
  ggtitle("") + 
  scale_y_continuous(breaks=c(0,1),
                     labels=c('y = 0, No damage', 'y = 1, Damage'))


## ----est1,echo=FALSE-----------------------------------------------------
cmod <- glm(fail ~ temp, family=binomial, data=data_original)
b0 <- coef(cmod)[1]
b1 <- coef(cmod)[2]


## ----theta-hat,echo=FALSE------------------------------------------------
tau <- 0.2
logit_tau <- log(tau / (1-tau))
theta_hat <- (logit_tau - b0) / b1


## ----plot-phat,echo=FALSE------------------------------------------------
prddat <- data.frame(temp=seq(min(data_original$temp),
                              max(data_original$temp),
                              length.out=200))
prddat$phat <- predict(cmod, prddat, type='response')

# Plot damage (Y) as a function of temperature
ggplot(data_original, aes(x=temp, y=fail))+
  geom_point(shape=1,position=position_jitter(width=0,height=0.01)) +
  mytheme + 
  xlab("Temperature (deg. F)") + 
  ylab("") +
  ggtitle("") + 
  annotate('text',label='Estimated damage probability',
           size=4,hjust=0,x=57,y=0.4,color='red')+
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1.0)) +
  geom_line(aes(temp,phat),
            color='red', data=prddat)


## ----bootSetup, echo=FALSE, cache=TRUE-----------------------------------
# Number of bootstrap samples
B <- 500
n <- nrow(data_original) # Number of observations
tau <- 0.2 # maximum probability of failure
logit_tau <- log(tau / (1-tau))

set.seed(108)


## ----doBoot, cache=TRUE, dependson='bootSetup',size='scriptsize'---------
# A vector to store the resulting hat(theta)_b
thetas_boot <- vector('numeric', B)

for (b in 1:B){
  
  # random indices for sample b
  ix_b <- sample(1:n, size=n, replace = TRUE) 
  
  # bootstrap sample b
  data_b <- data_original[ix_b, ]
  
  # fit the regression model using the bootstrap sample
  model_b <- glm(fail ~ temp, family=binomial, data = data_b)
  
  b0 <- coef(model_b)[1] # hat(beta)_0
  b1 <- coef(model_b)[2] # hat(beta)_1
  
  # compute hat(theta)_b
  thetas_boot[b] <- (log(0.2 / (1 - 0.2)) - b0) / b1 
}


## ----bootHist,echo=FALSE-------------------------------------------------
ggplot(data.frame(th = thetas_boot))+
  geom_histogram(aes(x=th), fill='white', color='black', bins=30) +
  mytheme +
  xlab(expression(hat(theta)[b])) + ylab("Count")


## ----bootPointEst,size='small'-------------------------------------------
(theta_hat_boot <- mean(thetas_boot))
(se_hat_boot <- sd(thetas_boot))


## ----bootCI,size='small'-------------------------------------------------
c(theta_hat_boot - 2 * se_hat_boot,
  theta_hat_boot + 2 * se_hat_boot)

