## Bayesian LT trends model

library(R2jags)
library(arm)
library(lme4)

##set data

dat <- read.table("modern.e10.txt", header = TRUE)
dat$lakes = dat$lagoslakeid
levels(dat$lakes) = c(1:length(unique(dat$lagoslakeid)))

##create subset of data to test model
lakes = unique(dat$lagoslakeid)[1:50]
dat <- dat[which(dat$lagoslakeid %in% lakes), ]

# Quick and dirty plot of nutrients over time
plot(tn_umol ~ sampleyear_cor, data=dat, pch=16, col=c(dat$lagoslakeid) )
# log-secchi
plot(log(tp_umol) ~ sampleyear_cor, data=dat, pch=16, col=c(dat$lagoslakeid) )

#linear mixed model (using LMER) comparison

tn.mod = lmer(log(tn_umol) ~ sampleyear_cor + (sampleyear_cor|lagoslakeid), data = dat)
#################################################################
########## BUGS CODE ############################################
#################################################################
setwd("~/Dropbox/Time series/Code")
# Define the model in the BUGS language and write a text file
sink("model.txt")
cat("
    model {
    
    
    # Likelihood: 
    # Level-1 of the model
    for (i in 1:n){ 
    y[i] ~ dnorm(mu[i], tau)               
    mu[i] <- alpha[group[i]] + beta[group[i]] * year[i]         
    } 
    
    
    # Level-2 of the model
    for(j in 1:J){
    alpha[j] <- BB[j,1]
    beta[j] <- BB[j,2]
    
    BB[j,1:K] ~ dmnorm(BB.hat[j,], Tau.B[,]) # bivriate normal
    
    BB.hat[j,1] <- mu.alpha
    BB.hat[j,2] <- mu.beta
    }
    
    
    # Priors and derived quantities
    sigma ~ dunif(0, 100)
    tau <- pow(sigma,-2) # precision
    sigma2 <- pow(sigma,2)
    
    mu.alpha ~ dnorm(0, 0.0001)
    mu.beta ~ dnorm(0, 0.0001)
    
    #### Dealing with variance-covariance of random slopes and intercepts
    # Convert covariance matrix to precision for use in bivariate normal above
    Tau.B[1:K,1:K] <- inverse(Sigma.B[,])
    
    # variance among intercepts
    Sigma.B[1,1] <- pow(sigma.a, 2)
    sigma.a ~ dunif (0, 100)
    
    # Variance among slopes
    Sigma.B[2,2] <- pow(sigma.b, 2)
    sigma.b ~ dunif (0, 100)
    
    # Covariance between alpha's and beta's
    Sigma.B[1,2] <- rho * sigma.a * sigma.b
    Sigma.B[2,1] <- Sigma.B[1,2]
    
    # Uniform prior on correlation
    rho ~ dunif (-1, 1)
    
    
    
    } # end model
    ",fill = TRUE)
sink()

# Number of parameters
K <- 2
# Number of lakes
J <- length(unique(dat$lagoslakeid))

head(df)

# Load data
data <- list(y = log(dat$tp_umol), group = as.numeric(dat$lakes), n = dim(dat)[1], J = J,
             year = dat$sampleyear_cor, K = K)



# Initial values
inits <- function (){
  list (mu.alpha = rnorm(1), mu.beta=rnorm(1), sigma=runif(1),
        BB=matrix(rnorm(J*K),nrow=J,ncol=K), sigma.a=runif(1), sigma.b=runif(1), rho=runif(1) )
}


# Parameters monitored
parameters <- c("mu.alpha","mu.beta","BB","sigma", "sigma.a", "sigma.b","rho")

# MCMC settings
ni <- 3000
nt <- 1
nb <- 1000
nc <- 3

start.time = Sys.time()         # Set timer 
# Call JAGS from R 

out <- jags(data, inits, parameters, "model.txt", n.chains = nc, 
            n.thin = nt, n.iter = ni, n.burnin = nb)

# 
end.time = Sys.time()
elapsed.time = round(difftime(end.time, start.time, units='mins'), dig = 2)
cat('Posterior computed in ', elapsed.time, ' minutes\n\n', sep='') 
# Calculate computation time


# Summarize posteriors
print(out, dig = 3)

### Sometimes you have many, many parameters to examine:
# Find which parameters, if any, have Rhat > 1.1
which(out$BUGSoutput$summary[, c("Rhat")] > 1.1)

# Or see what max Rhat value is
max(out$BUGSoutput$summary[, c("Rhat")])


str(out)


###################################################################
# Create MCMC object so you can use the CODA package to manipulate 
# and examine data
out.mcmc <- as.mcmc(out)
str(out.mcmc)
# look at summary
summary(out.mcmc)
# Create traceplots
# xyplot(out.mcmc)
# Look at posterior density plots
# densityplot(out.mcmc)

#### Just make plots for parameters of interest
out.mcmc2 <- out.mcmc[,c("mu.alpha","mu.beta","sigma", "sigma.a", "sigma.b","rho")]
xyplot(out.mcmc2)
densityplot(out.mcmc2)
tp.summary = summary(out.mcmc)
summary=tp.summary
#make a dataframe of only intercept values for each lake
lake.int = summary$quantiles[seq(1,2*length(unique(dat$lagoslakeid)), by = 2),]
lake.slope = summary$quantiles[seq(2,2*length(unique(dat$lagoslakeid)), by = 2),]

#find out how many slopes are different from zero
#negative slopes 

n.slope.neg = which(lake.slope[,1]<0 & lake.slope[,5]<0)
#16 lakes have negative TN slopes
n.slope.pos = which(lake.slope[,1]>0 & lake.slope[,5]>0)
#6 lakes have positive TN slopes

#save dataframe that has mean and confidence intervals for each lake, 
#and adds a column that says "positive", "negative" or "no change"

lake.slopes = data.frame(mean = summary$statistics[seq(2,2*length(unique(dat$lagoslakeid)), by = 2),1], lower = summary$quantiles[seq(2,2*length(unique(dat$lagoslakeid)), by = 2),1], upper = summary$quantiles[seq(2,2*length(unique(dat$lagoslakeid)), by = 2),5])

for (i in 1:nrow(lake.slopes)){
  if (lake.slopes$lower[i] > 0 & lake.slopes$upper[i] > 0) {
      lake.slopes$sig_change[i] = "positive"
} else {
  if (lake.slopes$lower[i] < 0 & lake.slopes$upper[i] < 0) {
    lake.slopes$sig_change[i] = "negative"
  } else {
    lake.slopes$sig_change[i] = "no change"
  }
}
}

#extract lake # from row names
lake.slopes$lakes = gsub(pattern = "BB\\[(\\d+),\\d\\]", replacement = "\\1", x = row.names(lake.slopes))

#merge to get lagos lake ids
dat.ids = dat[,c(1,9)]
dat.ids$lakes = as.character(dat.ids$lakes)
dat.ids = unique(dat.ids)
tp.lake.slopes = merge(lake.slopes, dat.ids, by = "lakes")


#####################################################################
# merge tn and tp outputs

lake.slopes = merge(tn.lake.slopes, tp.lake.slopes, by = "lakes")

pdf("Bayes_e10_TN_TP_change.pdf")
par(mar=c(5,5,1,1))
temp.tn = 100*lake.slopes$mean.x
temp.tp = 100*lake.slopes$mean.y
plot(temp.tn~temp.tp, 
     xlab = "% Change in TP per year", 
     ylab = "% Change in TN per year", 
     cex = 1.5, cex.lab = 2, cex.axis = 1.5)
abline(0,1)
abline(h=0, col = rgb(.2,.5,.5), lwd = 3)
abline(v=0, col = rgb(.5,.2,.5), lwd = 3)
text(max(temp.tp)*.75, min(temp.tn)*.75, 
     labels = paste(length(temp.tp), "lakes"), cex = 1.5)

#first plot points where TN is different from zero
points(temp.tn[lake.slopes$sig_change.x != "no change"]~temp.tp[lake.slopes$sig_change.x != "no change"], 
       col = rgb(.2,.5,.5, .5), pch=16, cex = 1.5)

points(temp.tn[lake.slopes$sig_change.y != "no change"]~temp.tp[lake.slopes$sig_change.y != "no change"], 
       col = rgb(.5,.2,.5, .5), pch=16, cex=1.5)
points(temp.tn[lake.slopes$sig_change.x != "no change" & lake.slopes$sig_change.y != "no change"]~temp.tp[lake.slopes$sig_change.x != "no change" & lake.slopes$sig_change.y != "no change"], 
       col = rgb(.2,.2,.2,.5), pch=16, cex = 1.5)
dev.off()
