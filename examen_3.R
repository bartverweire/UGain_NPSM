library(tidyverse)
simuleer.lognormale <- function(n1, n2, ratio = 1) {
  Y1 <- exp(rnorm(n1))-exp(.5) # gemiddelde = 0, zie wiki lognormale
  Y2 <- exp(rnorm(n2))-exp(.5) # gemiddelde = 0
  Y1 <- Y1*ratio # herschaal, ratio = 1 (gelijke varianties) of 5 (verschillende varianties)
  return(c(Y1,Y2))
}

# Eens snel via Monte-Carlo kijken of de code naar behoren werkt
n1 <- 100000 # een groot getal om de populatie te benaderen
n2 <- 100000
Y <- simuleer.lognormale(n1, n2, ratio = 1)
Y1 <- Y[1:n1]; Y2 <- Y[(n1+1):(n1+n2)]

hist(Y1, breaks = 100)
hist(Y2, breaks = 100)
d1 <- data.frame(y = Y1)
d2 <- data.frame(y = Y2)
d3 <- data.frame(y = rlnorm(n1, meanlog = 0) - exp(1/2))
ggplot(d1, aes(y)) +
  geom_density() +
  geom_density(data = d2, color = "red") +
  geom_density(data = d3, color = "blue") +
  coord_cartesian(xlim = c(-2,10))

n <- 1000000
mu1 <- -0.5 * log(2)
mu2 <- -0.5 * log(26/25)
sd1 <- sqrt(log(2))
sd2 <- sqrt(log(26/25))
d1 <- rlnorm(n, meanlog = mu1, sdlog = sd1)
d2 <- rlnorm(n, meanlog = mu2, sdlog = sd2)
mean(d1) 
mean(d2)
sd(d1)
sd(d2)

exp(1/2)
sqrt((exp(1) - 1) * exp(1))

log(1) - 1/2

# Distribution function
rlnorm2 <- function(n, mu = 1, sd = 1) {
  exp_sdlog_sq <- 1 + sd^2/mu^2
  sdlog <- sqrt(log(exp_sdlog_sq))
  meanlog <- log(mu/sqrt(exp_sdlog_sq))
  
  rlnorm(n, meanlog, sdlog)
}

dlnorm2 <- function(n, mu = 1, sd = 1) {
  exp_sdlog_sq <- 1 + sd^2/mu^2
  sdlog <- sqrt(log(exp_sdlog_sq))
  meanlog <- log(mu/sqrt(exp_sdlog_sq))
  
  dlnorm(n, meanlog, sdlog)
}

test.params <- expand.grid(c(1,2,3),c(0.5,1,2))

sapply(1:nrow(test.params), function(i) {
  mu <- test.params[i,1]
  sd <- test.params[i,2]
  test.data <- rlnorm2(1000, mu,sd)
  list(mu = mu, sd = sd, sample_mu = round(mean(test.data),1), sample_sd = round(sd(test.data),1))
})

sim.mctest <- function(n.total, 
                       sample.ratio = 1, 
                       dist = c("norm", "lnorm"), 
                       mu = 1, 
                       sd = 1, 
                       sd.ratio = 1,
                       test.type = c("pooled","Welch"),
                       R = 10000,
                       alpha = 0.05) {
  
  # calculate sample sizes
  n1 <- n.total / (1 + 1/sample.ratio)
  n2 <- n.total / (1 + sample.ratio)
  
  test.type <- test.type[1]
  if(!(test.type %in% c("pooled","Welch"))) {
    stop("Invalid test type")
  }
  # valid test type
  t.test.var.equal <- test.type == "Welch"
  
  mu1 <- mu
  mu2 <- mu
  sd1 <- sd
  sd2 <- sd / sd.ratio
  
  dist <- dist[1]
  if (!(dist %in% c("norm", "lnorm"))) {
    stop("invalid distribution")
  }
  
  # valid distribution type
  if (dist == "norm") {
    dist.fun <- rnorm
  } else {
    dist.fun <- rlnorm2
  }
  
  cnt.typeI <- 0
  for (i in 1:R) {
    d1 <- dist.fun(n1, mu1, sd1)
    d2 <- dist.fun(n2, mu2, sd2)
    
    pval <- t.test(d1, d2, var.equal = t.test.var.equal)$p.value
    if (pval < alpha) {
      cnt.typeI <- cnt.typeI + 1
    }
  }
  
  # output a list with all simulation parameters
  list(
    n.total = n.total,        # total sample size
    n1 = n1,      # sample 1 size
    n2 = n2,      # sample 2 size,
    dist = dist,  # distribution type
    d1 = d1,      # last d1 sample
    d2 = d2,      # last d2 sample
    mu1 = mu1,    
    mu2 = mu2,
    sd1 = sd1,
    sd2 = sd2,
    typeI.fout.pct = cnt.typeI / R
  )
  
}

eval.mctest <- function(test) {
  plot_data <- rbind(data.frame(sample = "sample 1", x = test$d1), data.frame(sample = "sample 2", x = test$d2))
  
  # valid distribution type
  if (test$dist == "norm") {
    dist.fun <- dnorm
  } else {
    dist.fun <- dlnorm2
  }
  
  dist_data_1 <- tibble(dist = "dist 1", 
                        x = seq(test$mu1 - 3*test$sd1, test$mu1 + 3 * test$sd1, by = 0.1),
                        y = dist.fun(x, test$mu1, test$sd1))
  dist_data_2 <- tibble(dist = "dist 2", 
                        x = seq(test$mu2 - 3*test$sd2, test$mu2 + 3 * test$sd2, by = 0.1),
                        y = dist.fun(x, test$mu2, test$sd2))
  
  p <- ggplot(plot_data, aes(x)) +
    geom_histogram(aes(y = ..density.., fill = sample), position = "dodge", stat) +
    ggtitle(paste("p-value", test$typeI.fout.pct)) +
    geom_line(aes(x, y, color = dist), data = dist_data_1) +
    geom_line(aes(x, y, color = dist), data = dist_data_2)
  
  p
}

# enkele testen
# met normale distributie
test1 <- sim.mctest(n.total = 20000, 
                    sample.ratio = 1, 
                    dist = "norm", 
                    mu = 1, 
                    sd = 1, 
                    sd.ratio = 1,
                    test.type = "Welch",
                    R = 1000,
                    alpha = 0.05)
eval.mctest(test1)

test2 <- sim.mctest(n.total = 200, 
                    sample.ratio = 1, 
                    dist = "norm", 
                    mu = 1, 
                    sd = 1, 
                    sd.ratio = 5,
                    test.type = "Welch",
                    R = 1000,
                    alpha = 0.05)
eval.mctest(test2)




sim.mctest(20, 1/3, 1, 1, 5)
sim.mctest(20, 1)
sim.mctest(200, 1/3)
sim.mctest(200, 1)
sim.mctest(200, 3, 1, 1, 1)
