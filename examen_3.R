library(tidyverse)
library(lubridate)
library(scales)
library(mgcv)

# Vraag 3

# Voorbereiding: Lognormale verdeling
# Random generator voor de Log-normale distributie
rlnorm2 <- function(n, mu = 1, sd = 1) {
  exp_sdlog_sq <- 1 + sd ^ 2 / mu ^ 2
  sdlog <- sqrt(log(exp_sdlog_sq))
  meanlog <- log(mu / sqrt(exp_sdlog_sq))
  
  rlnorm(n, meanlog, sdlog)
}

# Density functie voor de Log-normale distributie
dlnorm2 <- function(n, mu = 1, sd = 1) {
  exp_sdlog_sq <- 1 + sd ^ 2 / mu ^ 2
  sdlog <- sqrt(log(exp_sdlog_sq))
  meanlog <- log(mu / sqrt(exp_sdlog_sq))
  
  dlnorm(n, meanlog, sdlog)
}

# We kunnen dit testen op een aantal verschillende combinaties voor de parameters $\mu_{L}$ en $\sigma_{L}$.
# Eerst maken we een data frame met verschillende combinaties voor $\mu_{L}$ en $\sigma_{L}$. Dan gebruiken we de functie sapply om de gemiddelde waarden te berekenen voor een sample.
# Als de functie goed gedefinieerd is, moeten de gemeten gemiddelde waarde en standaardafwijking overeenkomen met de waarden van de parameters.
#
# Het resultaat van deze berekening is een matrix, waarbij de eerste 2 rijen de parameters weergeven, en de laatste 2 rijen de (afgeronde) gemeten waarden.

test.params <- expand.grid(c(1, 2, 3), c(0.5, 1, 2))

sapply(1:nrow(test.params), function(i) {
  mu <- test.params[i, 1]
  sd <- test.params[i, 2]
  test.data <- rlnorm2(100000, mu, sd)
  list(
    mu = mu,
    sd = sd,
    sample_mu = round(mean(test.data), 1),
    sample_sd = round(sd(test.data), 1)
  )
})


# De gemeten waarde komt inderdaad overeen met de waarden van de parameters, dus de functie werkt zoals het hoort.

## Monte-Carlo simulatie functie

# Er wordt gevraagd om Monte-Carlo simulaties uit te voeren in een aantal verschillende situaties. Hiervoor definiëren we een functie, met volgende parameters (algemener dan in de vraagstelling)
#
# * dist (waarden **norm** of **lnorm**) : Data uit een normale/lognormale verdeling
# * n.total : totaal aantal samples voor steekproef 1 en steekproef 2 samen
# * sample.ratio (default 1) : verhouding van het aantal samples in steekproef 1 tov. steekproef 2, bv.
# + n.total = 200, sample.ratio = 3 leidt tot 150 samples in steekproef 1 en 50 in steekproef 2
# + de default waarde leidt tot een gelijk aantal samples
# * mu : gemiddelde waarde voor beide steekproeven
# * sd : standaardafwijking voor steekproef 1
# * sd.ratio : verhouding van standaardafwijking voor steekproef 1 tov. die van steekproef 2
# + sd.ratio = 5 betekent $\sigma_{1} = 1$ en $\sigma_{2} = 1/5$
#   * test.type (waarden **pooled** of **Welch**) : geeft aan of de t-test gebruik maakt van de "pooled" variantie, of van de Welch benadering voor het aantal vrijheidsgraden.
# * R (default 10000) : aantal Monte-Carlo simulaties in elk scenario
# * alpha (default 0.05) : p-value drempelwaarde
#
# De functie geeft een list terug met daarin de gebruikte (afgeleide) parameterwaarden en de berekende waarde voor de typeI fouten tov. drempelwaarde alpha.
# De list bevat daarnaast ook het laatste paar steekproeven, voor controle.

sim.mctest <- function(dist = c("norm", "lnorm"),
                       n.total,
                       sample.ratio = 1,
                       mu = 1,
                       sd = 1,
                       sd.ratio = 1,
                       test.type = c("pooled", "Welch"),
                       R = 10000,
                       alpha = 0.05) {
  # calculate sample sizes
  n1 <- n.total / (1 + 1 / sample.ratio)
  n2 <- n.total / (1 + sample.ratio)
  
  test.type <- test.type[1]
  if (!(test.type %in% c("pooled", "Welch"))) {
    stop(paste("Invalid test type", test.type))
  }
  # valid test type
  t.test.var.equal <- test.type == "pooled"
  
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
    n.total = n.total,
    # total sample size
    n1 = n1,
    # sample 1 size
    n2 = n2,
    # sample 2 size,
    dist = dist,
    # distribution type
    test.type = test.type,
    # test type
    d1 = d1,
    # last d1 sample
    d2 = d2,
    # last d2 sample
    mu1 = mu1,
    mu2 = mu2,
    sd1 = sd1,
    sd2 = sd2,
    typeI.fout.pct = cnt.typeI / R
  )
  
}


## Test functie
# Om deze functie te testen definiëren we volgende functie.
# Deze functie resulteert in een ggplot object met daarin de (density) histogrammen van de laatste steekproeven, de theoretische density functie, en de berekende p-value.


eval.mctest <- function(test) {
  plot_data <-
    rbind(
      data.frame(sample = "sample 1", x = test$d1),
      data.frame(sample = "sample 2", x = test$d2)
    )
  
  # valid distribution type
  if (test$dist == "norm") {
    dist.fun <- dnorm
  } else {
    dist.fun <- dlnorm2
  }
  
  dist_data_1 <- tibble(
    dist = "dist 1",
    x = seq(test$mu1 - 3 * test$sd1, test$mu1 + 3 * test$sd1, by = 0.1),
    y = dist.fun(x, test$mu1, test$sd1)
  )
  dist_data_2 <- tibble(
    dist = "dist 2",
    x = seq(test$mu2 - 3 * test$sd2, test$mu2 + 3 * test$sd2, by = 0.1),
    y = dist.fun(x, test$mu2, test$sd2)
  )
  
  display_range <- c(min(c(
    min(dist_data_1$x), min(dist_data_2$x)
  )),
  max(c(
    max(dist_data_1$x), max(dist_data_2$x)
  )))
  
  p <- ggplot(plot_data, aes(x)) +
    geom_histogram(aes(y = ..density.., fill = sample),
                   position = "dodge",
                   binwidth = 0.1,
                   stat) +
    geom_line(aes(x, y, color = dist), data = dist_data_1) +
    geom_line(aes(x, y, color = dist), data = dist_data_2) +
    coord_cartesian(xlim = display_range) +
    ggtitle(paste(
      "Test type: ",
      test$test.type,
      "\np-value",
      test$typeI.fout.pct
    ))
  
  p
}

## Tests

# We voeren een aantal tests uit, met grote getallen, omdat in dit geval de berekende p-waarde de verwachte p-waarde van 0.05 goed moet benaderen.
#
# Een eerste test, met de normale verdeling

test1 <- sim.mctest(
  n.total = 20000,
  sample.ratio = 1,
  dist = "norm",
  mu = 1,
  sd = 1,
  sd.ratio = 1,
  test.type = "Welch",
  R = 10000,
  alpha = 0.05
)
eval.mctest(test1)

# Een tweede test, met de log-normale verdeling
test2 <- sim.mctest(
  n.total = 20000,
  sample.ratio = 1,
  dist = "lnorm",
  mu = 1,
  sd = 1,
  sd.ratio = 1,
  test.type = "Welch",
  R = 10000,
  alpha = 0.05
)
eval.mctest(test2)

# Een test, met de normale verdeling en de pooled-variance two-sample t-test.
test3 <- sim.mctest(
  n.total = 20000,
  sample.ratio = 1,
  dist = "norm",
  mu = 1,
  sd = 1,
  sd.ratio = 1,
  test.type = "pooled",
  R = 10000,
  alpha = 0.05
)
eval.mctest(test3)

# Een test, met de normale verdeling en de pooled-variance two-sample t-test en een log-normale verdeling.
test4 <- sim.mctest(
  n.total = 20000,
  sample.ratio = 1,
  dist = "lnorm",
  mu = 1,
  sd = 1,
  sd.ratio = 1,
  test.type = "pooled",
  R = 10000,
  alpha = 0.05
)
eval.mctest(test4)

test5 <- sim.mctest(
  n.total = 20000,
  sample.ratio = 1/3,
  dist = "lnorm",
  mu = 1,
  sd = 1,
  sd.ratio = 5,
  test.type = "pooled",
  R = 10000,
  alpha = 0.05
)
eval.mctest(test5)

## Scenario's
# Om alle scenario's uit te voeren, bouwen we eerst een data frame op met alle combinaties van voorwaarden :

params <- expand.grid(
  dist = c("norm", "lnorm"),
  sd.ratio = c(1, 5),
  n.total = c(20, 200),
  sample.ratio = c(1, 1 / 3),
  test.type = c("pooled", "Welch")
)

# En via lapply kunnen we de Monte-Carlo simulatie toepassen met de parameters in dit data frame.

sims <- sapply(1:nrow(params), function(i) {
  p <- params[i, ]
  
  p.val <- sim.mctest(
    n.total = p$n.total,
    sample.ratio = p$sample.ratio,
    dist = p$dist,
    mu = 1,
    sd = 1,
    sd.ratio = p$sd.ratio,
    test.type = p$test.type,
    R = 10000
  )$typeI.fout.pct
})

result <- params %>%
  mutate(p.val = sims)

result %>%
  arrange(sd.ratio, sample.ratio, test.type, dist, n.total)

# We kunnen de invloed van elk van de parameters bekijken door voor elke parameter een succes percentage te berekenen, dwz. het aantal gevallen waarbij de Monte-Carlo simulatie een "aanvaardbare" p-waarde oplevert.

success.rate <- function(p.val.threshold) {
  do.call("bind_rows", lapply(names(params), function(p) {
    res.table <- result %>%
      mutate(p.val.ok = p.val < p.val.threshold) %>%
      filter(p.val.ok == TRUE) %>%
      group_by_at(.vars = c(p, "p.val.ok")) %>%
      count() %>%
      ungroup() %>%
      mutate(parameter = p,
             success.pct = round(100 * n / 16, 0)) %>%
      rename_at(.vars = p, funs(paste0("value"))) %>%
      mutate(value = as.character(value)) %>%
      select(parameter, value, n, success.pct)
    
    
    res.table
  }))
}


# Voor een "aanvaardbare" p-waarde van 0.05 :
success.rate(0.05)

# Voor een "aanvaardbare" p-waarde van 0.055 :
success.rate(0.055)

# Voor een "aanvaardbare" p-waarde van 0.06 :
success.rate(0.06)

# De invloed van het type test, voor de verschillende combinaties van de andere parameters.

result %>%
  spread(test.type, p.val) %>%
  mutate(
    winner = if_else(pooled < Welch, "pooled", "Welch"),
    diff = round(100 * (Welch - pooled) / if_else(pooled < Welch, Welch, pooled), 1)
  )

## Conclusie
#
# De Monte-Carlo simulatie is efficiënter
#
# * voor een normale distributie dan voor een lognormale
# * wanneer de standaardafwijkingen van beide populaties gelijk zijn
# * wanneer de samples even groot zijn, en voldoende groot
# * voor gelijke standaardafwijkingen tov. verschillende standaardafwijkingen
# * een Welch t-test geeft betere resultaten dan een pooled, zeker wanneer de standaardafwijkingen verschillend zijn
