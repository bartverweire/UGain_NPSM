library(tidyverse)

data <- tibble(
  x = c(1, 2, 3),
  y = c(1.5, 2, 3),
  z = as.factor(x)
)
data
mod.num <- lm(y ~ x, data)
mod.num.pred <- predict(mod.num, se.fit = TRUE)

mod.fac <- lm(y ~ z, data)
mod.fac.pred <- predict(mod.fac, se.fit = TRUE)
data <- data %>% 
  mutate(fit.num = mod.num.pred$fit,
         l.num = mod.num.pred$fit - 1.96*mod.num.pred$se.fit,
         u.num = mod.num.pred$fit + 1.96*mod.num.pred$se.fit,
         fit.fac = mod.fac.pred$fit,
         l.fac = mod.fac.pred$fit - 1.96*mod.fac.pred$se.fit,
         u.fac = mod.fac.pred$fit + 1.96*mod.fac.pred$se.fit)

ggplot(data, aes(x,y)) +
  geom_point() +
  geom_line(aes(y = fit.num), color = "red") +
  geom_line(aes(y = fit.fac), color = "blue")
