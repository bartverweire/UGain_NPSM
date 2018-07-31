library(tidyverse)
library(lubridate)
library(mgcv)
library(car)
library(boot)
load(file = "unemploymentUS.rda")
str(unemploymentUS)
View(unemploymentUS)

unemploymentUS <- unemploymentUS %>% 
  mutate(date = ymd(paste(Year, Monthly, "01", sep="/")),
         date.number = Year + Monthly / 12)

ggplot(unemploymentUS, aes(date, Rate)) +
  geom_point() +
  scale_x_date(date_breaks = "year") +
  theme(axis.text.x = element_text(angle = 90))

# --> We zien een duidelijk niet-lineair verloop. Er lijkt een periodieke component te zijn, 
# maar de periode is groter dan 1 jaar.

# gam model met splines voor Year en Monthly
ue.mod <- gam(Rate ~ s(Year) + s(Monthly), data = unemploymentUS)
summary(ue.mod)
anova(ue.mod)

# --> Interpretatie : significant verband met Year, maar niet met Monthly


# Visualizeer resultaat
ue.mod.pred = predict(ue.mod, se.fit = TRUE)
ue.mod.data <- unemploymentUS %>% 
  mutate(fit = ue.mod.pred$fit,
         l = ue.mod.pred$fit - 1.96 * ue.mod.pred$se.fit,
         u = ue.mod.pred$fit + 1.96 * ue.mod.pred$se.fit,
         resid = residuals(ue.mod))

ggplot(ue.mod.data, aes(Year + Monthly / 12)) +
  geom_point(aes(y = Rate)) +
  geom_line(aes(y = fit), color = "red") +
  geom_ribbon(aes(ymin = l, ymax = u), fill = "red", alpha = 0.1) +
  scale_x_continuous(breaks = 1970:1990) +
  theme(axis.text.x = element_text(angle = 90))

ggplot(ue.mod.data, aes(fit, resid)) +
  geom_point()

plot(ue.mod, residuals = TRUE, select = 1, shade = TRUE)
plot(ue.mod, residuals = TRUE, select = 2, shade = TRUE)

# vergelijken met model enkel Year
ue.mod.year <- gam(Rate ~ s(Year), data = unemploymentUS)
summary(ue.mod.year)
anova(ue.mod.year)

AIC(ue.mod, ue.mod.year)
# --> Dit model heeft een iets lagere AIC

# Visualizeer resultaat
ue.mod.year.pred = predict(ue.mod.year, se.fit = TRUE)

ue.mod.year.data <- unemploymentUS %>% 
  mutate(fit = ue.mod.year.pred$fit,
         l = ue.mod.year.pred$fit - 1.96 * ue.mod.year.pred$se.fit,
         u = ue.mod.year.pred$fit + 1.96 * ue.mod.year.pred$se.fit,
         resid = residuals(ue.mod.year))

ggplot(ue.mod.year.data, aes(Year + Monthly / 12)) +
  geom_point(aes(y = Rate)) +
  geom_line(aes(y = fit), color = "red") +
  geom_ribbon(aes(ymin = l, ymax = u), fill = "red", alpha = 0.1) +
  scale_x_continuous(breaks = 1970:1990) +
  theme(axis.text.x = element_text(angle = 90))

plot(ue.mod.year, residuals = TRUE)

# Is het model significant niet-lineair ?
ue.mod.year.lin <- gam(Rate ~ Year, data = unemploymentUS)
summary(ue.mod.year.lin)
anova(ue.mod.year.lin)

# --> in het zuiver lineaire model zijn de coëfficiënten niet significant

# Visualizeer resultaat
ue.mod.year.lin.pred = predict(ue.mod.year.lin, se.fit = TRUE)
residuals(ue.mod.year.lin.pred)
ue.mod.year.lin.data <- unemploymentUS %>% 
  mutate(fit = ue.mod.year.lin.pred$fit,
         l = ue.mod.year.lin.pred$fit - 1.96 * ue.mod.year.lin.pred$se.fit,
         u = ue.mod.year.lin.pred$fit + 1.96 * ue.mod.year.lin.pred$se.fit)

ggplot(ue.mod.year.lin.data, aes(Year + Monthly / 12)) +
  geom_point(aes(y = Rate)) +
  geom_line(aes(y = fit), color = "red") +
  geom_ribbon(aes(ymin = l, ymax = u), fill = "red", alpha = 0.1) +
  scale_x_continuous(breaks = 1970:1990) +
  theme(axis.text.x = element_text(angle = 90))

# Effect van maand kan misschien beter inbegrepen worden door niet te fitten op year, maar op year + monthly / 12
# Dit komt neer op modelleren op de echte datum

ue.mod.date <- gam(Rate ~ s(date.number), data = unemploymentUS)
summary(ue.mod.date)
anova(ue.mod.date)

# --> eveneens significant
# Visualizeer resultaat
ue.mod.date.pred = predict(ue.mod.date, se.fit = TRUE)

ue.mod.date.data <- unemploymentUS %>% 
  mutate(fit = ue.mod.date.pred$fit,
         l = ue.mod.date.pred$fit - 1.96 * ue.mod.date.pred$se.fit,
         u = ue.mod.date.pred$fit + 1.96 * ue.mod.date.pred$se.fit)

ggplot(ue.mod.date.data, aes(Year + Monthly / 12)) +
  geom_point(aes(y = Rate)) +
  geom_line(aes(y = fit), color = "red") +
  geom_ribbon(aes(ymin = l, ymax = u), fill = "red", alpha = 0.1) +
  scale_x_continuous(breaks = 1970:1990) +
  theme(axis.text.x = element_text(angle = 90))

plot(ue.mod.date)
# Alle modellen samen gevisualiseerd : 

ggplot(unemploymentUS, aes(date.number, Rate)) +
  geom_point(aes(Year + Monthly/12), alpha = 0.1) +
  geom_line(aes(y = fit), data = ue.mod.data, color = "red") +
  geom_ribbon(aes(ymin = l, ymax = u), data = ue.mod.data, fill = "red", alpha = 0.1) +
  geom_line(aes(y = fit), data = ue.mod.year.data, color = "green") +
  geom_ribbon(aes(ymin = l, ymax = u), data = ue.mod.year.data, fill = "green", alpha = 0.1) +
  geom_line(aes(y = fit), data = ue.mod.date.data, color = "blue") +
  geom_ribbon(aes(ymin = l, ymax = u), data = ue.mod.date.data, fill = "blue", alpha = 0.1) +
  scale_x_continuous(breaks = 1970:1990) +
  theme(axis.text.x = element_text(angle = 90))


AIC(ue.mod, ue.mod.date, ue.mod.year)

# --> Het model op basis van Year + Monthly /12 is het beste

# Conclusie :
# Er is een verband tussen werkloosheidgraad en de tijd
# Dit verband is voornamelijk afhankelijk van de tijd, en er is een niet lineair verband
# De periode van de fluctuaties is groter dan 1 jaar, dus de invloed van de maand is niet significant

