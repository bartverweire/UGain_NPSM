library(tidyverse)
library(lubridate)
library(scales)
library(mgcv)

# Vraag 2 - Werkloosheidsgraad

## Data Visualisatie
load(file = "unemploymentUS.rda")

unemploymentUS <- unemploymentUS %>%
  mutate(date = ymd(paste(Year, Monthly, "01", sep = "/")),
         date.number = Year + Monthly / 12)

ggplot(unemploymentUS, aes(date, Rate)) +
  geom_point() +
  scale_x_date(labels = date_format("%Y-%m"), date_breaks = "year") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Unemployment Rate in US")

# We zien een duidelijk niet-lineair verloop. Er lijkt een periodieke component te zijn, maar de periode is groter dan 1 jaar.
#
# Gezien de niet-lineariteit is een gam-model aangewezen.
# Er zijn 2 onafhankelijke variabelen, namelijk Year en Monthly. We kunnen een aantal verschillende modellen opstellen

# * Year
# * Year en Monthly als onafhankelijke variabelen
# * effectieve datum (of numeriek : Year + Monthly/12) als onafhankelijke variabele
#
# De modellen kunnen vergeleken worden via AIC.

## gam model met splines voor Year en Monthly

ue.mod <- gam(Rate ~ s(Year) + s(Monthly), data = unemploymentUS)
summary(ue.mod)
anova(ue.mod)

# Er is een significant verband met Year, maar niet met Monthly.

ue.mod.pred = predict(ue.mod, se.fit = TRUE)
ue.mod.data <- unemploymentUS %>%
  mutate(
    fit = ue.mod.pred$fit,
    l = ue.mod.pred$fit - 1.96 * ue.mod.pred$se.fit,
    u = ue.mod.pred$fit + 1.96 * ue.mod.pred$se.fit,
    resid = residuals(ue.mod)
  )

ggplot(ue.mod.data, aes(Year + Monthly / 12)) +
  geom_point(aes(y = Rate)) +
  geom_line(aes(y = fit), color = "red") +
  geom_ribbon(aes(ymin = l, ymax = u), fill = "red", alpha = 0.1) +
  scale_x_continuous(breaks = 1970:1990) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Data + predictie voor een gam model op basis van Year en Monthly")

par(mfrow = c(1, 2))
plot(ue.mod,
     residuals = TRUE,
     select = 1,
     shade = TRUE)
plot(ue.mod,
     residuals = TRUE,
     select = 2,
     shade = TRUE)

## gam model met enkel Year

ue.mod.year <- gam(Rate ~ s(Year), data = unemploymentUS)
summary(ue.mod.year)
anova(ue.mod.year)

# De invloed van Year is significant.
# Het model heeft een iets lagere AIC dan het model met Year en Monthly.

AIC(ue.mod, ue.mod.year)

ue.mod.year.pred = predict(ue.mod.year, se.fit = TRUE)

ue.mod.year.data <- unemploymentUS %>%
  mutate(
    fit = ue.mod.year.pred$fit,
    l = ue.mod.year.pred$fit - 1.96 * ue.mod.year.pred$se.fit,
    u = ue.mod.year.pred$fit + 1.96 * ue.mod.year.pred$se.fit,
    resid = residuals(ue.mod.year)
  )

ggplot(ue.mod.year.data, aes(Year + Monthly / 12)) +
  geom_point(aes(y = Rate)) +
  geom_line(aes(y = fit), color = "red") +
  geom_ribbon(aes(ymin = l, ymax = u), fill = "red", alpha = 0.1) +
  scale_x_continuous(breaks = 1970:1990) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Data + predicties voor het gam model op Year")

plot(ue.mod.year, residuals = TRUE)

## Is het model significant niet-lineair ?
ue.mod.year.lin <- gam(Rate ~ Year, data = unemploymentUS)
summary(ue.mod.year.lin)
anova(ue.mod.year.lin)

# In het zuiver lineaire model zijn de coëfficiënten zwak significant.
#
# Zoals visueel al duidelijk was, kunnen we er beter van uitgaan dat de afhankelijkheid niet-lineair is.

ue.mod.year.lin.pred = predict(ue.mod.year.lin, se.fit = TRUE)
ue.mod.year.lin.data <- unemploymentUS %>%
  mutate(
    fit = ue.mod.year.lin.pred$fit,
    l = ue.mod.year.lin.pred$fit - 1.96 * ue.mod.year.lin.pred$se.fit,
    u = ue.mod.year.lin.pred$fit + 1.96 * ue.mod.year.lin.pred$se.fit
  )

ggplot(ue.mod.year.lin.data, aes(Year + Monthly / 12)) +
  geom_point(aes(y = Rate)) +
  geom_line(aes(y = fit), color = "red") +
  geom_ribbon(aes(ymin = l, ymax = u), fill = "red", alpha = 0.1) +
  scale_x_continuous(breaks = 1970:1990) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Data + predictie voor een lineair model")

## Gam model op tijd

# Aan gezien de periode groter is dan een jaar, kan het effect van maand misschien beter inbegrepen worden door niet te fitten op Year, maar op Year + Monthly / 12

ue.mod.datenr <- gam(Rate ~ s(date.number), data = unemploymentUS)
summary(ue.mod.datenr)
anova(ue.mod.datenr)

# De coëfficiënten in dit model zijn eveneens significant

ue.mod.datenr.pred = predict(ue.mod.datenr, se.fit = TRUE)

ue.mod.datenr.data <- unemploymentUS %>%
  mutate(
    fit = ue.mod.datenr.pred$fit,
    l = ue.mod.datenr.pred$fit - 1.96 * ue.mod.datenr.pred$se.fit,
    u = ue.mod.datenr.pred$fit + 1.96 * ue.mod.datenr.pred$se.fit
  )

ggplot(ue.mod.datenr.data, aes(Year + Monthly / 12)) +
  geom_point(aes(y = Rate)) +
  geom_line(aes(y = fit), color = "red") +
  geom_ribbon(aes(ymin = l, ymax = u), fill = "red", alpha = 0.1) +
  scale_x_continuous(breaks = 1970:1990) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Data + predictie voor gam model op Year + Monthly / 12")

# plot(ue.mod.datenr.pred)

## Vergelijking

AIC(ue.mod, ue.mod.datenr, ue.mod.year)

# Het model op basis van Year + Monthly /12 is het beste.

## Conclusie

# * Er is een niet-lineair verband tussen werkloosheidgraad en de tijd
# * De periode van de fluctuaties is groter dan 1 jaar, dus de invloed van het jaar is significant groter dan deze van de maand
