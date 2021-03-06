library(tidyverse)
library(lubridate)
library(scales)
library(mgcv)
library(knitr)

# Vraag 1 - Geheugentest

## Data Visualisatie

# Opladen data

load(file = "memory.rda")
memory.younger <- memory %>%
  filter(Age == "Younger")
memory.older <- memory %>%
  filter(Age == "Older")

# Visualiseer data
ggplot(memory, aes(Words, fill = Age)) +
  geom_histogram(position = "dodge") +
  facet_wrap( ~ Process)

ggplot(memory, aes(Words, fill = Process)) +
  geom_histogram(position = "dodge") +
  facet_wrap( ~ Age)

ggplot(memory, aes(interaction(Age, Process), Words)) +
  geom_boxplot()

# Uit de figuren kan afgeleid worden dat er een verschil is in het aantal gereproduceerde woorden tussen het Counting proces en de andere processen (Imagery en Intentional).
# Voor de processen Imagery en Intentional lijkt er ook een verschil te zijn tussen jongeren en ouderen.
#
# We onderzoeken nu of het aantal gereproduceerde woorden afhankelijk is van het proces, en of dit effect afhankelijk is van de leeftijd.

## Regressie

# We voeren een lineaire regressie uit. Merk op dat de onafhankelijke variabelen alle categorische variabelen zijn.
#
# Bij een lineaire regressie zijn de veronderstellingen :
#
#   * de observaties zijn onafhankelijk : we gaan ervan uit dat dit klopt, aangezien de onderzoekers de personen random in groepen verdeeld hebben.
# * de residuelen zijn normaal verdeeld
# * de variantie van de residuelen is onafhankelijk van de groep
#
# We kunnen de lineaire regressie uitvoeren op verschillende wijzen :
#
#   * onafhankelijk van de leeftijd
# * afhankelijk van proces en leeftijd, zonder interactie
# * afhankelijk van proces en leeftijd, met en zonder interactie tussen de variabelen Age en Process.

### Test functie

# Om de resultaten van het lineaire model eenvoudiger te vergelijken met de resultaten van de Wilcoxon testen (zie verder), definiëren we een functie om de resultaten van de lineaire regressie in een data.frame te bewaren.

lm.tests <- function(model, test.data) {
  pred <- predict(model, newdata = test.data, se.fit = TRUE)
  res.table <- test.data %>%
    bind_cols(
      value = pred$fit,
      lower = pred$fit - 1.96 * pred$se.fit,
      upper = pred$fit + 1.96 * pred$se.fit
    ) %>%
    mutate(subset = id)
  
  ind.comb <- combn(1:length(res.table), 2)
  
  pairwise.diffs <- lapply(1:ncol(ind.comb), function(i) {
    diff = res.table[ind.comb[1, i], "value"] - res.table[ind.comb[2, i], "value"]
    
    lm.val <- data.frame(subset1 = res.table[ind.comb[1, i], "subset"],
                         subset2 = res.table[ind.comb[2, i], "subset"],
                         diff = round(diff, 3))
    
    lm.val
  })
  
  do.call("bind_rows", pairwise.diffs)
}


# We zullen de test functie uitvoeren op de resultaten van de predict functie, met als nieuwe data een data frame met de mogelijke combinaties van Age en Process, of van Process (afhankelijk van het model).


test.data.age.process <- expand.grid(
  Age = c("Older", "Younger"),
  Process = c("Counting", "Imagery", "Intentional")
) %>%
  mutate(id = paste(Age, Process, sep = "."))
test.data.process <-
  data.frame(Process = c("Counting", "Imagery", "Intentional")) %>%
  mutate(id = Process)
test.data.process.younger <-
  data.frame(Process = c("Counting", "Imagery", "Intentional"),
             Age = "Younger") %>%
  mutate(id = Process)
test.data.process.older <-
  data.frame(Process = c("Counting", "Imagery", "Intentional"),
             Age = "Older") %>%
  mutate(id = Process)

test.data.age.process
test.data.process.younger
test.data.process.older



### Effect van het proces

mem.lm.proc = lm(Words ~ Process, data = memory)



summary(mem.lm.proc)
anova(mem.lm.proc)


# Er is een significant verband tussen het aantal gereproduceerde woorden en het proces.
#
# Zowel de t-test als de F-test wijzen op een significant verband is tussen het proces Counting enerzijds, en de processen Imagery en Intentional anderzijds (van een significant verschil tussen Imagery en Intentional kunnen we via dit model niet spreken).
#
# In de plot van het model zien we dat de voorwaarden rond normaliteit en gelijkheid van de variantie redelijk goed voldaan zijn.
# De variantie niet onafhankelijk van de gefitte waarde, en de residuals zijn niet perfect normaal verdeeld, maar het verschil is aanvaardbaar.


par(mfrow = c(2, 2))
plot(mem.lm.proc)


mem.lm.proc.pred <- predict(mem.lm.proc, se.fit = TRUE)
mem.lm.proc.data <- memory %>%
  mutate(
    fit = mem.lm.proc.pred$fit,
    l = mem.lm.proc.pred$fit - 1.96 * mem.lm.proc.pred$se.fit,
    u = mem.lm.proc.pred$fit + 1.96 * mem.lm.proc.pred$se.fit
  )

ggplot(mem.lm.proc.data,
       aes(Process, Words, color = "Lin.Model", fill = "Lin.Model")) +
  geom_point() +
  geom_line(aes(x = as.numeric(Process), y = fit)) +
  geom_ribbon(
    aes(as.numeric(Process), ymin = l, ymax = u),
    size = 0,
    linetype = 2,
    alpha = 0.1,
    color = NA
  ) +
  ggtitle("Plot van data en predictie via lineaire regressie op Process") +
  guides(fill = FALSE)


### Effect van leeftijd, zonder interactie

# We definiëren het volgende model


mem.lm <- lm(Words ~ Age + Process, data = memory)



summary(mem.lm)
anova(mem.lm)


# Zowel de t-test als de F-test wijzen op een significant verband is tussen het proces Counting enerzijds, en de processen Imagery en Intentional anderzijds (van een significant verschil tussen Imagery en Intentional kunnen we via dit model niet spreken).
# In het algemeen is er ook een significante invloed van de leeftijd : jongeren onthouden meer woorden dan ouderen.
#
# In de plot van het model zien we dat de voorwaarden rond normaliteit en gelijkheid van de variantie redelijk goed voldaan zijn.
#
# De variantie niet onafhankelijk van de gefitte waarde, en de residuals zijn niet perfect normaal verdeeld, maar het verschil is aanvaardbaar.


par(mfrow = c(2, 2))
plot(mem.lm)



mem.lm.pred <- predict(mem.lm, se.fit = TRUE)
mem.lm.data <- memory %>%
  mutate(
    fit = mem.lm.pred$fit,
    l = mem.lm.pred$fit - 1.96 * mem.lm.pred$se.fit,
    u = mem.lm.pred$fit + 1.96 * mem.lm.pred$se.fit
  )

ggplot(mem.lm.data, aes(Process, Words, color = Age, fill = Age)) +
  geom_point() +
  geom_line(aes(x = as.numeric(Process), y = fit)) +
  geom_ribbon(
    aes(as.numeric(Process), ymin = l, ymax = u),
    size = 0,
    linetype = 2,
    alpha = 0.1
  ) +
  ggtitle("Plot van data en predictie via lineaire regressie op Process en Age (geen interactie)") +
  guides(fill = FALSE)


### Met interactie

# We kijken nu naar de invloed van de interactie tussen Age en Process, via volgend model :


mem.lm.int <- lm(Words ~ Age * Process, data = memory)



summary(mem.lm.int)
anova(mem.lm.int)



# Ook de interactieterm is dus significant, vooral dan in geval van het Intentional Proces. Dit gaat ten koste van de algemene leeftijdsfactor, die nu niet significant is.




par(mfrow = c(2, 2))
plot(mem.lm.int)



mem.lm.int.pred <- predict(mem.lm.int, se.fit = TRUE)
mem.lm.int.data <- memory %>%
  mutate(
    fit = mem.lm.int.pred$fit,
    l = mem.lm.int.pred$fit - 1.96 * mem.lm.int.pred$se.fit,
    u = mem.lm.int.pred$fit + 1.96 * mem.lm.int.pred$se.fit
  )

ggplot(mem.lm.int.data, aes(Process, Words, color = Age, fill = Age)) +
  geom_point() +
  geom_line(aes(as.numeric(Process), fit)) +
  geom_ribbon(
    aes(
      x = as.numeric(Process),
      ymin = l,
      ymax = u
    ),
    size = 0,
    linetype = 2,
    alpha = 0.1
  ) +
  ggtitle("Plot van data en predictie via lineaire regressie op Process en Age (met interactie)")


# Merk op dat het model met interactie er op neer komt dat voor elke combinatie (Age, Process) het gemiddelde genomen wordt.

# We gebruiken het model om het aantal woorden te voorspellen.


pred.lm.int <- predict(mem.lm.int, newdata = test.data.age.process)
res.table <- test.data.age.process %>%
  bind_cols(value = pred.lm.int)


# En dit resultaat vergelijken we met een tabel met de gemiddelden :


avg.table.age.process <- memory %>%
  group_by(Age, Process) %>%
  summarize(mean.age.process = mean(Words))


# Beide resultaten kunnen vergeleken worden door de resultaten te joinen


comp.table <- res.table %>%
  inner_join(avg.table.age.process, by = c("Age", "Process"))
comp.table


### Keuze van het model


AIC(mem.lm, mem.lm.int)


# Het model met de interactie term levert de laagste AIC, en is dus te verkiezen.

## Rang-gebaseerde methodes

# De Kruskal-Wallis test laat toe om te kijken of er een significant verschil is in het aantal gereproduceerde woorden. We kunnen de test uitvoeren op
#
# * de subsets per Process
# * de subsets per Process en Age
#
# Deze test zal aanduiden of er een significant verschil is in het gemiddeld aantal gereproduceerde woorden, maar leert ons niets over het effectieve verschil. Hiervoor moeten we dan de Wilcoxon-Mann-Whitney two-sample tests uitvoeren per 2 subsets.
# Aan de hand van de Hodges-Lehman schatter kunnen we dan bekijken wat het verschil is in het aantal gereproduceerde woorden tussen de verschillende subsets.
#
# De rank-gebaseerde methodes veronderstellen geen normaliteit.
# Als we de Hodges-Lehman schatter gebruiken, dan is er uiteraard wel een veronderstelling van locatie-shift, dus in dit geval wordt wel verondersteld dat de varianties van de verschillende subsets gelijk zijn.


### Voorbereiding - data splitsen

# We kunnen de Kruskal-Wallis en Wilcoxon tests uitvoeren op verschillende subsets. Bv.
#
# * subsets per proces
# * subsets per leeftijd (invloed van process, voor een specifieke leeftijdscategorie)
# * subsets per proces en leeftijd (invloed van proces en leeftijd)
#
# Daarvoor definiëren we de subsets als volgt :


memory.by.age.process <-
  lapply(split(memory, list(memory$Age, memory$Process)), '[[', "Words")
memory.by.age <- lapply(split(memory, memory$Age), '[[', "Words")
memory.by.process <-
  lapply(split(memory, memory$Process), '[[', "Words")
memory.by.process.older <-
  lapply(split(memory.older, memory.older$Process), '[[', "Words")
memory.by.process.younger <-
  lapply(split(memory.younger, memory.younger$Process), '[[', "Words")


# Om efficiënter de verschillende Wilcoxon-Mann-Whitney testen te kunnen uitvoeren, en de resultaten in een data.frame terug te krijgen, definiëren we de functie :


wc.tests <- function(subsets, adjust.method = "bonferroni") {
  ind.comb <- combn(1:length(subsets), 2)
  
  pw.result <- data.frame()
  
  pairwise.tests <- lapply(1:ncol(ind.comb), function(i) {
    wc.test = wilcox.test(subsets[[ind.comb[1, i]]],
                          subsets[[ind.comb[2, i]]], conf.int = TRUE)
    
    pw.test <- data.frame(
      subset1 = names(subsets)[ind.comb[1, i]],
      subset2 = names(subsets)[ind.comb[2, i]],
      p.value = wc.test$p.value,
      lower = round(wc.test$conf.int[1], 3),
      upper = round(wc.test$conf.int[2], 3),
      location.shift = round(wc.test$estimate, 3)
    )
    
    pw.test
  })
  
  # Add adjusted p-values, based on the chosen method
  pw.df <- do.call("bind_rows", pairwise.tests)
  pw.df$p.value.adj <- p.adjust(pw.df$p.value, adjust.method)
  
  pw.df
}



### Kruskal-Wallis testen

#### Per process

# De Kruskal-Wallis test voor de subsets per Process


kruskal.test(memory.by.process)

# Zonder rekening te houden met leeftijd, is er een significant bewijs dat minstens 1 process een verschillend gemiddelde heeft voor het aantal gereproduceerde woorden.


#### Effect van leeftijd

# Om te controleren of er een effect is van leeftijd, doen we dezelfde test opnieuw, maar eens voor de data overeenkomend met jongeren, en de tweede keer voor de data van de oudere testpersonen.


kruskal.test(Words ~ Process, data = memory.younger)
kruskal.test(Words ~ Process, data = memory.older)


# Het effect is meer uitgesproken voor jongeren, maar in beide gevallen significant.

#### Effect van proces en leeftijd

# Overeenkomend met de interactie term in het lineaire model, kunnen we de Kruskal-Wallis test ook uitvoeren op de subsets per combinatie van Process en Age.


kruskal.test(memory.by.age.process)



### Wilcoxon-Mann-Whitney testen

# Uitgevoerd voor alle combinaties van Process en Age :


wc.df.age.process <- wc.tests(memory.by.age.process) %>%
  mutate(significant = p.value.adj < 0.05)
wc.df.age.process


# Leeftijd is volgens deze test niet altijd een significante parameter.


# Voor alle Processen, gelimiteerd tot de jongeren


wc.df.process.younger <- wc.tests(memory.by.process.younger) %>%
  mutate(significant = p.value.adj < 0.05)
wc.df.process.younger


# Voor alle Processen, gelimiteerd tot de ouderen


wc.df.process.older <- wc.tests(memory.by.process.older) %>%
  mutate(significant = p.value.adj < 0.05)
wc.df.process.older


# Er is zowel voor jongeren als voor ouderen enkel een significant verschil tussen het Counting proces en de twee andere processen, maar niet tussen de Imagery en Intentional processen onderling.



## Vergelijking van de methodes

# We kunnen dit vergelijken met de resultaten van het lineaire model :


lm.df.age.process <- lm.tests(mem.lm.int, test.data.age.process)
lm.df.process.younger <-
  lm.tests(mem.lm.int, test.data.process.younger)
lm.df.process.older <- lm.tests(mem.lm.int, test.data.process.older)


# via een inner join met het resultaat van de Wilcoxon-Mann-Whitney tests :

# Voor de jongeren :


comp.df.process.younger <- lm.df.process.younger %>%
  inner_join(wc.df.process.younger, by = c("subset1", "subset2"))
comp.df.process.younger


# Voor de ouderen


comp.df.process.older <- lm.df.process.older %>%
  inner_join(wc.df.process.older, by = c("subset1", "subset2"))
comp.df.process.older


# Voor de combinaties (Age, Process)


comp.df.age.process <- lm.df.age.process %>%
  inner_join(wc.df.age.process, by = c("subset1", "subset2"))
comp.df.age.process


# Deze tabel geeft weer voor welke subsets een significant verschil kan worden aangeduid (in dit geval op basis van de bonferroni-aangepaste p-waarde).
# Het Counting proces is significant verschillend van de Imagery en Intentional processen, voor elke leeftijdscategorie.
# Het Intentional proces voor ouderen is significant minder efficiënt dan wat de jongeren presteren in voor het Imagery en Intentional proces.

## Conclusie

# Beide methodes geven aan dat er een significant effect is van het proces, en van de leeftijd.
# De methode via lineaire regressie laat enkel toe om na te gaan of er een significant effect is tov. een referentietoestand. De Wilcoxon-Mann-Whitney tests daarentegen laten toe om alle subsets met elkaar te vergelijken.
