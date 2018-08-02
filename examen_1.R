library(tidyverse)
library(mgcv)
library(car)
library(boot)

# Opladen data
load(file = "memory.rda")

# Data leren kennen
str(memory)
memory %>% 
  group_by(Age, Process) %>% 
  count()

# Visualiseer data
ggplot(memory, aes(Words, fill = Age)) +
  geom_histogram(position = "dodge") +
  facet_wrap(~ Process)

ggplot(memory, aes(Words, fill = Process)) +
  geom_histogram(position = "dodge") +
  facet_wrap(~ Age)

ggplot(memory, aes(Words)) +
  geom_histogram() +
  facet_grid(Age ~ Process)
# Voor Counting : geen verschil tussen oud en jong
# Voor Imagery en Intentional : groter aantal woorden voor jong tov. oud

# Binnen leeftijdscategorie : 
#   Counting is steeds lager dan de andere groepen
#   Geen duidelijk verschil tussen Imagery en Intentional

# a. Via regressie
# Veronderstellingen : 
# - residuals zijn normaal verdeeld
# - variantie van de residuals zijn onafhankelijk van de waarde van de onafhankelijke veranderlijke
# - het gemiddelde is een lineaire functie van de onafhankelijke variabelen

# lineair model : 2 mogelijkheden
# - met interactie
# - zonder interactie

mem.lm <- lm(Words ~ Age + Process, data = memory)
summary(mem.lm)
anova(mem.lm)

par(mfrow=c(2,2))
plot(mem.lm)

mem.lm.pred <- predict(mem.lm, se.fit = TRUE)
mem.lm.data <- memory %>% 
  mutate(fit = mem.lm.pred$fit,
         l = mem.lm.pred$fit - 1.96*mem.lm.pred$se.fit,
         u = mem.lm.pred$fit + 1.96*mem.lm.pred$se.fit)

ggplot(mem.lm.data, aes(Process, Words, color = Age, fill = Age)) +
  geom_point() +
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(ymin = l, ymax = u), alpha = 0.1)

# --> Volgens summary significante invloed van Age en Process
# De voorwaarden zijn niet 100% voldaan : 
# - heteroscedasticiteit
# - errors niet perfect normaal verdeeld

# deze manier van werken komt overeen met een anova
mem.aov <- aov(Words ~ Age + Process, data = memory)
summary(mem.aov)
anova(mem.aov)

# Met Interactieterm
mem.lm.int <- lm(Words ~ Age * Process, data = memory)
summary(mem.lm.int)
anova(mem.lm.int)

# --> Ook de interactieterm lijkt significant
mem.lm.int.pred <- predict(mem.lm.int, se.fit = TRUE)
mem.lm.int.data <- memory %>% 
  mutate(fit = mem.lm.int.pred$fit,
         l = mem.lm.int.pred$fit - 1.96*mem.lm.int.pred$se.fit,
         u = mem.lm.int.pred$fit + 1.96*mem.lm.int.pred$se.fit)

ggplot(mem.lm.int.data, aes(Process, Words, color = Age, fill = Age)) +
  geom_point() +
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(ymin = l, ymax = u), alpha = 0.1)

par(mfrow=c(2,2))
plot(mem.lm.int)

# ook hier zelfde opmerkingen : niet homoscedastisch, errors redelijk goed normaal verdeeld

mem.lm.num <- lm(Words ~ as.numeric(Age) * as.numeric(Process), data = memory)
summary(mem.lm.num)
anova(mem.lm)
mem.lm.num.pred <- predict(mem.lm.num, se.fit = TRUE)
mem.lm.num.data <- memory %>% 
  mutate(fit = mem.lm.num.pred$fit,
         l = mem.lm.num.pred$fit - 1.96*mem.lm.num.pred$se.fit,
         u = mem.lm.num.pred$fit + 1.96*mem.lm.num.pred$se.fit)

ggplot(mem.lm.num.data, aes(as.numeric(Process), Words, color = Age, fill = Age)) +
  geom_point() +
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(ymin = l, ymax = u), alpha = 0.1)

# Beste model ?
AIC(mem.lm, mem.lm.int)

# lm voor categorische data met interactie doet --> gemiddelde per categorie
str(memory)
test.data <- expand.grid(Age = c("Older","Younger"), Process = c("Counting","Imagery","Intentional"))
str(test.data)
test.data

pred.lm.int <- predict(mem.lm.int, newdata = test.data, se.fit = TRUE)
res.table <- test.data %>% 
  bind_cols(value = pred.lm.int$fit,
            lower = pred.lm.int$fit - 1.96*pred.lm.int$se.fit,
            upper = pred.lm.int$fit + 1.96*pred.lm.int$se.fit) %>% 
  mutate(subset = paste(Age, Process, sep = "."))

# Vergelijken met gemiddelden
avg.table.age.process <- memory %>% 
  group_by(Age, Process) %>% 
  summarize(mean.age.process = mean(Words))


comp.table <- res.table %>% 
  inner_join(avg.table.age.process, by = c("Age","Process"))

comp.table

# Confidence interval via bootstrap ?
mem.lm.bs <- Boot(mem.lm)
confint(mem.lm.bs)
mem.lm.int.bs <- Boot(mem.lm.int)
confint(mem.lm.int.bs)

# Veronderstellingen lm
# - residuals normaal verdeeld
# - residuals onafhankelijk van gefitte waarde
# Rang-gebaseerde methodes:
# - Kruskal-Wallis test
#   - per leeftijdscategorie --> testen van verschillen in Proces
#   - per process --> testen van verschillen in leeftijd
#   - per process/Age --> testen van interacties ?
# - Wilcoxon test voor individuele data sets : H1 in termen van Prob. Index
# - Wilcoxon signed-rank test : niet, wegens geen gepaarde data (random toewijzing van personen aan de groepen)
# - Friedman test ? maar geen echte blocking factoren
# - Location-shift model ? --> via Hodges-Lehman schatter (ook via Wilcoxon test)

memory.split.all <- lapply(split(memory, list(memory$Age, memory$Process)), '[[', "Words")
memory.split.age <- lapply(split(memory, memory$Age), '[[', "Words")
memory.split.process <- lapply(split(memory, memory$Process), '[[', "Words")

str(memory.split.all)
str(memory.split.age)
str(memory.split.process)

# Kruskal-Wallis test op process
kruskal.test(Words ~ Process, data = memory)

# --> Zonder rekening te houden met leeftijd, is er een significant bewijs dat minstens 1 process een verschillende gemiddelde heeft voor het aantal woorden

# Is er een effect van leeftijd ? We doen de test voor ouderen en jongeren
kruskal.test(Words ~ Process, data = memory %>% filter(Age == "Older"))
kruskal.test(Words ~ Process, data = memory %>% filter(Age == "Younger"))

# --> Het effect is meer uitgesproken voor jongeren, maar in beide gevallen significant

# Kruskal-Wallis rank sum test op de verschillende subsets (dus alle subsets Age,Process met elkaar vergeleken)
kruskal.test(lapply(memory.split.all, '[[', "Words"))
# --> minstens 1 combinatie (Age,Process) heeft verschillende mu

# Wilcoxon-Mann-Whitney test voor individuele subsets 
# Eerst uitvoeren voor de verschillende processen, zonder rekening te houden met leeftijd

wc.tests <- function(subsets) {
  ind.comb <- combn(1:length(subsets), 2)
  
  pw.result <- data.frame()
  
  pairwise.tests <- lapply(1:ncol(ind.comb), function(i) { 
    wc.test = wilcox.test(subsets[[ind.comb[1,i]]],
                          subsets[[ind.comb[2,i]]], conf.int = TRUE)
    
    pw.test <- data.frame(
      subset1 = names(subsets)[ind.comb[1,i]],
      subset2 = names(subsets)[ind.comb[2,i]],
      p.value = wc.test$p.value,
      lower = round(wc.test$conf.int[1],3),
      upper = round(wc.test$conf.int[2],3),
      location.shift = round(wc.test$estimate,3)
    )
    
    # pw.result <- pw.result %>% 
    #   bind_rows(pw.test)
    pw.test
  })
  
  do.call("bind_rows", pairwise.tests)
}

lm.tests <- function(pred.results) {
  ind.comb <- combn(1:length(pred.results), 2) 
  
  pairwise.diffs <- lapply(1:ncol(ind.comb), function(i) { 
    # print(pred.results[ind.comb[1,i],])
    # print(pred.results[ind.comb[2,i],])
    diff = pred.results[ind.comb[1,i],"value"] - pred.results[ind.comb[2,i],"value"]
    
    lm.val <- data.frame(
      subset1 = pred.results[ind.comb[1,i],"subset"],
      subset2 = pred.results[ind.comb[2,i],"subset"],
      diff = round(diff, 3)
    )
    
    # pw.result <- pw.result %>% 
    #   bind_rows(pw.test)
    print(lm.val)
    lm.val
  })
  
  do.call("bind_rows", pairwise.diffs)
}

(lm.df.all <- lm.tests(res.table))

(wc.df.all <- wc.tests(memory.split.all) %>% 
    mutate(significant = p.value < 0.05 ))

(comp.df.all <- lm.df.all %>% 
    inner_join(wc.df.all, by = c("subset1","subset2")))

(wc.df.proc <- wc.tests(memory.split.process) %>% 
    mutate(significant = p.value < 0.05 ))

(wc.df.age <- wc.tests(memory.split.age) %>% 
    mutate(significant = p.value < 0.05 ))


