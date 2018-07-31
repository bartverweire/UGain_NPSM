library(tidyverse)
library(mgcv)
library(car)
library(boot)

load(file = "memory.rda")
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

# Voor Counting : geen verschil tussen oud en jong
# Voor Imagery en Intentional : groter aantal woorden voor jong tov. oud

# Binnen leeftijdscategorie : 
#   Counting is steeds lager dan de andere groepen
#   Geen duidelijk verschil tussen Imagery en Intentional

# Via regressie
# lineair model : 


mem.lm.proc <- lm(Words ~ as.numeric(Process), data = memory)
summary(mem.lm.proc)
anova(mem.lm.proc)
memory$lin.proc.fitted <- fitted(mem.lm.proc)
memory$lin.proc.resid <- residuals(mem.lm.proc)

ggplot(memory, aes(as.numeric(Process), Words, color = Age)) +
  geom_point() +
  geom_line(aes(x = as.numeric(Process), y = lin.proc.fitted, linetype = "Linear")) 

mem.lm <- lm(Words ~ Age + Process, data = memory)
summary(mem.lm)
anova(mem.lm)
memory$lin.fitted <- predict(mem.lm)
memory$lin.resid <- residuals(mem.lm)

mem.aov <- aov(Words ~ Age + Process, data = memory)
summary(mem.aov)
anova(mem.aov)

memory$aov.fitted <- predict(mem.aov)
memory$aov.resid <- residuals(mem.aov)

memory %>% 
  select(Age, Process, Words, lin.fitted, aov.fitted, lin.resid, aov.resid)
# --> Volgens summary significante invloed van Age en Process

# Met Interactieterm
mem.lm.int <- lm(Words ~ Age * Process, data = memory)
summary(mem.lm.int)
anova(mem.lm.int)

memory$linint.fitted <- predict(mem.lm.int)
memory$linint.resid <- residuals(mem.lm.int)


ggplot(memory, aes(as.numeric(Age), Words, color = Process)) +
  geom_point() +
  geom_line(aes(y = lin.fitted, linetype = "Linear")) +
  geom_line(aes(y = linint.fitted, linetype = "Linear with interaction")) +
  scale_linetype_manual(values = c("Linear" = 2, "Linear with interaction" = 3))

ggplot(memory, aes(as.numeric(Process), Words, color = Age)) +
  geom_point() +
  geom_line(aes(y = lin.fitted, linetype = "Linear")) +
  geom_line(aes(y = linint.fitted, linetype = "Linear with interaction")) +
  scale_linetype_manual(values = c("Linear" = 2, "Linear with interaction" = 3))

ggplot(memory, aes(lin.fitted, lin.resid)) +
  geom_point()

ggplot(memory, aes(linint.fitted, linint.resid)) +
  geom_point()

mem.lm.num <- lm(Words ~ as.numeric(Age) + as.numeric(Process), data = memory)
summary(mem.lm.num)
anova(mem.lm)
memory$lin.num.fitted <- predict(mem.lm.num)
memory$lin.num.resid <- residuals(mem.lm.num)

# lm voor categorische data doet een fit voor elke categorie apart

# Predictions
str(memory)
test.data <- expand.grid(Age = c("Older","Younger"), Process = c("Counting","Imagery","Intentional"))
str(test.data)
test.data

res.table <- test.data %>% 
  bind_cols(pred.lm.int = predict(mem.lm.int, newdata = test.data)) %>% 
  bind_cols(pred.lm = predict(mem.lm, newdata = test.data))

# Vergelijken met gemiddelden
avg.table.age.process <- memory %>% 
  group_by(Age, Process) %>% 
  summarize(mean.age.process = mean(Words))
avg.table.age <- memory %>% 
  group_by(Age) %>% 
  summarize(mean.age = mean(Words))
avg.table.process <- memory %>% 
  group_by(Process) %>% 
  summarize(mean.process = mean(Words))

comp.table <- res.table %>% 
  inner_join(avg.table.age.process, by = c("Age","Process")) %>% 
  inner_join(avg.table.age, by = c("Age")) %>% 
  inner_join(avg.table.process, by = c("Process"))

comp.table
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

memory.split.all <- split(memory, list(memory$Age, memory$Process))
memory.split.age <- split(memory, memory$Age)
memory.split.process <- split(memory, memory$Process)

str(memory.split.age)
str(memory.split.process)

lapply(memory.split.age, function(l) l$Words)
lapply(memory.split.age, '[[', "Words")

# Kruskal-Wallis test op process
kruskal.test(Words ~ Process, data = memory)

# Is er een effect van leeftijd ? We doen de test voor ouderen en jongeren
kruskal.test(Words ~ Process, data = memory %>% filter(Age == "Older"))
kruskal.test(Words ~ Process, data = memory %>% filter(Age == "Younger"))

# --> Het effect is meer uitgesproken voor jongeren

# Kruskal-Wallis rank sum test op de verschillende subsets
kruskal.test(lapply(memory.split.all, '[[', "Words"))
# --> minstens 1 combinatie (Age,Process) heeft verschillende mu

# Wilcoxon-Mann-Whitney test voor individuele subsets 
ind.comb <- combn(1:6, 2)
pairwise.tests <- lapply(1:ncol(ind.comb), function(i) { 
  # subset.names <- names(memory.split.all)[ind.comb[,i]]
  # print(memory.split.all[[ind.comb[1,i]]])
  wc.test = wilcox.test(memory.split.all[[ind.comb[1,i]]]$Words,memory.split.all[[ind.comb[2,i]]]$Words, conf.int = TRUE)
  list(
    subsets = names(memory.split.all)[ind.comb[,i]],
    p_value = wc.test$p.value,
    conf.int = wc.test$conf.int,
    value = wc.test$estimate
  )
})
names(pairwise.tests) <- sapply(1:ncol(ind.comb), function(i) { 
  paste(names(memory.split.all)[ind.comb[,i]], collapse = "-")
})


pairwise.tests
# enkel resultaten overhouden waarvoor p-value > 0.05
for (i in 1:length(pairwise.tests)) {
  test <- pairwise.tests[[i]]
  
  if (test$p_value < 0.05) {
    print(test$subsets)
    print(paste("Verschil:",round(test$value, 3)))
    print(paste("BI: [", paste(round(test$conf.int,3), collapse = ","), "]", sep = ""))
  }
}
