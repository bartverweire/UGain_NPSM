library(tidyverse)

load(file="memory.rda")
View(memory)

ggplot(memory, aes(Words, color = Age, fill = Age)) +
  geom_density(alpha = 0.2) +
  facet_wrap(~ Process)

ggplot(memory, aes(Words, color = Process, fill = Process)) +
  geom_density(alpha = 0.2, adjust = 0.5) +
  facet_wrap(~ Age)
