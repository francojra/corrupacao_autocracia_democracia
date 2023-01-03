
# Corrupção --------------------------------------------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 02/01/23 ---------------------------------------------------------------------------------------------------------------------------
# Referência: https://ourworldindata.org/corruption ----------------------------------------------------------------------------------------

# Sobre os dados ---------------------------------------------------------------------------------------------------------------------------


# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(hrbrthemes)
library(ggthemes)
library(cols4all)

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

corrup <- read.csv("TI-corruption-perception-index.csv")
view(corrup)
names(corrup)

# Manipular dados --------------------------------------------------------------------------------------------------------------------------

corrup <- corrup %>%
  select(-Code) %>%
  rename(corruption = Corruption.Perception.Index...Transparency.International..2018.) %>%
  view()

corrup1 <- corrup %>%
  filter(Entity %in% c("United States", "Germany", "Japan",
                       "China", "Cuba", "North Korea")) %>%
  group_by(Entity) %>%
  summarise(media = mean(corruption),
            sd = sd(corruption), n = n(),
            se = sd/sqrt(n)) %>%
  view()

corrup2 <- corrup %>%
  filter(Entity %in% c("United States", "Germany", "Japan",
                       "China", "Cuba", "North Korea")) %>%
  view()
