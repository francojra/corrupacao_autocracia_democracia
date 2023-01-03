
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

corrup3 <- corrup %>%
  filter(Entity %in% c("United States", "China", "Brazil")) %>%
  view()

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

c4a("safe", 6)

ggplot(corrup1, aes(x = fct_reorder(Entity, media), 
                    y = media, fill = Entity)) +
  geom_col(width = 0.9) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                    width = 0.2, size = 0.8) +
  scale_fill_manual(values = c("#88CCEE", "#CC6677",
                               "#DDCC77", "#117733",
                               "#332288", "#AA4499")) +
  scale_y_continuous(expand = expansion(mult = c(0,0))) +
  theme_ipsum(axis_title_size = 16,
              axis_text_size = 14) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black")) 
  
