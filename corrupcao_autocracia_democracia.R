
# Corrupção --------------------------------------------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 02/01/23 ---------------------------------------------------------------------------------------------------------------------------
# Referência: https://ourworldindata.org/corruption ----------------------------------------------------------------------------------------

# Sobre os dados ---------------------------------------------------------------------------------------------------------------------------

### Esse relatório apresenta dados e evidências empírica sobre corrupção - um importante problema
### de custos políticos, econômicos e ambientais no mundo.

### Corrupção envolve muitos diferentes aspectos, e portanto, é difícil dar uma precisa e compreensiva
### definição. Entretanto, a maioria das definiçõs sobre corrupção é a ideia de que a corrupção
### implica no abuso da confiança do poder para ganhos privados. Exemplos clássicos incluem suborno,
### clientelismo e desvio de fundos. Outros exemplos, muitas vezes mais sutis e por vezes até legais 
### de corrupção incluem o lobbying e o patrocínio.

### Apesar dos dados sobre corrupção a longo prazo serem muito limitados, exemplos históricos sugerem
### que corrupção tem sido uma persistente característica das sociedades humanas no tempo e espaço.


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
  scale_x_discrete(labels = c("Coreia do\n Norte", "China", "Cuba",
                              "Estados\n Unidos", "Japão", "Alemanha")) +
  labs(x = "Países", y = "Índice de Transparência\n da Corrupção") +
  theme_ipsum(axis_title_size = 16,
              axis_text_size = 14) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black")) 

ggplot(corrup2, aes(x = Year, y = corruption,
                    group = Entity, color = Entity)) +
  geom_point(shape = 15, size = 2.5) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("#88CCEE", "#CC6677",
                               "#DDCC77", "#117733",
                               "#332288", "#AA4499"),
                               labels = c("China", "Cuba", "Alemanha",
                                          "Japão", "Coreia do Norte",
                                          "Estados Unidos")) +
  labs(x = "Tempo (anos)", y = "Índice de Transparência\n da Corrupção",
       color = "Países") +
  theme_ipsum(axis_title_size = 16,
              axis_text_size = 14) +
  theme(axis.text = element_text(color = "black"))

ggplot(corrup3, aes(x = Year, y = corruption,
                    group = Entity, color = Entity)) +
  geom_line(size = 2) +
  scale_color_manual(values = c('#1B9E77', '#999999','#E69F00'),
                     labels = c("Brasil", "China", "Estados Unidos")) +
  labs(x = "Tempo (anos)", y = "Índice de Transparência\n da Corrupção",
       color = "Países") +
  theme_light() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(color = "black", size = 15),
        legend.text = element_text(size = 12))
