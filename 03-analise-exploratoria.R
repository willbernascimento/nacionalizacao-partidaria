### -------------------------------------------------------------------------- ##
## -----------------------  Nacionalização Partidária ------------------------ ##
#
#  Descrição: Esse script automatiza uma análise da nacionalização partidária
#             no Brasil. Tem como objetivo aplicar técnicas de programação e
#             análise de dados com R.
#
## Autor: Willber Nascimento
## -------------------------------------------------------------------------- ##

library(dplyr)
library(ggplot2)
library(ggthemes)


nacionalizacao <- readRDS("./dados/manipulados/nacionalizacao.rds")

## -------------------------------------------------------------------------- ##
## --: PARTE 1: Nacionalização ---------------------------------------------- ##
## -------------------------------------------------------------------------- ##


# descritivos da nacionalização partidária

desc_nacionalizacao <- nacionalizacao %>% 
  group_by(ANO_ELEICAO) %>% 
  summarise(
    N = n(),
    Mínimo = min(gini),
    Máximo = max(gini),
    Média  = mean(gini),
    Desvio = sd(gini),
    Coef.V = (Desvio/Média)*100
  ) %>% rename("Eleição" = ANO_ELEICAO )

write.table(desc_nacionalizacao,file='./tabelas/desc_nacio.csv', sep = ';', dec = ',', row.names = F)



ggINPBox <- 
nacionalizacao %>% 
  ggplot(., aes(factor(ANO_ELEICAO), gini))+
  geom_boxplot() + 
  labs(x="", y="Nacionalização partidária (INP)", 
       title="Descritivos do INP por eleição",
       caption = "Willber Nascimento \nFonte: TSE/electionsBR")+
  theme_hc()

ggsave("./graficos/ggINPBox.png", height=3, width=5, units='in', dpi=450)

ggINPmedia <- 
nacionalizacao %>% 
  group_by(ANO_ELEICAO) %>% 
  summarise(
    N = n(),
    Mínimo = min(gini),
    Máximo = max(gini),
    Média  = mean(gini),
    Desvio = sd(gini),
    Coef.V = (Desvio/Média)*100
  ) %>% ggplot(., aes(as.factor(ANO_ELEICAO), Média, group=1))+
  geom_line()+
  geom_point()+
  labs(x='', y='Nacionalização partidária (INP)',
       title = "Média do INP por eleição",
       caption = "Willber Nascimento \nFonte: TSE/electionsBR") +
  theme_hc()

ggsave("./graficos/ggINPmedia.png", height=3, width=5, units='in', dpi=450)


## 1998

ggINPpartidos98 <- 
ggplot(nacionalizacao[nacionalizacao$ANO_ELEICAO == 1998 &
                        nacionalizacao$perc_nacio >= 1, ],
       aes(reorder(SIGLA_PARTIDO,gini), gini)) +
  geom_bar(stat = 'identity') +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(
          angle = 90,
          vjust = 0.5,
          hjust = 1
        )) +
  geom_hline(yintercept = 0.449, linetype = "dashed") +
  labs(x = '', y = 'Média do INP') +
  coord_flip() +
  theme_hc()



ggsave("./graficos/bar1998.png", height=3, width=5, units='in', dpi=450)

## 2002
ggINPpartidos02 <- 
ggplot(nacionalizacao[nacionalizacao$ANO_ELEICAO==2002 & nacionalizacao$perc_nacio>=1,], 
       aes(reorder(SIGLA_PARTIDO, gini), gini))+
  geom_bar(stat = 'identity')+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  geom_hline(yintercept=0.44, linetype="dashed")+
  labs(x='', y='Média do INP') +
  coord_flip() +
  theme_hc()

ggsave("./graficos/bar2002.png", height=3, width=5, units='in', dpi=450)


## 2006
ggINPpartidos06 <- 
ggplot(nacionalizacao[nacionalizacao$ANO_ELEICAO==2006 & nacionalizacao$perc_nacio>=1,], 
       aes(reorder(SIGLA_PARTIDO, gini), gini))+
  geom_bar(stat = 'identity')+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  geom_hline(yintercept=0.49, linetype="dashed")+
  labs(x='', y='Média do INP') +
  coord_flip() +
  theme_hc()

ggsave("./graficos/bar2006.png", height=3, width=5, units='in', dpi=450)

# 2010
ggINPpartidos10 <- 
ggplot(nacionalizacao[nacionalizacao$ANO_ELEICAO==2010 & nacionalizacao$perc_nacio>=1,], 
       aes(reorder(SIGLA_PARTIDO, gini), gini))+
  geom_bar(stat = 'identity')+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  geom_hline(yintercept=0.49, linetype="dashed")+
  labs(x='', y='Média do INP') +
  coord_flip() +
  theme_hc()

ggsave("./graficos/bar2010.png", height=3, width=5, units='in', dpi=450)


# 2014
ggINPpartidos14 <- 
ggplot(nacionalizacao[nacionalizacao$ANO_ELEICAO==2014 & nacionalizacao$perc_nacio>=1,], 
       aes(reorder(SIGLA_PARTIDO, gini), gini))+
  geom_bar(stat = 'identity')+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  geom_hline(yintercept=0.498, linetype="dashed")+
  labs(x='', y='Média do INP') +
  coord_flip() +
  theme_hc()

ggsave("./graficos/bar2014.png", height=3, width=5, units='in', dpi=450)

# 2018

ggINPpartidos18 <- 
ggplot(nacionalizacao[nacionalizacao$ANO_ELEICAO==2018 & nacionalizacao$perc_nacio>=1,], 
       aes(reorder(SIGLA_PARTIDO, gini), gini))+
  geom_bar(stat = 'identity')+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  geom_hline(yintercept=0.532, linetype="dashed")+
  labs(x='', y='Média do INP') +
  coord_flip() +
  theme_hc()

ggsave("./graficos/bar2018.png", height=3, width=5, units='in', dpi=450)


## ------------------------------------------------------------------------ ##
## ------------------------------------------------------------------------ ##
## ---------------------- PARTIDOS SELECIONADOS --------------------------- ##
## ------------------------------------------------------------------------ ##
## ------------------------------------------------------------------------ ##

nacionalizacao %>% 
  filter(SIGLA_PARTIDO %in% c('PP', 'PFL', 'PTB', 'PMDB', 'PSDB', 'PDT','PT')) %>% 
  ggplot(aes(ANO_ELEICAO, gini, fill=SIGLA_PARTIDO, colour=SIGLA_PARTIDO))+
  geom_line(size=1)+
  geom_point()


# nacionalizacao %>% 
#   group_by(ANO_ELEICAO) %>% 
#   summarise(
#     N = n(),
#     Mínimo = min(),
#     Máximo = min(),
#     Média  = mean(),
#     Desvio = sd(),
#     Coef.V = (Desvio/Média)*100
#   )


nacionalizacao %>% 
  group_by(ANO_ELEICAO) %>% 
  summarise(
    INSP=sum(gini*(perc_nacio/100))
  )

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##