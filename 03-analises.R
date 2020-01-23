### -------------------------------------------------------------------------- ##
## -----------------------  Nacionalização Partidária ------------------------ ##
#
#  Descrição: Esse script automatiza uma análise da nacionalização partidária
#             no Brasil. Tem como objetivo aplicar técnicas de programação e
#             análise de dados com R.
#
## Autor: Willber Nascimento
## -------------------------------------------------------------------------- ##


## -------------------------------------------------------------------------- ##
## --: PARTE 1: Nacionalização ---------------------------------------------- ##
## -------------------------------------------------------------------------- ##

library(dplyr)


# descritivos da nacionalização partidária
nacionaliz %>% 
  group_by(ANO_ELEICAO) %>% 
  summarise(
    N = n(),
    Mínimo = min(gini),
    Máximo = max(gini),
    Média  = mean(gini),
    Desvio = sd(gini),
    Coef.V = (Desvio/Média)*100
  ) %>% write.table(.,file='./tabelas/desc_nacio.csv', sep = ';', dec = ',', row.names = F)


library(ggplot2)

nacionaliz %>% 
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
  labs(x='', y='Média do INP')

ggsave("./graficos/media_naci.png", height=3, width=5, units='in', dpi=450)


## 1998

ggplot(nacionaliz[nacionaliz$ANO_ELEICAO==1998 & nacionaliz$perc_nacio>=1,], 
       aes(reorder(SIGLA_PARTIDO, -gini), gini))+
  geom_bar(stat = 'identity')+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  geom_hline(yintercept=0.449, linetype="dashed")+
  labs(x='', y='Média do INP')

ggsave("./graficos/bar1998.png", height=3, width=5, units='in', dpi=450)

## 2002

ggplot(nacionaliz[nacionaliz$ANO_ELEICAO==2002 & nacionaliz$perc_nacio>=1,], 
       aes(reorder(SIGLA_PARTIDO, -gini), gini))+
  geom_bar(stat = 'identity')+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  geom_hline(yintercept=0.437, linetype="dashed")+
  labs(x='', y='Média do INP')

ggsave("./graficos/bar2002.png", height=3, width=5, units='in', dpi=450)


## 2006
ggplot(nacionaliz[nacionaliz$ANO_ELEICAO==2006 & nacionaliz$perc_nacio>=1,], 
       aes(reorder(SIGLA_PARTIDO, -gini), gini))+
  geom_bar(stat = 'identity')+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  geom_hline(yintercept=0.487, linetype="dashed")+
  labs(x='', y='Média do INP')

ggsave("./graficos/bar2006.png", height=3, width=5, units='in', dpi=450)

# 2010
ggplot(nacionaliz[nacionaliz$ANO_ELEICAO==2010 & nacionaliz$perc_nacio>=1,], 
       aes(reorder(SIGLA_PARTIDO, -gini), gini))+
  geom_bar(stat = 'identity')+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  geom_hline(yintercept=0.489, linetype="dashed")+
  labs(x='', y='Média do INP')

ggsave("./graficos/bar2010.png", height=3, width=5, units='in', dpi=450)


# 2014
ggplot(nacionaliz[nacionaliz$ANO_ELEICAO==2014 & nacionaliz$perc_nacio>=1,], 
       aes(reorder(SIGLA_PARTIDO, -gini), gini))+
  geom_bar(stat = 'identity')+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  geom_hline(yintercept=0.498, linetype="dashed")+
  labs(x='', y='Média do INP')

ggsave("./graficos/bar2014.png", height=3, width=5, units='in', dpi=450)

# 2018
ggplot(nacionaliz[nacionaliz$ANO_ELEICAO==2018 & nacionaliz$perc_nacio>=1,], 
       aes(reorder(SIGLA_PARTIDO, -gini), gini))+
  geom_bar(stat = 'identity')+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  geom_hline(yintercept=0.532, linetype="dashed")+
  labs(x='', y='Média do INP')

ggsave("./graficos/bar2018.png", height=3, width=5, units='in', dpi=450)


## ------------------------------------------------------------------------ ##
## ------------------------------------------------------------------------ ##
## ---------------------- PARTIDOS SELECIONADOS --------------------------- ##
## ------------------------------------------------------------------------ ##
## ------------------------------------------------------------------------ ##

nacionaliz %>% 
  filter(SIGLA_PARTIDO %in% c('PP', 'PFL', 'PTB', 'PMDB', 'PSDB', 'PDT','PT')) %>% 
  ggplot(aes(ANO_ELEICAO, gini, fill=SIGLA_PARTIDO, colour=SIGLA_PARTIDO))+
  geom_line(size=1)+
  geom_point()


nacionaliz %>% 
  group_by(ANO_ELEICAO) %>% 
  summarise(
    N = n(),
    Mínimo = min(),
    Máximo = min(),
    Média  = mean(),
    Desvio = sd(),
    Coef.V = (Desvio/Média)*100
  )


nacionaliz %>% 
  group_by(ANO_ELEICAO) %>% 
  summarise(
    INSP=sum(gini*(perc_nacio/100))
  )

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##