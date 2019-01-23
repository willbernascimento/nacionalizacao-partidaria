## -------------------------------------------------------------------------- ##
## ---------------  DESEMPENHO CANDIDATURAS NACIONAL DOS PARTIDOS ----------- ##
## -------------------------------------------------------------------------- ##
## -------: Esse script baixa e sumariza informações sobre as candidaturas -- ##
## -----: dos partidos nas eleições gerais de 1998-2018. Usamos como fonte -- ##
## -----: o pacote electionsBR. --------------------------------------------- ##
## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# baixando os dados do TSE

library(electionsBR)

cand98 <- candidate_fed(1998)
cand02 <- candidate_fed(2002)
cand06 <- candidate_fed(2006)
cand10 <- candidate_fed(2010)
cand14 <- candidate_fed(2014)
cand18 <- candidate_fed(2018)

# juntando todos em um objeto (14 e 15 tem N de variáveis distintos)
cand_comp <- rbind(cand98, cand02, cand06, cand10)

cand_comp2 <- rbind(cand14, cand18)

# filtrando variáveis
cand_comp <- cand_comp %>% 
select(ANO_ELEICAO,NUM_TURNO,SIGLA_UF,DESCRICAO_CARGO,SIGLA_PARTIDO,DESC_SIT_TOT_TURNO)

# filtrando variáveis
cand_comp2 <- cand_comp2 %>% 
select(ANO_ELEICAO,NR_TURNO,SG_UF,DS_CARGO,SG_PARTIDO,DS_SIT_TOT_TURNO)

# renomeando
names(cand_comp2) <- c('ANO_ELEICAO','NUM_TURNO','SIGLA_UF','DESCRICAO_CARGO','SIGLA_PARTIDO','DESC_SIT_TOT_TURNO')

# juntando na mesma base
cand_comp <- rbind(cand_comp, cand_comp2)

# remove NA's
cand_comp <- cand_comp[!is.na(cand_comp$DESC_SIT_TOT_TURNO),]

# manter candidatos para os quais haja resultados normais
# remover: caçados, subsituidos, renuncias, falecimentos, registros negados

cand_comp <- cand_comp[cand_comp$DESC_SIT_TOT_TURNO %in% 
                         c('SUPLENTE', 'NÃO ELEITO','MÉDIA','ELEITO POR QP',
                           'ELEITO POR MÉDIA','ELEITO','2º TURNO '),]

# remover Vices e Presidente
cand_comp <- cand_comp[cand_comp$DESCRICAO_CARGO %in% 
                         c('DEPUTADO DISTRITAL', 'DEPUTADO ESTADUAL',
                           'DEPUTADO FEDERAL', 'GOVERNADOR', 'SENADOR'),]

# o DF tem Deputados Distritais. Precisamos renomeá-los
# para Estadual, como as outras Uf's

cand_comp$DESCRICAO_CARGO[cand_comp$DESCRICAO_CARGO=='DEPUTADO DISTRITAL'] <-'DEPUTADO ESTADUAL'

# PFL trocou de nome

cand_comp$SIGLA_PARTIDO[cand_comp$SIGLA_PARTIDO=='PFL'] <- 'DEM'
cand_comp$SIGLA_PARTIDO[cand_comp$SIGLA_PARTIDO=='SOLIDARIEDADE'] <- 'SD'
cand_comp$SIGLA_PARTIDO[cand_comp$SIGLA_PARTIDO=='MDB'] <- 'PMDB'
cand_comp$SIGLA_PARTIDO[cand_comp$SIGLA_PARTIDO=='PPB'] <- 'PP'

cand_comp$SIGLA_PARTIDO <- gsub(' ', '', cand_comp$SIGLA_PARTIDO)
cand_comp$SIGLA_PARTIDO <- toupper(cand_comp$SIGLA_PARTIDO)
# somando o total de candidaturas

cand_comp_ncand <- cand_comp %>% 
  filter(NUM_TURNO==1) %>% 
  group_by(ANO_ELEICAO, DESCRICAO_CARGO, SIGLA_PARTIDO) %>% 
  summarise(
    nCand = n()
  ) %>% mutate(percCand=(nCand/sum(nCand))*100)

# total de eleitos

cand_comp_ncandEleitos <- cand_comp %>% 
  filter(DESC_SIT_TOT_TURNO %in% c('MÉDIA','ELEITO POR QP','ELEITO POR MÉDIA','ELEITO')) %>% 
  group_by(ANO_ELEICAO, DESCRICAO_CARGO, SIGLA_PARTIDO) %>% 
  summarise(
    nCandEleitos = n()
  ) %>% mutate(percCandEleitos=(nCandEleitos/sum(nCandEleitos))*100)

# unificar as bases de candidaturas e eleitos dos partidos

cand_desemp <- 
merge(
  cand_comp_ncand,
  cand_comp_ncandEleitos,
  by=c('ANO_ELEICAO', 'DESCRICAO_CARGO', 'SIGLA_PARTIDO'),
  all = T
  )

# substituir NA por 0

cand_desemp$nCandEleitos[is.na(cand_desemp$nCandEleitos)] <- 0
cand_desemp$percCandEleitos[is.na(cand_desemp$percCandEleitos)] <- 0

# Inserir o Gini (nacionalização)

cand_desemp <- 
merge(
  cand_desemp,
  nacionaliz,
  by=c('ANO_ELEICAO', 'SIGLA_PARTIDO'),
  all.x = T
)


