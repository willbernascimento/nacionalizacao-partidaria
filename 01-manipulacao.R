### -------------------------------------------------------------------------- ##
## -----------------------  Nacionalização Partidária ------------------------ ##
#
#  Descrição: Esse script automatiza uma análise da nacionalização partidária
#             no Brasil. Tem como objetivo aplicar técnicas de programação e
#             análise de dados com R.
#
## Autor: Willber Nascimento
## -------------------------------------------------------------------------- ##


## ----------- Informações sobre a estrutura do banco de dados ---------------##
# A unidade de analise de análise do banco de nacionalização partidária é 
# o partido politico i na eleição j. Conseguimos isso agregando os votos de seus
# deputados federais dentro de cada Estado e tomando sua porcentagem. Depois disso
# aplica-se a fórmula de gini. 
## ---------------------------------------------------------------------------##

## --------------- Carregamento de pacotes

library(electionsBR)
library(dplyr)
library(reshape2)
library(ineq)


## --------------- Download dos dados usando electionsBR

# obs:
# Você vai precisar de internet e muita memória (pelo menos 16GB) RAM pra 
# alocar todos os objetos abaixo. Como eu não possuo, vou salvá-los
# e deletar da memória. Além disso, vou dividir as tarefas para cada
# um dos objetos.

# Criando as pastas para alocar os dados: Linux

system("mkdir -p ./dados/{brutos/{votos,candidatos},manipulados}")

# Windowns: # Não sei.

# --------------- dowload dos dados -----------
# Faz o download, salva o banco, filtra os deputados federais

bd98 <- vote_mun_zone_fed(1998)
bd02 <- vote_mun_zone_fed(2002)
bd06 <- vote_mun_zone_fed(2006)
bd10 <- vote_mun_zone_fed(2010)

saveRDS(bd98, "./dados/brutos/votos/vote_mun_zone_fed_1998.rds")
saveRDS(bd02, "./dados/brutos/votos/vote_mun_zone_fed_2002.rds")
saveRDS(bd06, "./dados/brutos/votos/vote_mun_zone_fed_2006.rds")
saveRDS(bd10, "./dados/brutos/votos/vote_mun_zone_fed_2010.rds")

bd98 <- bd98[bd98$DESCRICAO_CARGO=='DEPUTADO FEDERAL',]
bd02 <- bd02[bd02$DESCRICAO_CARGO=='DEPUTADO FEDERAL',]
bd06 <- bd06[bd06$DESCRICAO_CARGO=='DEPUTADO FEDERAL',]
bd10 <- bd10[bd10$DESCRICAO_CARGO=='DEPUTADO FEDERAL',]

# Eleição 2014
bd14 <- vote_mun_zone_fed(2014)
saveRDS(bd14, "./dados/brutos/votos/vote_mun_zone_fed_2014.rds")
bd14 <- bd14[bd14$DESCRICAO_CARGO=='Deputado Federal',]
bd14$TRANSITO <- NULL


# Eleições 2018
bd18 <- vote_mun_zone_fed(2018)
saveRDS(bd18, "./dados/brutos/votos/vote_mun_zone_fed_2018.rds")
bd18 <- bd18[bd18$DESCRICAO_CARGO=='Deputado Federal',]


## ------------- Agregação dos dados

# Juntar bancos de dados que possuem a mesma estrutura.

bd_comp <- rbind(bd98, bd02, bd06, bd10, bd14)

# agregação do total de votos por partido e UF
# em suma: somamos os votos dos deputados para cada partido

bd_comp <- aggregate(TOTAL_VOTOS ~ ANO_ELEICAO + SIGLA_UF + SIGLA_PARTIDO,
            bd_comp,sum)

# Agregar dados de 2018

bd18 <- aggregate(TOTAL_VOTOS ~ ANO_ELEICAO + SIGLA_UF + SIGLA_PARTIDO,
                  bd18,sum)

# juntar dados de 2018 com os demais

bd_comp <- rbind(bd_comp, bd18)



## A partir de agora inicia-se os calculos necessários para criar o indice
## de nacionalização partidária

# ------------ agregar o desempenho eleitoral por eleição e UF
# calculo da proporção de votos dos partidos por UF 
# ordenar o banco de acordo com proporção de votos

bd_comp <- bd_comp %>%
  group_by(ANO_ELEICAO, SIGLA_UF) %>%
  summarise(
    votos_validos = sum(TOTAL_VOTOS)
  ) %>% left_join(.,bd_comp,by = c('ANO_ELEICAO','SIGLA_UF')) %>%
  mutate(prop_votos=(TOTAL_VOTOS/votos_validos)) %>%
  arrange(ANO_ELEICAO, SIGLA_UF,-prop_votos)

# total de votos dos partidos somados nacionalmente

votos_nacio <-
  bd_comp %>%
  group_by(ANO_ELEICAO, SIGLA_PARTIDO) %>%
  summarise(
    votos_nacio = sum(TOTAL_VOTOS)
  )

# proporção de votos nacionais

votos_nacio <- 
  votos_nacio %>%
  group_by(ANO_ELEICAO) %>%
  summarise(
    validos_nacio = sum(votos_nacio)
  ) %>% left_join(.,votos_nacio, by='ANO_ELEICAO') %>%
  mutate(prop_nacio = votos_nacio/validos_nacio)



## -----------  Nacionalização partidária
## calculo do indice de nacionalização partidária seguindo proposta de 
## Jones e Mainwaring (2003).
## o índice é a subtração de 1 do coeficiente de Gini da proporção de votos
## dos partidos em cada uma das UFs, em cada uma das eleições;

nacionaliz <-  
  bd_comp %>%
  group_by(ANO_ELEICAO, SIGLA_PARTIDO) %>%
  summarise(
    gini = 1-(ineq(prop_votos, type = 'Gini'))
  )

# agregar a nacionalização com as informações disponiveis

nacionaliz <- left_join(
  nacionaliz, votos_nacio[,c(1,3:5)], 
  by=c('ANO_ELEICAO', 'SIGLA_PARTIDO')
)

# toma o percentual 

nacionaliz$prop_nacio <- nacionaliz$prop_nacio*100

# renomeia a variável

names(nacionaliz)[5] <- 'perc_nacio'

## ----------- formatações no nome dos partidos
## altera o nome de partidos, remove espaços e dispoe em caixa alta

nacionaliz$SIGLA_PARTIDO <- gsub(' ', '', nacionaliz$SIGLA_PARTIDO)
nacionaliz$SIGLA_PARTIDO <- toupper(nacionaliz$SIGLA_PARTIDO)
nacionaliz$SIGLA_PARTIDO[nacionaliz$SIGLA_PARTIDO=='PFL'] <- 'DEM'
nacionaliz$SIGLA_PARTIDO[nacionaliz$SIGLA_PARTIDO=='SOLIDARIEDADE'] <- 'SD'
nacionaliz$SIGLA_PARTIDO[nacionaliz$SIGLA_PARTIDO=='MDB'] <- 'PMDB'
nacionaliz$SIGLA_PARTIDO[nacionaliz$SIGLA_PARTIDO=='PPB'] <- 'PP'

nacionaliz$SIGLA_PARTIDO <-
  recode(
    nacionaliz$SIGLA_PARTIDO,
    PFL = "DEM",
    PMDB = "MDB",
    PPB = "PP",
    SOLIDARIEDADE = "SD"
  )

saveRDS(nacionaliz, "dados/manipulados/nacionalizacao.rds")
