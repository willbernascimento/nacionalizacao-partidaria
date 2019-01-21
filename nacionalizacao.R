
# carregamento de pacotes

library(electionsBR)
library(dplyr)
library(reshape2)
library(ineq)

# ---------------------- carregamento de funções -------------------------------
gsub2 <- function(pattern, replacement, x, ...) {
  for(i in 1:length(pattern))
    x <- gsub(pattern[i], replacement[i], x, ...)
  x
}

clean.accent <- function(x) {	gsub2(c('ä','ã','à','á','â','ê','ë','è','é','ï','ì','í','ö','õ','ò','ó','ô','ü','ù','ú','û','À','Á','É','Ê','Í','Ó','Ú','ñ','Ñ','ç','Ç','ª','º','Õ','Ô','Ã','Â','Ü',"'",'-'), c('a','a','a','a','a','e','e','e','e','i','i','i','o','o','o','o','o','u','u','u','u','A','A','E','E','I','O','U','n','n','c','C','_','_','O','O','A','A','U',' ',' '),x)
}
# ------------------------------------------------------------------------------

bd98 <- vote_mun_zone_fed(1998)
bd02 <- vote_mun_zone_fed(2002)
bd06 <- vote_mun_zone_fed(2006)
bd10 <- vote_mun_zone_fed(2010)
bd14 <- vote_mun_zone_fed(2014)

bd98 <- bd98[bd98$DESCRICAO_CARGO=='DEPUTADO FEDERAL',]
bd02 <- bd02[bd02$DESCRICAO_CARGO=='DEPUTADO FEDERAL',]
bd06 <- bd06[bd06$DESCRICAO_CARGO=='DEPUTADO FEDERAL',]
bd10 <- bd10[bd10$DESCRICAO_CARGO=='DEPUTADO FEDERAL',]
bd14 <- bd14[bd14$DESCRICAO_CARGO=='DEPUTADO FEDERAL',]
bd14$TRANSITO <- NULL

bd_comp <- rbind(bd98, bd02, bd06, bd10, bd14)

bd_comp$SIGLA_PARTIDO[bd_comp$SIGLA_PARTIDO=='PFL'] <- 'DEM'

bd_comp <-
  aggregate(TOTAL_VOTOS ~ ANO_ELEICAO + SIGLA_UF + SIGLA_PARTIDO,
            bd_comp,sum)

bd_comp <- bd_comp %>%
  group_by(ANO_ELEICAO, SIGLA_UF) %>%
  summarise(
    votos_validos = sum(TOTAL_VOTOS)
  ) %>% left_join(.,bd_comp,by = c('ANO_ELEICAO','SIGLA_UF')) %>%
  mutate(prop_votos=(TOTAL_VOTOS/votos_validos)) %>%
  arrange(ANO_ELEICAO, SIGLA_UF,-prop_votos)

votos_nacio <-
  bd_comp %>%
  group_by(ANO_ELEICAO, SIGLA_PARTIDO) %>%
  summarise(
    votos_nacio = sum(TOTAL_VOTOS)
  )

votos_nacio <- 
  votos_nacio %>%
  group_by(ANO_ELEICAO) %>%
  summarise(
    validos_nacio = sum(votos_nacio)
  ) %>% left_join(.,votos_nacio, by='ANO_ELEICAO') %>%
  mutate(prop_nacio = votos_nacio/validos_nacio)

# Nacionalização partidária

nacionaliz <-  
  bd_comp %>%
  group_by(ANO_ELEICAO, SIGLA_PARTIDO) %>%
  summarise(
    gini = 1-(ineq(prop_votos, type = 'Gini'))
  )

nacionaliz <- left_join(
  nacionaliz, votos_nacio[,c(1,3:5)], 
  by=c('ANO_ELEICAO', 'SIGLA_PARTIDO')
)
