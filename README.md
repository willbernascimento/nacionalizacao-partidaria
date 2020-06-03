# nacionalizacao-partidaria

Nosso objetivo é identificar o nível de nacionalização dos partidos políticos e do sistema partidário brasileiro. Coletamos dados eleitorais dos partidos políticos nas eleições 1998 à 2014 para a Câmara dos Deputados em cada um dos estados.

Aplicamos o coeficiente de Gini para mensurar a desigualde de votos entre as unidades federativas como proposto por [Jones e Mainwaring (2003)](https://journals.sagepub.com/doi/10.1177/13540688030092002). Nesse sentido, a lógica é avaliar o grau em que o apoio eleitoral dos partidos políticos é homogêneo entre as unidades federativas estaduais.

# Softwares

Para este projeto utilizamos o [R](https://cran.r-project.org/) e uma coleção de seus pacotes, entre eles o [electionsBR](https://cran.fiocruz.br/web/packages/electionsBR/vignettes/introduction.html), [ineq](https://cran.r-project.org/web/packages/ineq/ineq.pdf), [dplyr](https://cran.r-project.org/web/packages/dplyr/vignettes/dplyr.html), [ggplot2](https://ggplot2.tidyverse.org/). 

# Relatório

Você pode encontrar os gráficos e as tabelas desse projeto no arquivo `relatorio.pdf`.

# Dados

Os dados brutos utilizados neste projeto são oriundos os TSE, acessados pelo pacote `electionsBR`. Devido a seu tamanho não é possível alocá-los aqui no GH. Contudo, os dados manipulados encontram-se na pasta  [dados](./dados/).

# Publicações

Esse projeto contribuiu com as seguintes publicações acadêmicas:

- [A nacionalização dos partidos e do sistema partidário brasileiro](http://e-legis.camara.leg.br/cefor/index.php/e-legis/article/view/517)