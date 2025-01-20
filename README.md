# PASSO A PASSO Microdados-PNADC

Análise e manipulação dos microdados da Pesquisa Nacional por Amostra de Domicílios Contínua(PNADC). Passo a passo de como fazer o download dos microdados, por trimestre, da PNAD contínua de 2012 a 2024.3

### Dicionário das variáveis usadas:

UF \>\> estados\
V2005 \>\> condição no domicílio\
VD4002 \>\> condição de ocupação (1 = ocupado 2 = desocupado)\
V2010 \>\> raça\
V20082 \>\> ano de nascimento\
V2009 \>\> idade(0 a 130)\
V2007 \>\> sexo (1 = homem 2 = mulher)\
VD4016 \>\> rendimento mensal habitual(em reais)\
VD3005 \>\> anos de estudo\
V4040 \>\> tempo de emprego(categorias: 1 mês, menos de 1 ano, menos de 2 anos e 2 anos ou mais)\
V40403 \>\> tempo de emprego(2 a 98 = 2 anos ou mais)\

### Baixando base de dados de 2012 a 2024.3

##### Cria uma lista para armazenar os dados de todos os trimestres de 2012 a 2024.3 e realiza um loop FOR para não precisar ficar repetindo o código para todos os trimestres.

``` R
#criando lista vazia
PNADC_trimestres <- list()

#loop FOR
for(ano in 2012:2013) {
  trimestres <- ifelse(ano==2024, 1:3, 1:4)
  for (trimestre in trimestres) {
    # Obtém os dados e armazena cada trimestre na lista
    PNADC_trimestres[[paste0(ano, "Trimestre", trimestre)]] <- get_pnadc(
      year = ano,
      quarter = trimestre,
      design = FALSE,
      labels = FALSE,
      deflator = TRUE,
      vars = varselec
    )
  }
}
```


#### Limpando environment, instalando e carregando pacotes

```R
#Limpando o environment
rm(list = ls())

#instalando e carregando pacotes
install.packages("PNADcIBGE")
install.packages("dplyr")
library(PNADcIBGE)
library(dplyr)
```

#### Selecionando variáveis baseado no dicionário da PNAD

``` R
#selecionado as variáveis
varselec <-  c("UF","V2005","VD4002","V2010","V2009", "V2007","VD4016", "VD4019", "VD3005","V4040","V40401","V40402", "V40403", "V20082")
```

#### Função get_pnadc para baixar os microdados

``` R

```

#### Filtragem dos dados e criação de dummies

``` R

```

#### criando ID

``` R

```

#### resolvendo o problema de dados de renda faltante para indivíduos menores de 14 anos

``` R

```
#### 
