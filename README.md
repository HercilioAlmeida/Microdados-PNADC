# **PASSO A PASSO Microdados-PNADC**

Análise e manipulação dos microdados da Pesquisa Nacional por Amostra de Domicílios Contínua(PNADC). Passo a passo de como fazer o download dos microdados, por trimestre, da PNAD contínua de 2012 a 2024.3

<br>

#### **Dicionário das variáveis usadas:**

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
V40403 \>\> tempo de emprego(2 a 98 = 2 anos ou mais)


<br>

#### **Limpando environment, instalando e carregando pacotes**

```R
#Limpando o environment
rm(list = ls())

instalando e carregando pacotes
install.packages("PNADcIBGE")
install.packages("dplyr")
install.packages("survey")
library(PNADcIBGE)
library(dplyr)
library(survey)
```
<br>

#### **Selecionando variáveis baseado no dicionário da PNAD**

``` R
#selecionado as variáveis
varselec <-  c("UF","V2005","VD4002","V2010","V2009", "V2007","VD4016", "VD4019", "VD3005","V4040","V40401","V40402", "V40403", "V20082")
```
<br>

#### **Baixando base de dados de 2012 a 2024.3**
O código abaixo cria uma lista para armazenar os dados de todos os trimestres de 2012 a 2024.3 baixados com a função `get_pnadc`
e realiza um loop `for` para não precisar ficar repetindo o código para todos os trimestres.

``` R
#criando lista vazia
PNADC_trimestres <- list()

#loop for
for(ano in 2012:2013) {
  trimestres <- if(ano==2024) 1:3 else 1:4
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
<br>

#### **Salvando dados por ano**
```R
# Usando um loop para combinar os data frames e salvar 4 trimestres formando um ano
for(ano in 2012:2013) {
  trimestres <- if(ano==2024) 1:3 else 1:4
  dados_combinados <- do.call(rbind, PNADC_trimestres[paste0(ano, "Trimestre", trimestres)])

  # Nome dinâmico para o objeto
  dados_anuais <- paste0("PNADC", ano)

  assign(dados_anuais, dados_combinados)
}

```

<br>


#### **Selecionando apenas as colunas com as variáveis escolhidas com um loop `for` para cada ano**
Os blocos abaixo de filtragem e criação de dummies estão interligados por pipes( |> )
``` R
dados_PNAD <- get(paste0("PNADC", ano))
  dados_PNAD <- dados_PNAD |> 
    select(
      Ano, Trimestre, UF, UPA, V1008, V1014, V2005, VD4002, V2010, V20082, 
      V2007, V2009, VD4016, VD4019, VD3005, V4040, V40401, V40402, V40403, Habitual
    ) |> 
```
<br>

#### **Criação da média de anos escolares, dos sexos e da raça, além disso, criação das dummies de sexo e raça**

``` R
# Criando dummies
    mutate(
      MédiaAnosEscolar = mean(as.numeric(VD3005), na.rm = TRUE)
    ) |> 
    mutate(
      Homem = ifelse(V2007 == 1, 1, 0),
      Mulher = ifelse(V2007 == 1, 0, 1),
      MédiaHomem = ifelse(V2007 == 1, mean(Homem)*100, 0),
      MédiaMulher = ifelse(V2007 == 1, 0, mean(Mulher)*100)
    ) |> 
    mutate(
      Branco = ifelse(V2010 == 1, 1, 0),
      ñBranco = ifelse(V2010 == 1, 0, 1),
      MédiaBranco = ifelse(V2010 == 1, mean(Branco)*100, 0),
      Média_ñBranco = ifelse(V2010 == 1, 0, mean(ñBranco)*100)
    ) |> 
    mutate(
      Domicílio = paste0(UPA, V1008, V1014)
    ) |>
```
<br>

#### **Atualização monetária com a variável "Habitual" que vem na base de dados quando `get_pnadc(deflator = TRUE)`**

``` R
   mutate(
      VD4019real = VD4019*Habitual
    ) |> 
```
<br>

#### **Criando dummies e média com base na variável "condição de domicílio"**

``` R
    group_by(Domicílio) |>
    mutate(
      ChefeFamilia = ifelse(V2005 == "01", 1, 0),
      Conjuge = ifelse(V2005 == "02", 1, 0),
      Casados = ifelse(ChefeFamilia == 1 | (Conjuge == 1 & any(ChefeFamilia == 1)), 1, 0),
      ñcasados = ifelse(Casados == 0 & V2009 > 18, 1, 0),
      Filhos_menores = ifelse(V2005 >= "04" & V2005 <= "06" & V2009 < 18, 1, 0),
      CasadosComFilhoMenor = ifelse(Casados == 1 & any(Filhos_menores == 1), 1, 0),
      CasadosSemFilhoMenor = ifelse(Casados == 1 & !any(Filhos_menores == 1), 1, 0)
    ) |>
    ungroup()
```
<br>

#### **Criação das ID e renda média, renda média e tempo médio de emprego por ID**

``` R
# Criando ID com ano de nascimento e sexo que são as colunas 10 e 11, respectivamente
  dados_PNAD$ID <- apply(dados_PNAD[, 10:11], 1, paste, collapse = "")
  
  # Média por ID
  dados_PNAD <- dados_PNAD |> 
    group_by(ID) |> 
    mutate(
      RendaMédia = mean(VD4019, na.rm = TRUE),
      RendaMédiaReal = mean(VD4019real, na.rm = TRUE),
      TempoMédioEmprego_Acima2anos = mean(V40403, na.rm = TRUE)
    ) |> 
    ungroup()
```

<br>

#### **Resolvendo o problema da falta de dados de remuneração e ocupação de pessoas menores de 14 anos**
Foi preciso primeiramente separar as pessoas menores de 14 do banco de dados porque ao fazer 
a filtragem por condição de ocupação e remuneração esses indivíduos seriam excluídos, visto 
que essas variáveis só captam indivíduos com 14 anos ou mais. Então, após esta separação foi feito o filtro
somente para pessoas ocupadas, com 60 anos ou menos e que recebem até R$100 mil de remuneração. 
E por fim, a junção novamente com os dados de indivíduos menor de 14 anos.
``` R
# Filtrando indivíduos sem dados de ocupação e renda (idade < 14)
  SEMocupacao_SEMrenda <- dados_PNAD |> 
    filter(V2009 < 14)
  
  # Garantindo que as colunas específicas sejam preenchidas com NA em SEMocupacao_SEMrenda
  SEMocupacao_SEMrenda <- SEMocupacao_SEMrenda |> 
    mutate(
      VD4002 = NA,
      VD4016 = NA,
      V4040 = NA,
      V40401 = NA,
      V40402 = NA,
      V40403 = NA,
      RendaMédia = NA,
      TempoMédioEmprego_Acima2anos = NA
    )
  
  # Filtrando dados pela condição de ocupação, nascimento e renda
  dados_PNAD <- dados_PNAD |> 
    filter(
      VD4002 == 1 &
        V20082 >= 1964 &
        VD4019 <= 100000
    )
  
  # Juntando os dados de indivíduos < 14 com a base de dados do ano
  dados_PNAD <- bind_rows(dados, SEMocupacao_SEMrenda)
```
<br>

## Estatística Descritiva
