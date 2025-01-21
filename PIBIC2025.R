#UNIVERSIDADE FEDERAL DA PARAÍBA
#CENTRO DE CIÊNCIAS SOCIAIS APLICADAS
#DEPARTAMENTO DE ECONOMIA
#PIBIC - A evolução do rendimento salarial no mercado de trabalho
#PROF. DR. PAULO AGUIAR DO MONTE
#ALUNO -> HERCÍLIO ALMEIDA BARBOSA [20210161076]

#Limpando o environment
rm(list = ls())

#instalando e carregando pacotes
#install.packages("PNADcIBGE")
#install.packages("dplyr")
#install.packages("survey")
library(PNADcIBGE)
library(dplyr)
library(survey)
#setwd("C:/Users/herci/OneDrive/Documentos/Projetos R-Studio/PIBIC/PIBIC 2025")
# salvando e abrindo csv ---- !!!!! SÓ RODAR LINHAS DO BLOCO ABAIXO CASO QUEIRA SALVAR OU ABRIR A BASE DE DADOS !!!!!!!
{
# Salvar a base de dados em um arquivo CSV 
  #write.csv(dados2023, file = "dados2023.csv", row.names = FALSE)
# abrir base de dados offiline depois de ter salvo em csv
  #dados2023 <- read.csv("/cloud/project/dados2023.csv")
}

#selecionado as variáveis
varselec <-  c("UF","V2005","VD4002","V2010","V2009", "V2007","VD4016", "VD4019", 
               "VD3005","V4040","V40401","V40402", "V40403", "V20082")

# dicionário pnad
{
# UF >> estados
# V2005 >> condição no domicílio
# VD4002 >> condição de ocupação (1 = ocupado 2 = desocupado)
# V2010 >> raça
# V2009 >> idade(0 a 130)
# V2007 >> sexo (1 = homem 2 = mulher)
# VD4016 >> rendimento mensal habitual(em reais)
# VD3005 >> anos de estudo
# V4040 >> tempo de emprego(categorias: 1 mês, menos de 1 ano, menos de 2 anos e 2 anos ou mais)
# V40403 >> tempo de emprego(2 a 98 = 2 anos ou mais)
# V20082 >> ano de nascimento
}

# PUXAR OS MICRODADOS DOS TRIMESTRES

# !!!!!!!!!!!!!!!!!!!!! ATENÇÃO !!!!!!!!!!!!!!!!!!!!!!!!!!!

# Observação: Quando rodar a próxima linha de código, irá demorar
# para aparecer o objeto "PNADC2024.1" no environment, mas é só 
# esperar aparecer(se não houver erro), irá aparecer outra 
# janela(donwload progress) para acompanhar o donwload da base.
# Aguarde até aparecer o objeto no environment



# baixando bases de dados de 2012 a 2024.3 ----------
# Cria uma lista para armazenar os dados dos trimestres de 2012
PNADC_trimestres <- list()

for(ano in 2012:2012) {
  trimestres <- if(ano==2024) 1:3 else 1:4
  for (trimestre in trimestres) {
    # Obtém os dados e armazena cada trimestre na lista
    PNADC_trimestres[[paste0(ano, "Trimestre", trimestre)]] <- get_pnadc(
      year = ano,
      quarter = trimestre,
      labels = FALSE,
      deflator = TRUE,
      vars = varselec
    )
  }
}


# Usando um loop para combinar os data frames e salvar dinamicamente
for(ano in 2012:2012) {
  trimestres <- if(ano==2024) 1:3 else 1:4
  dados_combinados <- do.call(rbind, PNADC_trimestres[paste0(ano, "Trimestre", trimestres)])

  # Nome dinâmico para o objeto
  dados_anuais <- paste0("PNADC", ano)

  # Salvando o resultado com assign
  assign(dados_anuais, dados_combinados)
}


# FILTRANDO E MANIPULANDO DADOS ----

for (ano in 2012:2012) {
  
  dados_PNAD <- get(paste0("PNADC", ano))
  
  dados_PNAD <- dados_PNAD |> 
    select(
      Ano, Trimestre, UF, UPA, V1008, V1014, V2005, VD4002, V2010, V20082, 
      V2007, V2009, VD4016, VD4019, VD3005, V4040, V40401, V40402, V40403, Habitual
    ) |> 
    # Criando dummies ----
  mutate(
    MediaAnosEscolar = mean(as.numeric(VD3005), na.rm = TRUE)
  ) |> 
    mutate(
      Homem = ifelse(V2007 == 1, 1, 0),
      Mulher = ifelse(V2007 == 2, 1, 0),
      Branco = ifelse(V2010 == 1, 1, 0),
      NaoBranco = ifelse(V2010 != 1, 1, 0)
    ) |> 
    mutate(
      Domicilio = paste0(UPA, V1008, V1014),
      VD4019real = VD4019 * Habitual
    ) |> 
    group_by(Domicilio) |> 
    mutate(
      ChefeFamilia = ifelse(V2005 == "01", 1, 0),
      Conjuge = ifelse(V2005 == "02", 1, 0),
      FilhosMenores = ifelse(V2005 %in% c("04", "05", "06") & V2009 < 18, 1, 0),
      Casados = ifelse(ChefeFamilia == 1 | (Conjuge == 1 & any(ChefeFamilia == 1)), 1, 0),
      CasadosComFilhoMenor = ifelse(Casados == 1 & any(FilhosMenores == 1), 1, 0)
    ) |> 
    ungroup()
  
  # Criando ID com ano de nascimento e sexo
  dados_PNAD$ID <- apply(dados_PNAD[, c("V20082", "V2007")], 1, paste, collapse = "")
  
  # Média por ID
  dados_PNAD <- dados_PNAD |> 
    group_by(ID) |> 
    mutate(
      RendaMedia = mean(VD4019, na.rm = TRUE),
      RendaMediaReal = mean(VD4019real, na.rm = TRUE),
      TempoMedioEmprego = mean(V40403, na.rm = TRUE)
    ) |> 
    ungroup()
  
  # Filtrando indivíduos sem dados de ocupação e renda (idade < 14)
  SEMocupacao_SEMrenda <- dados_PNAD |> 
    filter(V2009 < 14) |> 
    mutate(
      VD4002 = NA,
      VD4016 = NA,
      V4040 = NA,
      V40401 = NA,
      V40402 = NA,
      V40403 = NA,
      RendaMedia = NA,
      TempoMedioEmprego = NA
    )
  
  # Filtrando dados pela condição de ocupação, nascimento e renda
  dados_PNAD <- dados_PNAD |> 
    filter(
      VD4002 == 1 &
        V20082 >= 1964 &
        VD4019 <= 100000
    )
  
  # Juntando os dados de indivíduos < 14 com a base de dados do ano
  dados_PNAD <- bind_rows(dados_PNAD, SEMocupacao_SEMrenda)
  
  # Salvando os dados filtrados
  assign(paste0("dadosfiltrados", ano), dados_PNAD)
}

#estatistica descritiva

#numero de observações
nrow(dadosfiltrados2012)

#quantidade de homens e mulheres

summarise()

#médias

#graficos e tabelas

