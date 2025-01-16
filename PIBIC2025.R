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
library(PNADcIBGE)
library(dplyr)

#setwd("C:/Users/herci/OneDrive/Documentos/Projetos R-Studio/PIBIC/PIBIC 2025")
# salvando e abrindo csv ---- !!!!! SÓ RODAR LINHAS DO BLOCO ABAIXO CASO QUEIRA SALVAR OU ABRIR A BASE DE DADOS !!!!!!!
{
# Salvar a base de dados em um arquivo CSV 
  #write.csv(dados2023, file = "dados2023.csv", row.names = FALSE)
# abrir base de dados offiline depois de ter salvo em csv
  #dados2023 <- read.csv("/cloud/project/dados2023.csv")
}

#selecionado as variáveis
varselec <-  c("UF","V2005","VD4002","V2010","V2009", "V2007","VD4016", 
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
# Lista para armazenar os resultados
pnadc_dados <- list()

# Loop para obter os dados de 2012 a 2024
for (ano in 2012:2024) {
  # Inicializa um data frame vazio para juntar todos os trimestres
  dados_ano_completo <- NULL
  
  # Condicional para limitar os trimestres de 2024 a 3
  if (ano == 2024) {
    trimestres <- 1:3  # Apenas os três primeiros trimestres de 2024
  } else {
    trimestres <- 1:4  # Para os outros anos, inclui todos os 4 trimestres
  }
  
  for (trimestre in trimestres) {  # Loop para os trimestres de 1 a 4 (ou até 3 em 2024)
    # Obtém os dados e armazena na lista
    dados_trimestre <- get_pnadc(
      year = ano,         # Determina o ano
      quarter = trimestre, # Determina o trimestre (1 a 4)
      design = FALSE,
      labels = FALSE,
      vars = varselec     # Variáveis selecionadas
    )
    
    # Armazenar os dados individuais do trimestre
    pnadc_dados[[paste0(as.character(ano), "_Trimestre", trimestre)]] <- dados_trimestre
    
    # Juntar os dados de todos os trimestres para o ano
    dados_ano_completo <- bind_rows(dados_ano_completo, dados_trimestre)
  }
  
  # Armazenar o ano completo na lista
  pnadc_dados[[paste0(as.character(ano), "_AnoCompleto")]] <- dados_ano_completo
  
  # Salvar o ano completo como um objeto no ambiente global
  assign(paste0("PNAD", ano), dados_ano_completo)
}



# FILTRANDO E MANIPULANDO DADOS ----
{
# Loop para processar os dados de 2012 a 2024
for (ano in 2012:2024) {
  
  # Acessa os dados do ano específico (por exemplo, PNAD2012, PNAD2013, etc.)
  dados <- get(paste0("PNAD", ano))  # Usando get() para acessar a variável correspondente
  
  # FILTRANDO E MANIPULANDO DADOS ----
  dados <- dados |> 
    select(
      Ano, Trimestre, UF, UPA, V1008, V1014, V2005, VD4002, V2010, V20082, 
      V2007, V2009, VD4016, VD3005, V4040, V40401, V40402, V40403
    ) |> 
    # Criando dummies ----
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

  
  # Criando ID com ano de nascimento e sexo
  dados$ID <- apply(dados[, 10:11], 1, paste, collapse = "")
  
  # Média por ID
  dados <- dados |> 
    group_by(ID) |> 
    mutate(
      RendaMédia = mean(VD4016, na.rm = TRUE),
      TempoMédioEmprego_Acima2anos = mean(V40403, na.rm = TRUE)
    ) |> 
    ungroup()
  
  # Filtrando indivíduos sem dados de ocupação e renda (idade < 14)
  SEMocupacao_SEMrenda <- dados |> 
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
  dados <- dados |> 
    filter(
      VD4002 == 1 &
        V20082 >= 1964 &
        VD4020 <= 100000
    )
  
  # Juntando os dados de indivíduos < 14 com a base de dados do ano
  dados <- bind_rows(dados, SEMocupacao_SEMrenda)
  
  
  # Atribuindo os dados manipulados de volta à variável do ano específico
  assign(paste0("dados", ano), dados)
  
}

}



