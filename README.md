# Author: Matheus Ferreira da Silva Costa 
# Work: API TESOURO NACIONAL 
# email: matheus.costa@fazenda.rj.gov.br
# Last Update : 13/11/24



#                             API TESOURO NACIONAL 



##################################################### 
# Carregar pacotes
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  dplyr, ggplot2, tidyverse, sidrar, stringr, patchwork, 
  readxl, data.table, openxlsx, curl, stargazer, 
  xtable, gridExtra, zoo, scales, janitor, lubridate, 
  RColorBrewer, writexl, reshape2, geobr, fuzzyjoin,
  rjson, jsonlite, httr, purrr, DT, xlsx, plotly, rJava, DT
)
##################################################### 
# tirar formatação científica
##################################################### 
options(scipen = 999)





##################################
json2df = function(a){ 
  
  f_api <-   GET(a)
  f_txt <- content(f_api, as="text", encoding="UTF-8")
  f_json <- fromJSON(f_txt, flatten = FALSE)
  Sys.sleep(1)
  f_df <-as.data.frame(f_json[[1]]) 
}
##################################



##################################
# função para juntar data frames
bind_json2df = function(a){ map(a,json2df)}
# funcao para gerar df com os dados dos relatórios
relatorios = function(a){map_dfr(bind_json2df(a), bind_rows)}
##################################


# SABENDO QUAIS ENTES ESTÃO DISPONIVEIS 


##################################
# acessar url com dados dos entes
df_entes <- json2df("http://apidatalake.tesouro.gov.br/ords/siconfi/tt/entes")
datatable(df_entes)
##################################




# SABENDO QUAIS ANEXOS E RELATÓRIOS ESTÃO DISPONIVEIS 


################################################################################
# Definir a URL base da API
base_url <- "https://apidatalake.tesouro.gov.br/ords/siconfi/tt/anexos-relatorios"

# Realizar a requisição para obter os anexos e relatórios
response <- GET(base_url)

# Verificar o código de status da resposta
if (status_code(response) == 200) {
  # Se a resposta for bem-sucedida, converter o conteúdo em JSON
  content_data <- content(response, as = "text", encoding = "UTF-8")
  json_data <- fromJSON(content_data)
  
  # Exibir o conteúdo dos dados (ou fazer algo mais com eles)
  print(json_data)
} else {
  # Se a resposta não for bem-sucedida, exibir uma mensagem de erro
  cat("Erro ao acessar a API:", status_code(response), "\n")
}

# Transformar o conteúdo 'items' da resposta em um data frame
items_data <- json_data$items

# Converter em um data frame
df_anexos <- as.data.frame(items_data)
################################################################################


#                       RREO: Relatório Resumido da Execução Orçamentária



# Base URL para a API RREO
base_url_rreo <- "https://apidatalake.tesouro.gov.br/ords/siconfi/tt/rreo?"

# Parâmetros fixos
ano <- "2024"
tipo_relatorio <- "RREO"
ente <- "3304557"

# Loop para os bimestres (1 a 6)
for (bimestre in 1:6) {
  
  # Loop para os anexos (de RREO-Anexo+01 até RREO-Anexo+07)
  for (anexo_num in 1:7) {
    
    # Criar o nome do anexo
    num_anexo <- paste0("RREO-Anexo+", sprintf("%02d", anexo_num))  # Exemplo: "RREO-Anexo+01"
    
    # Montar a URL da API com os parâmetros
    chamada_api_rreo <- paste0(base_url_rreo,
                               "an_exercicio=", ano, "&",
                               "nr_periodo=", bimestre, "&",
                               "co_tipo_demonstrativo=", tipo_relatorio, "&",
                               "no_anexo=", num_anexo, "&",
                               "id_ente=", ente)
    
    # Fazer a requisição
    rreo <- GET(chamada_api_rreo)
    
    # Verificar o status da requisição
    if (status_code(rreo) == 200) {
      # Obter o conteúdo da resposta
      rreo_txt <- content(rreo, as = "text", encoding = "UTF-8")
      
      # Converter o conteúdo para JSON
      rreo_json <- fromJSON(rreo_txt, flatten = FALSE)
      
      # Verificar se há dados no JSON
      if (length(rreo_json[["items"]]) > 0) {
        # Converter os itens para data frame
        rreo_df <- as.data.frame(rreo_json[["items"]])
        
        # Criar o nome do data frame
        df_name <- paste0("rreo_bimestre_", bimestre, "_anexo_", sprintf("%02d", anexo_num))
        
        # Salvar o data frame no ambiente global
        assign(df_name, rreo_df, envir = .GlobalEnv)
        
        # Exibir a primeira linha do data frame
        print(paste("Data frame:", df_name))
        print(knitr::kable(head(rreo_df)))
      } else {
        print(paste("Nenhum dado encontrado para bimestre", bimestre, "e anexo", num_anexo))
      }
    } else {
      print(paste("Erro na requisição para bimestre", bimestre, "e anexo", num_anexo))
    }
  }
}


################################################################################


#                       RGF: Relatório de Gestão Fiscal



# Definir a URL base
base_url_rgf <- "https://apidatalake.tesouro.gov.br/ords/siconfi/tt/rgf"

# Definir os parâmetros da consulta
an_exercicio <- 2024          # Exercício do relatório
in_periodicidade <- "Q"       # Periodicidade (Q para quadrimestral, S para semestral)
co_tipo_demonstrativo <- "RGF" # Tipo do Demonstrativo
co_esfera <- "M"              # Filtro de esfera (M = Municípios)
co_poder <- "E"               # Código do poder (E = Executivo)
id_ente <- 3304557            # Código IBGE do ente

# Loop para quadrimestre (1, 2, 3)
for (nr_periodo in 1:3) {
  # Loop para anexos (RGF-Anexo 01 até RGF-Anexo 05)
  for (i in 1:5) {
    # Nome do anexo
    no_anexo <- paste("RGF-Anexo", sprintf("%02d", i))  # Cria "RGF-Anexo 01", "RGF-Anexo 02", etc.
    
    # Montar a URL com os parâmetros e codificar
    url <- paste0(base_url_rgf, "?an_exercicio=", an_exercicio, 
                  "&in_periodicidade=", in_periodicidade, 
                  "&nr_periodo=", nr_periodo, 
                  "&co_tipo_demonstrativo=", co_tipo_demonstrativo, 
                  "&no_anexo=", URLencode(no_anexo),  # Codificando o parâmetro no_anexo
                  "&co_esfera=", co_esfera, 
                  "&co_poder=", co_poder, 
                  "&id_ente=", id_ente)
    
    # Verificando a URL
    print(url)
    
    # Fazer a requisição à API
    response <- GET(url)
    
    # Verificar o status da requisição
    if (status_code(response) == 200) {
      # Obter o conteúdo da resposta (JSON)
      content_txt <- content(response, as = "text", encoding = "UTF-8")
      
      # Converter o conteúdo JSON para uma lista R
      data_json <- fromJSON(content_txt, flatten = TRUE)
      
      # Verificar se a resposta contém dados e extrair
      if (!is.null(data_json$items)) {
        # Converter a parte relevante dos dados para um data frame
        data_df <- as.data.frame(data_json$items)
        
        # Nome do data frame dinâmico
        df_name <- paste0("rgf_quadrimestre_", nr_periodo, "_anexo_", i)
        
        # Criar o data frame no ambiente
        assign(df_name, data_df, envir = .GlobalEnv)
        
        # Exibir o nome do data frame criado e os primeiros dados
        print(paste("Data frame criado:", df_name))
        print(head(data_df))
      } else {
        print(paste("Nenhum dado encontrado para o quadrimestre", nr_periodo, "e anexo", i))
      }
    } else {
      print(paste("Erro na requisição para quadrimestre", nr_periodo, "e anexo", i, 
                  ". Código de status:", status_code(response)))
    }
  }
}


################################################################################


#                       Contas Anuais 

# VERIFICANDO OS TIPOS DE ANEXOS DA DCA EM AMBITO MUNICIPAL


# Definir a URL base da API
base_url_dca <- "https://apidatalake.tesouro.gov.br/ords/siconfi/tt/dca"

# Definir parâmetros fixos
an_exercicio <- 2024                          # Exercício do relatório
id_ente <- 3304557                            # Código IBGE da cidade do Rio de Janeiro
co_esfera <- "M"                              # Esfera de governo

# Lista de anexos que você quer consultar
anexos <- c("DCA-Anexo I-AB", "DCA-Anexo I-C", "DCA-Anexo I-D", "DCA-Anexo I-E", 
            "DCA-Anexo I-F", "DCA-Anexo I-G", "DCA-Anexo I-HI")

# Criar uma lista para armazenar os data frames
dados_list <- list()

# Loop para percorrer todos os anexos e consultar a API
for (anexo in anexos) {
  # Montar a URL com os parâmetros
  url <- paste0(base_url_dca, "?an_exercicio=", an_exercicio, 
                "&no_anexo=", URLencode(anexo),  # Codificando o parâmetro no_anexo
                "&id_ente=", id_ente,
                "&co_esfera=", co_esfera)  # Adicionando o parâmetro co_esfera
  
  # Fazer a requisição à API
  response <- GET(url)
  
  # Verificar se a requisição foi bem-sucedida
  if (status_code(response) == 200) {
    # Obter o conteúdo da resposta (JSON)
    content_txt <- content(response, as = "text", encoding = "UTF-8")
    
    # Converter o conteúdo JSON para uma lista R
    data_json <- fromJSON(content_txt, flatten = TRUE)
    
    # Verificar se há dados
    if (length(data_json$items) > 0) {
      # Converter os dados em um data frame
      data_df <- as.data.frame(data_json$items)
      
      # Adicionar o data frame à lista
      dados_list[[anexo]] <- data_df
    } else {
      print(paste("Sem dados para o anexo:", anexo))
    }
  } else {
    print(paste("Erro na requisição para o anexo:", anexo, "Código de status:", status_code(response)))
  }
}

# Exibir os data frames carregados
for (anexo in anexos) {
  if (!is.null(dados_list[[anexo]])) {
    print(paste("Dados para o anexo:", anexo))
    print(head(dados_list[[anexo]]))  # Exibe os primeiros dados do data frame
  }
}


for (anexo in anexos) {
  if (!is.null(dados_list[[anexo]])) {
    # Criar uma variável no ambiente com o nome do anexo (substituindo espaços por "_")
    assign(paste0(gsub(" ", "_", anexo)), dados_list[[anexo]])
    
    # Exibir os primeiros dados
    print(paste("Dados para o anexo:", anexo))
    print(head(dados_list[[anexo]]))  # Exibe os primeiros dados do data frame
  }
}



# Definir os nomes dos data frames
data_frames <- c("data_df", "DCA-Anexo_I-AB", "DCA-Anexo_I-C", "DCA-Anexo_I-D", "DCA-Anexo_I-E", 
                 "DCA-Anexo_I-F", "DCA-Anexo_I-G", "DCA-Anexo_I-HI", "df_anexos", "df_entes", 
                 "items_data", "rgf_quadrimestre_1_anexo_1", "rgf_quadrimestre_1_anexo_2", "rgf_quadrimestre_1_anexo_3",
                 "rgf_quadrimestre_1_anexo_4", "rgf_quadrimestre_1_anexo_5", "rgf_quadrimestre_2_anexo_1",
                 "rgf_quadrimestre_2_anexo_2", "rgf_quadrimestre_2_anexo_3", "rgf_quadrimestre_2_anexo_4",
                 "rgf_quadrimestre_2_anexo_5", "rgf_quadrimestre_3_anexo_1", "rgf_quadrimestre_3_anexo_2",
                 "rgf_quadrimestre_3_anexo_3", "rgf_quadrimestre_3_anexo_4", "rgf_quadrimestre_3_anexo_5",
                 "rreo_bimestre_1_anexo_01", "rreo_bimestre_1_anexo_02", "rreo_bimestre_1_anexo_03",
                 "rreo_bimestre_1_anexo_04", "rreo_bimestre_1_anexo_06", "rreo_bimestre_1_anexo_07",
                 "rreo_bimestre_2_anexo_01", "rreo_bimestre_2_anexo_02", "rreo_bimestre_2_anexo_03",
                 "rreo_bimestre_2_anexo_04", "rreo_bimestre_2_anexo_06", "rreo_bimestre_2_anexo_07",
                 "rreo_bimestre_3_anexo_01", "rreo_bimestre_3_anexo_02", "rreo_bimestre_3_anexo_03",
                 "rreo_bimestre_3_anexo_04", "rreo_bimestre_3_anexo_06", "rreo_bimestre_3_anexo_07",
                 "rreo_bimestre_4_anexo_01", "rreo_bimestre_4_anexo_02", "rreo_bimestre_4_anexo_03",
                 "rreo_bimestre_4_anexo_04", "rreo_bimestre_4_anexo_06", "rreo_bimestre_4_anexo_07",
                 "rreo_bimestre_5_anexo_01", "rreo_bimestre_5_anexo_02", "rreo_bimestre_5_anexo_03",
                 "rreo_bimestre_5_anexo_04", "rreo_bimestre_5_anexo_06", "rreo_bimestre_5_anexo_07",
                 "rreo_bimestre_6_anexo_01", "rreo_bimestre_6_anexo_02", "rreo_bimestre_6_anexo_03",
                 "rreo_bimestre_6_anexo_04", "rreo_bimestre_6_anexo_06", "rreo_bimestre_6_anexo_07", 
                 "rreo_df")

# Criar listas vazias para cada grupo de data frames
dca_list <- list()
rreo_list <- list()
rgf_list <- list()

# Filtrar os data frames para cada grupo
for (df_name in data_frames) {
  # Verifica se o data frame está no ambiente
  if (exists(df_name)) {
    df <- get(df_name)
    
    # Adiciona aos grupos correspondentes
    if (grepl("^DCA-Anexo", df_name)) {
      dca_list[[df_name]] <- df
    } else if (grepl("^rreo_bimestre", df_name)) {
      rreo_list[[df_name]] <- df
    } else if (grepl("^rgf_quadrimestre", df_name)) {
      rgf_list[[df_name]] <- df
    }
  }
}



# SALVE AQUI A BASE DE DADOS(LEMBRE DE ALTERAR O CAMINHO)


# Salvar os data frames de cada grupo em arquivos .xlsx
write_xlsx(dca_list, path = "/Data/Data_Rio_de_janeiro_2022/DCA.xlsx")
write_xlsx(rreo_list, path = "/Data/Data_Rio_de_janeiro_2022/RREO.xlsx")
write_xlsx(rgf_list, path = "Data/Data_Rio_de_janeiro_2022/RGF.xlsx")





