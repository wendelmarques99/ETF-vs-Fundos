library(tidyverse)
library(data.table)
library(rio)
library(readxl)
library(xlsx)
library(quantmod)
library(tidyquant)
library(ggthemes)
library(timetk)
library(scales)
library(PerformanceAnalytics)
library(ggcorrplot)

CVM_fundos = function(data_inicio, data_final, CNPJ){
  
  data_inicio = as.Date(data_inicio)
  data_final = as.Date(data_final)
  ano_1 = format(seq(data_inicio, data_final, by = 'month'),'%Y%m')
  
  if (as.integer(format(data_inicio, '%Y')) >2016 & as.integer(format(data_final,'%Y')) >2016){
    
    
    urls = map_chr(ano_1, ~paste0('http://dados.cvm.gov.br/dados/FI/DOC/INF_DIARIO/DADOS/inf_diario_fi_', .x,'.csv'))
    
    dados_fundos= urls %>%
      map_dfr(~fread(.x)) %>%
      filter(DT_COMPTC >= data_inicio, DT_COMPTC <= data_final, CNPJ_FUNDO %in% c(CNPJ))
    
    return(dados_fundos)
    
  }
  else if (as.integer(format(data_final, '%Y')) <= 2016 & as.integer(format(data_inicio,'%Y')) <= 2016) {
    
    anos_2 = as.integer(format(data_inicio,'%Y')):as.integer(format(data_final,'%Y'))
    
    fundos_2 = map_chr(anos_2,
                       ~paste0('http://dados.cvm.gov.br/dados/FI/DOC/INF_DIARIO/DADOS/HIST/inf_diario_fi_', .x,'.zip'))
    filename = paste0('Arquivo', 1:length(fundos_2), '.zip')
    map2(.x = fundos_2, .y = filename, download.file)
    
    files = map(filename, .f = unzip, exdir = getwd()) %>%
      unlist()
    dados_fundos2 = files %>%
      map_dfr(~fread(.)) %>%
      filter(DT_COMPTC >= data_inicio, DT_COMPTC <= data_final,  CNPJ_FUNDO %in% c(CNPJ))
    file.remove(files)
    file.remove(filename)
    
    return(dados_fundos2)
    
  }
  
  else if (as.integer(format(data_final, '%Y')) > 2016 &
           as.integer(format(data_inicio,'%Y')) >= 2005 &
           as.integer(format(data_inicio,'%Y')) <= 2016) {
    
    anos_3 = as.integer(format(data_inicio,'%Y')):2016
    
    fundos_3 = map_chr(anos_3,
                       ~paste0('http://dados.cvm.gov.br/dados/FI/DOC/INF_DIARIO/DADOS/HIST/inf_diario_fi_', .x,'.zip'))
    
    filename = paste0('Arquivo', 1:length(fundos_3), '.zip')
    
    map2(.x = fundos_3, .y = filename, download.file)
    
    files = map(filename, .f = unzip, exdir = getwd()) %>%
      unlist()
    
    dados_fundos3 = files %>%
      map_dfr(~fread(.))%>%
      filter(DT_COMPTC >= data_inicio, DT_COMPTC <= data_final, CNPJ_FUNDO %in% c(CNPJ))
    
    file.remove(files)
    file.remove(filename)
    
    anos_4 = format(seq(as.Date('2017-01-01'), data_final, by = 'month'),'%Y%m')
    
    dados_fundos4 = map_chr(anos_4, ~paste0('http://dados.cvm.gov.br/dados/FI/DOC/INF_DIARIO/DADOS/inf_diario_fi_', .x,'.csv')) %>%
      map_dfr(~fread(.x)) %>%
      filter(DT_COMPTC >= as.Date('2017-01-01'), DT_COMPTC <= data_final, CNPJ_FUNDO %in% c(CNPJ))
    
    
    fundos_final = bind_rows(dados_fundos3, dados_fundos4)
    return(fundos_final)
    
  }
}




# Start
fundos = CVM_fundos(data_inicio = as.Date('2018/07/26'), data_final = as.Date('2021/07/26'),
                    CNPJ = c('28.386.218/0001-45', '30.317.454/0001-51', '10.500.884/0001-05', "26.210.505/0001-74"))

# Mudando estrutura da data
fundos$DT_COMPTC = as.Date(fundos$DT_COMPTC)


# Selecionando variaveis
fundos = fundos %>%
  select(CNPJ_FUNDO, DT_COMPTC, VL_QUOTA)

## salvando
saveRDS(fundos, 'fundos.Rds')


