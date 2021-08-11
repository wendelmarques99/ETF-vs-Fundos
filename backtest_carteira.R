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


## lendo arquivo RDS
fundos = readRDS('Fundos.Rds')

# Symbols

symbols = c('SMAL11.SA', 'BOVA11.SA')


# prices 'SMAL11.SA', 'BOVA11.SA', "MCHI", "EZU"
prices = getSymbols(symbols, src ='yahoo',
                    from = '2018-07-26',
                    to = '2021-07-26',
                    auto.assign = T,
                    warnings = F) %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  as.data.frame() %>%
  mutate(data = rownames(.))


# transformando a estrutura da data
prices$data = as.Date(prices$data)

# lendo arquibo
IVVB11 = read_csv('IVVB11 Dados Historicos.csv')

# formatando para date
IVVB11$Data = as.Date(IVVB11$Data, '%d.%m.%Y')

# Slecionando colunas em IVVB11
IVVB11 = IVVB11 %>%
  select(Último, Data) %>%
  rename('data' = 'Data')


# Adicionando IVVB1 a matrix de precos
prices_matrix = left_join(IVVB11, prices , by = 'data') %>%
  na.omit() %>%
  arrange(data)

names(prices_matrix)[1] = 'IVVB11'


## IXIC
getSymbols('^IXIC', src ='yahoo',
           from = '2018-07-26',
           to = '2021-07-26',
           auto.assign = T,
           warnings = F)

prices_matrix = IXIC %>%
  as.data.frame() %>%
  mutate(data = rownames(.)) %>%
  select(c(IXIC.Adjusted, data)) %>%
  mutate(data = as.Date(data)) %>%
  left_join(prices_matrix, by = "data") %>%
  na.omit()


prices_matrix = prices_matrix %>%
  pivot_longer(!data, names_to = 'Ativo', values_to = 'VL_QUOTA')

funds_merge = fundos %>%
   na.omit() %>%
   select(CNPJ_FUNDO, VL_QUOTA, DT_COMPTC)


funds_merge$CNPJ_FUNDO = replace(funds_merge$CNPJ_FUNDO,
        funds_merge$CNPJ_FUNDO == '10.500.884/0001-05',
        'REAL INVESTOR')

funds_merge$CNPJ_FUNDO = replace(funds_merge$CNPJ_FUNDO,
        funds_merge$CNPJ_FUNDO =='30.317.454/0001-51',
        'DAHLIA TOTAL RETURN')

funds_merge$CNPJ_FUNDO = replace(funds_merge$CNPJ_FUNDO,
        funds_merge$CNPJ_FUNDO == '28.386.218/0001-45',
        'MILES ACER')

funds_merge$CNPJ_FUNDO = replace(funds_merge$CNPJ_FUNDO,
                                 funds_merge$CNPJ_FUNDO == '26.210.505/0001-74',
                                 'TRUXT 1 LB')


names(funds_merge) = c('ativo','prices','data')

funds_merge = funds_merge %>%
   group_by(ativo) %>%
   group_split() %>%
   map(., ~left_join(x = prices_matrix, y = ., by = 'data')) %>%
  reduce(bind_rows) %>%
   select(prices, data, ativo)


names(prices_matrix) = c('data','ativo','prices')

#### MUDANDO OS NOMES DOS ATIVOS
prices_matrix$ativo = replace(prices_matrix$ativo,
                              prices_matrix$ativo == 'BOVA11.SA.Adjusted',c('BOVA11'))
prices_matrix$ativo = replace(prices_matrix$ativo,
                              prices_matrix$ativo == 'SMAL11.SA.Adjusted', c('SMAL11'))
# prices_matrix$ativo = replace(prices_matrix$ativo,
#                               prices_matrix$ativo == 'IEUR.Adjusted', c('MSCI Europe'))
# prices_matrix$ativo = replace(prices_matrix$ativo,
#                               prices_matrix$ativo == 'MCHI.Adjusted', c('MSCI China'))
prices_matrix$ativo = replace(prices_matrix$ativo,
                              prices_matrix$ativo == 'IXIC.Adjusted', c('iShares NASDAQ'))



# Juntando os dfs
matrix_return = bind_rows(prices_matrix, funds_merge)

# Matrix dos retornos
matrix_return = matrix_return %>%
  group_by(ativo) %>%
  mutate(returns = log(prices) - log(lag(prices))) %>%
  na.omit() %>%
  mutate(acumulado = exp(cumsum(returns))-1)

wts_map <- tibble(
  symbols = c("iShares NASDAQ", "IVVB11", "SMAL11", "BOVA11"),
  weights = c(.25, 0.25, .25, .25)
)


# grafico individual
ggplot(matrix_return, aes(x = data, y =acumulado)) +
  geom_line(aes(color = ativo)) +
  guides(fill= F) +
  facet_wrap(~ativo) +
  ggtitle('Retornos Acumulados') +
  xlab('Data') +
  ylab('%') +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank())



rownames(matrix_return_ETF) = matrix_return_ETF$data

matrix_return_funds = matrix_return %>%
  filter(ativo %in% c('TRUXT 1 LB','REAL INVESTOR','MILES ACER', "DAHLIA TOTAL RETURN")) %>%
  select(-prices)


matrix_return_funds %>%
  filter(ativo == "REAL INVESTOR") %>%
  tail()



# Construindo o portfolio -------------------------------------------------

port_etf = matrix_return %>%
  filter(ativo %in% c("iShares NASDAQ", "IVVB11", "SMAL11", "BOVA11")) %>%
  # Portfolio
  tq_portfolio(assets_col = ativo,
               returns_col = returns,
               weights = wts_map,
               col_rename = 'returns') %>%
  mutate(acumulado = exp(cumsum(returns))-1) %>%
  mutate(ativo = "port_etf")


portfolios = rbind(port_etf, matrix_return_funds)


# Resultado final ---------------------------------------------------------


portfolios %>%
  ggplot(aes(x = as.Date(data), y = acumulado, color = ativo)) +
  geom_line() +
  labs(title = 'Retorno Acumulado') +
  xlab('Data') +
  theme(plot.subtitle = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5)) +
  theme_fivethirtyeight() +
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title = element_text(family = "serif", hjust = .5))


