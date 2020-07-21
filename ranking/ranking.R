library(tidyverse)
library(tmap)
library(cagedExplorer)
theme_set(custom_theme())

# Loading and filtering dataset -----------------------------------------------
snis_esgoto <- readRDS('data/snis-esgoto.rds')
snis_agua <- readRDS('data/snis-agua.rds')

snis <- snis_agua %>% 
  select(municipio, municipio_clean, regiao_governo, regiao_administrativa,
         pib2017,
         pib_per_capita2017,
         tipo_prestador = nat_jur_simplified,
         populacao = pop2017, inv_per_capita,
         tarifa_agua = in004_tarifa_media_praticada,
         desempenho = in012_indicador_de_desempenho_financeiro,
         perdas = in013_indice_de_perdas_faturamento,
         consumo_agua = in022_consumo_medio_percapita_de_agua,
         atendimento = in055_indice_de_atendimento_total_de_agua) %>% 
  inner_join(
    snis_esgoto %>% 
      select(
        municipio_clean, Codmun7,
        tarifa_esgoto = in006_tarifa_media_de_esgoto,
        coleta = in015_indice_de_coleta_de_esgoto,
        tratamento = in046_indice_de_esgoto_tratado_referido_à_agua_consumida
        ),
    by = 'municipio_clean'
    )

# Function to normalize variables
minmax_norm <- function(x, reverse = FALSE,
                        scale_to_100 = TRUE) {
  if (reverse) numerator <- max(x) - x
  if (!reverse) numerator <- x - min(x)
  denominator <- max(x) - min(x)
  x_norm <- numerator / denominator
  if (scale_to_100) x_norm <-  x_norm * 100
  x_norm
}

# Computing overall score
snis_ranking <- snis %>% 
  drop_na() %>% 
  select(-municipio_clean) %>% 
  mutate(perdas = ifelse(perdas > 0, perdas, 0)) %>% # <----
  mutate(sqrt_inv_per_capita = sqrt(inv_per_capita)) %>% 
  mutate(
    tarifa_agua_norm = minmax_norm(tarifa_agua, reverse = TRUE),
    tarifa_esgoto_norm = minmax_norm(tarifa_esgoto, reverse = TRUE),
    inv_per_capita_norm = minmax_norm(inv_per_capita),
    sqrt_inv_per_capita_norm = minmax_norm(sqrt_inv_per_capita),
    perdas_norm = minmax_norm(perdas, reverse = TRUE),
    desempenho_norm = minmax_norm(desempenho),
    # Score as average of normalized indicators
    score = (atendimento + coleta + tratamento +
              tarifa_agua_norm + tarifa_esgoto_norm +
              sqrt_inv_per_capita_norm + perdas_norm +
               desempenho_norm) / 8
) %>% 
  mutate(ranking = dense_rank(desc(score))) %>% 
  arrange(ranking)

# Saving results in csv files
municipios_sp %>% 
  select(municipio, municipio_clean, Codmun7) %>% 
  left_join(snis_ranking %>% 
              select(-municipio),
            by = 'Codmun7') %>% 
  select(municipio, regiao_administrativa, regiao_governo,
         populacao2017 = populacao, pib2017, pib_per_capita2017,
         score, ranking,
         tarifa_agua, tarifa_esgoto,
         atendimento, coleta, tratamento,
         inv_per_capita, perdas, desempenho) %>% 
  arrange(ranking) %>% 
  write_excel_csv2('ranking-saneamento-sp-variaveis-nao-normalizadas.csv')

municipios_sp %>% 
  select(municipio, municipio_clean, Codmun7) %>% 
  left_join(snis_ranking %>% 
              select(-municipio),
            by = 'Codmun7') %>% 
  select(municipio, regiao_administrativa, regiao_governo,
         populacao2017 = populacao, pib2017, pib_per_capita2017,
         score, ranking,
         tarifa_agua_norm, tarifa_esgoto_norm,
         atendimento, coleta, tratamento,
         sqrt_inv_per_capita_norm, perdas_norm, desempenho_norm) %>% 
  arrange(ranking) %>% 
  write_excel_csv2('ranking-saneamento-sp-variaveis-normalizadas.csv')

snis_ranking %>% 
  select(-Codmun7) %>% 
  select(ranking, municipio, score, regiao_governo:desempenho_norm) %>% 
  saveRDS('ranking/snis_ranking.rds')
  
snis_ranking %>% 
  select(-Codmun7) %>% 
  select(ranking, municipio, score, regiao_governo:desempenho_norm) %>% 
  write.csv('ranking/snis_ranking.csv', row.names = FALSE)

# Table with top 10 and worst 10 cities with at least 50k habitants -----------
snis_ranking_50k <- snis_ranking %>% 
  filter(populacao >= 50e+3)

highlights <- snis_ranking_50k %>% 
  select(ranking, municipio, score,
         tipo_prestador, populacao, regiao_administrativa, regiao_governo,
         atendimento, coleta, tratamento,
         tarifa_agua, tarifa_esgoto, perdas, inv_per_capita , desempenho) %>%
  slice(c(1:10, (nrow(snis_ranking_50k) - 9):(nrow(snis_ranking_50k)))) %>% 
  mutate_if(.predicate = is.numeric,
            .funs = partial(round, digits = 2)) ; highlights

# Persisting highlights (csv and rds)
write.csv(highlights, file = 'ranking/highlights_ranking.csv', row.names = FALSE)
write_rds(highlights, 'ranking/highlights_ranking.rds')

# Table with results for main cities from administrative regions --------------
# Codes for the main cities
codigos_sedes <- municipios_sp %>% 
  filter(municipio == regiao_administrativa) %>% 
  pull(Codmun7)

# Filtering: keeping only results for the 15 main cities
snis_sedes <- snis_ranking %>% 
  filter(Codmun7 %in% codigos_sedes)

ranking_sedes <- snis_sedes %>% 
  select(ranking, municipio, score,
         tipo_prestador, populacao,
         atendimento, coleta, tratamento,
         tarifa_agua, tarifa_esgoto, perdas, inv_per_capita , desempenho) %>%
  mutate_if(.predicate = is.numeric,
            .funs = partial(round, digits = 2)) ; ranking_sedes

# Persisting highlights (csv and rds)
write.csv(ranking_sedes, file = 'ranking/ranking_sedes.csv', row.names = FALSE)
write_rds(ranking_sedes, 'ranking/ranking_sedes.rds')

# Plotting score distribution -------------------------------------------------
sao_paulo <- snis_ranking %>% filter(municipio == 'São Paulo') %>% pull(score)
campinas <- snis_ranking %>% filter(municipio == 'Campinas') %>% pull(score)
ribeirao <- snis_ranking %>% filter(municipio == 'Ribeirão Preto') %>% pull(score)

height_campinas <- 80
height_sp <- 100
height_rp <- 80

hist_ranking <- snis_ranking %>% 
  ggplot() +
  geom_histogram(
    aes(x = score, fill = tipo_prestador),
    bins = 15
  ) +
  geom_path(
    data = tibble(x = rep(ribeirao, 2),y = c(0, height_rp)),
    mapping = aes(x = x, y = y), linetype = 'dotted', alpha = .8,
  ) +
  geom_label(
    x = ribeirao, y = height_rp, color = 'gray15', fill = 'gray97',
    label = str_c('Ribeirão Preto\n', round(ribeirao, 1)) %>%
      str_replace('\\.', ','),
    family = 'serif', size = 3
  ) +
  geom_path(
    data = tibble(x = rep(sao_paulo, 2),y = c(0, height_sp)),
    mapping = aes(x = x, y = y), linetype = 'dotted', alpha = .8,
  ) +
  geom_label(
    x = sao_paulo, y = height_sp, color = 'gray15', fill = 'gray97',
    label = str_c('São Paulo\n', round(sao_paulo, 1)) %>%
      str_replace('\\.', ','),
    family = 'serif', size = 3
  ) +
  geom_path(
    data = tibble(x = rep(campinas, 2),y = c(0, height_campinas)),
    mapping = aes(x = x, y = y), linetype = 'dotted', alpha = .8,
  ) +
  geom_label(
    x = campinas, y = height_campinas, color = 'gray15', fill = 'gray97',
    label = str_c('Campinas\n', round(campinas, 1)) %>%
      str_replace('\\.', ','),
    family = 'serif', size = 3
  ) +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = c(.15, .875)) +
  scale_fill_manual(
    values = c('#fed976', '#fb6a4a', '#225ea8'),
    labels = c('Sociedade de economia mista',
               'Administração pública',
               'Empresa privada')
    ) +
  labs(
    x = 'Média dos indicadores normalizados (0 a 100)',
    y = 'Número de municípios',
    title = 'Qualidade dos serviços de saneamento básico (água e esgoto)',
    subtitle = 'Distribuição das notas dos municípios paulistas'
  )

hist_ranking

# Saving
ggsave(plot = hist_ranking, width = 7.5, height = 5.5,
       filename = 'plots/histogram-ranking-agua-esgoto.png')


# Average by provider type ----------------------------------------------------
avg_by_provider_type <- snis_ranking %>% 
  group_by(tipo_prestador) %>% 
  summarise(n = n(),
            avg_pop = mean(populacao),
            median_pop = median(populacao),
            media_ponderada = weighted.mean(score, w = populacao),
            media_simples = mean(score)) %>% 
  ungroup()

saveRDS(avg_by_provider_type, 'avg_by_provider_type.rds')

write.csv(x = avg_by_provider_type,
          file = 'avg_by_provider_type.csv',
          row.names = FALSE)


# Map -------------------------------------------------------------------------
library(cagedExplorer)
source('custom_map_settings.R')

mapa_ranking <- municipios_sp %>% 
  select(Codmun7) %>% 
  left_join(snis_ranking, by = 'Codmun7') %>% 
  add_geometry_municipios() %>% 
  rename(`Pontuação` = score) %>% 
  tm_shape() +
  tm_style(
    "beaver",
    legend.format = 
      list(fun = function(x) formatC(round(x),
                                     big.mark = '.',
                                     decimal.mark = ','),
           text.separator = " a ")
  ) +
  tm_fill(
    'Pontuação',
    palette = 'Blues',
    style = 'quantile',
    n = 5,
    alpha = 1,
    id = "municipio",
    textNA = "Sem dados",
    colorNA = '#fff7bc'
  ) +
  tm_layout(main.title = 
              'Média dos indicadores - Água e Esgoto - Municípios paulistas') +
  custom_map_settings ; mapa_ranking

# Saving
tmap_save(mapa_ranking, width = 6, height = 6,
          filename = 'plots/mapa-ranking.png')


# Checking investment distributions
snis_ranking %>% 
  select(municipio, inv_per_capita_norm, sqrt_inv_per_capita_norm) %>% 
  pivot_longer(cols = -municipio) %>% 
  mutate(name = ifelse(name == 'inv_per_capita_norm', 'MinMax', 'MinMax da raiz quadrada')) %>% 
  ggplot() +
  geom_histogram(aes(x = value)) +
  theme(panel.grid = element_blank()) +
  labs(x = 'Investimento per capita (normalizado de 0 a 100)',
       y = 'Número de municípios',
       title = 'Distribuição do investimento per capita',
       subtitle = 'Comparação entre diferentes métodos de normalização') +
  facet_wrap(~ name, nrow = 1)

ggsave(filename = 'histogramas-inv-per-capita-comparacao-normalizacao.png',
       width = 5, height = 4)

