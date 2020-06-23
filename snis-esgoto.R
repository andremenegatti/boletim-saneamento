library(tidyverse)
library(tmap)
library(cagedExplorer)
source('geom_flat_violin.R')

# Loading and filtering dataset -----------------------------------------------
snis <- readRDS('data/snis-2018-clean.rds')

snis_esgoto <- snis %>% 
  filter(!(municipio_clean %in% 
             c('MAUA', 'SALTO', 'SANTA MARIA DA SERRA') &
             tipo_servico == 'Água'))

# saveRDS(snis_esgoto, 'data/snis-esgoto.rds')

# Custom map theme and settings -----------------------------------------------
theme_set(custom_theme())
source('custom_map_settings.R')

# Mapa: natureza jurídica -----------------------------------------------------
mapa_esgoto_tipo <- snis_esgoto %>% 
  rename(`Tipo de prestador` = nat_jur_simplified) %>% 
  add_geometry_municipios() %>% 
  tm_shape() +
  tm_style("beaver") +
  tm_fill(
    'Tipo de prestador',
    palette = c('#fed976', '#fb6a4a', '#225ea8', '#bdbdbd'),
    alpha = 1,
    id = "municipio_clean"
  ) +
  tm_layout(main.title = 
              'Tipo de prestador de serviços - Esgoto') +
  custom_map_settings ; mapa_esgoto_tipo

# Saving
tmap_save(mapa_esgoto_tipo, height = 6, width = 6,
          filename = 'plots/esgoto/mapa-tipo-prestador-esgoto.png')

# Mapa: índice de coleta de esgoto --------------------------------------------
mapa_coleta_esgoto <- snis_esgoto %>% 
  add_geometry_municipios() %>% 
  rename(`Índice de coleta` =
           in015_indice_de_coleta_de_esgoto) %>% 
  tm_shape() +
  tm_style(
    "beaver",
    legend.format = list(fun = function(x) str_c(round(x), '%'),
                         text.separator = " a ")
  ) +
  tm_fill(
    'Índice de coleta',
    palette = 'Blues',
    style = 'quantile',
    n = 6,
    alpha = 1,
    id = "municipio_clean",
    textNA = "Sem dados",
    colorNA = '#fff7bc'
  ) +
  tm_layout(main.title = 
              'Índice de coleta de esgoto - Municípios paulistas') +
  custom_map_settings ; mapa_coleta_esgoto

# Saving
tmap_save(mapa_coleta_esgoto, height = 6, width = 6,
          filename = 'plots/esgoto/mapa-coleta-esgoto.png')

# Histograma: indice de coleta de esgoto --------------------------------------
hist_coleta_esgoto <- ggplot(snis_esgoto) +
  geom_histogram(aes(x = in015_indice_de_coleta_de_esgoto,
                     fill = nat_jur_simplified),
                 bins = 30) +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = 'bottom') +
  scale_fill_manual(values = c('#fed976', '#fb6a4a', '#225ea8')) +
  labs(
    x = 'Índice de coleta de esgoto',
    y = 'Número de municípios',
    title = 'Distribuição do índice de coleta de esgoto'
  ) ; hist_coleta_esgoto

# Saving
ggsave(plot = hist_coleta_esgoto, width = 6, height = 7,
       filename = 'plots/esgoto/histogram-indice-coleta-esgoto.png')

# Mapa: índice de tratamento de esgoto ----------------------------------------
mapa_tratamento_esgoto <- snis_esgoto %>% 
  add_geometry_municipios() %>% 
  rename(`Índice de tratamento` =
           in046_indice_de_esgoto_tratado_referido_à_agua_consumida) %>% 
  tm_shape() +
  tm_style(
    "beaver",
    legend.format = list(fun = function(x) str_c(round(x), '%'),
                         text.separator = " a ")
  ) +
  tm_fill(
    'Índice de tratamento',
    palette = 'Blues',
    style = 'quantile',
    n = 6,
    alpha = 1,
    id = "municipio_clean",
    textNA = "Sem dados",
    colorNA = '#fff7bc'
  ) +
  tm_layout(main.title = 
              'Índice de tratamento de esgoto (relativo ao consumo de água)') +
  custom_map_settings ; mapa_tratamento_esgoto

# Saving
tmap_save(mapa_tratamento_esgoto, height = 6, width = 6,
          filename = 'plots/esgoto/mapa-tratamento-esgoto.png')

# Histograma: indice de tratamento de esgoto ----------------------------------
hist_trat_esgoto <- ggplot(snis_esgoto) +
  geom_histogram(
    aes(x = in046_indice_de_esgoto_tratado_referido_à_agua_consumida,
        fill = nat_jur_simplified),
    bins = 30
    ) +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = 'bottom') +
  scale_fill_manual(values = c('#fed976', '#fb6a4a', '#225ea8')) +
  labs(
    x = 'Índice de tratamento de esgoto',
    y = 'Número de municípios',
    title = 'Distribuição do índice de tratamento de esgoto'
  ) ; hist_trat_esgoto

ggsave(plot = hist_trat_esgoto, width = 6, height = 7,
       filename = 'plots/esgoto/histogram-indice-tratamento-esgoto.png')

# Mapa: tarifa de esgoto ------------------------------------------------------
mapa_tarifa_esgoto <- snis_esgoto %>% 
  add_geometry_municipios() %>% 
  rename(`Tarifa média (R$/m3)` =
           in006_tarifa_media_de_esgoto) %>% 
  tm_shape() +
  tm_style(
    "beaver",
    legend.format = 
      list(fun = function(x) formatC(x, digits = 2,
                                     big.mark = '.', decimal.mark = ','),
           text.separator = " a ")
  ) +
  tm_fill(
    'Tarifa média (R$/m3)',
    palette = 'Blues',
    style = 'quantile',
    n = 7,
    alpha = 1,
    id = "municipio_clean",
    textNA = "Sem dados",
    colorNA = '#fff7bc'
  ) +
  tm_layout(main.title = 
              'Tarifa média - Esgoto - Municípios paulistas') +
  custom_map_settings ; mapa_tarifa_esgoto

# Saving
tmap_save(mapa_tarifa_esgoto,  height = 6, width = 6,
          filename = 'plots/esgoto/mapa-tarifa-media-esgoto.png')

# Boxplot: tarifa esgoto ------------------------------------------------------
snis_boxplot_tarifa <- snis_esgoto %>% 
  filter(nat_jur_simplified != 'Sem dados') %>% 
  mutate(nat_jur_simplified = droplevels(nat_jur_simplified) %>% 
           fct_reorder(.x = in006_tarifa_media_de_esgoto,
                       .fun = median, na.rm=TRUE))

summary_table_tarifa <- snis_boxplot_tarifa %>% 
  group_by(nat_jur_simplified) %>% 
  summarise(Mediana = median(in006_tarifa_media_de_esgoto,
                             na.rm = TRUE)) %>% 
  mutate(Label = round(Mediana, 2) %>% str_replace('\\.', ','))

boxplot_tarifa_esgoto <- ggplot(snis_boxplot_tarifa) +
  geom_flat_violin(aes(x = nat_jur_simplified,
                       y = in006_tarifa_media_de_esgoto,
                       fill = nat_jur_simplified),
                   alpha = 0.5) +
  geom_boxplot(aes(x = nat_jur_simplified,
                   y = in006_tarifa_media_de_esgoto),
               width = 0.1, outlier.alpha = 0.3) +
  geom_text(data = summary_table_tarifa,
            aes(x = nat_jur_simplified,
                y = Mediana, label = Label),
            size = 3, nudge_x = 0.17, family = 'serif') +
  scale_x_discrete(labels = c('Adm. Pública',
                              'Soc. de Econ. Mista',
                              'Empresa Privada')) +
  scale_fill_manual(values = c('#fb6a4a', '#fed976', '#225ea8')) +
  theme(legend.position = 'none', panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = 'Tipo de prestador de serviços',
    y = 'Tarifa média do serviço de esgotamento (R$/m3)',
    title = 'Distribuição das médias municipais da tarifa de esgoto',
    subtitle = 'Comparação entre diferentes tipos de prestadores de serviços'
  ) ; boxplot_tarifa_esgoto

ggsave(plot = boxplot_tarifa_esgoto, width = 5, height = 5.5,
       filename = 'plots/esgoto/boxplot-tarifa-esgoto.png')

# Boxplot: tratamento esgoto --------------------------------------------------
# Data wrangling
snis_boxplot_tratamento <- snis_esgoto %>% 
  filter(nat_jur_simplified != 'Sem dados') %>% 
  mutate(nat_jur_simplified = droplevels(nat_jur_simplified) %>% 
           fct_reorder(.x = in046_indice_de_esgoto_tratado_referido_à_agua_consumida,
                       .fun = median, na.rm=TRUE))

summary_table_tratamento <- snis_boxplot_tratamento %>% 
  group_by(nat_jur_simplified) %>% 
  summarise(Mediana = 
              median(in046_indice_de_esgoto_tratado_referido_à_agua_consumida,
                     na.rm = TRUE)
            ) %>% 
  mutate(Label = round(Mediana, 2) %>% str_replace('\\.', ','))

# Plotting
boxplot_tratamento_esgoto <- ggplot(snis_boxplot_tratamento) +
  geom_flat_violin(aes(x = nat_jur_simplified,
                       y = in046_indice_de_esgoto_tratado_referido_à_agua_consumida,
                       fill = nat_jur_simplified),
                   alpha = 0.5) +
  geom_boxplot(aes(x = nat_jur_simplified,
                   y = in046_indice_de_esgoto_tratado_referido_à_agua_consumida),
               width = 0.05, outlier.alpha = 0.3) +
  geom_text(data = summary_table_tratamento,
            aes(x = nat_jur_simplified,
                y = Mediana, label = Label),
            size = 3, nudge_x = 0.17, family = 'serif') +
  scale_x_discrete(labels = c('Adm. Pública',
                              'Soc. de Econ. Mista',
                              'Empresa Privada')) +
  scale_fill_manual(values = c('#fb6a4a', '#fed976', '#225ea8')) +
  theme(legend.position = 'none', panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = 'Tipo de prestador de serviços',
    y = 'Índice de tratamento de esgoto',
    title = 'Distribuição do índice de tratamento',
    subtitle = 'Comparação entre diferentes tipos de prestadores de serviços'
  ) ; boxplot_tratamento_esgoto

# Saving
ggsave(plot = boxplot_tratamento_esgoto, width = 5, height = 5.5,
       filename = 'plots/esgoto/boxplot-tratamento-esgoto.png')


# Boxplot: coleta esgoto ------------------------------------------------------
# Data wrangling
snis_boxplot_coleta <- snis_esgoto %>% 
  filter(nat_jur_simplified != 'Sem dados') %>% 
  mutate(nat_jur_simplified = droplevels(nat_jur_simplified) %>% 
           fct_reorder(.x = in015_indice_de_coleta_de_esgoto,
                       .fun = median, na.rm=TRUE))

summary_table_coleta <- snis_boxplot_coleta %>% 
  group_by(nat_jur_simplified) %>% 
  summarise(Mediana = 
              median(in015_indice_de_coleta_de_esgoto,
                     na.rm = TRUE)
  ) %>% 
  mutate(Label = round(Mediana, 2) %>% str_replace('\\.', ','))

# Plotting
boxplot_coleta_esgoto <- ggplot(snis_boxplot_coleta) +
  geom_flat_violin(aes(x = nat_jur_simplified,
                       y = in015_indice_de_coleta_de_esgoto,
                       fill = nat_jur_simplified),
                   alpha = 0.5) +
  geom_boxplot(aes(x = nat_jur_simplified,
                   y = in015_indice_de_coleta_de_esgoto),
               width = 0.1, outlier.alpha = 0.3) +
  geom_text(data = summary_table_tratamento,
            aes(x = nat_jur_simplified,
                y = Mediana, label = Label),
            size = 3, nudge_x = 0.17, family = 'serif') +
  scale_x_discrete(labels = c('Adm. Pública',
                              'Soc. de Econ. Mista',
                              'Empresa Privada')) +
  scale_fill_manual(values = c('#fb6a4a', '#fed976', '#225ea8')) +
  theme(legend.position = 'none', panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = 'Tipo de prestador de serviços',
    y = 'Índice de coleta de esgoto',
    title = 'Distribuição do índice de coleta',
    subtitle = 'Comparação entre diferentes tipos de prestadores de serviços'
  ) ; boxplot_coleta_esgoto

# Saving
ggsave(plot = boxplot_coleta_esgoto, width = 5, height = 5.5,
       filename = 'plots/esgoto/boxplot-coleta-esgoto.png')