library(tidyverse)
library(tmap)
library(cagedExplorer)

# Loading and filtering dataset -----------------------------------------------
snis <- readRDS('snis-2018-clean.rds')

snis_agua <- snis %>% 
  filter(
    !(municipio_clean %in% c('MAUA', 'SALTO', 'SANTA MARIA DA SERRA') &
        tipo_servico == 'Esgotos')
  )

# Custom map theme and settings -----------------------------------------------
theme_set(custom_theme())

custom_map_settings <- 
  tm_layout(main.title.size = 1.2, fontfamily = 'serif', scale = 1.1,
            main.title.fontface = 'bold', bg.color = "white",
            inner.margins = c(.1, .1, .1, .1)) +
  tm_compass(north = 0, type = "8star",size = 2,
             position = c("right", "bottom")) +
  tm_scale_bar(text.size = 0.6, text.color = NA, lwd = 1,
               color.dark = "black", color.light = "white") +
  tm_legend(legend.position = c(0.01,0.08)) +
  tm_borders(col = "black", lwd = 0.3)

# Mapa: natureza jurídica -----------------------------------------------------
mapa_agua_nat_jur <- snis_agua %>% 
  rename(`Natureza jurídica` = nat_jur_simplified) %>% 
  add_geometry_municipios() %>% 
  tm_shape() +
  tm_style("beaver") +
  tm_fill(
    'Natureza jurídica',
    palette = c('#fed976', '#fb6a4a', '#225ea8', '#bdbdbd'),
    alpha = 1,
    id = "municipio_clean"
    ) +
  tm_layout(main.title = 'Natureza jurídica do prestador de serviços - Água') +
  custom_map_settings ; mapa_agua_nat_jur

# Saving
tmap_save(mapa_agua_nat_jur, height = 6, width = 6,
          filename = 'mapa_natureza_juridica_agua.png')

# Barplot: natureza jurídica  -------------------------------------------------
# Data wrangling
snis_agua_barplot <- snis_agua %>% 
  mutate(natureza_juridica = 
           case_when(
             sigla_prestador == 'SABESP' ~ 'SABESP',
             str_detect(natureza_juridica, 'ista') ~ 'Outros',
             str_detect(natureza_juridica, 'direta') ~ 'Adm. pública direta',
             TRUE ~ natureza_juridica
             ) %>% fct_relevel(
               'SABESP',
               'Outros',
               'Adm. pública direta',
               'Autarquia',
               'Empresa pública',
               'Empresa privada',
               'Sem dados')
         )

# Plotting
barplot_nat_jur <- ggplot(snis_agua_barplot) +
  geom_bar(aes(x = nat_jur_simplified, fill = natureza_juridica),
           col = 'gray25') +
  scale_fill_manual(values = c('#fed976', '#ffffcc', '#fcbba1',
                               '#fb6a4a', '#a50f15',
                               '#225ea8', '#bdbdbd')) +
  scale_x_discrete(labels = c('Soc. de Econ. Mista', 'Adm. Pública',
                              'Empresa Privada', 'Sem Dados')) +
  scale_y_continuous(breaks = seq(0, 400, by = 50)) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank()) +
  labs(
    x = 'Natureza jurídica',
    y = 'Número de municípios',
    title = 'Fornecimento de água em SP',
    subtitle = 'Natureza jurídica dos prestadores de serviço'
  ) ; barplot_nat_jur

# Saving
ggsave(plot = barplot_nat_jur, width = 6, height = 7,
       filename = 'barplot-fornecimento-agua.png')

# Histograma: indice atendimento de agua --------------------------------------
hist_atend_agua <- ggplot(snis_agua) +
  geom_histogram(aes(x = in055_indice_de_atendimento_total_de_agua,
                     fill = nat_jur_simplified),
                 bins = 30) +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = 'bottom') +
  scale_fill_manual(values = c('#fed976', '#fb6a4a', '#225ea8')) +
  labs(
    x = 'Índice de atendimento total de água',
    y = 'Número de municípios',
    title = 'Distribuição do índice de atendimento de água'
    ) ; hist_atend_agua

# Saving
ggsave(plot = hist_atend_agua, width = 6, height = 6,
       filename = 'histogram-indice-atendimento-agua.png')

# Mapa: índice de atendimento -------------------------------------------------
mapa_atendimento_agua <- snis_agua %>% 
  add_geometry_municipios() %>% 
  rename(`Índice de atendimento` =
           in055_indice_de_atendimento_total_de_agua) %>% 
  tm_shape() +
  tm_style(
    "beaver",
    legend.format = list(fun = function(x) str_c(round(x), '%'),
                         text.separator = " a ")
    ) +
  tm_fill(
    'Índice de atendimento',
    palette = 'Blues',
    style = 'quantile',
    n = 6,
    alpha = 1,
    id = "municipio_clean",
    textNA = "Sem dados",
    colorNA = '#fff7bc'
    ) +
  tm_layout(main.title = 
      'Índice de atendimento total de água - Municípios paulistas') +
  custom_map_settings ; mapa_atendimento_agua

# Saving
tmap_save(mapa_atendimento_agua, height = 6, width = 6,
          filename = 'mapa_atendimento_agua.png')

# Mapa: tarifa de água --------------------------------------------------------
mapa_tarifa_agua <- snis_agua %>% 
  add_geometry_municipios() %>% 
  rename(`Tarifa média (R$/m3)` =
           in005_tarifa_media_de_agua) %>% 
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
    n = 6,
    alpha = 1,
    id = "municipio_clean",
    textNA = "Sem dados",
    colorNA = '#fff7bc'
    ) +
  tm_layout(main.title = 
              'Tarifa média do fornecimento de água - Municípios paulistas') +
  custom_map_settings ; mapa_tarifa_agua

# Saving
tmap_save(mapa_tarifa_agua,  height = 6, width = 6,
          filename = 'mapa_tarifa_media_agua.png')

# Mapa: tarifa média (água + esgoto) ------------------------------------------
mapa_tarifa_media <- snis_agua %>% 
  add_geometry_municipios() %>% 
  rename(`Tarifa média (R$/m3)` =
           in004_tarifa_media_praticada) %>% 
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
    n = 6,
    alpha = 1,
    id = "municipio_clean",
    textNA = "Sem dados",
    colorNA = '#fff7bc'
  ) +
  tm_layout(main.title = 
              'Tarifa média - Água e esgoto - Municípios paulistas') +
  custom_map_settings ; mapa_tarifa_media

# Saving
tmap_save(mapa_tarifa_media,  height = 6, width = 6,
          filename = 'mapa_tarifa_media_agua_e_esgoto.png')


# Mapa: investimento per capita -----------------------------------------------
# Data wrangling
snis_inv <- snis_agua %>% 
  rename(inv_prest = 
           fn033_investimentos_totais_realizados_pelo_prestador_de_servicos,
         inv_est = fn058_investimentos_totais_realizados_pelo_estado,
         inv_mun = fn048_investimentos_totais_realizados_pelo_municipio) %>% 
  mutate(na_inv_prest = is.na(inv_prest),
         na_inv_est = is.na(inv_est),
         na_inv_mun = is.na(inv_mun)) %>% 
  mutate_at(.vars = vars(inv_prest, inv_est, inv_mun),
            .funs = function(x) ifelse(is.na(x), 0, x)) %>% 
  mutate(inv_total = 
           ifelse(na_inv_prest & na_inv_est & na_inv_mun,
                  NA_real_,
                  inv_prest + inv_est + inv_mun)) %>% 
  mutate(`Inv. per capita (R$)` =
           inv_total / pop)

# Plotting
mapa_investimento_per_capita <- snis_inv %>% 
  add_geometry_municipios() %>% 
  tm_shape() +
  tm_style(
    "beaver",
    legend.format = 
      list(fun = function(x) formatC(x, big.mark = '.', decimal.mark = ','),
           text.separator = " a ")
  ) +
  tm_fill(
    'Inv. per capita (R$)',
    palette = c(RColorBrewer::brewer.pal(6, 'Blues')[-6], "#08306B"),
    style = 'fixed',
    breaks = c(0, 2, 15, 27, 43, 72, 400, 1357),
    alpha = 1,
    id = "municipio_clean",
    textNA = "Sem dados",
    colorNA = '#fff7bc'
  ) +
  tm_layout(main.title = 
      'Investimento per capita - Água e Esgoto - Municípios paulistas') +
  custom_map_settings ; mapa_investimento_per_capita

# Saving
tmap_save(mapa_investimento_per_capita, width = 6, height = 6,
          filename = 'mapa_investimento_total_per_capita.png')

# Mapa: indicador de desempenho financeiro ------------------------------------
mapa_desempenho_financeiro <- snis_inv %>% 
  rename(`Ind. Desemp. Financeiro` =
           in012_indicador_de_desempenho_financeiro) %>% 
  add_geometry_municipios() %>% 
  tm_shape() +
  tm_style(
    "beaver",
    legend.format = 
      list(fun = function(x) round(x) %>% 
             str_replace('\\.', ','),
           text.separator = " a ")
    ) +
  tm_fill(
    'Ind. Desemp. Financeiro',
    palette = 'Blues',
    style = 'quantile',
    n = 6,
    alpha = 1,
    id = "municipio_clean",
    textNA = "Sem dados",
    colorNA = '#fff7bc'
    ) +
  tm_layout(main.title = 
      'Investimento per capita - Água e Esgoto - Municípios paulistas') + 
  custom_map_settings ; mapa_desempenho_financeiro

# Saving
tmap_save(mapa_desempenho_financeiro, width = 6, height = 6,
          filename = 'mapa_desempenho_financeiro.png')

snis_agua %>% glimpse()
