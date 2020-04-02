library(tidyverse)
library(tmap)
library(cagedExplorer)
source('geom_flat_violin.R')

# Loading and filtering dataset -----------------------------------------------
snis <- readRDS('snis-2018-clean.rds')

snis_agua <- snis %>% 
  filter(!(municipio_clean %in% 
             c('MAUA', 'SALTO', 'SANTA MARIA DA SERRA') &
        tipo_servico == 'Esgotos'))

# Joining GDP data ------------------------------------------------------------
snis_agua <- snis_agua %>% 
  left_join(readxl::read_excel('pib-municipios-2017.xlsx') %>% 
              select(Codmun7 = codigo, pib2017 = pib) %>% 
              mutate(Codmun7 = as.integer(Codmun7)),
            by = 'Codmun7') %>% 
  left_join(readxl::read_excel('estimativa-populacao-municipios-2017.xlsx') %>%
              select(Codmun7, pop2017) %>% 
              mutate(Codmun7 = as.integer(Codmun7)),
            by = 'Codmun7') %>% 
  mutate(pib_per_capita2017 = pib2017 / pop2017) # In thousands of BRL

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

# Mapa: tipo de prestador -----------------------------------------------------
mapa_agua_tipo <- snis_agua %>% 
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
  tm_layout(main.title = 'Tipo do prestador de serviços - Água') +
  custom_map_settings ; mapa_agua_tipo

# Saving
tmap_save(mapa_agua_tipo, height = 6, width = 6,
          filename = 'plots/agua/mapa-tipo-prestador-agua.png')

# Barplot: tipo de prestador  -------------------------------------------------
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
barplot_tipo <- ggplot(snis_agua_barplot) +
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
    x = 'Tipo de prestador de serviços',
    y = 'Número de municípios',
    title = 'Fornecimento de água em SP',
    subtitle = 'Por tipo de prestador de serviço'
  ) ; barplot_tipo

# Saving
ggsave(plot = barplot_tipo, width = 6, height = 7,
       filename = 'plots/agua/barplot-fornecimento-agua.png')

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
       filename = 'plots/agua/histogram-indice-atendimento-agua.png')

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
          filename = 'plots/agua/mapa-atendimento-agua.png')

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
          filename = 'plots/agua/mapa-tarifa-media-agua.png')

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
          filename = 'plots/mapa-tarifa-media-agua-e-esgoto.png')

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
          filename = 'plots/mapa-investimento-total-per-capita.png')

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
      'Indicador de desempenho financeiro - Municípios paulistas') + 
  custom_map_settings ; mapa_desempenho_financeiro

# Saving
tmap_save(mapa_desempenho_financeiro, width = 6, height = 6,
          filename = 'plots/mapa-desempenho-financeiro.png')

# Boxplot: atendimento água ---------------------------------------------------
# Data wrangling
snis_boxplot_atendimento <- snis_agua %>% 
  filter(nat_jur_simplified != 'Sem dados') %>% 
  mutate(nat_jur_simplified = droplevels(nat_jur_simplified) %>% 
           fct_reorder(.x = in055_indice_de_atendimento_total_de_agua,
                       .fun = median, na.rm=TRUE))

summary_table_atendimento <- snis_boxplot_atendimento %>% 
  group_by(nat_jur_simplified) %>% 
  summarise(Mediana = median(in055_indice_de_atendimento_total_de_agua,
                             na.rm = TRUE)) %>% 
  mutate(Label = round(Mediana, 2) %>% str_replace('\\.', ','))

# Plotting
boxplot_atendimento_agua <- ggplot(snis_boxplot_tarifa) +
  geom_flat_violin(aes(x = nat_jur_simplified,
                       y = in055_indice_de_atendimento_total_de_agua,
                       fill = nat_jur_simplified),
                   alpha = 0.5) +
  geom_boxplot(aes(x = nat_jur_simplified,
                   y = in055_indice_de_atendimento_total_de_agua),
               width = 0.1, outlier.alpha = 0.3) +
  geom_text(data = summary_table_atendimento,
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
    y = 'Percentual da população atendida',
    title = 'Distribuição dos índices municip. de atendimento (água)',
    subtitle = 'Comparação entre diferentes tipos de prestadores de serviços'
  ) ; boxplot_atendimento_agua

# Saving
ggsave(plot = boxplot_atendimento_agua, width = 5, height = 5.5,
       filename = 'plots/agua/boxplot-atendimento-agua.png')

# Boxplot: tarifa água --------------------------------------------------------
# Data wrangling
snis_boxplot_tarifa <- snis_agua %>% 
  filter(nat_jur_simplified != 'Sem dados') %>% 
  mutate(nat_jur_simplified = droplevels(nat_jur_simplified) %>% 
           fct_reorder(.x = in005_tarifa_media_de_agua,
                       .fun = median, na.rm=TRUE))

summary_table_tarifa <- snis_boxplot_tarifa %>% 
  group_by(nat_jur_simplified) %>% 
  summarise(Mediana = median(in005_tarifa_media_de_agua,
                             na.rm = TRUE)) %>% 
  mutate(Label = round(Mediana, 2) %>% str_replace('\\.', ','))

# Plotting
boxplot_tarifa_agua <- ggplot(snis_boxplot_tarifa) +
  geom_flat_violin(aes(x = nat_jur_simplified,
                       y = in005_tarifa_media_de_agua,
                       fill = nat_jur_simplified),
                   alpha = 0.5) +
  geom_boxplot(aes(x = nat_jur_simplified,
                   y = in005_tarifa_media_de_agua),
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
    y = 'Tarifa média do fornecimento de água (R$/m3)',
    title = 'Distribuição das médias municipais da tarifa de água',
    subtitle = 'Comparação entre diferentes tipos de prestadores de serviços'
  ) ; boxplot_tarifa_agua

# Saving
ggsave(plot = boxplot_tarifa_agua, width = 5, height = 5.5,
       filename = 'plots/agua/boxplot-tarifa-agua.png')

# Boxplot: tarifa água + esgoto -----------------------------------------------
# Data wrangling
snis_boxplot_tarifa_total <- snis_agua %>% 
  filter(nat_jur_simplified != 'Sem dados') %>% 
  mutate(nat_jur_simplified = droplevels(nat_jur_simplified) %>% 
           fct_reorder(.x = in004_tarifa_media_praticada,
                       .fun = median, na.rm=TRUE))

summary_table_tarifa_total <- snis_boxplot_tarifa_total %>% 
  group_by(nat_jur_simplified) %>% 
  summarise(Mediana = median(in004_tarifa_media_praticada,
                             na.rm = TRUE)) %>% 
  mutate(Label = round(Mediana, 2) %>% str_replace('\\.', ','))

# Plotting
boxplot_tarifa_total <- ggplot(snis_boxplot_tarifa) +
  geom_flat_violin(aes(x = nat_jur_simplified,
                       y = in004_tarifa_media_praticada,
                       fill = nat_jur_simplified),
                   alpha = 0.5) +
  geom_boxplot(aes(x = nat_jur_simplified,
                   y = in004_tarifa_media_praticada),
               width = 0.1, outlier.alpha = 0.3) +
  geom_text(data = summary_table_tarifa_total,
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
    y = 'Tarifa média (R$/m3)',
    title = 'Distribuição das médias municipais da tarifa (água e esg.)',
    subtitle = 'Comparação entre diferentes tipos de prestadores de serviços'
  ) ; boxplot_tarifa_total

# Saving
ggsave(plot = boxplot_tarifa_total, width = 5, height = 5.5,
       filename = 'plots/boxplot-tarifa-agua-e-esgoto.png')

# Boxplot: investimento per capita -----------------------------------------------
# Data wrangling
snis_boxplot_inv <- snis_inv %>% 
  filter(nat_jur_simplified != 'Sem dados') %>% 
  mutate(nat_jur_simplified = droplevels(nat_jur_simplified) %>% 
           fct_reorder(.x = `Inv. per capita (R$)`,
                       .fun = median, na.rm=TRUE)) %>% 
  mutate(`Inv. per capita (R$)` = `Inv. per capita (R$)` + 0.01)

summary_table_inv <- snis_boxplot_inv %>% 
  group_by(nat_jur_simplified) %>% 
  summarise(Mediana = median(`Inv. per capita (R$)`,
                             na.rm = TRUE)) %>% 
  mutate(Label = round(Mediana, 2) %>% str_replace('\\.', ','))

# Plotting
boxplot_inv <- ggplot(snis_boxplot_inv) +
  geom_flat_violin(aes(x = nat_jur_simplified,
                       y = `Inv. per capita (R$)`,
                       fill = nat_jur_simplified),
                   alpha = 0.5) +
  geom_boxplot(aes(x = nat_jur_simplified,
                   y = `Inv. per capita (R$)`),
               width = 0.03, outlier.alpha = 0.3) +
  geom_text(data = summary_table_inv,
            aes(x = nat_jur_simplified,
                y = Mediana, label = Label),
            size = 3, nudge_x = 0.17, family = 'serif') +
  scale_x_discrete(labels = c('Adm. Pública',
                              'Soc. de Econ. Mista',
                              'Empresa Privada')) +
  scale_y_log10(breaks = c(0.01, 1, 2, 5, 30, 100, 500, 1000),
                labels = c(0, 1, 2, 5, 30 , 100, 500, 1000)) +
  scale_fill_manual(values = c('#fb6a4a', '#fed976', '#225ea8')) +
  theme(legend.position = 'none', panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = 'Tipo de prestador de serviços',
    y = 'Investimento per capita em R$ (escala log.)',
    title = 'Distribuição do total de investimentos na rede',
    subtitle = 'Comparação entre diferentes tipos de prestadores de serviços'
  ) ; boxplot_inv

# Saving
ggsave(plot = boxplot_inv, width = 5, height = 5.5,
       filename = 'plots/boxplot-investimento.png')


# Scatterplot: tarifa vs. atendimento: single plot ----------------------------
scatterplot_tarifa_atendimento <- snis_agua %>% 
  filter(nat_jur_simplified != 'Sem dados') %>% 
  mutate(nat_jur_simplified = droplevels(nat_jur_simplified)) %>% 
  ggplot() +
  geom_point(aes(x = in055_indice_de_atendimento_total_de_agua,
                 y = in005_tarifa_media_de_agua,
                 col = nat_jur_simplified,
                 size = pop), alpha = 0.5) +
  geom_smooth(aes(x = in055_indice_de_atendimento_total_de_agua,
                  y = in005_tarifa_media_de_agua),
              method = 'lm', col = 'gray25',
              fill = 'lightgray', alpha = 0.5) +
  scale_color_manual(values = c('#fb6a4a', '#fed976', '#225ea8')) +
  scale_y_continuous(breaks = 0:6) +
  theme(panel.grid = element_blank(), legend.position = 'bottom',
        legend.title = element_blank()) +
  guides(size = FALSE) +
  labs(
    x = 'Índice de atendimento de água',
    y = 'Tarifa média de água',
    title = 'Relação entre tarifa e índice de atendimento',
    subtitle = 'Fornecimento de água nos municípios paulistas',
    caption =
      'Fonte: SNIS (2018)\nNota: o tamanho dos pontos é proporcional à população dos municípios.'
  ); scatterplot_tarifa_atendimento

ggsave(plot = scatterplot_tarifa_atendimento, width = 6, height = 6,
       filename = 'plots/agua/scatterplot-tarifa-vs-atendimento-agua.png')

# Scatterplot: tarifa vs. atendimento - faceted -------------------------------
scatterplot_tarifa_atendimento_faceted <- scatterplot_tarifa_atendimento +
  facet_wrap(~ nat_jur_simplified, nrow = 3) +
  guides(size = FALSE, color = FALSE) ; scatterplot_tarifa_atendimento_faceted

ggsave(plot = scatterplot_tarifa_atendimento_faceted, width = 6, height = 8,
       filename = 'plots/agua/scatterplot-tarifa-vs-atendimento-agua-faceted.png')

# Scatterplot: tarifa vs. PIB - single plot  ----------------------------------
scatterplot_tarifa_pib <- snis_agua %>% 
  filter(nat_jur_simplified != 'Sem dados') %>% 
  mutate(nat_jur_simplified = droplevels(nat_jur_simplified)) %>% 
  ggplot(aes(x = pib2017, y = in005_tarifa_media_de_agua)) +
  geom_point(aes(size = pop, col = nat_jur_simplified),
             alpha = 0.4) +
  geom_smooth(method = 'lm', col = 'gray25',
              fill = 'lightgray', alpha = 0.5) +
  scale_x_log10(breaks = c(1e+5, 1e+6, 1e+7, 1e+8, 700000000),
                labels = c('100mi', '1bi', '10bi', '100bi', '700bi')) +
  scale_y_continuous(breaks = 0:6) +
  scale_color_manual(values = c('#fb6a4a', '#fed976', '#225ea8')) +
  labs(
    x = 'PIB (escala logarítmica)',
    y = 'Tarifa média de água',
    title = 'Relação entre tarifa e PIB',
    subtitle = 'Fornecimento de água nos municípios paulistas',
    caption =
      'Fonte: dados tarifários do SNIS (2018); dados de PIB do IBGE (2017).\nNota: o tamanho dos pontos é proporcional à população dos municípios.'
  ) +
  guides(size = FALSE)  +
  theme(panel.grid = element_blank(), legend.position = 'bottom',
        legend.title = element_blank()); scatterplot_tarifa_pib

# Saving
ggsave(plot = scatterplot_tarifa_pib, width = 6, height = 6,
       filename = 'plots/agua/scatterplot-tarifa-agua-vs-pib.png')

# Scatterplot: tarifa vs. PIB - faceted  -------------------------------------
scatterplot_tarifa_pib_faceted <- scatterplot_tarifa_pib +
  facet_wrap(~ nat_jur_simplified, nrow = 3) +
  guides(size = FALSE, color = FALSE) ; scatterplot_tarifa_pib_faceted

# Saving
ggsave(plot = scatterplot_tarifa_pib_faceted, width = 5, height = 8,
       filename = 'plots/agua/scatterplot-tarifa-agua-vs-pib-faceted.png')

# Scatterplot: tarifa vs. PIB per capita - single plot  -----------------------
scatterplot_tarifa_pib_per_capita <- snis_agua %>% 
  filter(nat_jur_simplified != 'Sem dados') %>% 
  mutate(nat_jur_simplified = droplevels(nat_jur_simplified)) %>% 
  ggplot(aes(x = pib_per_capita2017, y = in005_tarifa_media_de_agua)) +
  geom_point(aes(size = pop, col = nat_jur_simplified),
             alpha = 0.4) +
  geom_smooth(method = 'lm', col = 'gray25',
              fill = 'lightgray', alpha = 0.5) +
  scale_x_log10(breaks = c(10, 30, 100, 300),
                labels = c('10 mil', '30 mil', '100 mil', '300 mil')) +
  scale_y_continuous(breaks = 0:6) +
  scale_color_manual(values = c('#fb6a4a', '#fed976', '#225ea8')) +
  labs(
    x = 'PIB per capita em reais (escala logarítmica)',
    y = 'Tarifa média de água',
    title = 'Relação entre tarifa e PIB per capita',
    subtitle = 'Fornecimento de água nos municípios paulistas',
    caption =
      'Fonte: dados tarifários do SNIS (2018); dados de PIB e população do IBGE (2017).\nNota: o tamanho dos pontos é proporcional à população dos municípios.'
  ) +
  guides(size = FALSE)  +
  theme(panel.grid = element_blank(), legend.position = 'bottom',
        legend.title = element_blank()); scatterplot_tarifa_pib_per_capita

# Saving
ggsave(plot = scatterplot_tarifa_pib_per_capita, width = 6, height = 6,
       filename = 'plots/agua/scatterplot-tarifa-agua-vs-pib-per-capita.png')

# Scatterplot: tarifa vs. PIB per capita - faceted  ---------------------------
scatterplot_tarifa_pib_per_capita_faceted <- 
  scatterplot_tarifa_pib_per_capita +
  facet_wrap(~ nat_jur_simplified, nrow = 3) +
  guides(size = FALSE, color = FALSE) ; scatterplot_tarifa_pib_per_capita_faceted

# Saving
ggsave(plot = scatterplot_tarifa_pib_per_capita_faceted, width = 5, height = 8,
       filename = 'plots/agua/scatterplot-tarifa-agua-vs-pib-per-capita-faceted.png')

# Scatterplot: atendimento vs PIB - single plot -------------------------------
scatterplot_atendimento_pib <- snis_agua %>% 
  filter(nat_jur_simplified != 'Sem dados') %>% 
  mutate(nat_jur_simplified = droplevels(nat_jur_simplified)) %>% 
  ggplot(aes(x = pib2017, y = in055_indice_de_atendimento_total_de_agua)) +
  geom_point(aes(size = pop, col = nat_jur_simplified),
             alpha = 0.4) +
  geom_smooth(method = 'lm', col = 'gray25',
              fill = 'lightgray', alpha = 0.5) +
  scale_x_log10(breaks = c(1e+5, 1e+6, 1e+7, 1e+8, 700000000),
                labels = c('100mi', '1bi', '10bi', '100bi', '700bi')) +
  coord_cartesian(ylim = c(20, 100)) +
  scale_color_manual(values = c('#fb6a4a', '#fed976', '#225ea8')) +
  labs(
    x = 'PIB (escala logarítmica)',
    y = 'Índice de atendimento',
    title = 'Relação entre índice de atendimento de água e PIB',
    subtitle = 'Fornecimento de água nos municípios paulistas',
    caption =
      'Fonte: dados de atendimento do SNIS (2018); dados de PIB do IBGE (2017).\nNota: o tamanho dos pontos é proporcional à população dos municípios.'
  ) +
  guides(size = FALSE) +
  theme(panel.grid = element_blank(), legend.title = element_blank(),
        legend.position = 'bottom') ; scatterplot_atendimento_pib

ggsave(plot = scatterplot_atendimento_pib, width = 6, height = 6,
       filename = 'plots/agua/scatterplot-atendimento-agua-vs-pib.png')

# Scatterplot: atendimento vs. PIB - faceted  -------------------------------------
scatterplot_atendimento_pib_faceted <- scatterplot_atendimento_pib +
  scale_y_continuous(breaks = seq(0, 100, by = 20)) +
  coord_cartesian() +
  facet_wrap(~ nat_jur_simplified, nrow = 3, scales = 'free_y') +
  guides(size = FALSE, color = FALSE) ; scatterplot_atendimento_pib_faceted

# Saving
ggsave(plot = scatterplot_atendimento_pib_faceted, width = 5, height = 8,
       filename = 'plots/agua/scatterplot-atendimento-agua-vs-pib-faceted.png')

# Scatterplot: atendimento vs PIB per capita - single plot --------------------
scatterplot_atendimento_pib_per_capita <- snis_agua %>% 
  filter(nat_jur_simplified != 'Sem dados') %>% 
  mutate(nat_jur_simplified = droplevels(nat_jur_simplified)) %>% 
  ggplot(aes(x = pib_per_capita2017, y = in055_indice_de_atendimento_total_de_agua)) +
  geom_point(aes(size = pop, col = nat_jur_simplified),
             alpha = 0.4) +
  geom_smooth(method = 'lm', col = 'gray25',
              fill = 'lightgray', alpha = 0.5) +
  scale_x_log10(breaks = c(10, 30, 100, 300),
                labels = c('10 mil', '30 mil', '100 mil', '300 mil')) +
  coord_cartesian(ylim = c(20, 100)) +
  scale_color_manual(values = c('#fb6a4a', '#fed976', '#225ea8')) +
  labs(
    x = 'PIB per capita em reais (escala logarítmica)',
    y = 'Índice de atendimento',
    title = 'Relação entre índice de atendimento de água e PIB per capita',
    subtitle = 'Fornecimento de água nos municípios paulistas',
    caption =
      'Fonte: dados de atendimento do SNIS (2018); dados de PIB e população do IBGE (2017).\nNota: o tamanho dos pontos é proporcional à população dos municípios.'
  ) +
  guides(size = FALSE) +
  theme(panel.grid = element_blank(), legend.title = element_blank(),
        legend.position = 'bottom') ; scatterplot_atendimento_pib_per_capita

ggsave(plot = scatterplot_atendimento_pib_per_capita, width = 6, height = 6,
       filename = 'plots/agua/scatterplot-atendimento-agua-vs-pib-per-capita.png')

# Scatterplot: atendimento vs. PIB per capita - faceted  -------------------------------------
scatterplot_atendimento_pib_per_capita_faceted <- scatterplot_atendimento_pib_per_capita +
  scale_y_continuous(breaks = seq(0, 100, by = 20)) +
  coord_cartesian() +
  facet_wrap(~ nat_jur_simplified, nrow = 3, scales = 'free_y') +
  guides(size = FALSE, color = FALSE) ; scatterplot_atendimento_pib_per_capita_faceted

# Saving
ggsave(plot = scatterplot_atendimento_pib_faceted, width = 5, height = 8,
       filename = 'plots/agua/scatterplot-atendimento-agua-vs-pib-per-capita-faceted.png')
