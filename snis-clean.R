library(tidyverse)
library(cagedExplorer)

# Reading full dataset --------------------------------------------------------
snis <- readxl::read_excel('snis-sp-desagregado-2018.xlsx', range = 'A1:HY622')
colnames(snis) <- tolower(colnames(snis))

# Keeping relevant features only ----------------------------------------------
snis2 <- snis %>% 
  select(
    codigo_municipio, prestador, sigla_prestador, tipo_servico,
    natureza_juridica, starts_with('pop_'),
    starts_with('ag001_'), starts_with('ag026_'),
    starts_with('es001_'), starts_with('fn_017_'),
    # Investimentos prestador de serviços
    starts_with('fn023'), starts_with('fn024'),
    starts_with('fn025'), starts_with('fn033'),
    # Investimentos município
    starts_with('fn042'), starts_with('fn043'),
    starts_with('fn044'), starts_with('fn048'),
    # Investimentos estado
    starts_with('fn052'), starts_with('fn053'),
    starts_with('fn054'), starts_with('fn058'),
    # Índices
    starts_with('in003'), contains('_tarifa_media_'),
    starts_with('in008'), starts_with('in012'), starts_with('in013'),
    starts_with('in015'), starts_with('in016'), starts_with('in022'),
    starts_with('in024'), starts_with('in046'), starts_with('in047'),
    starts_with('in055'), starts_with('in056')
  )

# Adding city-level data from an auxiliary dataset ----------------------------
snis3 <- municipios_sp %>% # Dataset from 'cagedExplorer' package
  select(Codmun7, codigo, municipio, municipio_clean,
         regiao_administrativa, regiao_governo) %>% 
  left_join(snis2 %>% 
              mutate(codigo_municipio = as.integer(codigo_municipio)),
            by = c('codigo' = 'codigo_municipio')) %>% 
  select(
    Codmun7, codigo, municipio, municipio_clean, regiao_administrativa,
    regiao_governo, pop = pop_tot_populacao_total_do_municipio_do_ano_de_referencia,
    pop_urb = pop_urb_populacao_urbana_do_municipio_do_ano_de_referencia,
    prestador, sigla_prestador, tipo_servico, natureza_juridica,
    ag001_populacao_total_atendida_com_abastecimento_de_agua:in056_indice_de_atendimento_total_de_esgoto_referido_aos_municipios_atendidos_com_agua
  )

# New feature: simplified version of 'natureza_jurica' ------------------------
snis4 <- snis3 %>% 
  mutate(natureza_juridica = case_when(
    is.na(natureza_juridica) ~ 'Sem dados',
    str_detect(natureza_juridica,'economia') ~ 
      'Soc. de Econ. Mista com Adm. Pública',
    TRUE ~ natureza_juridica
  )) %>% 
  mutate(nat_jur_simplified = case_when(
    natureza_juridica %in% c('Administração pública direta',
                             'Autarquia',
                             'Empresa pública') ~ 'Administração pública',
    TRUE ~ natureza_juridica
  ) %>% fct_relevel('Soc. de Econ. Mista com Adm. Pública',
                    'Administração pública',
                    'Empresa privada',
                    'Sem dados'))

# Joining population and GDP data ---------------------------------------------
snis5 <- snis4 %>% 
  left_join(readxl::read_excel('pib-municipios-2017.xlsx') %>% 
              select(Codmun7 = codigo, pib2017 = pib) %>% 
              mutate(Codmun7 = as.integer(Codmun7)),
            by = 'Codmun7') %>% 
  left_join(readxl::read_excel('estimativa-populacao-municipios-2017.xlsx') %>%
              select(Codmun7, pop2017) %>% 
              mutate(Codmun7 = as.integer(Codmun7)),
            by = 'Codmun7') %>% 
  mutate(pib_per_capita2017 = pib2017 / pop2017) # In thousands of BRL

# Dealing with special cases: cities with two service providers ---------------
# Creating dataframe with only these special cases
snis_2prestadores <- snis5 %>% 
  filter(municipio_clean %in% c('MAUA', 'SALTO', 'SANTA MARIA DA SERRA')) %>%
  group_by(municipio_clean) %>% # Grouping by city
  mutate(
    # Total investment is the sum of values indicated for each provider
    fn033_investimentos_totais_realizados_pelo_prestador_de_servicos =
      sum(fn033_investimentos_totais_realizados_pelo_prestador_de_servicos,
          na.rm = TRUE),
    fn048_investimentos_totais_realizados_pelo_municipio =
      sum(fn048_investimentos_totais_realizados_pelo_municipio,
          na.rm = TRUE),
    fn058_investimentos_totais_realizados_pelo_estado =
      sum(fn058_investimentos_totais_realizados_pelo_estado,
          na.rm = TRUE),
    # Overall performance indicator as average of the providers' individual scores
    in012_indicador_de_desempenho_financeiro = 
      mean(in012_indicador_de_desempenho_financeiro)
  ) %>% 
  ungroup()

# Overall fees (water + sewage) should also be the average of the providers'
# fees, within each city. Let' do that manually to the cities of 'MAUA' and 'SALTO':
snis_2prestadores$in004_tarifa_media_praticada[snis_2prestadores$municipio_clean == 'MAUA'] <- 
  (snis_2prestadores$in005_tarifa_media_de_agua[snis_2prestadores$municipio_clean == 'MAUA' 
                                                & snis_2prestadores$tipo_servico == 'Água'] +
     snis_2prestadores$in006_tarifa_media_de_esgoto[snis_2prestadores$municipio_clean == 'MAUA' 
                                                    & snis_2prestadores$tipo_servico == 'Esgotos']) / 2

snis_2prestadores$in004_tarifa_media_praticada[snis_2prestadores$municipio_clean == 'SALTO'] <- 
  (snis_2prestadores$in005_tarifa_media_de_agua[snis_2prestadores$municipio_clean == 'SALTO' 
                                                & snis_2prestadores$tipo_servico == 'Água'] +
     snis_2prestadores$in006_tarifa_media_de_esgoto[snis_2prestadores$municipio_clean == 'SALTO' 
                                                    & snis_2prestadores$tipo_servico == 'Esgotos']) / 2

# For 'SANTA MARIA DA SERRA', we set overall fees equal to water fees, since we have no data on sewage fees.
snis_2prestadores$in004_tarifa_media_praticada[snis_2prestadores$municipio_clean == 'SANTA MARIA DA SERRA'] <- 
  snis_2prestadores$in005_tarifa_media_de_agua[snis_2prestadores$municipio_clean == 'SANTA MARIA DA SERRA' 
                                               & snis_2prestadores$tipo_servico == 'Água']

# Replacing lines in the full dataframe with updated values
snis6 <- snis5 %>% 
  filter(!municipio_clean %in% c('MAUA', 'SALTO', 'SANTA MARIA DA SERRA')) %>% 
  bind_rows(snis_2prestadores)

# Per capita investment -------------------------------------------------------
snis_inv <- snis6 %>%
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
  mutate(inv_per_capita = inv_total / pop)

# Adding per capita investment feature
snis7 <- snis6 %>%
  left_join(snis_inv %>% select(municipio_clean, tipo_servico, inv_per_capita),
            by = c('municipio_clean', 'tipo_servico'))

# Saving ----------------------------------------------------------------------
saveRDS(snis7, 'snis-2018-clean.rds')