library(tidyverse)
library(cagedExplorer)

snis <- readxl::read_excel('snis-sp-desagregado-2018.xlsx', range = 'A1:HY622')
colnames(snis) <- tolower(colnames(snis))

snis2 <- snis %>% 
  select(
    codigo_municipio, prestador, sigla_prestador, tipo_servico,
    natureza_juridica, starts_with('pop_'),
    starts_with('ag001_'), starts_with('ag026_'),
    starts_with('es001_'), starts_with('fn_017_'),
    # Investimentos prestador de serviços
    starts_with('fn023'), starts_with('fn024'), starts_with('fn025'), starts_with('fn033'),
    # Investimentos município
    starts_with('fn042'), starts_with('fn043'), starts_with('fn044'), starts_with('fn048'),
    # Investimentos estado
    starts_with('fn052'), starts_with('fn053'), starts_with('fn054'), starts_with('fn058'),
    # Índices
    starts_with('in003'), contains('_tarifa_media_'),
    starts_with('in008'), starts_with('in012'),
    starts_with('in015'), starts_with('in016'),
    starts_with('in022'), starts_with('in024'),
    starts_with('in046'), starts_with('in047'),
    starts_with('in055'), starts_with('in056')
  )

snis3 <- municipios_sp %>% 
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

saveRDS(snis4, 'snis-2018-clean.rds')