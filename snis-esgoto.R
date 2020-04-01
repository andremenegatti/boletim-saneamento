library(tidyverse)
library(tmap)
library(cagedExplorer)

# Loading and filtering dataset -----------------------------------------------
snis <- readRDS('snis-2018-clean.rds')

snis_esgoto <- snis %>% 
  filter(
    !(municipio_clean %in% c('MAUA', 'SALTO', 'SANTA MARIA DA SERRA') &
        tipo_servico == 'Água')
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
mapa_esgoto_nat_jur <- snis_esgoto %>% 
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
  tm_layout(main.title = 'Natureza jurídica do prestador de serviços - Esgoto') +
  custom_map_settings ; mapa_agua_nat_jur

# Saving
tmap_save(mapa_esgoto_nat_jur, height = 6, width = 6,
          filename = 'mapa_natureza_juridica_esgoto.png')
