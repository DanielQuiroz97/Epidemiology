library(readxl)
library(tidyverse)
# Data retrieved from: https://geosalud.msp.gob.ec/geovisualizador/
salud <- read_xlsx(file.choose(), sheet = 1)
data <- salud %>% group_by(canton) %>% summarise(N = n()) %>% 
  mutate(County = ifelse(canton == 'ARCHIDONA', 'Archidona', 
                      ifelse(canton == 'TENA', 'Tena', 
                             ifelse(canton == 'QUIJOS', 'Quijos',
                                    ifelse(canton == "EL CHACO", "El Chaco",
                                           'C.J.A. Tola'))))) %>% 
  mutate(County = factor(County, levels = rev(c('C.J.A. Tola', 'Archidona',
                                      "El Chaco", 'Quijos',
                                      'Tena')) ))

ggplot(data, aes(County, N, fill = County)) +
  geom_bar(stat = "identity") + theme_bw() +
  coord_flip() + ylab("Number of healthcare units") +
  scale_fill_grey() +theme(legend.position = "none")

                            