# Load Needed libraries
library('tidyverse')
library('magrittr')
library('readxl')
library('ggalluvial')
library("RColorBrewer")

#### Create color palletes ####
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

#### Funcionts ####
# Modify default strsplit fuciont
strsplit2 <- function(chr, splt, rt) {
  rst <- strsplit(chr, splt)[[1]][rt]
  rst
}

readDB <- function(years){
  data <- read_xlsx('Data/ParasitologyDB.xlsx', sheet = as.character(years) )
  names(data) <- names(data) %>%  tolower
  data %<>% mutate(YEAR = years) 
  data
}


ExportPlot <- function(gplot, filename, width=2, height=1.5) {
  # Export plot in PDF and EPS.
  # Notice that A4: width=11.69, height=8.27 [in]
  ggsave(filename = paste0("Plots/",filename, '.pdf', sep=""),
         plot = gplot, width = width, height = height, units = 'in')
  postscript(file = paste0("Plots/", filename, '.eps', sep=""),
             width = width, height = height)
  print(gplot)
  dev.off()
  png(file = paste0("Plots/",filename, '.png'), width = width * 100, height = height * 100)
  print(gplot)
  dev.off()
}

#### Data Treatment ####
# Read the entire xlsx in the whole time
ParasitologyDB <- map(2013:2017, readDB) %>%  bind_rows()

# Read Unique Disease Data
uni_dis <- read_xlsx('Data/unique_diseases.xlsx') %>% 
  select(CODE, DIS_NAME, TROPISM, ZOONOTIC)

phylum <- read.csv('Data/phylum.csv', na.strings = '')

# Prepare Data for Select only data for deseases
cleanDB <- ParasitologyDB %>% 
  filter(residencia_provincia_paciente == '15| - NAPO') %>% # Select Napo dwellers
  mutate(edad_años_paciente = ifelse(is.na(edad_años_paciente), 
                                     edad_años_paciente...51,
                                     edad_años_paciente) ) %>% 
  select(SEX = sexo_paciente, 
         PROV = residencia_provincia_paciente,
         CAN = residencia_cantón_paciente, 
         DISEASE = código_cie_a, 
         YEAR,
         PATIENT_y = edad_años_paciente) %>% # Remove undesired characters
  transmute(SEX = map_chr(.$SEX, strsplit2,'-', 2) , 
            PROV = map_chr(.$PROV, strsplit2, '-', 2),
            CAN = map_chr(.$CAN, strsplit2, '-',  2),
            DISEASE = DISEASE,
            YEAR = YEAR,
            PATIENT_y = PATIENT_y) %>% 
  separate(col = DISEASE, into = c('CODE', 'DIS_NAME'), 
           sep = ' - ', remove = T) %>%
  mutate(CODE = substring(CODE, 1, 4),
         CAN = ifelse(CAN == ' CARLOS JULIO AROSEMENA TOLA', 
                      'C.J.A. Tola', CAN), # Abbreviate a county name
         DIS_NAME = ifelse(is.na(DIS_NAME), 'NA', DIS_NAME),
         PATIENT_y = ifelse(PATIENT_y < 18, '<18', # Categorize data
                            ifelse(PATIENT_y > 65, '>65',
                                   '>18|<45')) ) %>% 
  mutate(PATIENT_y = factor(PATIENT_y, # Transform age into categorical data
                            levels = c('>65','>18|<45', '<18'))) %>% 
  filter( !(CODE %in% c('B829', 'B89X', 'A085')) ) %>% 
          # SEX != ' INTERSEXUAL') # Remove not parasitic disease
  mutate(SEX = ifelse(SEX %in% " HOMBRE", "MEN",
                         ifelse(SEX %in% " MUJER", "WOMEN", "Unknown")))

cleanDB2 <- left_join(cleanDB, uni_dis, phylum,by = 'CODE') %>% 
  mutate(CODE = factor(CODE)) %>% 
  left_join(phylum, by = 'CODE') %>% 
  mutate(CODE = factor(CODE)) %>% 
  mutate(ZOONOTIC = factor(ZOONOTIC, levels = c("Sí", "No"),
                           labels =c('Yes', 'No')  ))


## Make Unique Diseases
#dis_unique <- cleanDB %>% distinct(CODE, DIS_NAME)



#### First Plot ####
first_plot <-  cleanDB %>% group_by(CAN, CODE, YEAR) %>% 
  summarise(N = n()) %>% group_by(YEAR, CAN) %>% 
  mutate(Cumulative = N/sum(N) ) %>% 
  arrange(YEAR) %>% 
  filter(Cumulative > 0.08) %>% ungroup() %>% 
  mutate(CAN = ifelse(CAN == ' ARCHIDONA', 'Archidona', 
                      ifelse(CAN == ' TENA', 'Tena', 
                             ifelse(CAN == ' QUIJOS', 'Quijos', 'C.J.A. Tola'))))

# Plot
Figure1 <- first_plot %>% ggplot(aes(CAN, Cumulative, fill = CODE)) +
  geom_bar(stat = 'identity', position = 'fill') +
  theme_bw() +
  facet_grid(.~YEAR, scales = 'free_y', 
             switch = "y", space = "free_y") +
  theme(axis.text.x  = element_text(angle = 35, hjust = 1)) +
  scale_fill_manual(values = col_vector[seq(15)]) +
  xlab('County') + ylab("Disease prevalence [%]") +
  labs(fill = "WHO\ncode")

ExportPlot(gplot = Figure1, filename = 'Figure2', width = 8, height = 5.5)

#### Second Plot ####
second_plot <- cleanDB %>% group_by(CAN, CODE, YEAR, PATIENT_y, SEX) %>% 
  summarise(N = n()) %>% group_by(YEAR, CAN, PATIENT_y, SEX) %>% 
  mutate(Cumulative = N/sum(N)) %>% 
  arrange(YEAR) %>% 
  filter(Cumulative > 0.35) 


Figure2 <- second_plot %>% as.data.frame %>% 
  ggplot(aes(weight = Cumulative, axis1 = CAN, axis3 = SEX, axis2 = PATIENT_y)) +
  geom_alluvium(aes(fill = CODE), width = 1/24, ribbon_bend = 1/5) +
  geom_stratum(width = 1/8, fill = "black", color = "grey") +
  geom_label(stat = "stratum", label.strata = TRUE, size = 2.5) +
  scale_x_continuous(breaks = 1:3, labels = c("County", "Age", "Sex")) +
  scale_fill_brewer(type = "qual", palette = "Paired") +
  facet_wrap('YEAR', scales = 'free', nrow = 3, ncol = 2) +
  labs(fill = "WHO\ncode") +
  theme_bw() + theme( axis.ticks.x=element_blank(), 
                      axis.ticks.y=element_blank(),
                      axis.text.y=element_blank()) 


ExportPlot(gplot = Figure2, filename = 'Figure3', width = 10, height = 5.5)

## For zoonotic plot
third_plot <- cleanDB2 %>% group_by(CAN, YEAR, ZOONOTIC)%>% 
  summarise(N = n()) %>% group_by(YEAR, CAN) %>%  mutate(PERCENT = N/sum(N)*100) %>% 
  arrange(YEAR) %>% 
  filter(!is.na(ZOONOTIC))

SFigure3 <- third_plot %>% ggplot(aes(CAN, PERCENT, fill = ZOONOTIC)) +
  geom_bar(stat = 'identity', position = 'fill') +
  theme_bw() +
  facet_grid(.~YEAR, scales = 'free_y', 
             switch = "y", space = "free_y") +
  theme(axis.text.x  = element_text(angle = 25, hjust = 1)) +
  scale_fill_manual(values = c('red3',  'royalblue3')) 

# For fourth plot
## For zoonotic plot
fourth_plot <- cleanDB2 %>% mutate(TROPISM = factor(TROPISM), 
                                   Phylum = factor(Filo)) %>% 
  dplyr::select(-Filo) %>% 
  group_by(CAN, CODE, YEAR, TROPISM, Phylum, ZOONOTIC) %>% 
  summarise(N = n()) %>% group_by(YEAR, CAN) %>% 
  mutate(CUMULATIVE = N/sum(N)) %>% 
  arrange(YEAR) %>% 
  filter(CUMULATIVE > 0.04) %>% 
  filter(!is.na(Phylum))

Figure3 <- fourth_plot %>% as.data.frame %>% 
  ggplot(aes(weight = CUMULATIVE,  axis1 = TROPISM, axis2 = Phylum,
             axis3 = ZOONOTIC)) +
  geom_alluvium(aes(fill = CODE), width = 1/24, ribbon_bend = 1/5) +
  geom_stratum(width = 1/8, fill = "black", color = "grey") +
  geom_label(stat = "stratum", label.strata = TRUE, size = 2.5) +
  scale_x_continuous(breaks = 1:3, labels = c("Tropism", "Phylum", 'Zoonotic')) +
  facet_wrap('YEAR', scales = 'free', nrow = 3, ncol = 2) +
  scale_fill_manual(values = col_vector[seq(13)]) +
  theme_bw() + theme( axis.ticks.x=element_blank(), 
                      axis.ticks.y=element_blank(),
                      axis.text.y=element_blank()) 

ExportPlot(gplot = Figure3, filename = 'Figure4', width = 8.5, height = 5.5)