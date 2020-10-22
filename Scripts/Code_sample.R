
# Preambulo  --------------------------------------------------------------

rm(list = ls())
# Paquetes a usar
pkg <- c('readxl','DBI','dplyr','tidyr','ggplot2','plotly','xlsx')
new.packages <- pkg[!(pkg %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(pkg, require, character.only = TRUE)

# Key con razón social  ----------------------------------------------

key1 <- 
  read_excel('completo_sin_RS.xlsx') %>% select(identificacion,NIT)

key1 <- 
  read.csv('infoRazonSocial.csv', sep = ';', stringsAsFactors = FALSE, fileEncoding = 'latin1') %>% 
  select(identificacionCifrada,identificacion,RazonSocial) %>%
  rename(NIT = identificacion, identificacion = identificacionCifrada) %>% select(-identificacion) %>%
  right_join(key1, by = 'NIT') 

key2 <- 
  read.csv('razonessociales(descifrado).csv', sep = ';', stringsAsFactors = FALSE, fileEncoding = 'latin1') %>% 
  rename(NIT = identificacion, identificacion = identificacionCifrada) #%>% View()


key <- 
  rbind(key1,key2) %>% mutate(NIT = as.numeric(ifelse(nchar(NIT) == 10, substr(NIT,1,9),NIT))) %>% 
  mutate(largo = nchar(RazonSocial)) %>%
  arrange(-largo) %>% 
  group_by(NIT) %>% mutate(RazonSocial = first(RazonSocial)) %>% ungroup() %>% 
  group_by(identificacion) %>%
  summarise_all(list(first)) %>% select(-largo) 

# Jurídicos descubiertos --------------------------------------------------

juridicos.enero <- read.csv('juridicos_enero.csv', stringsAsFactors = FALSE) %>% 
  group_by(identificacion,Entidad,tipoProducto) %>%
  summarise(saldo.ene = sum(saldo,na.rm = TRUE), intereses.ene = sum(intereses, na.rm = TRUE))

juridicos.abril <- read.csv('juridicos_abril.csv', stringsAsFactors = FALSE) %>% 
  group_by(identificacion,Entidad,tipoProducto) %>%
  summarise(saldo.abr = sum(saldo,na.rm = TRUE), intereses.abr = sum(intereses, na.rm = TRUE))

juridicos <- 
  full_join(juridicos.enero, juridicos.abril, by = c('identificacion','Entidad','tipoProducto')) 

n_distinct(juridicos$identificacion)

# Base SIREM 2018 -------------------------------------------------------------------

setwd('InfoFinanciera_2018_1000')
files <- dir()
Caratula <- ESF <- ERI <- c()
system.time(
  for (file in files) {
    for (sheet in c('Caratula','ERI','ESF')) {
      
      if (sheet == 'Caratula') {
        aux <- read_excel(file, sheet = sheet) %>% 
          dplyr::select(
            Nit,`Razón social de la sociedad`,`Clasificación Industrial Internacional Uniforme Versión 4 A.C`,
            `Fecha Corte`
          )
        Caratula <- c(Caratula, list(aux))
      } else if (sheet == 'ERI') {
        aux <- read_excel(file, sheet = sheet) %>% dplyr::select(Nit,`Ganancia (pérdida)`,Periodo,`Fecha Corte`)
        ERI <- c(ERI, list(aux))
      } else if (sheet == 'ESF') {
        aux <- read_excel(file, sheet = sheet) %>% dplyr::select(Nit,`Total de activos`,Periodo,`Fecha Corte`)
        ESF <- c(ESF, list(aux))
      }
      
    }
  }
)


# PIE ---------------------------------------------------------------------

pie <- read_excel('RPT_ConsultaGeneralSociedades.xls') %>% #[,c(1,7,10,14,28,33:53)] %>% 
  select(NIT,`Razón Social`,CIIU) %>% 
  mutate(CIIU = substr(CIIU,2,5)) %>% 
  distinct(NIT, .keep_all = TRUE) #,`Actividad CIIU`


# SFC ---------------------------------------------------------------------

sfc <- read_excel('entidades_general.xlsx') %>% 
  select(NIT,`Denominación social de la Entidad`,Tipo) %>%
  distinct(NIT, .keep_all = TRUE) %>% mutate(NIT = as.numeric(substr(NIT,1,9)), Tipo = as.numeric(Tipo)) %>%
  rename(`Razón Social` = `Denominación social de la Entidad`) %>%
  distinct(NIT, .keep_all = TRUE) %>% filter(!is.na(NIT))

sfc.key <- read_excel('CIIU - código SFC.xlsx') %>% select('Tipo','CIIU')

sfc <- sfc %>% left_join(sfc.key, by = 'Tipo') %>% select(-Tipo)


# Cruce para encontrar CIIUs -------------------------------------------------------------------

base <- rbind(
  inner_join(key[!is.na(key$CIIU),] %>% select(-CIIU), 
             sfc %>% select(-`Razón Social`), 
             by = 'NIT'),
  inner_join(key[!is.na(key$CIIU),] %>% select(-CIIU), 
             pie %>% select(-`Razón Social`),  
             by = 'NIT'), 
  inner_join(key[!is.na(key$CIIU),] %>% select(-CIIU), 
             sirem %>% select(NIT,CIIU) %>% filter(!NIT %in% unique(pie$NIT)), 
             by = 'NIT')
)

nrow(base) # 16231 adicionales
nrow(base)/nrow(key) # 10.7pp adicional

# Arreglo del key ----------------------------------------------------------

# Información por sectores
key2join <- key2join %>% 
  mutate(
    CIIU = ifelse(nchar(CIIU) == 3,paste0(CIIU,'0'),CIIU),
    seccion = case_when(
      as.numeric(substr(CIIU,1,2)) >= 1 & as.numeric(substr(CIIU,1,2)) <= 3 ~ 'A',
      as.numeric(substr(CIIU,1,2)) >= 5 & as.numeric(substr(CIIU,1,2)) <= 9 ~ 'B',
      as.numeric(substr(CIIU,1,2)) >= 10 & as.numeric(substr(CIIU,1,2)) <= 33 ~ 'C',
      as.numeric(substr(CIIU,1,2)) == 35 ~ 'D',
      as.numeric(substr(CIIU,1,2)) >= 36 & as.numeric(substr(CIIU,1,2)) <= 39 ~ 'E',
      as.numeric(substr(CIIU,1,2)) >= 41 & as.numeric(substr(CIIU,1,2)) <= 43 ~ 'F',
      as.numeric(substr(CIIU,1,2)) >= 45 & as.numeric(substr(CIIU,1,2)) <= 47 ~ 'G',
      as.numeric(substr(CIIU,1,2)) >= 49 & as.numeric(substr(CIIU,1,2)) <= 53 ~ 'H',
      as.numeric(substr(CIIU,1,2)) >= 55 & as.numeric(substr(CIIU,1,2)) <= 56 ~ 'I',
      as.numeric(substr(CIIU,1,2)) >= 58 & as.numeric(substr(CIIU,1,2)) <= 63 ~ 'J',
      as.numeric(substr(CIIU,1,2)) >= 64 & as.numeric(substr(CIIU,1,2)) <= 66 ~ 'K',
      as.numeric(substr(CIIU,1,2)) == 68 ~ 'L',
      as.numeric(substr(CIIU,1,2)) >= 69 & as.numeric(substr(CIIU,1,2)) <= 75 ~ 'M',
      as.numeric(substr(CIIU,1,2)) >= 77 & as.numeric(substr(CIIU,1,2)) <= 82 ~ 'N',
      as.numeric(substr(CIIU,1,2)) == 84 ~ 'O',
      as.numeric(substr(CIIU,1,2)) == 85 ~ 'P',
      as.numeric(substr(CIIU,1,2)) >= 86 & as.numeric(substr(CIIU,1,2)) <= 88 ~ 'Q',
      as.numeric(substr(CIIU,1,2)) >= 90 & as.numeric(substr(CIIU,1,2)) <= 93 ~ 'R',
      as.numeric(substr(CIIU,1,2)) >= 94 & as.numeric(substr(CIIU,1,2)) <= 96 ~ 'S',
      as.numeric(substr(CIIU,1,2)) >= 97 & as.numeric(substr(CIIU,1,2)) <= 98 ~ 'T',
      as.numeric(substr(CIIU,1,2)) == 99 ~ 'U'
    )
  )



# Pegue de las bases ------------------------------------------------------

join <- 
  full_join(juridicos, key2join, by = 'identificacion') 
# distinct(NIT,Entidad,saldo.ene,saldo.abr, .keep_all = TRUE)

join <- join %>% mutate(saldo.ene = saldo.ene + intereses.ene, saldo.abr = saldo.abr + intereses.abr) %>%
  select(-intereses.ene,-intereses.abr)

# Saldos de NITs con CIIU para enero y abril
sum(join[!is.na(join$CIIU),]$saldo.ene, na.rm = TRUE) / sum(join$saldo.ene, na.rm = TRUE) # 88.2%
sum(join[!is.na(join$CIIU),]$saldo.abr, na.rm = TRUE) / sum(join$saldo.abr, na.rm = TRUE) # 86.5%

# NITs sin asignar CIIUs
join %>% filter(is.na(CIIU)) %>% group_by(NIT) %>%
  summarise(
    saldo.ene = sum(saldo.ene, na.rm = TRUE),
    saldo.abr = sum(saldo.abr, na.rm = TRUE),
    RazonSocial = first(RazonSocial)) %>% 
  arrange(-saldo.abr) %>%
  View()

nits <- join %>% filter(is.na(RazonSocial)) %>% ungroup() %>% distinct(NIT) %>% unlist()
key1 %>% filter(NIT %in% nits) %>% View() # Son los que no tienen razón social en key1
join %>% filter(is.na(RazonSocial)) %>% ungroup() %>% #View()
  summarise(saldo.ene = sum(saldo.ene, na.rm = TRUE), saldo.abr = sum(saldo.abr, na.rm = TRUE))

# join <- join %>% ungroup() %>% filter(!is.na(RazonSocial))
join <- join %>% filter(identificacion != 'ION S A S') %>% ungroup()

# Base a nivel de nit
join.nit <- join %>% group_by(NIT) %>% 
  summarise(
    RazonSocial = first(RazonSocial),
    saldo.ene = sum(saldo.ene,na.rm = TRUE), 
    saldo.abr = sum(saldo.abr,na.rm = TRUE),
    CIIU = first(CIIU),
    seccion = first(seccion)
  ) %>% ungroup()






# << ANÁLISIS >> ----------------------------------------------------------------



# Descripción de la base (join) ----------------------------------------------------------------

# Número total de jurídicos descubiertos
n_distinct(join$NIT) # 130106 nits

# Personas por sector
ggplot(join.nit %>% na.omit(), aes(x = seccion)) + 
  geom_bar() + 
  geom_text(stat = 'count', aes(label=..count..), vjust = -1, size = 7) + 
  theme_minimal() + 
  labs(y = '', x = '') + 
  theme(
    axis.text = element_text(size = 20), 
    legend.text = element_text(size = 20),
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) + ylim(0,10000)

sum(is.na(join.nit$seccion))

# Jurídicos en enero
join %>% filter(!is.na(saldo.ene)) %>% select(NIT) %>% n_distinct() # 129180 nits 

# Jurídicos en abril
join %>% filter(!is.na(saldo.abr)) %>% select(NIT) %>% n_distinct() # 129936 nits 

# Juridicos-entidad que entraron
# join.nit %>%  
#   filter(saldo.ene == 0) %>% #summarise(saldo = sum(saldo.abr,na.rm = TRUE)) 
#   View() # 1633 nits entraron a ser descubiertos con 1.11b 

# Jurídicos-entidad que salieron
# join.nit %>%
#   filter(saldo.abr == 0) %>% #summarise(saldo = sum(saldo.ene,na.rm = TRUE)) 
#   View() # 376 nits sacaron 136mm

# Crecimientos preexitencias
neto <- join.nit %>% ungroup() %>% mutate(crecimiento = saldo.abr - saldo.ene, entra = crecimiento > 0) 

neto %>% group_by(entra) %>% summarise(crecimiento = sum(crecimiento, na.rm = TRUE))

# Flujos de saldos totales
join %>% ungroup() %>% 
  summarise(saldo.ene = sum(saldo.ene, na.rm = TRUE)/1e12, saldo.abr = sum(saldo.abr, na.rm = TRUE)/1e12) %>%
  mutate(crec.neto = saldo.abr - saldo.ene)


# Distribución del crecimiento de saldos
join.nit %>% 
  filter(floor(saldo.ene) != 0) %>% 
  mutate(crecimiento = saldo.abr - saldo.ene) %>% 
  select(crecimiento) %>% unlist() %>% #summary() 
  View()


# Cuántos (NIT,ent) aumentaron y redujeron saldos (scatter de saldos)
join.nit %>% ggplot(aes(x = saldo.ene, y = saldo.abr)) + geom_point(alpha = .1) +
  geom_abline(slope = 1, color = 'red') + scale_y_log10() + scale_x_log10()

# NITs sin CIIU
# export <- join %>% mutate(crecimiento = saldo.abr-saldo.ene) %>% filter(is.na(CIIU)) %>% arrange(-crecimiento)
# export %>% filter(crecimiento %in% boxplot.stats(export$crecimiento)$out) %>% View()
#   # write.csv(export,'juridicos_NA.csv',row.names = FALSE)
# export %>% select(NIT) %>% n_distinct() # 72879 (56%) nits sin ciiu de 130106 nits en total
# 
# corpus(paste(export$RazonSocial, collapse = ' ')) %>% 
#   tokens(remove_numbers = TRUE,  remove_punct = TRUE) %>% 
#   dfm(remove = c(stopwords("spanish"), letters), stem = TRUE) %>% t() %>% convert(to = 'data.frame') %>% 
#   arrange(-text1) %>% View()
# 
# export %>% group_by(NIT) %>% 
#   summarise(crecimiento = sum(crecimiento, na.rm = TRUE), RazonSocial = first(RazonSocial)) %>% 
#   arrange(-crecimiento) %>% View()


# Análisis por tipo de producto ---------------------------------

join %>% mutate(
  crecimiento = saldo.abr - saldo.ene,
  producto = case_when(
    tipoProducto == 0 ~ 'Cuenta Corriente',
    tipoProducto == 2 ~ 'CDT',
    tipoProducto == 3 ~ 'Cuenta de Ahorro',
    TRUE ~ 'Otros'
  )
) %>% group_by(producto) %>%
  summarise(
    saldo.abr = sum(saldo.abr, na.rm = TRUE)/1e12, 
    saldo.ene = sum(saldo.ene, na.rm = TRUE)/1e12, 
    crecimiento = sum(crecimiento, na.rm = TRUE)/1e12) %>%
  pivot_longer(cols = c('saldo.ene','saldo.abr'),names_prefix = 'saldo.', names_to = 'Mes',values_to = 'Saldo') %>%
  ggplot(aes(x = producto, y = Saldo, fill = reorder(Mes,desc(Mes)))) + 
  geom_col(position = 'dodge') + 
  labs(fill = '' , y = '', x = '') + 
  scale_fill_brewer(labels = c('Enero','Abril'), palette = "Accent") + 
  theme_minimal() + 
  geom_text(aes(label = paste0(round(Saldo,1),'b')), vjust = -1, position = position_dodge(width = 1), size = 7) +
  theme(
    axis.text = element_text(size = 20), 
    legend.text = element_text(size = 20),
    axis.text.y = element_blank(),
    legend.position = 'bottom', 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) + ylim(0,165)

# Saldos por producto y por grupo 
join %>% mutate(
  crecimiento = saldo.abr - saldo.ene,
  producto = case_when(
    tipoProducto == 0 ~ 'Cuenta Corriente',
    tipoProducto == 2 ~ 'CDT',
    tipoProducto == 3 ~ 'Cuenta de Ahorro',
    TRUE ~ 'Otros'
  ),
  grupo = 
    case_when(
      is.na(seccion) ~ 'G0',
      seccion %in% c('S','P','F','I') ~ 'G1',
      seccion %in% c('M', 'H', 'G', 'L', 'C', 'T', 'Q', 'R', 'J', 'N', 'E', 'A') ~ 'G2',
      seccion %in% c('K','B','U','O','D') ~ 'G3'
    )
) %>% group_by(producto, grupo) %>%
  summarise(
    saldo.abr = sum(saldo.abr, na.rm = TRUE)/1e12, 
    saldo.ene = sum(saldo.ene, na.rm = TRUE)/1e12, 
    crecimiento = sum(crecimiento, na.rm = TRUE)/1e12) %>%
  pivot_longer(cols = c('saldo.ene','saldo.abr'),names_prefix = 'saldo.', names_to = 'Mes',values_to = 'Saldo') %>%
  # filter(grupo == 1) %>%
  #  Gráfica
  ggplot(aes(x = producto, y = Saldo, fill = reorder(Mes,desc(Mes)))) + 
  geom_col(position = 'dodge') + 
  facet_grid(grupo~.) +
  labs(fill = '' , y = '', x = '') + 
  scale_fill_brewer(labels = c('Enero','Abril'), palette = "Accent") + 
  theme_minimal() + 
  geom_text(aes(label = paste0(round(Saldo,1),'b')), vjust = -1, position = position_dodge(width = 1), size = 7) +
  theme(
    axis.text = element_text(size = 20), 
    legend.text = element_text(size = 20),
    axis.text.y = element_blank(),
    strip.text.y = element_text(size = 20, angle = 0),
    legend.position = 'bottom', 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) + ylim(0,165)




# Sectores ----------------------------------------------------------------

# Flujos de saldos por sector
neto %>% 
  filter(!is.na(seccion)) %>% 
  group_by(seccion, entra) %>% 
  summarise(crecimiento = sum(crecimiento,na.rm = TRUE)) %>% #View()
  group_by(entra) %>%
  mutate(p.crecimiento = crecimiento/sum(crecimiento)) %>%
  ggplot(aes(x = seccion, y = abs(crecimiento)/1e12, fill = entra)) + 
  geom_bar(stat = 'identity', position = 'dodge') + theme_minimal() + 
  scale_fill_discrete(labels = c('Salidas','Ingresos')) + 
  labs(y = 'Saldos (billones)', x = '', fill = '') + 
  theme(
    axis.text = element_text(size = 20), 
    legend.text = element_text(size = 20),
    legend.position = 'bottom'  
  )


# Grupos de secciones ---------------------------------------------------------------

# grupo 1 (no tan perjudicados):  S, NA, P, F, I = más de 40% (3er cuartil) de los nits con menos de 100M
# grupo 2 (medio jodidos): M, H, G, L, C, T, Q, R, J, N, E, A = mas de 50% de los nits con menos de 1MM
# grupo 3 (jodidos): K, B, U, O, D = más de 10% de los nits con al menos 10MM


# Concentración hasta 100M
join.nit %>% group_by(seccion) %>%
  mutate(N = n()) %>%
  filter(saldo.abr < 100e6) %>% summarise(x = n()/mean(N)) -> con1

# Concentración hasta 1MM
join.nit %>% group_by(seccion) %>%
  mutate(N = n()) %>%
  filter(saldo.abr < 1e9) %>% summarise(x = n()/mean(N)) -> con2

# Concentración hasta 10MM
join.nit %>% group_by(seccion) %>%
  mutate(N = n()) %>%
  filter(saldo.abr < 10e9) %>% summarise(x = n()/mean(N)) -> con3

# Asignación de grupos
join.nit <- join.nit %>% mutate( grupo = 
                                   case_when(
                                     is.na(seccion) ~ 0,
                                     seccion %in% c('S','P','F','I') ~ 1,
                                     seccion %in% c('M', 'H', 'G', 'L', 'C', 'T', 'Q', 'R', 'J', 'N', 'E', 'A') ~ 2,
                                     seccion %in% c('K','B','U','O','D') ~ 3
                                   )
)

join.nit %>% nrow()
join.nit %>% ungroup() %>% group_by(grupo) %>% 
  summarise(
    N = n(), 
    saldo.abr = sum(saldo.abr, na.rm = TRUE)/1e12,
    saldo.ene = sum(saldo.ene, na.rm = TRUE)/1e12
  )

join.nit %>% group_by(grupo) %>% 
  filter(saldo.abr > 50e6 & saldo.abr <= 100e6) %>% summarise(N = n())

join.nit %>% group_by(seccion) %>%
  filter(saldo.abr > 50e6 & saldo.abr <= 100e6) %>% summarise(N = n())


# Saldos y personas -------------------------------------------------------

join.seccion <- join.nit %>%
  mutate(cortes = cut(saldo.abr, 
                      breaks = c(0,50e6,100e6,200e6,500e6,1e9,2e9,5e9,10e9,100e9,1e12,Inf), 
                      labels = c('cubierto',
                                 '[50m,100m)','[100m,200m)','[200m,500m)','[500m,1mm)','[1mm,2mm)','[2mm,5mm)',
                                 '[5mm,10mm)','[10mm,100mm)','[100mm,1b)','>1b'),
                      right = FALSE)
  ) 


# Saldos y personas TOTAL
join.seccion %>%
  group_by(cortes) %>% 
  summarise(saldo = sum(saldo.abr, na.rm = TRUE)/1e12, N = n()) %>% filter(cortes != 'cubierto') %>%
  pivot_longer(cols = c('N','saldo'), names_to = 'variable', values_to = 'Valor') %>%
  # Gráfica
  ggplot(aes(x = cortes, y = Valor)) +
  geom_col() + 
  facet_grid(
    variable ~ ., scales = "free", 
    labeller = labeller(variable =  c(N = "Depositantes", saldo = "Saldos (billones)"))
  ) + 
  labs(y = '', x = 'Saldos') + 
  theme_minimal() + 
  theme(
    axis.text.y = element_text(size = 15), 
    axis.text.x = element_text(size = 12),
    strip.text.y = element_text(size = 15)
  )



# Saldos y personas por grupos
join.seccion %>% #View()
  group_by(cortes, grupo) %>% 
  summarise(saldo = sum(saldo.abr, na.rm = TRUE)/1e12, N = n()/1e3) %>% filter(cortes != 'cubierto') %>%
  pivot_longer(cols = c('N','saldo'), names_to = 'variable', values_to = 'Valor') %>% group_by(variable) %>%
  mutate(Valor.p = Valor/sum(Valor)) -> aux 
#   Gráfica
ggplot(aux, aes(x = cortes, y = Valor, fill = as.factor(grupo))) + 
  geom_col() + 
  facet_grid(
    variable ~ ., scales = "free", 
    labeller = labeller(variable =  c(N = "Depositantes (miles)", saldo = "Saldos (billones)"))
  ) + 
  labs(y = '', x = 'Saldos', fill = 'Grupo') + 
  theme_minimal() + 
  theme(
    axis.text.y = element_text(size = 15), 
    axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 15), 
    legend.title = element_text(size = 15), 
    strip.text.y = element_text(size = 15)
  ) + 
  geom_text(aes(cortes, Valor, label = paste0(round(Valor.p*100,1),'%'), fill = NULL, vjust = -1), 
            data = aux %>% ungroup() %>% group_by(variable,cortes) %>% summarise_all(list(sum))) +
  geom_blank(data = aux %>% ungroup() %>% mutate(Valor = ifelse(variable == 'N', Valor + 20, Valor + 50)), 
             aes(x = cortes, y = Valor)) 


#  Saldos y personas por secciones
etiquetas <- list(
  c('Construcción','Alojamiento','Educación','Otros serv.'),
  c('Agropecuario','Manufactura','Acueducto','Comercio','Transporte','Comunicaciones','Inmobiliario','Ciencia',
    'Administrativo','Salud','Arte/entretenimiento','Hogar'),
  c('Minas','Servicios dom.','Financiero','Público','Organizaciones')
)
grupo.filtro <- 1
join.seccion %>% filter(grupo == grupo.filtro) %>%
  group_by(cortes, seccion) %>% 
  summarise(saldo = sum(saldo.abr, na.rm = TRUE)/1e12, N = n()/1e3) %>% filter(cortes != 'cubierto') %>%
  pivot_longer(cols = c('N','saldo'), names_to = 'variable', values_to = 'Valor') %>% group_by(variable) %>%
  mutate(Valor.p = Valor/sum(Valor)) -> aux
#  Gráfica
ggplot(aux, aes(x = cortes, y = Valor, fill = as.factor(seccion))) + 
  geom_col() + 
  facet_grid(
    variable ~ ., scales = "free", 
    labeller = labeller(variable =  c(N = "Depositantes (miles)", saldo = "Saldos (billones)"))
  ) + 
  labs(y = '', x = 'Saldos', fill = 'Grupo') + 
  theme_minimal() + 
  theme(
    axis.text.y = element_text(size = 15), 
    axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 15), 
    legend.title = element_text(size = 15), 
    strip.text.y = element_text(size = 15)
  ) + 
  geom_text(aes(cortes, Valor, label = paste0(round(Valor.p*100,1),'%'), fill = NULL, vjust = -1), 
            data = aux %>% ungroup() %>% group_by(variable,cortes) %>% select(-seccion) %>% summarise_all(list(sum))) +
  geom_blank(data = aux %>% group_by(variable,cortes) %>% mutate(Valor = max(sum(Valor))*1.2), 
             aes(x = cortes, y = Valor)) +
  scale_fill_discrete(labels = etiquetas[[grupo.filtro]])


# Preguntas y respuestas --------------------------------------------------

# Rotulos
neto %>% group_by(grupo,entra) %>% summarise(sum(crecimiento))

# D23: Cuántos depositantes hay en la barra de saldos mayores a 1b? 
neto %>% filter(abs(crecimiento) > 1e12) %>% View()
# D28: Cuántos depositantes hay en la barra de saldos mayores a 1b? Quiénes son (razón social)?
join.nit %>% filter(saldo.abr > 1e12) %>% View()

# D30: Quién en sector construcción aumentó saldos en el rango 10mm – 100mm
neto %>% filter(saldo.abr > 10e9 & saldo.abr < 100e9 & seccion == 'F') %>% View()

# D32: Quién en manufactura hizo movimientos entre 10 mm y 100 mm
neto %>% filter(saldo.abr > 10e9 & saldo.abr < 100e9 & seccion == 'C') %>% View()
neto %>% filter(saldo.abr > 10e9 & saldo.abr < 100e9 & seccion == 'G') %>% View()
