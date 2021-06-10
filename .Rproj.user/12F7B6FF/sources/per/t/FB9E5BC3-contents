rm(list = ls())

if(!require('pacman')) install.packages('pacman')
pacman::p_load(flexdashboard, ggtext, janitor, tidyverse, extrafont, scales, patchwork, 'Cairo', htmlwidgets, tint, rgdal, rgeos, miniUI, units, lubridate, zoo, leaflet, htmltools, readxl, data.table, plotly)

# TEMA DEL GRÁFICO

temasinejes <-  theme(axis.line = element_blank(),
                      plot.margin = margin(10, 25, 10, 25),
                      plot.title = element_markdown(family = "Lato Black", size = 16),  
                      plot.subtitle = element_text(family = "Lato Light", size = 6, color = "black"), 
                      axis.text.x = element_text(family = "Lato", size =5, hjust=0.95,vjust=0.5),
                      axis.text.y = element_text(family = "Lato", size =5, hjust=0.95,vjust=0.5),
                      plot.background = element_rect(fill = "white", color = "black", size = 3),
                      axis.title.x = element_text(family = "Lato", size = 7, hjust=0.5),
                      axis.title.y = element_text(family = "Lato", size = 7, hjust=0.5), 
                      plot.caption = element_text(family = "Lato", size = 6, color = "gray50"),
                      legend.position = "none",  legend.justification="left", plot.title.position = 'plot', plot.caption.position = 'plot')


# ELECCIÓN A GOBERNADOR

casillas <- read_csv("data/SON_GOB_2021.csv", skip = 4,
                     col_types = cols(FECHA_HORA_CAPTURA = col_datetime(format = "%d/%m/%Y %H:%M")))

GOBLIMP <- casillas %>% 
  filter(TOTAL_PERSONAS_VOTARON!=0) %>%  
  group_by(DISTRITO_LOCAL, SECCION) %>% 
  summarise(TOTAL_VOTANTES=sum(as.numeric(TOTAL_PERSONAS_VOTARON)), 
            MORENAv=sum(as.numeric(CC_MORENA_PT_PVEM_NA)), ALIANZAv=sum(as.numeric(CC_PAN_PRI_PRD)), 
            LN=sum(as.numeric(LISTA_NOMINAL)), VOTOS=sum(as.numeric(TOTAL_VOTOS_CALCULADO))) 
GOBLIMP[is.na(GOBLIMP)] <- 0

GOBLIMP <- GOBLIMP %>% 
  mutate(PARTICIPACION=VOTOS*100/LN, MORENA=MORENAv*100/VOTOS,ALIANZA=ALIANZAv*100/VOTOS) %>% mutate(DIF=MORENA-ALIANZA) %>% mutate(CLASF=if_else(DIF>0,"MORENA","ALIANZA"))

ggplot(GOBLIMP) +
  geom_point(aes(x=PARTICIPACION, y=DIF, color=CLASF), alpha=0.2, size=1) +
  # geom_smooth(aes(x=PARTICIPACION, y=DIF), method="lm", se=FALSE, color="orange", linetype="dashed", size=0.5) +
  geom_hline(yintercept=0, color="black")+
  geom_text(aes(x = 85, y = 4,
                label = "Ventaja JUNTOS HAREMOS HISTORIA"), stat = "unique", family = "Lato Black",
            size = 2, color = "#73264D", hjust=0)+
  geom_text(aes(x = 85, y = 18,
                label = "1,136 secciones\n155,649 votos de diferencia\n25.7 pts de diferencia\n42% de participación"), stat = "unique", family = "Lato Black",
            size = 2, color = "#73264D", hjust=0)+
  geom_text(aes(x = 85, y = -4,
                label = "Ventaja VA X SONORA"), stat = "unique", family = "Lato Black",
            size = 2, color = "#0097CB", hjust=0)+
  geom_text(aes(x = 85, y = -18,
                label = "250 secciones\n37,037 votos de diferencia\n21.8 pts de diferencia\n54% de participación"), stat = "unique", family = "Lato Black",
            size = 2, color = "#0097CB", hjust=0)+
  geom_text(aes(x = 70, y = 70,
                label = "Cada punto es una sección electoral"), stat = "unique", family = "Lato Black",
            size = 2, color = "black", hjust=0)+
  geom_text(aes(x = 42.5, y = -70,
                label = "Participación estatal = 44.1%"), stat = "unique", family = "Lato Black",
            size = 1.5, color = "gray70", angle=90,hjust=0)+
  geom_vline(xintercept=44.0964, color="gray70", linetype="dashed", size=0.5)+
  geom_curve(aes(x = 68, y = 70, xend = 50, yend = 43),
             size = 0.5, color = "black",
             arrow = arrow(length = unit(0.02, "npc"))) +
  scale_color_manual(values=c("#73264D","#0097CB"), 
                     name="Coalición",
                     breaks=c("MORENA", "ALIANZA"),
                     labels=c("MORENA", "ALIANZA"))+
  guides(color = guide_legend(label.position = "bottom",
                              title.position = "left", title.vjust = 1)) +
  theme_minimal() + temasinejes + theme(legend.position="none") +
  labs(x="PARTICIPACIÓN CIUDADANA (%)", y="DIFERENCIA EN % DE VOTACIÓN (JHH - VXS)", 
       title  = "<span style = 'font-size:8pt'>Elección Sonora 2021:</span><br><span>Elección de Gobernador</span>", 
       subtitle= "Resultado de los principales contendientes para Gobernador\ny el porcentaje de participación en las secciones de Sonora", 
       caption ="Elaboración Luis Armando Moreno (@dogomoreno)\ncon información del PREP 2021 de IEE Sonora")

ggsave("Gráficos/GOB_SON.png", width = 5 , height = 5, type = "cairo", dpi = 300)

write_csv(GOBLIMP, "Resultado/GOBSECC.csv") # CSV con los datos procesados

# ELECCIÓN A GOBERNADOR EN HERMOSILLO

disthmo <- c(6,8,9,10,11,12) # Distritos locales de Hermosillo

ggplot(subset(GOBLIMP, DISTRITO_LOCAL %in% disthmo)) +
  geom_point(aes(x=PARTICIPACION, y=DIF, color=CLASF), alpha=0.2, size=1) +
  # geom_smooth(aes(x=PARTICIPACION, y=DIF), method="lm", se=FALSE, color="orange", linetype="dashed", size=0.5) +
  geom_hline(yintercept=0, color="black")+
  geom_text(aes(x = 90, y = 4,
                label = "Ventaja JUNTOS HAREMOS HISTORIA"), stat = "unique", family = "Lato Black",
            size = 2, color = "#73264D", hjust=0)+
  geom_text(aes(x = 90, y = 18,
                label = "286 secciones\n33,597 votos de diferencia\n22.4 pts de diferencia\n39% de participación"), stat = "unique", family = "Lato Black",
            size = 2, color = "#73264D", hjust=0)+
  geom_text(aes(x = 90, y = -4,
                label = "Ventaja VA X SONORA"), stat = "unique", family = "Lato Black",
            size = 2, color = "#0097CB", hjust=0)+
  geom_text(aes(x = 90, y = -18,
                label = "95 secciones\n22,475 votos de diferencia\n26.3 pts de diferencia\n61% de participación"), stat = "unique", family = "Lato Black",
            size = 2, color = "#0097CB", hjust=0)+
  geom_text(aes(x = 50, y = 70,
                label = "Cada punto es una sección electoral"), stat = "unique", family = "Lato Black",
            size = 2, color = "black", hjust=0)+
  geom_text(aes(x = 42.5, y = -70,
                label = "Participación estatal = 44.1%"), stat = "unique", family = "Lato Black",
            size = 1.5, color = "gray70", angle=90,hjust=0)+
  geom_vline(xintercept=44.0964, color="gray70", linetype="dashed", size=0.5)+
  geom_curve(aes(x = 48, y = 70, xend = 30, yend = 43),
             size = 0.5, color = "black",
             arrow = arrow(length = unit(0.02, "npc"))) +
  scale_color_manual(values=c("#73264D","#0097CB"), 
                     name="Coalición",
                     breaks=c("MORENA", "ALIANZA"),
                     labels=c("MORENA", "ALIANZA"))+
  guides(color = guide_legend(label.position = "bottom",
                              title.position = "left", title.vjust = 1)) +
  theme_minimal() + temasinejes + theme(legend.position="none") +
  labs(x="PARTICIPACIÓN CIUDADANA (%)", y="DIFERENCIA EN % DE VOTACIÓN (JHH - VxS)", 
       title  = "<span style = 'font-size:8pt'>Elección Sonora 2021:</span><br><span>Elección de Gobernador en Hermosillo</span>", 
       subtitle= "Resultado de los principales contendientes para Gobernador\ny el porcentaje de participación en las secciones de Hermosillo", 
       caption ="Elaboración Luis Armando Moreno (@dogomoreno)\ncon información del PREP 2021 de IEE Sonora")

ggsave("Gráficos/GOB_HMO.png", width = 5 , height = 5, type = "cairo", dpi = 300)

# ELECCIÓN A GOBERNADOR FUERA DE HERMOSILLO

ggplot(subset(GOBLIMP, !(DISTRITO_LOCAL %in% disthmo))) +
  geom_point(aes(x=PARTICIPACION, y=DIF, color=CLASF), alpha=0.2, size=1) +
  # geom_smooth(aes(x=PARTICIPACION, y=DIF), method="lm", se=FALSE, color="orange", linetype="dashed", size=0.5) +
  geom_hline(yintercept=0, color="black")+
  geom_text(aes(x = 80, y = 4,
                label = "Ventaja JUNTOS HAREMOS HISTORIA"), stat = "unique", family = "Lato Black",
            size = 2, color = "#73264D", hjust=0)+
  geom_text(aes(x = 80, y = 18,
                label = "850 secciones\n122,052 votos de diferencia\n26.8 pts de diferencia\n43% de participación"), stat = "unique", family = "Lato Black",
            size = 2, color = "#73264D", hjust=0)+
  geom_text(aes(x = 80, y = -4,
                label = "Ventaja VA X SONORA"), stat = "unique", family = "Lato Black",
            size = 2, color = "#0097CB", hjust=0)+
  geom_text(aes(x = 80, y = -18,
                label = "155 secciones\n14,562 votos de diferencia\n17.2 pts de diferencia\n50% de participación"), stat = "unique", family = "Lato Black",
            size = 2, color = "#0097CB", hjust=0)+
  geom_text(aes(x = 60, y = 70,
                label = "Cada punto es una sección electoral"), stat = "unique", family = "Lato Black",
            size = 2, color = "black", hjust=0)+
  geom_text(aes(x = 42.5, y = -70,
                label = "Participación estatal = 44.1%"), stat = "unique", family = "Lato Black",
            size = 1.5, color = "gray70", angle=90,hjust=0)+
  geom_vline(xintercept=44.0964, color="gray70", linetype="dashed", size=0.5)+
  geom_curve(aes(x = 58, y = 70, xend = 40, yend = 43),
             size = 0.5, color = "black",
             arrow = arrow(length = unit(0.02, "npc"))) +
  scale_color_manual(values=c("#73264D","#0097CB"), 
                     name="Coalición",
                     breaks=c("MORENA", "ALIANZA"),
                     labels=c("MORENA", "ALIANZA"))+
  guides(color = guide_legend(label.position = "bottom",
                              title.position = "left", title.vjust = 1)) +
  theme_minimal() + temasinejes + theme(legend.position="none") +
  labs(x="PARTICIPACIÓN CIUDADANA (%)", y="DIFERENCIA EN % DE VOTACIÓN (JHH - VxS)", 
       title  = "<span style = 'font-size:8pt'>Elección Sonora 2021:</span><br><span>Elección de Gobernador fuera de Hermosillo</span>", 
       subtitle= "Resultado de los principales contendientes para Gobernador\ny el porcentaje de participación en las secciones excluyendo Hermosillo", 
       caption ="Elaboración Luis Armando Moreno (@dogomoreno)\ncon información del PREP 2021 de IEE Sonora")

ggsave("Gráficos/GOB_NOHMO.png", width = 5 , height = 5, type = "cairo", dpi = 300)


# ELECCIÓN DE AYUNTAMIENTO EN HERMOSILLO

ayuntamientos <- read_csv("data/SON_AYUN_2021.csv", 
                          skip = 4)


HMO <- ayuntamientos %>% filter(MUNICIPIO=="HERMOSILLO")

HMOLIMP <- HMO %>% 
  filter(TOTAL_PERSONAS_VOTARON!=0) %>% 
  group_by(SECCION) %>% 
  summarise(TOTAL_VOTANTES=sum(as.numeric(TOTAL_PERSONAS_VOTARON)), 
            MORENAv=sum(as.numeric(MORENA)), ALIANZAv=sum(as.numeric(CC_PAN_PRI_PRD)), 
            LN=sum(as.numeric(LISTA_NOMINAL)), VOTOS=sum(as.numeric(TOTAL_VOTOS_CALCULADO))) %>% 
  mutate(PARTICIPACION=VOTOS*100/LN, MORENA=MORENAv*100/VOTOS,ALIANZA=ALIANZAv*100/VOTOS) %>% mutate(DIF=MORENA-ALIANZA) %>% mutate(CLASF=if_else(DIF>0,"MORENA","ALIANZA"))


ggplot(HMOLIMP) +
  geom_point(aes(x=PARTICIPACION, y=DIF, color=CLASF), alpha=0.5, size=1) +
  #geom_smooth(aes(x=PARTICIPACION, y=DIF), method="lm", se=FALSE, color="orange", linetype="dashed", size=0.5) +
  geom_hline(yintercept=0, color="black")+
  geom_text(aes(x = 80, y = 4,
                label = "Ventaja MORENA"), stat = "unique", family = "Lato Black",
            size = 3, color = "#73264D", hjust=0)+
  geom_text(aes(x = 80, y = 18,
                label = "258 secciones\n17,015 votos de diferencia\n12.9 pts de diferencia\n38% de participación"), stat = "unique", family = "Lato",
            size = 2, color = "#73264D", hjust=0)+
  geom_text(aes(x = 80, y = -4,
                label = "Ventaja VA X SONORA"), stat = "unique", family = "Lato Black",
            size = 3, color = "#0097CB", hjust=0)+
  geom_text(aes(x = 80, y = -18,
                label = "133 secciones\n23,351 votos de diferencia\n22.1 pts de diferencia\n58% de participación"), stat = "unique", family = "Lato",
            size = 2, color = "#0097CB", hjust=0)+
  geom_text(aes(x = 50, y = 70,
                label = "Cada punto es una sección electoral"), stat = "unique", family = "Lato Black",
            size = 2, color = "black", hjust=0)+
  geom_text(aes(x = 42.5, y = -70,
                label = "Participación estatal = 44.1%"), stat = "unique", family = "Lato Black",
            size = 1.5, color = "gray70", angle=90,hjust=0)+
  geom_vline(xintercept=44.0964, color="gray70", linetype="dashed", size=0.5)+
  geom_curve(aes(x = 48, y = 70, xend = 30, yend = 43),
             size = 0.5, color = "black",
             arrow = arrow(length = unit(0.02, "npc"))) +
  scale_color_manual(values=c("#73264D","#0097CB"), 
                     name="Coalición",
                     breaks=c("MORENA", "ALIANZA"),
                     labels=c("MORENA", "ALIANZA"))+
  guides(color = guide_legend(label.position = "bottom",
                              title.position = "left", title.vjust = 1)) +
  theme_minimal() + temasinejes + theme(legend.position="none") +
  labs(x="PARTICIPACIÓN CIUDADANA (%)", y="DIFERENCIA EN % DE VOTACIÓN (MORENA - VXS)", 
       title  = "<span style = 'font-size:8pt'>Elección Sonora 2021:</span><br><span>Elección de Ayuntamiento en Hermosillo</span>", 
       subtitle= "Resultado de los principales contendientes para Ayuntamiento\ny el porcentaje de participación en las secciones de Hermosillo", 
       caption ="Elaboración Luis Armando Moreno (@dogomoreno)\ncon información del PREP 2021 de IEE Sonora")

ggsave("Gráficos/AYUNT_HMO.png", width = 5 , height = 5, type = "cairo", dpi = 300) 

write_csv(HMOLIMP, "Resultado/HMOSECC.csv") # CSV con los datos procesados