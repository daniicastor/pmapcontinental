###################################
#
# POSTER DADOS PMAP MG - ES
#
#
##################################

# Carregar pacotes

library(tidyverse)
library(sf)
library(ggplot2)
library(patchwork)  # para o painel
library(scales)
library(viridis)
library(readr)
library(janitor)
library(ggspatial)

library(grid)  # incluir logo
library(png)  # incluir logo
library(leaflet) # para mapa interatico com leaflet
library(htmltools) # para o painel interativo

#diretorio

# setwd("~/Documents/PMAP MG-ES/poster") #home
setwd("~/Documentos/PMAP MG-ES/poster_pmap") #office
dir()

# Carregar dados

dados <- read.csv2("dat_dados.csv", sep = ";", header = TRUE, fileEncoding = "latin1")

pontos <- read.csv2("locais_descargas.csv", sep = ";", header = TRUE, fileEncoding = "UTF-8")

logo <- png::readPNG(
  "~/Documentos/PMAP MG-ES/poster_pmap/logo_pmap.png"
)

logo_grob <- rasterGrob(
  logo,
  interpolate = TRUE
)

# organizar os dados
municipios_resumo <- dados %>%
  group_by(estado, municipio) %>%
  summarise(
    captura = sum(kg, na.rm = TRUE),
    renda = sum(valor, na.rm = TRUE),
    descargas = n_distinct(codigo_referencia),
    pescadores = n_distinct(unidade_produtiva),
    dias_pesca = sum(dias_pesca, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(captura))

municipios_resumo

# Shapes para para o mapa

# Carregar shape do rio doce
#rio_doce <- st_read("/Users/danicastor/Documents/PMAP MG-ES/mapas/sig/Curso Rio/curso do rio.shp", options = "ENCODING=LATIN1") #home
rio_doce <- st_read("~/Documentos/PMAP MG-ES/sig/curso do rio/curso d costeiro.shp", options = "ENCODING=LATIN1") #office


rio_principal <- rio_doce %>%
  filter(identifica == "Rio Doce")

rios_secundarios <- rio_doce %>%
  filter(identifica != "Rio Doce")


# Carregar shape dos municipios por estado/monitorados
#MN_MG_continental <- st_read("~/Documents/PMAP MG-ES/mapas/sig/MN_MG_Continental/MN_MG_Continental.shp")
#MN_ES_continental <- st_read("~/Documents/PMAP MG-ES/mapas/sig/MN_ES_Continental/MN_ES_Continental.shp")


# VERIFICAR COORDENADAS
#st_crs(MN_ES_continental)
#st_crs(MN_MG_continental)

# unir os shapes dos municipios

#MN_continetal <- bind_rows(MN_ES_continental, MN_MG_continental)

MN_continetal <-st_read("~/Documentos/PMAP MG-ES/sig/MN_PMAP_CONTINENTAL.shp")

# chegcar CRS

st_crs <- MN_continetal

# padronizar dados 

MN_continetal$NM_MUN_2[MN_continetal$NM_MUN_2 == "Sem-Peixe"] <- "Sem Peixe"
MN_continetal$NM_MUN_2[MN_continetal$NM_MUN_2 == "Pingo-d'Água"] <- "Pingo d Água"
MN_continetal$NM_MUN_2[MN_continetal$NM_MUN_2 == "Baixo Guandu"] <- "Baixo Guandú"


# Carregar municípios de MG e ES
# Baixar mapa dos estados desejados
#estados_mg <- read_state(code_state = "MG", year = 2020)
#estados_es <- read_state(code_state = "ES", year = 2020)

# OU DIRETO DO SHAPE 

estados_mg <- st_read("~/Documentos/PMAP MG-ES/sig/MG_UF_2022/MG_UF_2022.shp")
estados_es <-st_read("~/Documentos/PMAP MG-ES/sig/ES_UF_2022/ES_UF_2022.shp")
# Juntar os dois
estados <- bind_rows(estados_mg, estados_es)

# Baixar municipios
#mun_mg <- read_municipality(code_muni = "MG", year = 2020)
#mun_es <- read_municipality(code_muni = "ES", year = 2020)

# OU 

mun_mg <- st_read("~/Documentos/PMAP MG-ES/sig/MG_Municipios_2022.shp")
mun_es  <-st_read("~/Documentos/PMAP MG-ES/sig/ES_Municipios_2022.shp")
# Juntar os dois
municipios <- bind_rows(mun_mg, mun_es)


# Organizar dados espaciais carregados

pontos %>%
  filter(is.na(lon) | is.na(lat))

summary(pontos$lon)
summary(pontos$lat)

# remover NA 

pontos_limpo <- pontos %>%
  filter(
    !is.na(lon),
    !is.na(lat)
  )


# apenas o ativos

pontos_limpo <- pontos %>%
  filter(
    is_ativo == "VERDADEIRO",
    !is.na(lon),
    !is.na(lat)
  )

# apenas os pontos dos municipios da área e ativos

pontos_filtrado <- pontos_limpo %>%
  filter(is_ativo == "VERDADEIRO") %>%
  semi_join(
    dados %>% distinct(municipio),
    by = "municipio"
  )

unique(pontos_filtrado$municipio)
nrow(pontos_filtrado)

# Transformar os pontos e, sf

pontos_sf <- st_as_sf(
  pontos_filtrado,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE   #mantem o lat e lon
)

###################### MAPA #######

mapa_municipios <- MN_continetal %>%
  left_join(
    municipios_resumo,
    by = c("NM_MUN" = "municipio")
  )

ggplot() +
  
  # Estados
  geom_sf(
    data = estados,
    fill = "grey95",
    color = "grey70",
    linewidth = .3
  ) +
  
  # Municípios monitorados
  geom_sf(
    data = MN_continetal,
    fill = "#D9D9D9",
    color = "white",
    linewidth = .2
  ) +
  
  # Rio Doce
  geom_sf(
    data = rio_doce %>%
      filter(identifica == "Rio Doce"),
    color = "lightblue",
    linewidth = 1
  ) +
  
  # Pontos de descarga
  geom_sf(
    data = pontos_sf,
    shape = 21,
    fill = "#E41A1C",
    color = "white",
    stroke = .25,
    size = 2.2,
    alpha = .9
  ) +
  
  annotation_scale(location = "bl") +
  
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_fancy_orienteering()
  ) +
  
  coord_sf() +
  
  labs(
    title = "Locais de descarga monitorados",
    subtitle = "Programa de Monitoramento da Atividade Pesqueira - PMAP"
  ) +
  
  theme_void() +
  
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 11)
  )

# FILTRAR POR MUNICIPIO

mun <- "Bugre"

mun_shape <- MN_continetal %>%
  filter(NM_MUN_2 == mun)

pontos_mun <- pontos_sf %>%
  filter(municipio == mun)


bbox <- st_bbox(mun_shape)

coord_sf(
  xlim = c(bbox["xmin"] - 0.05, bbox["xmax"] + 0.05),
  ylim = c(bbox["ymin"] - 0.05, bbox["ymax"] + 0.05),
  expand = FALSE
)


######### COMEÇO DO LOOPING MAPA POR MUNICIPIO #######

lista_municipios <- unique(dados$municipio)

for(mun in lista_municipios){
  
  mun_shape <- MN_continetal %>%
    filter(NM_MUN_2 == mun)
  
  pontos_mun <- pontos_sf %>%
    filter(municipio == mun)
  
  bbox <- st_bbox(
    st_transform(mun_shape, 4326)
  )
  
  # garante CRS
  mun_shape <- st_transform(mun_shape, 4326)
  pontos_mun <- st_transform(pontos_mun, 4326)
  
  xlim <- c(bbox["xmin"] - 0.05, bbox["xmax"] + 0.05)
  ylim <- c(bbox["ymin"] - 0.05, bbox["ymax"] + 0.05)
  
  mapa <- ggplot() +
    
    geom_sf(data = estados,
            fill = "grey95",
            color = "grey70") +
    
    geom_sf(data = MN_continetal,
            fill = "grey90",
            color = "white") +
    
    geom_sf(
      data = mun_shape,
      fill = "#F6E58D",
      color = "#8B7500",
      linewidth = .8
    ) +
    
    geom_sf(data = rio_doce %>% filter(identifica== "Rio Doce"),
            color = "lightblue",
            linewidth= 2
    ) +
    
    geom_sf(data = pontos_mun,
            shape = 21,
            fill = "red",
            color = "white",
            size = 3) +
    
    coord_sf(
      xlim = xlim,
      ylim = ylim,
      expand = FALSE
    ) +
    
    annotation_scale(
      location = "bl",
      width_hint = 0.25
    ) +
    
    annotation_north_arrow(
      location = "tr",
      which_north = "true",
      style = north_arrow_minimal,
      height = unit(0.8, "cm"),
      width = unit(0.8, "cm")
    ) +
    
    
    
    theme_void()
  
  
  mapa
  
  ggsave(
    paste0("mapas/", mun, ".png"),
    mapa,
    width = 6,
    height = 6,
    dpi = 300
  )
  
}

print(mapa)

##### LOPPING PAINEL POR MUNICIPIO #######

dir.create("paineis_municipios", showWarnings = FALSE)

#########################
# DADOS DO MUNICÍPIO
#########################

dados_mun <- dados %>%
  filter(municipio == mun)


lista_municipios <- unique(dados$municipio)


for(mun in lista_municipios){
  
  
  print(paste("Gerando:", mun))
  
  dados_mun <- dados %>%
    filter(municipio == mun)
  
  
  #########################
  # INDICADORES DO MUNICÍPIO
  #########################
  
  indicadores <- dados_mun %>%
    summarise(
      captura = sum(kg, na.rm = TRUE),
      valor = sum(valor, na.rm = TRUE),
      pescadores = n_distinct(unidade_produtiva, na.rm = TRUE),
      descargas = n_distinct(codigo_referencia)
    )
  
  
  
  #########################
  # MAPA MUNICIPAL
  #########################
  
  mun_shape <- MN_continetal %>%
    filter(NM_MUN_2 == mun)
  
  pontos_mun <- pontos_sf %>%
    filter(municipio == mun)
  
  bbox <- st_bbox(mun_shape)
  
  #texto do rio doce fixado em todos os mapas
  
  rio_label <- rio_doce %>%
    filter(identifica == "Rio Doce") %>%
    st_union() %>%
    st_point_on_surface() %>%
    st_as_sf()
  ##### local ####
  mapa_localizacao <- ggplot() +
    geom_sf(
      data = estados,
      fill = "grey95",
      color = "grey60"
    ) +
    geom_sf(
      data = MN_continetal,
      fill = "grey90",
      color = "white",
      linewidth = .1
    ) +
    geom_sf(
      data = mun_shape,
      fill = "#F6E58D",
      color = "#8B7500",
      linewidth = .5
    ) +
    theme_void() 
  ##### municipio ####
  mapa_mun <- ggplot() +
    
    # Estados de MG e ES
    geom_sf(
      data = estados,
      fill = "grey95",
      color = "grey70",
      linewidth = .3
    ) +
    
    # Todos municípios monitorados
    geom_sf(
      data = MN_continetal,
      fill = "grey90",
      color = "white",
      linewidth = .2
    ) +
    
    # Nome dos municípios
    geom_sf_text(
      data = MN_continetal,
      aes(label = NM_MUN_2),
      size = 2.5,
      color = "grey30"
    ) +
    
    # Município selecionado
    geom_sf(
      data = mun_shape,
      fill = "#F6E58D",
      color = "#8B7500",
      linewidth = .8
    ) +
    
    #  rios
    geom_sf(
      data = rio_doce %>%
        filter(identifica == "Rio Doce"),
      color = "lightblue",
      linewidth = 1
    ) +
    geom_sf_text(
      data = rio_label,
      aes(label = "Rio Doce"),
      color = "#0072B2",
      fontface = "bold",
      size = 3
    ) +
    # Pontos
    geom_sf(
      data = pontos_mun,
      shape=21,
      fill="#E41A1C",
      color="white",
      size=3
    ) +
    
    coord_sf(
      xlim=c(
        bbox["xmin"]-.05,
        bbox["xmax"]+.05
      ),
      ylim=c(
        bbox["ymin"]-.05,
        bbox["ymax"]+.05
      ),
      expand=FALSE
    ) +
    
    labs(
      title = "Locais descargas"
    ) +
    #escala
    annotation_scale(
      location = "bl",
      width_hint = 0.25,
      text_cex = 0.8
    ) +
    #rosa dos ventos
    annotation_north_arrow(
      location = "tr",
      which_north = "true",
      style = north_arrow_minimal,
      height = unit(0.8, "cm"),
      width = unit(0.8, "cm")
    ) +
    theme_bw() +
    theme(
      panel.grid.major = element_line(
        colour = "grey85",
        linewidth = 0.3,
        linetype = "dashed"
      ),
      panel.grid.minor = element_blank(),
      axis.title = element_blank(),
      axis.text = element_text(size = 8),
      axis.ticks = element_line(),
      panel.border = element_rect(colour = "black", fill = NA)
    )
  
  
  #########################
  # ESPÉCIES
  #########################
  
  graf_especies <- dados_mun %>%
    
    group_by(nome_referencia) %>%
    
    summarise(
      kg=sum(kg,na.rm=TRUE),
      .groups="drop"
    ) %>%
    
    slice_max(
      kg,
      n=8
    ) %>%
    
    ggplot(
      aes(
        reorder(nome_referencia,-kg),
        kg
      )
    )+
    
    geom_col(
      fill="#2C7FB8"
    )+
    
    labs(
      title="Categoria de pescado",
      x="",
      y="kg"
    )+
    
    theme_minimal()+
    theme(
      axis.text.x = element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 1,
        size = 9
      )
    )
  
  
  
  #########################
  # PETRECHOS
  #########################
  
  graf_petrecho <- dados_mun %>%
    
    group_by(petrecho) %>%
    
    summarise(
      kg=sum(kg,na.rm=TRUE),
      .groups="drop"
    ) %>%
    
    slice_max(
      kg,
      n=8
    ) %>%
    
    ggplot(
      aes(
        reorder(petrecho,-kg),
        kg
      )
    )+
    
    geom_col(
      fill="#2C7FB8"
    )+
    
    labs(
      title="Aparelho de pesca",
      x="",
      y="kg"
    )+
    
    theme_minimal() +
    theme(
      axis.text.x = element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 1,
        size = 9
      )
    )
  
  
  #########################
  # PRODUÇÃO ANUAL
  #########################
  
  graf_ano <- dados_mun %>%
    
    filter(
      !is.na(ano),
      !is.na(kg)
    ) %>%
    
    group_by(ano) %>%
    
    summarise(
      kg=sum(kg),
      .groups="drop"
    ) %>%
    
    ggplot(
      aes(
        ano,
        kg
      )
    )+
    
    geom_line(
      color="#D95F02",
      linewidth=1
    )+
    
    geom_point(
      size=3
    )+
    
    labs(
      title="Evolução da produção",
      x="Ano",
      y="kg"
    )+
    
    theme_minimal()+
    
    theme(
      plot.title=
        element_text(
          face="bold",
          hjust=.5
        )
    )
  
  
  ##### CAIXA INDICADORES
  
  
  graf_indicadores <- ggplot() +
    #fundo cinza
    annotate(
      "rect",
      xmin=0,
      xmax=6,
      ymin=0,
      ymax=4,
      fill="grey90",
      color="grey70",
      linewidth = .8
    ) +
    
    # LOGO CENTRAL
    annotation_custom(
      logo_grob,
      xmin=1.8,
      xmax=4.2,
      ymin=2.2,
      ymax=3.8
    ) +
    
    annotate(
      "text",
      x=1,
      y=0.8,
      label=paste0(
        "CAPTURA\n\n",
        scales::comma(indicadores$captura),
        " kg"
      ),
      size=5,
      fontface="bold"
    ) +
    
    
    annotate(
      "text",
      x=2.5,
      y=0.8,
      label=paste0(
        "VALOR\n\nR$ ",
        scales::comma(indicadores$valor)
      ),
      size=5,
      fontface="bold"
    ) +
    
    
    annotate(
      "text",
      x=4,
      y=0.8,
      label=paste0(
        "DESCARGAS\n\n",
        indicadores$descargas
      ),
      size=5,
      fontface="bold"
    ) +
    
    
    annotate(
      "text",
      x=5.5,
      y=0.8,
      label=paste0(
        "PESCADORES\n\n",
        indicadores$pescadores
      ),
      size=5,
      fontface="bold"
    ) +
    
    
    xlim(0,6)+
    ylim(0,4)+
    
    theme_void()
  
  
  
  #########################
  # MONTAR MINI-PAINEL
  #########################
  
  painel_municipio <-
    graf_indicadores /
    
    (mapa_mun | graf_ano) /
    
    (graf_petrecho | graf_especies) +
    
    plot_annotation(
      title = paste(
        mun
      ),
      subtitle =
        "Programa de Monitoramento da Atividade Pesqueira - PMAP MG/ES"
    ) &
    
    theme(
      plot.title =
        element_text(
          size=30,
          face="bold",
          hjust = .5
        )
    )
  
  print(painel_municipio)
  
  #########################
  # SALVAR
  #########################
  ggsave(
    filename = paste0(
      "paineis_municipios/",
      mun,
      "_painel.png"
    ),
    plot = painel_municipio,
    width = 30,
    height = 35,
    units = "cm",
    dpi = 300
  )
  
  
}


####### PAINEL INTERATIVO PMAP MG ES #######

#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("plotly")

#PACOTES

library(shiny)
library(shinydashboard)
library(plotly)



dir.create("www", showWarnings = FALSE)

file.copy(
  "~/Documentos/PMAP MG-ES/logo_pmap.png",
  "www/logo_pmap.png",
  overwrite = TRUE
)


# DADOS GRANDES PARA rds.

saveRDS(MN_continetal,"MN_continetal.rds")
saveRDS(rio_doce,"rio_doce.rds")
saveRDS(pontos_sf,"pontos_sf.rds")
saveRDS(dados, "dados.rds")
saveRDS(estados, "estados.rds")




#############################
# CARREGAR DADOS
#############################

dados <- readRDS("dados.rds")

MN_continetal <- readRDS(
  "MN_continetal.rds"
)

rio_doce <- readRDS(
  "rio_doce.rds"
)

pontos_sf <- readRDS(
  "pontos_sf.rds"
)

estados <- readRDS(
  "estados.rds"
)


logo <- png::readPNG(
  "logo_pmap.png"
)

logo_grob <- rasterGrob(
  logo,
  interpolate = TRUE
)


lista_municipios <- unique(
  dados$municipio
)


##### CRIAR A TELA DO PAINEL ####


ui <- dashboardPage(
  
  dashboardHeader(
    
    title = tags$div(
      
      style = "
    display:flex;
    align-items:center;
    ",
      
      tags$img(
        src="logo_pmap.png",
        height="35px",
        style="
      margin-right:10px;
      "
      ),
      
      tags$span(
        "PMAP MG/ES Continental",
        style="
      font-weight:bold;
      "
      )
      
    )
    
  ),
  
  
  dashboardSidebar(
    
    selectInput(
      inputId="mun",
      label="Município",
      choices=lista_municipios
    )
    
  ),
  
  
  dashboardBody(
    
    fluidRow(
      
      valueBoxOutput("captura"),
      valueBoxOutput("valor"),
      valueBoxOutput("descargas"),
      valueBoxOutput("pescadores")
      
    ),
    
    
    fluidRow(
      
      plotOutput(
        "mapa",
        height="600px"
      )
      
    ),
    
    
    fluidRow(
      
      plotlyOutput("especies"),
      plotlyOutput("petrecho")
      
    ),
    
    
    fluidRow(
      
      plotlyOutput("ano")
      
    )
    
  )
  
)


##### SERVIDOR #########

server <- function(input, output){
  
  
  #############################
  # DADOS MUNICÍPIO
  #############################
  
  dados_mun <- reactive({
    
    dados %>%
      filter(
        municipio == input$mun
      )
    
  })
  
  
  #############################
  # INDICADORES
  #############################
  
  output$captura <- renderValueBox({
    
    valueBox(
      scales::comma(
        sum(dados_mun()$kg,
            na.rm = TRUE)
      ),
      "Captura (kg)",
      icon = icon("fish")
    )
    
  })
  
  
  output$valor <- renderValueBox({
    
    valueBox(
      paste0(
        "R$ ",
        scales::comma(
          sum(dados_mun()$valor,
              na.rm = TRUE)
        )
      ),
      "Valor comercializado",
      icon = icon("money-bill")
    )
    
  })
  
  
  output$descargas <- renderValueBox({
    
    valueBox(
      n_distinct(
        dados_mun()$codigo_referencia
      ),
      "Descargas",
      icon = icon("ship")
    )
    
  })
  
  
  output$pescadores <- renderValueBox({
    
    valueBox(
      n_distinct(
        dados_mun()$unidade_produtiva
      ),
      "Unidades produtivas",
      icon = icon("users")
    )
    
  })
  
  
  
  #############################
  # MAPA MUNICIPAL
  #############################
  
  output$mapa <- renderPlot({
    
    
    mun <- input$mun
    
    
    mun_shape <- MN_continetal %>%
      filter(
        NM_MUN_2 == mun
      )
    
    
    pontos_mun <- pontos_sf %>%
      filter(
        municipio == mun
      )
    
    
    bbox <- st_bbox(
      mun_shape
    )
    
    
    
    rio_label <- rio_doce %>%
      filter(
        identifica == "Rio Doce"
      ) %>%
      st_union() %>%
      st_point_on_surface() %>%
      st_as_sf()
    
    
    
    ggplot() +
      
      geom_sf(data = estados,
              fill= "grey95",
              color = "black")+
      
      geom_sf(
        data = MN_continetal,
        fill="grey90",
        color="white"
      )+
      
      
      geom_sf(
        data = mun_shape,
        fill="#F6E58D",
        color="#8B7500",
        linewidth=.8
      )+
      
      
      geom_sf(
        data = rio_doce %>%
          filter(
            identifica=="Rio Doce"
          ),
        color="lightblue",
        linewidth=1.5
      )+
      
      
      geom_sf_text(
        data=rio_label,
        aes(label="Rio Doce"),
        color="#0072B2",
        fontface="bold",
        size=5
      )+
      
      
      geom_sf(
        data=pontos_mun,
        shape=21,
        fill="red",
        color="white",
        size=3
      )+
      
      
      coord_sf(
        xlim=c(
          bbox["xmin"]-.05,
          bbox["xmax"]+.05
        ),
        ylim=c(
          bbox["ymin"]-.05,
          bbox["ymax"]+.05
        ),
        expand=FALSE
      )+
      
      
      annotation_scale(
        location="bl"
      )+
      
      
      annotation_north_arrow(
        location="tr",
        which_north="true",
        style=north_arrow_minimal()
      )+
      
      
      theme_bw()
    
  })
  
  
  
  #############################
  # GRAFICO ESPÉCIES
  #############################
  
  output$especies <- renderPlotly({
    
    
    graf <- dados_mun() %>%
      
      group_by(nome_referencia)%>%
      
      summarise(
        kg=sum(kg,na.rm=TRUE)
      )%>%
      
      slice_max(
        kg,
        n=8
      )%>%
      
      ggplot(
        aes(
          reorder(nome_referencia,-kg),
          kg
        )
      )+
      
      geom_col(
        fill="#2C7FB8"
      )+
      
      labs(
        title="Principais categorias de pescado",
        x="",
        y="kg"
      )+
      
      theme_minimal()
    
    
    ggplotly(graf)
    
  })
  
  
  
  #############################
  # GRAFICO PETRECHOS
  #############################
  
  output$petrecho <- renderPlotly({
    
    
    graf <- dados_mun()%>%
      
      group_by(petrecho)%>%
      
      summarise(
        kg=sum(kg,na.rm=TRUE)
      )%>%
      
      slice_max(
        kg,
        n=8
      )%>%
      
      ggplot(
        aes(
          reorder(petrecho,-kg),
          kg
        )
      )+
      
      geom_col(
        fill="#41B6C4"
      )+
      
      labs(
        title="Principais aparelhos",
        x="",
        y="kg"
      )+
      
      theme_minimal()
    
    
    ggplotly(graf)
    
  })
  
  
  
  #############################
  # EVOLUÇÃO ANUAL
  #############################
  
  output$ano <- renderPlotly({
    
    
    graf <- dados_mun()%>%
      
      group_by(ano)%>%
      
      summarise(
        kg=sum(kg,na.rm=TRUE)
      )%>%
      
      ggplot(
        aes(
          ano,
          kg
        )
      )+
      
      geom_line(
        color="#D95F02",
        linewidth=1
      )+
      
      geom_point(
        size=3
      )+
      
      labs(
        title="Evolução da produção",
        x="Ano",
        y="kg"
      )+
      
      theme_minimal()
    
    
    ggplotly(graf)
    
    
  })
  
  
}

##### PAINEL #######


shinyApp(
  ui,
  server
)
