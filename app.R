
#SETUPEAMOS EL DIRECTORIO DE TRABAJO 
#setwd("/home/farguto/Escritorio/TP GRUPAL/ultimos")


# INSTALAMOS LAS LIBRERIAS 
# 
# install.packages("shinyWidgets")
# install.packages ("shinythemes")
#install.packages("ggraph")
#install.packages("circlize")
#install.packages("reshape2")
#devtools::install_github("mattflor/chorddiag")
#install.packages("treemap")
#install.packages ("scales")

# CARGAMOS LAS LIBRERIAS

library(tidyverse)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)
library(DT)
library(plotly)
library(htmlwidgets)
#library(chorddiag)
library(reshape2)
library(circlize)
library(readxl)
library(ggraph)
library(igraph)
library(RColorBrewer)
library(tools)
library(stringr)
library(rjson)
library(treemap)
library(highcharter)
library(data.table)
library(scales)
source(file = 'scripts/getcomtrade.R')
pais <- fread("data/pais.csv")


ui <- fluidPage(theme = shinytheme("spacelab"),
                
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
                

                  
  # Jumbotron como header -----
  title = "ComExArg",
  div( id = 'encabezado',
       class = 'jumbotron',
       style = "background-image:url(banner.jpg);background-repeat:no-repeat; background-size: 100% 100%;",
       
       
       div(
         class = 'container',
         style = 'background-color:#FFFFFF9c;',
         h1('ComExArg', style = 'color:#000000;'),
         p('Sistema de consulta del Comercio Exterior Argentino')
       )
       
  ),
  hr (),
  
  div(
    class = 'container', 
    id = 'aplicacion',
   
    fluidRow(
    
    # Vamos a hacer primero la columna donde va a estar la selecciÃ³n de los usuarios.
    column(
      width = 3,
      wellPanel(
        div(
          id = 'input_titulo',
          strong(h3('Filtros'))
        ),
        hr(),
        
        h4('Seleccione el país'),
        
        pickerInput(
          inputId = "usuario_pais",
          label = 'País',
          choices = unique (pais$nombre) %>% as.character (),
          options= list(`actions-box`=TRUE,
                        `none-selected-text` = "Elija un país"),
          
        ),
        
        
        hr(),
        
        h4('Elija un año'),
        
        sliderInput(
          inputId = "usuario_año",
          label = "Año",
          value = 2019,
          min = 2000,
          max = 2019),
        
        # agregamos el botón de consulta
        actionButton(inputId = 'click', label = 'Consulta'),
        
        
        br(),
        br(),
        br(),
        br(),
        br(),
        div(
          p("Desarrollado por Martínez, Pérez Ibáñez y Arguto"),
          p("Fuente: UN-Comtrade"),
        )
        
      ),
    ),
    
    # Y acá la columna donde van a estar todas las pestañas con las visualizaciones
    
          column(
            width = 9,
            div(
              class = 'panel',
              tabsetPanel(
                
                # Primera pestaña: exportaciones
                tabPanel(
                  'Exportaciones',
                  
                  
                  div(
                    class = 'panel_header',
                    style = 'padding-left:10px; padding-top: 10px;',
                    h4('Principales exportaciones')
                  ),
                  hr(),
                  div(
                    class = 'panel_body',
                    highchartOutput('productos_exportacion')
                  ),
                  hr(),
                  h4 ('Top ten exportaciones'),
                  div(
                    class = 'panel_body2',
                    dataTableOutput ('top_ten_exportaciones')
                  ),
                  hr(),
                  h4 ('Total exportaciones'),
                  
                  div(
                    class = 'result',
                    style = 'font-size:20px; font-weight: bold;',
                    textOutput ('total_exportaciones')),
                  
          
                ),
                
                
                
                
                
                
                #Segunda pestaña: importaciones
                
                tabPanel(
                  'Importaciones',
                  div (
                    class = 'panel_header2',
                    style = 'padding-left:10px; padding-top: 10px;',
                    h4 ('Principales importaciones')
                  ),
                  div(
                    class = 'panel_body2',
                    highchartOutput('productos_importacion')),
                  hr(),
                  h4 ('Top ten importaciones'),
                  div(
                    class = 'panel_body2',
                    dataTableOutput ('top_ten_importaciones')
                  ),
                  hr(),
                  h4 ('Total importaciones'),
                  div(
                    class = 'result',
                    style = 'font-size:20px; font-weight: bold;',
                    textOutput ('total_importaciones')),
                  
                  
                )
              )
            )
          )
        ))) 




# SERVER ----

server <- function(input, output){ 
  

  
  # Instanciamos los reactive_values
  
    reactive_values <- reactiveValues()
    
   # Creamos los valores iniciales#
      reactive_values$usuario_pais <- "Todos"
      reactive_values$usuario_año <- 2019
      
  # Seteamos el observer
    observeEvent(input$click, {
    
    reactive_values$usuario_pais <- input$usuario_pais
    reactive_values$usuario_año <- input$usuario_año
    
  
})
  # Gráficos
    
  #PESTAÑA EXPORTACIONES
  
  output$productos_exportacion <- renderHighchart({
    
    Nomenclador <- read_excel("data/Nomenclador.xls")
    Nomenclador$Códi = as.character(Nomenclador$Códi)
    Nomenclador$Códi<-str_pad(Nomenclador$Códi, 2, pad = "0")
    Nomenclador <- Nomenclador %>% 
      select("Códi" , "Descripción")

    exportacionesproduc <-get.Comtrade(r="32", p= pais %>% filter (nombre == reactive_values$usuario_pais) %>% .[,1] %>%  as.character(), cc ="All", fmt = "csv", rg = "2", ps = as.character(reactive_values$usuario_año), px = "HS")

  
    #Exportaciones de Argentina x producto
   
    exportacionesproduc <- exportacionesproduc[["data"]]
    
    principalesExportacionesproduc <- exportacionesproduc %>% 
      select("Year", "Commodity.Code", "Aggregate.Level", "Trade.Value..US..", "Commodity")%>% 
      filter(Aggregate.Level == "2")%>% 
      arrange(desc(Trade.Value..US..))%>%  
      rename("Año" = "Year", "Códi" = "Commodity.Code", "Nivel de Agregacion" = "Aggregate.Level", "Valor comercial en USD" = "Trade.Value..US..")
    
    #joint con nomeclador
    
    principalesExportacionesproduc <-left_join(principalesExportacionesproduc,Nomenclador, by = "Códi")
    
    #armado tabla definitiva
    
    principalesExportacionesproduc <- principalesExportacionesproduc %>% select("Códi","Valor comercial en USD","Descripción" )
    principalesExportacionesproduc <- head(principalesExportacionesproduc, n=10L)

    hchart(principalesExportacionesproduc, "treemap", hcaes(x = Descripción, value = `Valor comercial en USD`, color = `Valor comercial en USD`)) %>% 
      hc_chart( style = list(fontFamily = "Helvetica")) %>% 
      hc_colorAxis(stops = color_stops(colors = viridis::inferno(10)))
    
  })

    output$top_ten_exportaciones <- renderDataTable({
      
      Nomenclador <- read_excel("data/Nomenclador.xls")
      Nomenclador$Códi = as.character(Nomenclador$Códi)
      Nomenclador$Códi<-str_pad(Nomenclador$Códi, 2, pad = "0")
      Nomenclador <- Nomenclador %>% 
        select("Códi" , "Descripción")
      
      exportacionesproduc <-get.Comtrade(r="32", p= pais %>% filter (nombre == reactive_values$usuario_pais) %>% .[,1] %>%  as.character(), cc ="All", fmt = "csv", rg = "2", ps = as.character(reactive_values$usuario_año), px = "HS")


      #Exportaciones de Argentina x producto

      exportacionesproduc <- exportacionesproduc[["data"]]
      
      principalesExportacionesproduc <- exportacionesproduc %>% 
        select("Year", "Commodity.Code", "Aggregate.Level", "Trade.Value..US..", "Commodity")%>% 
        filter(Aggregate.Level == "2")%>% 
        arrange(desc(Trade.Value..US..))%>%  
        rename("Año" = "Year", "Códi" = "Commodity.Code", "Nivel de Agregacion" = "Aggregate.Level", "Valor comercial en USD" = "Trade.Value..US..")
      
      #joint con nomeclador
      
      principalesExportacionesproduc <-left_join(principalesExportacionesproduc,Nomenclador, by = "Códi")
      
      #armado tabla definitiva
      
      principalesExportacionesproduc <- principalesExportacionesproduc %>% select("Códi","Valor comercial en USD","Descripción" )
      principalesExportacionesproduc <- head(principalesExportacionesproduc, n=10L)
      principalesExportacionesproduc$`Valor comercial en USD` <- principalesExportacionesproduc$`Valor comercial en USD` %>% as.numeric () %>% dollar (.,prefix = "US$", big.mark = ".", decimal.mark = ",")
      principalesExportacionesproduc
      
    })
    
    #TOTALIZADOR
    output$total_exportaciones <- renderText ({
    exportaciones <-get.Comtrade(r="32", p= pais %>% filter (nombre == reactive_values$usuario_pais) %>% .[,1], fmt = "csv", rg = "2", ps = as.character(reactive_values$usuario_año), px = "HS")
    exportaciones <- exportaciones[["data"]]
    
    principalesExportaciones <- exportaciones %>% 
      select("Year", "Reporter", "Partner", "Trade.Value..US..","Partner.ISO") %>% 
      arrange(desc(Trade.Value..US..)) %>% 
      rename("Año" = "Year", "Reporte" = "Reporter", "Socio comercial" = "Partner", "Valor comercial en USD" = "Trade.Value..US..","iso-a3" = "Partner.ISO")
    
    #Obtenemos el valor de las exportaciones mundiales
    
    exportacionesTotal <- principalesExportaciones[1,4] %>% as.numeric () %>% dollar (.,prefix = "US$", big.mark = ".", decimal.mark = ",") 
    
    exportacionesTotal
    })
  
  #PESTAÑA IMPORTACIONES
    output$productos_importacion <- renderHighchart({
      
      Nomenclador <- read_excel("data/Nomenclador.xls")
      Nomenclador$Códi = as.character(Nomenclador$Códi)
      Nomenclador$Códi<-str_pad(Nomenclador$Códi, 2, pad = "0")
      Nomenclador <- Nomenclador %>% 
        select("Códi" , "Descripción")
       
      importaciones <-get.Comtrade(r="32", p= pais %>% filter (nombre == reactive_values$usuario_pais) %>% .[,1] %>%  as.character(), cc ="All", fmt = "csv", rg = "1", ps = as.character(reactive_values$usuario_año), px = "HS")
      importaciones <- importaciones[["data"]]
       
       #Importaciones de Argentina x producto
       
      principalesImportacionesproduc <- importaciones %>% 
        select("Year", "Commodity.Code", "Aggregate.Level", "Trade.Value..US..", "Commodity")%>% 
        filter(Aggregate.Level == 2 )%>% 
        arrange(desc(Trade.Value..US..))%>% 
        rename("Año" = "Year", "Códi" = "Commodity.Code", "Nivel de Agregacion" = "Aggregate.Level", "Valor comercial en USD" = "Trade.Value..US..")
       
       #joint con nomeclador
       
       
      principalesImportacionesproduc <-left_join(principalesImportacionesproduc,Nomenclador, by = "Códi")
       
       #armado tabla definitiva
       
      principalesImportacionesproduc <- principalesImportacionesproduc %>% select("Códi","Valor comercial en USD","Descripción" )
      principalesImportacionesproduc <- head(principalesImportacionesproduc, n=10L)
      
      hchart(principalesImportacionesproduc, "treemap", hcaes(x = Descripción, value = `Valor comercial en USD`, color = `Valor comercial en USD`)) %>% 
        hc_chart( style = list(fontFamily = "Helvetica")) %>% 
        hc_colorAxis(stops = color_stops(colors = viridis::inferno(10)))
    
    
    })

  #TABLA TOP TEN
  output$top_ten_importaciones <- renderDataTable({
    
    Nomenclador <- read_excel("data/Nomenclador.xls")
    Nomenclador$Códi = as.character(Nomenclador$Códi)
    Nomenclador$Códi<-str_pad(Nomenclador$Códi, 2, pad = "0")
    Nomenclador <- Nomenclador %>% 
      select("Códi" , "Descripción")
    
    importaciones <-get.Comtrade(r="32", p= pais %>% filter (nombre == reactive_values$usuario_pais) %>% .[,1] %>%  as.character(), cc ="All", fmt = "csv", rg = "1", ps = as.character(reactive_values$usuario_año), px = "HS")
    importaciones <- importaciones[["data"]]
    
    #Importaciones de Argentina x producto

    principalesImportacionesproduc <- importaciones %>% 
      select("Year", "Commodity.Code", "Aggregate.Level", "Trade.Value..US..", "Commodity")%>% 
      filter(Aggregate.Level == 2 )%>% 
      arrange(desc(Trade.Value..US..))%>% 
      rename("Año" = "Year", "Códi" = "Commodity.Code", "Nivel de Agregacion" = "Aggregate.Level", "Valor comercial en USD" = "Trade.Value..US..")
    
    #joint con nomeclador
    
    
    principalesImportacionesproduc <-left_join(principalesImportacionesproduc,Nomenclador, by = "Códi")
    
    #armado tabla definitiva
    
    principalesImportacionesproduc <- principalesImportacionesproduc %>% select("Códi","Valor comercial en USD","Descripción" )
    principalesImportacionesproduc <- head(principalesImportacionesproduc, n=10L)
    principalesImportacionesproduc$`Valor comercial en USD` <- principalesImportacionesproduc$`Valor comercial en USD` %>% as.numeric () %>% dollar (.,prefix = "US$", big.mark = ".", decimal.mark = ",")
    principalesImportacionesproduc
    

  })
  
  #TOTALIZADOR
  output$total_importaciones <- renderText ({
    importaciones <-get.Comtrade(r="32", p= pais %>% filter (nombre == reactive_values$usuario_pais) %>% .[,1], fmt = "csv", rg = "1", ps = as.character(reactive_values$usuario_año), px = "HS")
    importaciones <- importaciones[["data"]]
    
    principalesImportaciones <- importaciones %>% 
      select("Year", "Reporter", "Partner", "Trade.Value..US..","Partner.ISO") %>% 
      arrange(desc(Trade.Value..US..)) %>% 
      rename("Año" = "Year", "Reporte" = "Reporter", "Socio comercial" = "Partner", "Valor comercial en USD" = "Trade.Value..US..","iso-a3" = "Partner.ISO")
    
    #Obtenemos el valor de las exportaciones mundiales
    
    importacionesTotal <- principalesImportaciones[1,4] %>% as.numeric () %>% dollar (.,prefix = "US$", big.mark = ".", decimal.mark = ",")
    
    importacionesTotal
  
})
}

  #Corremos la app
  shinyApp(ui = ui, server = server)



