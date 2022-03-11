#ESTE ARCHIVO CONTIENE TODAS LAS FUNCIONES USADAS EN LA SHINY - COMEXARG

get.Comtrade <- function(url="http://comtrade.un.org/api/get?"
                         ,maxrec=50000
                         ,type="C"
                         ,freq="A"
                         ,px="HS"
                         ,ps="now"
                         ,r
                         ,p
                         ,rg="all"
                         ,cc="TOTAL"
                         ,fmt="json"
)
{
  string<- paste(url
                 ,"max=",maxrec,"&" #maximum no. of records returned
                 ,"type=",type,"&" #type of trade (c=commodities)
                 ,"freq=",freq,"&" #frequency
                 ,"px=",px,"&" #classification
                 ,"ps=",ps,"&" #time period
                 ,"r=",r,"&" #reporting area
                 ,"p=",p,"&" #partner country
                 ,"rg=",rg,"&" #trade flow
                 ,"cc=",cc,"&" #classification code
                 ,"fmt=",fmt        #Format
                 ,sep = ""
  )
  string
  if(fmt == "csv") {
    raw.data<- read.csv(string,header=TRUE)
    return(list(validation=NULL, data=raw.data))
  } else {
    if(fmt == "json" ) {
      raw.data<- fromJSON(file=string)
      data<- raw.data$dataset
      validation<- unlist(raw.data$validation, recursive=TRUE)
      ndata<- NULL
      if(length(data)> 0) {
        var.names<- names(data[[1]])
        data<- as.data.frame(t( sapply(data,rbind)))
        ndata<- NULL
        for(i in 1:ncol(data)){
          data[sapply(data[,i],is.null),i]<- NA
          ndata<- cbind(ndata, unlist(data[,i]))
        }
        ndata<- as.data.frame(ndata)
        colnames(ndata)<- var.names
      }
      return(list(validation=validation,data =ndata))
      
    }
  }
}




# plot_importaciones <- function (base) {
# 
#   base %>% 
#   importacionesproduc <-get.Comtrade(r="32", p= pais %>% filter (nombre == input$usuario_pais) %>% .[,1] %>%  as.character(), cc ="All", fmt = "csv", rg = "1", px = "BEC")
#   importacionesproduc <- importacionesproduc[["data"]]
#   principalesImportacionesproduc <- importacionesproduc %>%
#     select("Year", "Commodity.Code", "Aggregate.Level", "Trade.Value..US..", "Commodity")%>%
#     filter(Aggregate.Level == "1")%>%
#     arrange(desc(Aggregate.Level = "l"))%>%
#     rename("Año" = "Year", "Codigo Producto" = "Commodity.Code", "Nivel de Agregacion" = "Aggregate.Level", "Valor comercial en USS" = "Trade.Value..US..")
#   return (hchart(principalesImportacionesproduc, "treemap", hcaes(x = Commodity, value = `Valor comercial en USS`)))
# 
# }

# # Esta es una manera de crear un directorio si quisieran hacerlo desde acá.
# # library(fs)
# # fs::dir_create('scripts') 
# 
# dump(
#   list = c('get_tasa_mercado_laboral', 'plot_tasa_ocupacion'), # hacemos una lista con las funciones
#   file = 'scripts/00_scripts_eph', append = FALSE) # y escribimos dónde las queremos
