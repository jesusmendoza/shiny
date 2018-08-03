


# Source helper functions -----
source("helpers.R")

# global
source("global.R")


# User interface ----
ui <- navbarPage("Citio",theme = shinytheme("united"),
  tabPanel("Mapa interactivo",
  sidebarLayout(
    
    sidebarPanel(
      helpText("Crea mapas de ubicaciones de las estaciones"),
      numericInput("num", label = "Elige el número de usuarios", value = 1000, min = 10, max = 50000, step = 10),
      br(),
      helpText("Número de estaciones"),
      textOutput("usuarios"),
      br(),
      selectInput("estacion","Selecciona una estación", choices = c(0)),
      br(),
      h4("Promedio de cambios:"),
      p(textOutput("cambios")),
      
      h4("Promedio de carga restante:"),
      p(textOutput("carga_restante")),
      h4("Usuarios únicos:"),
      p(textOutput("usuarios_unicos")),
      h4("Cambios por hora:"),
      plotOutput('cargas_hora')
      ),
    
    mainPanel(leafletOutput("map", width = "100%", height = 800))

              
              
              
              
  )),

tabPanel('Resultados Simulación',
    sidebarLayout(
           
    sidebarPanel(
       p('Consideramos un escenario como el número de usuarios que el sistema tendría al final del primer año, el crecimiento'),
       br(),
       selectInput("escenario", label = "Elige el escenario", choices = c(500,1000,1500,2000,2500,3000,3500,4000,4500,5000)),
       sliderInput("dr", 
                   "Elige un rango de fechas:", 
                   min = as.Date("2019-01-01"), max = as.Date("2026-01-01"),value =c(as.Date("2018-01-01"), as.Date("2026-01-01")),
                   timeFormat = "%Y"
                   ),
       selectInput("tipo",label = "Seleccione un tipo de crecimiento",
                   choices = list("Itálika"=1,"Uniforme"=2)),
       selectInput("tasa",label = "Seleccione una tasa de crecimiento (%)",
                   choices = 1:100)
        ),
    mainPanel(
      plotOutput("n_usuarios", width = '100%',height = 400),
      p('Un párrafo'),
      plotOutput("n_baterias", width = '100%',height = 400),
      p('Un párrafo'),
      plotOutput("n_estaciones", width = '100%',height = 400),
      p('Un párrafo'),
      plotOutput("rBU", width = '100%',height = 400)
    )
    )
       ),
  
tabPanel('Modelo de negocio',
         sidebarLayout(
           
           sidebarPanel(
             p('Consideramos un escenario como el número de usuarios que el sistema tendría al final del primer año, el crecimiento'),
             br(),
             selectInput("escenario_modelo", label = "Escenario", choices = c(500,1000,1500,2000,2500,3000,3500,4000,4500,5000)),
             sliderInput("dr_modelo", 
                         "Rango de fechas:", 
                         min = as.Date("2019-01-01"), max = as.Date("2026-01-01"),value =c(as.Date("2018-01-01"), as.Date("2026-01-01")),
                         timeFormat = "%Y"
             ),
             selectInput("tipo_modelo",label = "Tipo de crecimiento",
                         choices = list("Itálika"=1,"Uniforme"=2)),
             selectInput("tasa_modelo",label = "Tasa de crecimiento (%)",
                         choices = 1:100),
             textInput("sellingprice", "Precio de venta", value = "30000"),
             textInput("membership1", "Membresía 1", value = "540"),
             textInput("membership2", "Membrasía 2", value = "640"),
             textInput("membership3", "Membresía 3", value = "840"),
             textInput("membership4", "Membresía 4", value = "1230"),
             textInput("costperbike", "Costo por motocicleta", value = "16000"),
             textInput("costperbattery","Costo por bateria", value = "10000"),
             textInput("kwhperbattery", "kw/H por batería", value = "1.7"),
             textInput("kwhprice", "Precio por kw/H ($)", value = "1.8"),
             textInput("charginmodulecostA", "Costo módulo de carga A", value = "60000"),
             textInput("charginmodulecostB", "Costo módulo de carga B", value = "50000"),
             textInput("extracost","Costos extra mensuales",value = "500000"),
             textInput('extracostmoduleA','Costos extra mensuales por módulo A',value = "1000"),
             textInput('extracostmoduleB','Costos extra mensuales por módulo B',value = "500"),
             textInput("batterydepreciation", "Depreciación de la batería (Años)", value = "7"),
             textInput("chargingdepreciation", "Depreciación del módulo (Años)", value = "10")
           ),
           mainPanel(
             p('Un párrafo'),
             plotOutput("flow_capital", width = '100%',height = 400),
             plotOutput("entradas_salidas"),
             p("Margen de ganancia: "),
             p(textOutput('gross_margin')),
             plotOutput("motos",width = '100%',height = 400),
             plotOutput("planes",width = '100%',height = 400)
           )
         )
),

tabPanel('TCO',
         sidebarLayout(
           
           sidebarPanel(
             textInput("n_meses","Número de meses", value = "12"),
             textInput("costbykeoil","Costo motocicleta a gasolina", value = '25000'),
             textInput("costbykeelectric","Costo motocicleta eléctrica", value = '30000'),
             textInput("kmrecorridos", "Km recorridos por semana", value = "100"),
             textInput("oilcost","Costo por L de gasolina", value = "19.00"),
             textInput("rendimiento","Km/L",value = "23"),
             textInput("kmporcarga","Km por carga",value="100"),
             h4("Plan 1"),
             textInput("p1_cargas","# de Cargas", value = "5"),
             textInput("p1_precio","Precio",value = "540"),
             h4("Plan 2"),
             textInput("p2_cargas","# de Cargas", value = "10"),
             textInput("p2_precio","Precio",value = "640"),
             h4("Plan 3"),
             textInput("p3_cargas","# de Cargas", value = "20"),
             textInput("p3_precio","Precio",value = "840"),
             textInput("p4_cargas","# de Cargas", value = "40"),
             textInput("p4_precio","Precio",value = "1240"),             
             textInput("periodo","Periodo (años)",value='3')
           ),

           mainPanel(
              plotOutput("tco",width = '100%',height = 400)
           )

         )
)
)

  



