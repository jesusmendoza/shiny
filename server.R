source("global.R")
# Source helper functions -----
source("helpers.R")

server <- function(input, output,session) {

     

    output$value <- renderPrint({ input$num })
    output$map <- renderLeaflet({
    devolver_estaciones(input$num)
    })
    output$usuarios <- renderText(obtener_usuarios(input$num))

    outVar = reactive({
      mydata = obtener_estaciones(input$num)$indice
      mydata
    })
    observe({
      updateSelectInput(session,"estacion",choices = outVar())
    })
    output$cambios <- renderText({
      cambios <- resultado_general_be[resultado_general_be[,'estacion']==obtener_id(input$estacion) & resultado_general_be[,'usuarios']==input$num,c('cambios')]
      cambios <- as.double(cambios)
      })
    output$carga_restante <- renderText({
      id <- obtener_id(input$estacion)
      carga_restante <- resultado_general_be[resultado_general_be[,'estacion']==id & resultado_general_be[,'usuarios']==input$num,c('carga_restante')]
      carga_restante <- as.double(carga_restante)
    })
    output$usuarios_unicos <- renderText({
      id <- obtener_id(input$estacion)
      usuarios_unicos <- resultado_general_be[resultado_general_be[,'estacion']==id & resultado_general_be[,'usuarios']==input$num,c('usuarios_be')]
      usuarios_unicos <- as.integer(usuarios_unicos)
    })
    output$cargas_hora <- renderPlot({
      id <- obtener_id(input$estacion)
      t <- resultado_general_be[resultado_general_be[,'estacion']==id & resultado_general_be[,'usuarios']==input$num,c('h0','h1','h2',
                        'h3','h4','h5','h6','h7','h8','h9','h10','h11','h12','h13','h14','h15','h16','h17','h18','h19','h20','h21','h22','h23')]
      t =  t(t)

      plot(t,main ='Demanda por hora', xlab = 'Hora',ylab ='Cambios por hora',type = 'o')})
    
    output$n_usuarios <- renderPlot({
      u <-input$escenario
      u <- obtener_escenario(as.integer(u),input$dr,input$tipo,input$tasa)
      plot(u$Fecha,u$usuarios, 
           type = "l", 
           lty  = "dashed",
           col  = "red", 
           xlab = 'Año',
           ylab = '# de usuarios',
           main = 'Tasa de crecimiento de la red en número de usuarios',
           xaxs="i", yaxs="i"
      )
      grid(nx=NA, ny= NULL, col='grey',lwd=2)

                        })

    
    output$n_baterias <- renderPlot({
      u <-input$escenario
      u <- obtener_escenario(as.integer(u),input$dr,input$tipo,input$tasa)
      plot(u$Fecha,u$n_baterias, 
           type = "l", 
           lty  = "dashed",
           col  = "red", 
           xlab = 'Año',
           ylab = '# de baterias',
           main = 'Tasa de crecimiento de la red en número de baterías',
           xaxs="i", yaxs="i"
      )
      grid(nx=NA, ny= NULL, col='grey',lwd=2)

    })
    
    output$n_estaciones <- renderPlot({
      u <-input$escenario
      u <- obtener_escenario(as.integer(u),input$dr,input$tipo,input$tasa)
      plot(u$Fecha,u$Estaciones_A, 
           type = "l", 
           lty  = 1:2,
           col  = "red", 
           xlab = 'Año',
           ylab = '# de módulos',
           main = 'Tasa de crecimiento de la red en número de Módulos A y B',
           xaxs="i", yaxs="i",ylim = c(0,max(max(u$Estaciones_B),max(u$Estaciones_A)))
        
      )
      lines(u$Fecha,u$Estaciones_B,col='green')
      legend('topleft','groups', legend=c("Módulos A", "Módulos B"),
            col=c("red", "green"), lty=1:2, cex=0.8,lwd = 2)
      grid(nx=NA, ny= NULL, col='grey',lwd=2)
    })
    
    output$rBU <- renderPlot({
      u <-input$escenario
      u <- obtener_escenario(as.integer(u),input$dr,input$tipo,input$tasa)
      plot(u$Fecha,u$rBU, 
           type = "l", 
           lty  = "dashed",
           col  = "red", 
           xlab = 'Año',
           ylab = "Baterías por usuario",
           main = 'Razón entre el número de baterías y usuarios',
           xaxs="i", yaxs="i",ylim = c(min(u$rBU)-1,max(u$rBU)+1)
      )
      grid(nx=NA, ny= NULL, col='grey',lwd=2)
      
    })
    output$flow_capital <- renderPlot({
      u <-input$escenario_modelo
      u <- obtener_escenario(as.integer(u),input$dr_modelo,input$tipo_modelo,input$tasa_modelo,in_modelo = TRUE)
      ### Salidas
      u$flow_capital = u$usuarios*(as.numeric(input$sellingprice)-as.numeric(input$costperbike))
      u$flow_capital = u$flow_capital - u$n_baterias*as.numeric(input$costperbattery)
      u$cambios = cumsum(u$cambios)
      u$flow_capital = u$flow_capital-u$Estaciones_A*as.numeric(input$charginmodulecostA)-u$Estaciones_A*as.numeric(input$charginmodulecostB)
      u$flow_capital = u$flow_capital-u$cambios*2*as.numeric(input$kwhperbattery)*as.numeric(input$kwhprice)
      u$flow_capital = u$flow_capital-cumsum(rep(as.numeric(input$extracost),length(u$flow_capital)))
      ### Entradas
      precio_promedio = 0.43*as.numeric(input$membership1)+0.36*as.numeric(input$membership2)+0.2*as.numeric(input$membership3)+0.01*as.numeric(input$membership4)
      precio_promedio = precio_promedio + 50*0.049 + 400*0.09 + 30*38*0.068
      u$flow_capital = u$flow_capital+cumsum(u$usuarios)*precio_promedio
      salidas_por_estaciones = as.numeric(input$extracostmoduleA)*u$Estaciones_A+as.numeric(input$extracostmoduleB)*u$Estaciones_B
      salidas_por_estaciones = cumsum(salidas_por_estaciones)
      u$flow_capital = u$flow_capital - salidas_por_estaciones
      
      #Salidas por depreciacion
      u$compra_modulos_A = c(u$Estaciones_A[1],diff(u$Estaciones_A))
      u$compra_modulos_B = c(u$Estaciones_B[1],diff(u$Estaciones_B))
      ## modulos
      u$EA_d <- shift(u$compra_modulos_A,12*as.integer(input$chargingdepreciation),default=0)
      u$EB_d <- shift(u$compra_modulos_B,12*as.integer(input$chargingdepreciation),default=0)
      for(i in 2:6){
        u$EA_d <- u$EA_d+shift(u$compra_modulos_A,i*12*as.integer(input$chargingdepreciation),default=0)
        u$EB_d <- u$EB_d+shift(u$compra_modulos_B,i*12*as.integer(input$chargingdepreciation),default=0)
      }
      u$EA_d = cumsum(u$EA_d)
      print(u$EA_d*as.numeric(input$charginmodulecostA))
      u$EB_d = cumsum(u$EB_d)
      u$flow_capital = u$flow_capital - u$EA_d*as.numeric(input$charginmodulecostA) - u$EB_d*as.numeric(input$charginmodulecostB)
      #baterias
      u$n_baterias = c(u$n_baterias[1],diff(u$n_baterias))
      u$bd <- shift(u$n_baterias,12*as.integer(input$batterydepreciation),default=0)
      for(i in 2:6){
        u$bd <- u$bd + shift(u$n_baterias,i*12*as.integer(input$batterydepreciation),default=0)
      }
      print(u$n_baterias)
      u$bd <- cumsum(u$bd)
      u$flow_capital <- u$flow_capital-u$bd*as.numeric(input$costperbattery)

      

      
      #en millones de pesos
      u$flow_capital = u$flow_capital/1000000
      u$Fecha <- as.Date(ISOdate(2018+u$Anyo,u$Mes,1))
      dr = input$dr_modelo
      u = u[u[,"Fecha"] >= dr[1] & u[,"Fecha"]<=dr[2],]
      plot(u$Fecha,u$flow_capital, 
           type = "l", 
           lty  = "dashed",
           col  = "red", 
           xlab = 'Año',
           ylab = "Flujo (Millones de pesos)",
           main = 'Flujo de capital'
      )
      grid(nx=NA, ny= NULL, col='grey',lwd=2)
      
    })
    

    
    output$motos <- renderPlot({
      u <-input$escenario_modelo
      u <- obtener_escenario(as.integer(u),input$dr_modelo,input$tipo_modelo,input$tasa_modelo,in_modelo = TRUE)
      u$ventas_motos = u$usuarios
      u$ventas_motos = c(u$ventas_motos[1],diff(u$ventas_motos))
      u$ventas_motos = u$ventas_motos*(as.numeric(input$sellingprice)-as.numeric(input$costperbike))
      u$ventas_motos = u$ventas_motos/1000000
      u$Fecha <- as.Date(ISOdate(2018+u$Anyo,u$Mes,1))
      dr = input$dr_modelo
      u = u[u[,"Fecha"] >= dr[1] & u[,"Fecha"]<=dr[2],]
      barplot(names.arg=format(u$Fecha,'%Y %m'),height=u$ventas_motos, 
           type = "b", 
           #lty  = "dashed",
           col  = "red", 
           xlab = 'Año',
           ylab = "Ganancia Bruta (Millones de pesos)",
           main = 'Motorcicle Sales'
      )
      grid(nx=NA, ny= NULL, col='grey',lwd=2)
    })
    output$planes <- renderPlot({
      u <-input$escenario_modelo
      u <- obtener_escenario(as.integer(u),input$dr_modelo,input$tipo_modelo,input$tasa_modelo,in_modelo = TRUE)
      precio_promedio = 0.43*as.numeric(input$membership1)+0.36*as.numeric(input$membership2)+0.2*as.numeric(input$membership3)+0.01*as.numeric(input$membership4)
      precio_promedio = precio_promedio + 50*0.049 + 400*0.09 + 30*38*0.068
      u$planes <- u$usuarios*precio_promedio
      u$planes = u$planes/1000000

      u$Fecha <- as.Date(ISOdate(2018+u$Anyo,u$Mes,1))
      dr = input$dr_modelo
      u = u[u[,"Fecha"] >= dr[1] & u[,"Fecha"]<=dr[2],]
      barplot(names.arg=format(u$Fecha,'%Y %m'),height=u$planes, 
              type = "b", 
              #lty  = "dashed",
              col  = "red", 
              xlab = 'Año',
              ylab = "Ganancia Bruta (Millones de pesos)",
              main = 'Ganancia por planes'
      )
      grid(nx=NA, ny= NULL, col='grey',lwd=2)
    })
    output$entradas_salidas <- renderPlot({
      u <-input$escenario_modelo
      u <- obtener_escenario(as.integer(u),input$dr_modelo,input$tipo_modelo,input$tasa_modelo,in_modelo = TRUE)
      #entradas por motos
      u$ventas_motos = u$usuarios
      u$ventas_motos = c(u$ventas_motos[1],diff(u$ventas_motos))
      u$ventas_motos = u$ventas_motos*(as.numeric(input$sellingprice)-as.numeric(input$costperbike))
      u$entradas_por_mes = u$ventas_motos
      #salidas por baterias
      u$n_baterias = c(u$n_baterias[1],diff(u$n_baterias))
      u$entradas_por_mes = u$entradas_por_mes - as.numeric(input$costperbattery)*u$n_baterias
      #entrada por planes y cargas extra
      precio_promedio = 0.43*as.numeric(input$membership1)+0.36*as.numeric(input$membership2)+0.2*as.numeric(input$membership3)+0.01*as.numeric(input$membership4)
      precio_promedio = precio_promedio + 50*0.049 + 400*0.09 + 30*38*0.068
      u$planes <- u$usuarios*precio_promedio
      u$entradas_por_mes = u$entradas_por_mes +  u$planes
      #salidas por compra de modulos
      u$compra_modulos_A = c(u$Estaciones_A[1],diff(u$Estaciones_A))
      u$compra_modulos_B = c(u$Estaciones_B[1],diff(u$Estaciones_B))
      u$entradas_por_mes = u$entradas_por_mes-u$compra_modulos_A*as.numeric(input$charginmodulecostA)-u$compra_modulos_B*as.numeric(input$charginmodulecostB)
      #Salidas por energia
      u$entradas_por_mes = u$entradas_por_mes-u$cambios*2*as.numeric(input$kwhperbattery)*as.numeric(input$kwhprice)      
      #Salidas por extras mensuales
      u$entradas_por_mes = u$entradas_por_mes -as.numeric(input$extracost)
      #Salidas por costos de modulos
      u$entradas_por_mes = u$entradas_por_mes - u$Estaciones_A*as.numeric(input$extracostmoduleA) - u$Estaciones_B*as.numeric(input$extracostmoduleB)
      
      
      #Salidas por depreciacion
      ## modulos
      u$EA_d <- shift(u$compra_modulos_A,12*as.integer(input$chargingdepreciation),default=0)
      u$EB_d <- shift(u$compra_modulos_B,12*as.integer(input$chargingdepreciation),default=0)
      for(i in 2:6){
        u$EA_d <- u$EA_d+shift(u$compra_modulos_A,i*12*as.integer(input$chargingdepreciation),default=0)
        u$EB_d <- u$EB_d+shift(u$compra_modulos_B,i*12*as.integer(input$chargingdepreciation),default=0)
      }
      u$entradas_por_mes = u$entradas_por_mes - u$EA_d*as.numeric(input$charginmodulecostA) - u$EB_d*as.numeric(input$charginmodulecostB)
      #baterias
      u$bd <- shift(u$n_baterias,12*as.integer(input$batterydepreciation),default=0)
      for(i in 2:6){
        u$bd <- u$bd + shift(u$n_baterias,i*12*as.integer(input$batterydepreciation),default=0)
      }
      u$entradas_por_mes <- u$entradas_por_mes-u$bd*as.numeric(input$costperbattery)
      
      #filtro por fecha
      u$Fecha <- as.Date(ISOdate(2018+u$Anyo,u$Mes,1))
      dr = input$dr_modelo
      u = u[u[,"Fecha"] >= dr[1] & u[,"Fecha"]<=dr[2],] 
      
      #en millones de pesos
      u$entradas_por_mes = u$entradas_por_mes/1000000
      plot(u$Fecha,u$entradas_por_mes, 
              type = "l", 
              lty  = "dashed",
              col  = "red", 
              xlab = 'Año',
              ylab = "Entradas-Salidas (Millones de pesos)",
              main = 'Entradas - Salidas por mes'
      )
      grid(nx=NA, ny= NULL, col='grey',lwd=2)
    })
    
    output$tco <- renderPlot({
      n_semanas <- as.integer(as.numeric(input$n_meses)*30.5/7)
      v.1 = c(1:n_semanas)
      v.2 = cumsum(v.1)
      df <- data.frame(v.1,v.2)
      colnames(df) = c('Semanas',"KM")
      df$KM <- rep(input$kmrecorridos,nrow(df))
     
      df$KM <- cumsum(df$KM)
      df$TCO_gasolina <- (df$KM/as.numeric(input$rendimiento))*as.numeric(input$oilcost)
      df$TCO_gasolina <- df$TCO_gasolina + as.numeric(input$costbykeoil)
      u <- merge(df,serv_gas,by=NULL)
      u <- u[u$KM.x<=u$KM.y,]
      u$Servicio2 <- shift(u$Servicio,1,invert = TRUE)
      u$Servicio3 <- u$Servicio2 - u$Servicio
      u <- u[u$Servicio3 == 1, ]
      u <- merge(df,u[,c("KM.x","Servicio","Total")],by.x='KM',by.y='KM.x', all = TRUE)
      u <- u[u$Semanas<n_semanas,]
      u <- u %>%
        mutate(Total = if_else(is.na(Total), 0, Total))

      df <- u
      df$Total <- cumsum(df$Total)
      df$TCO_gasolina <- df$TCO_gasolina + df$Total
      u$TCO_electrico <- rep(0,nrow(u))
      u$TCO_electrico <- u$TCO_electrico + as.numeric(input$costbykeelectric)
      plan1_km = as.numeric(input$p1_cargas)*as.numeric(input$kmporcarga)
      plan2_km = as.numeric(input$p2_cargas)*as.numeric(input$kmporcarga)
      plan3_km = as.numeric(input$p3_cargas)*as.numeric(input$kmporcarga)
      plan4_km = as.numeric(input$p4_cargas)*as.numeric(input$kmporcarga)
      km_necesarios = 30.5*as.numeric(input$kmrecorridos)/7.0
      recargas_necesarias = as.integer(km_necesarios+as.numeric(input$kmporcarga))%/%as.numeric(input$kmporcarga)  
      if(recargas_necesarias>20){
        plan = 4
        precio = input$p4_precio
      }
      else{
        if(recargas_necesarias>10){
          plan = 3
          precio = input$p3_precio
        }
        else{
          if(recargas_necesarias>5){
            plan = 2
            precio = input$p2_precio
          }
          else{
            plan = 1
            precio = input$p1_precio
          }
        }
      }
      precio_semanal = as.numeric(precio) * 7.0/30.5
      
      u$cobro_semanal = rep(precio_semanal,nrow(u))
      u$Servicio <- as.double(u$Servicio)
      u <- u %>%
        mutate(Servicio = if_else(is.na(Servicio), 0, Servicio))
      
      u$TCO_electrico <- u$TCO_electrico + cumsum(u$cobro_semanal)
      u <- u[,c("Semanas",'Servicio','TCO_electrico','TCO_gasolina')]
      u <- left_join(u,serv_elec[,c("Servicio","Total")],by = "Servicio")
      u <- u %>%
        mutate(Total = if_else(is.na(Total), 0, Total)) 
      u$Total <- cumsum(u$Total)
      u$TCO_electrico <- u$TCO_electrico + u$Total
      
      print(u)
      plot(df$Semanas,df$TCO_gasolina, type = "l", 
           lty  = "dashed",
           col  = "red", 
           xlab = 'Semana',
           ylab = "Cost",
           main = 'TCO',
           #xlim = c(min(df$Semanas),max(df$Semanas)),
           ylim = c(min(min(u$TCO_electrico),min(df$TCO_gasolina)),max(max(u$TCO_electrico),max(df$TCO_gasolina)))
      )
      lines(u$Semanas,u$TCO_electrico,type = "l",
            lty  = "dashed",
            col  = "blue", 
            xlab = 'Semana',
            ylab = "Cost",
            main = 'TCO')
      legend('topleft','groups', legend=c("TCO_gasolina", "TCO_electrico"),
             col=c("red", "blue"), lty=1:2, cex=0.8,lwd = 2)
    })
    
    
}

