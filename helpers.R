# Note: percent map is designed to work with the counties data set
# It may not work correctly with other data sets if their row order does 
# not exactly match the order in which the maps package plots counties

source("global.R")

devolver_estaciones <- function(n){
  
  shape = obtener_estaciones(n)
  m = leaflet(data = shape) %>%
    addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
             attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%  # Add default OpenStreetMap map tiles
    addMarkers(.,popup = shape$indice) 
    
  return(m)
}

obtener_estaciones <- function(n){
  n <- as.integer(n)
  n = obtener_usuarios(n)
  dsn_ = paste("data/estaciones/")
  shape <- readOGR(dsn = dsn_)
  return(head(shape,n))
}

obtener_usuarios <- function(n){
  
  if(n<10){n<-10}
  if(n>45990){n <- 45990}
  n <- 10 * (n%/%10)
  n <- ubec[ubec[,'usuarios']==n,c('n_estaciones')]
  return(n)
}

obtener_id <- function(n){
  shape = obtener_estaciones(n)
  u <- data.frame(shape)
  n <- u[u[,'indice']==n,c('id_left')]
  
  n <- as.numeric(as.character(n))
  return(n)
}

obtener_escenario <- function(n,dr,tipo, tasa,in_modelo = FALSE){
  if (in_modelo == TRUE){
    dr = c( min = as.Date("2019-01-01"), max = as.Date("2026-01-01"))
  }
  u <- crecimiento
  if (tipo == 1){
    n <- n/50000
    u$usuarios <- 10 * as.integer(crecimiento$usuarios * n)
  }
  else{
    t <- as.integer(tasa)
    t = t/100.0
    beta = exp((1.0/12.0)*log(1+t))-1.0
    A = n/(((1+beta)**12-1)/beta)
    v = rep(A,12*7)
    v2 = 1:(12*7)
    v <- v*(1+beta)**v2
    v <- 10*as.integer(round(v/10))
    v <- cumsum(v)
    u$usuarios <- v
  }
  
  u <- merge(u,ubec)
  u$Fecha <- as.Date(ISOdate(2018+u$Anyo,u$Mes,1))
  u = u[u[,"Fecha"] >= dr[1] & u[,"Fecha"]<=dr[2],]
  u$Estaciones_A = u$n_estaciones
  u$Estaciones_B = (((u$n_baterias-3*u$Estaciones_A)+abs(u$n_baterias-3*u$Estaciones_A))/2)%/%4
  u$n_baterias <- u$n_baterias*2 + 2*u$usuarios
  u$rBU <- u$n_baterias/u$usuarios
  return(u)
}



shift <- function(x, n, invert=FALSE, default=0){
  if (length(x)>n){
  if(n==0){
    return(x)
  }
  n <- ifelse(invert, n*(-1), n)
  if(n<0){
    n <- abs(n)
    forward=FALSE
  }else{
    forward=TRUE
  }
  if(forward){
    return(c(rep(default, n), x[seq_len(length(x)-n)]))
  }
  if(!forward){
    return(c(x[seq_len(length(x)-n)+n], rep(default, n)))
  }
  }
  else{
    return(rep(0,length(x)))
    }
}





