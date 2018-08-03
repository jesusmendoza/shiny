library(rsconnect)
rsconnect::deployApp('../shiny/')

ubec = read.csv('data/relacion_ubec.csv')
resultado_general = read.csv('data/resultado_general.csv')
resultado_general_be = read.csv('data/resultado_general_be.csv')


