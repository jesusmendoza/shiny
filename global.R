# Load packages ----
library(shiny)
library(maps)
library(mapproj)
library(leaflet)
library(PBSmapping)
library(shinythemes)
library(rgdal)
library(htmltools)
library(dplyr)
library(graphics)
# Load data ----

ubec = read.csv('data/relacion_ubec.csv')
resultado_general = read.csv('data/resultado_general.csv')
resultado_general_be = read.csv('data/resultado_general_be.csv')
crecimiento = read.csv('data/crecimiento_elektra.csv')
serv_elec = read.csv('data/servicios_electrico.csv')
serv_gas = read.csv('data/servicios_gasolina.csv')