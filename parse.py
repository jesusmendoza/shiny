### Parsing los archivos obtenidos de la simulación para ser leidos por el shiny
### Debe:
# 1. Crear un archivo con las ubicaciones de tooooodas las estaciones hasta 50000 usuarios
# 2. Ordenar las estaciones de acuerdo a las epocas con respecto al número de usuarios
# 3. A cada epoca obtener el número de usuarios por hora para cada estación, considerar que el tiempo de espera debe ser menor al tiempo en el que el usuario se va a otra estación
# 4. 

import os
import pandas as pd

path = '../'

directorios = os.listdir()
directorios_ = []
n_usuarios = []
n_estaciones = []
for d in directorios:
	if d[-4:] != '.csv':
		directorios_.append(d)
		n_usuarios.append(int(d.split('=')[1].split('n')[0]))
		n_estaciones.append(int(d.split('=')[2]))
directorios = pd.DataFrame(list(zip(directorios_,n_usuarios,n_estaciones)))
directorios




