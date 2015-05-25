##-------------------------------------------------##
##-----------      Modelos Linelaes     -----------##
##-----------        Trabajo 01         -----------##
##-- Nombre: Katherine Morales


# 2.1 Leer el archivo de datos data.txt, y analizar de que estructura de datos se trata.
# Utilice la función read.table()
data<- read.table("data.txt", header = TRUE, dec=",", sep="\t")
str(data)

# 2.2 Calcular el mínimo, la media, el máximo de la variable Edad.
# Utilice las funciones min(), mean(), max(), de ser necesario utilice 
# el parámetro na.rm = TRUE
edad<-data[,"Edad"]
minimo<-min(edad,na.rm =T )
maximo<-max(edad, na.rm =T)
media<-mean(edad,na.rm = T)
minimo
maximo
media
# 2.3 Para la variable Genero, contar cuantos sujetos son de Genero: Femenino.
# Utilice la función table()
data_femenino<-subset(data, subset=data[,"Genero"]=="Femenino")
#table(data[,"Genero"])
table(data_femenino[,"Genero"])
num_femeninos<-nrow(data_femenino)
num_femeninos
# 2.4 Encontrar la Edad mínima, media, máxima de los sujetos que Si son dependientes.

data_dependiente<-subset(data, subset=data[,"Dependiente"]=="Si")
#table(data[,"Dependiente"])
table(data_dependiente[,"Dependiente"])
edad_dep<-data_dependiente[,"Edad"]
minimo_dep<-min(edad_dep,na.rm =T )
maximo_dep<-max(edad_dep, na.rm =T)
media_dep<-mean(edad_dep,na.rm = T)
minimo_dep
maximo_dep
media_dep

# 2.5 Identificar el tipo de elementos que contiene cada variable.
# Utilice la función typeof()

tipo<-numeric(ncol(data))
for(i in 1:ncol(data)){
  tipo[i]<-typeof(data[,i])
}
tipo


# 2.6 Identificar la clase de cada variable (columna).
# Utilice la función class()
clase<-numeric(ncol(data))
for(i in 1:ncol(data)){
  clase[i]<- class(data[,i])
}
clase

# 2.7 Calcular la media de todas las variables numéricas (double, integer).
# Recordar que para un factor no es posible obtener la media debido a que 
# éstos representan variables
media <- numeric(ncol(data))
for (i in 1:ncol(data)){
  media[i] <- mean(data[,i],na.rm = T)
}
media

# 2.8 Calcular el porcentaje de valores perdidos que contiene cada variable.
# Utilice la función is.na()
val_perd<-numeric(ncol(data))
porcentaje<-numeric(ncol(data))
total<-nrow(data)
for(i in 1: ncol(data)){
  val_perd[i]<-sum(is.na(data[,i]))
  #porcentaje[i]<-(val_perd[i]/total)
  
}
val_perd
prop.table(val_perd)
#porcentaje
# 3. Selecionando sujetos mediante un determinado criterio:
# 3.1 Seleccione los sujetos con una Edad mayor a 40 años.
# Utilice la función subset()
data_criterio1<-subset(data, subset=data[,"Edad"]>40)
table(data_criterio1[,"Edad"])
n<-nrow(data_criterio1)
n
#En total se tiene n personas que satisfacen una edad mayor a 40 años


# 3.2 Seleccione los sujetos que tienen Vivienda Propia.

data_criterio2<-subset(data, subset=data[,"Vivienda"]=="Propia")
table(data[,"Vivienda"])
table(data_criterio2[,"Vivienda"])
m<-nrow(data_criterio2)
m
#En total se tiene m personas que satisfacen tener vivienda propia


# 3.3 Seleccione los sujetos que tienen más ($>$) de dos cargas familiatres.
data_criterio3<-subset(data, subset=data[,"Cargas"]>2)
table(data[,"Cargas"])
table(data_criterio3[,"Cargas"])
p<-nrow(data_criterio3)
p

#En total se tiene p personas que satisfacen tener más de dos cargas


# 3.4 Seleccione los sujetos con una Deuda superior o igual a 500 dólares
# y más ($>$) de 8 Dias_Atraso.

data_criterio4<-subset(data,(subset=data[,"Deuda"]>=500)&(subset =data[,"Dias_Atraso"]>8))
table(data_criterio4[,"Deuda"], data_criterio4[,"Dias_Atraso"])


# 3.5 Seleccione los sujetos con un Score mayor o igual a 900 puntos, una Edad menor
# o igual a 35 años y con más ($>$) de 3 tarjetas de crédito (Numero_TC).

data_criterio5<-subset(data,(subset=data[,"Score"]>="900")&(subset =data[,"Edad"]<="35")&(subset =data[,"Numero_TC"]>3))
table(data_criterio5[,"Score"], data_criterio5[,"Edad"],data_criterio5[,"Numero_TC"])


# 4. Gráficos:
# 4.1 Realice un histograma de la variable Edad, utilice como color de relleno: red

hist(edad, col = "red")

# 4.2 Realice un diagrama de cajas de la variable Edad, utilice como color de relleno: green
# Utilice la función boxplot()
boxplot(edad, col = "green")
