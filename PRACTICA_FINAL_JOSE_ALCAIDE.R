###############################PRACTICA DE JOSE ALCAIDE###############################3
#Necesitamos cargar estas liberias para poder realizar la practica.
install.packages('lattice')
install.packages('ggplot2')
install.packages('bit64')
install.packages('Rcpp')
install.packages('colorspace')
install.packages('stringi')
install.packages('dplyr')
install.packages('purrr')
install.packages('gower')


suppressPackageStartupMessages () 

library(ggplot2)
library(lattice)
library(caret)
library(data.table)
library(corrplot)
library(dplyr)
library(ROCR)
library(scales)
library(lubridate)
library(pROC)
library(lmtest)
library(faraway)
library(gplots)
library(bit64)









#Empezamos a trabajar con el archivo

summary(train)
str(train)

#PRIMERAS CONCLUSIONES, A SIMPLE VISTA
#NOTA IMPORTANTE, EL NUMERO TOTAL DE VARIABLES SI COINCIDE EN AMBOS CASOS(EXCEL DE INFORMACION Y N DE VARIABLES DE ESTUDIO)
#EN EL EXCEL APARACE 2 VECES LA MISMA VARIABLE "Dist_Sum_NAL" Y "Dist_sum_NAL" Y TIENEN EL MISMO CONCEPTO EXPLICATIVO
# Distancia máxima recorrida a nivel nacional (en millas). pero en el TRAIN aparece una variable que no
#aparece en el EXCEL explicativo y es "Dist_Max_INTER"
#MAS ADELANTE VEMOS COMO ACTUAMOS CON ESAS VARIABLES



##Necesitamos cambiar de formato la siguientes variables

train$id <- as.character(train$id)
train$VALOR <- as.integer(train$VALOR)
train$FRAUDE <- as.factor(train$FRAUDE)
train$DIASEM <- as.factor(train$DIASEM)
train$NROPAISES <- as.integer(train$NROPAISES)


##########PRESCINDIMOS DE ESTAS VARIABLES, YA QUE NO SON UTILES PARA NUESTRO ESTUDIO
train$OFICINA_VIN <-  NULL#OFICINA DE VINCLULACION=LA VAMOS A QUITAR PUES NO NOS APORTA 
#INFORMACION RELEVANTE PORQUE NO SE DONDE SE ENCUENTRAN

####################################################################################################
#######################ESTUDIOS DE POSIBLES REALCIONES ENTRE VARIABLES##########################
#AL SER DOS VARIABLES CATEGORICAS, PARA VER SI SON INDEPENDIENTES USAMOS ESL TESt DE LA CHI CUADRADO

table(train$CANAL)
table(train$Canal1)
t.canal <- table(train$CANAL, train$Canal1)
chisq.test(t.canal)
##H0=SON INDEP
##H1=NO SON INDEP
#p-value < 2.2e-16, por lo tanto rechazo HO,NO SON INDEPENDIENTES, POR TANTO, SON DEPENDIENTES,
#PRESCINDIMOS DE  CANAL 1

train$Canal1 <- NULL
###########################################################################################
#################ESTUDIAR LA CORRELACION ENTRE VARIABLES FECHAS############################
str(train)

m_corr_FECHAS<- train[, c('FECHA','FECHA_VIN', 'DIAMES')]


cor(m_corr_FECHAS, use='pairwise.complete.obs') #obtenemos la matriz de correlaciones



m.cor_FECHAS<- cor(m_corr_FECHAS, use='pairwise.complete.obs')
corrplot.mixed(m.cor_FECHAS)


#por lo tanto, existe una correlacion perfecta entre DIAMES Y FECHA, PRESCINDIMOS DE DIA MES.

train$DIAMES <- NULL

######################################################################################################
#######ESTUDIAR LA CORRELACION ENTRE LAS VARIABLES DISTANCIAS####################################


m_corr_DIST <-train[, c('Dist_Sum_INTER', 'Dist_Mean_INTER', 
                        'Dist_Max_INTER', 'Dist_Mean_NAL','Dist_HOY',
                        'Dist_sum_NAL', 'Dist_max_NAL')]
cor(m_corr_DIST, use='pairwise.complete.obs') #obtenemos la matriz de correlaciones



m.cor_DIST<- cor(m_corr_DIST, use='pairwise.complete.obs')
corrplot.mixed(m.cor_DIST)
#EXISTE UNA CORRELACION ALTA Y POSITIVA ENTRE Dist_Max_INTER Y Dist_Mean_INTER, PRESCINDIMOS DE:

train$Dist_Max_INTER <- NULL 



######################################################################################################
#####################################################################################################
#######################MAS CONCLUSIONES EXTRAIDAS DEL SUMMARY############################################3
#EXISTEN VARIABLES CON VALORES AUSENTES COMO SEXO=55 Y SEGMENTO = 24
#EXITEN VARIABLES CON NA`S :FECHA_VIN= 24
                            #OFICINA_VIN=24
                            #EDAD = 24
                            #INGRESOS=24
                            #EGRESOS =24
                            #Dist_Sum_INTER=1547 
                            #Dist_Mean_INTER=1547
                            #Dist_Max_INTER=1547
                            #Dist_Mean_NAL=457


##MEDIDAS A TOMAR:
#vamos a tratar la variable id para que no nos desvirtue el modelo de prediccion
#PARA LAS VARIABLES SEXO Y SEGMENTO CREAREMOS UN NUEVA CLASE
#EDAD: LA VAMOS A CATEGORIZAR INTERVALOS DE EDADES, PARA QUE SEA MAS UTIL EN EL MODELO 
#INGRESOS Y EGRESOS SUSTITUIREMOS LOS NAS CON LA MEDIANA DE CADA VARIABLE, Y
#A QUE ES UN ESTADISTICO MAS ROBUSTO A LA VARIABILIDAD Y DESSPUES LE APLICAREMOS LOGARITMO PORQUE TIENE MUCHA DIFERNCIA ENTRE LOS VALORES PEQUEÑOS Y GRANDE
#CON LAS DISTANCIAS LAS ESCALAMOS COMO UNA NORMAL, PORQUE TIENEN VALORES MUY CONCENTRADOS, ASI NOS MEJORA EL MODELO


##########################################################################################
#######################################ALGUNOS GRAFICOS################################
plot(train$EGRESOS)#LAS DISTRIBUCIONES, NO NOS APORTAN NADA, TENDREMOS QUE HACER ALGUN TIPO DE TRANSFORMACION
plot(train$INGRESOS)
hist(train$EGRESOS)#desplazada hacia la izq 

plot(train$Dist_Mean_INTER)##no sigue ninguna distribucion, habra que aplicarles alguna transformacion#estaha sesgada hacia la derecha
plot(train$Dist_Mean_NAL)  #no tiene distribucion conocida

hist(train$Dist_Mean_NAL, col = rainbow(12), ###12 divisiones, y cada division 1 color distinto
     xlab = "Distancia media nacinal",
     ylab = "Frecuncias",
     main = "Histograma de las distancias medias",
     breaks = 12)


barplot(prop.table(table(train$FRAUDE)), col=c('red', 'green'),
        legend.text=c('NO FR', 'SI FR'),
        xlim = c(0,05))

barplot(prop.table(table(train$DIASEM)),
        ylim=c(0,0.2),
        legend.text=c('0=DO', '1=LU', '2=MA', '3=MI', '4=JU', '5=VI', '6=SA'),
        xlim = c(0,14),
        col = rainbow(7),
        main = 'FRECUENCIAS DE FRAUDE EN DIA DE LA SEMANA')


boxplot(train$EDAD)##parece que hay muchos outliers por arriba y pot abajo...


#################################################################################
#HACER TRANSFORMACIONES EN LA VARIABLE id PARA QUITARLA, YA QUE NO APORTA NADA
#################################################################################
table(apply(train, 1, function(x) sum(is.na(x))))##UNA FUNCION PARA AVERIGUAR CUANTOS NAS HAY DENTRO DEL traiN, por frecuencias
nrow(train)-length(unique(train$id))#elimino ids duplicados. Los localizamos con la ID
train_x <- train[!duplicated(train)]#eliminamos las filas iguales con todas las variables iguales, es decir, quitamos 5 
nrow(train_x)-length(unique(train_x$id))####AVERIGUAMOS CUANTAS FILAS NOS QUEDAN CON LOS ID REPETIDO
train_id <- train_x[duplicated(train_x$id)]
#son el conjunto de duplicados, pero pueden tener distintos registros
#la misma persona que ha hecho transacciones diferentes

repetidos_id <- train_x[train_x$id %in% train_id$id,]
#quiero ver dónde están duplicados. me quedo con todos
#los id que tienen id duplicadas,vemos qué valor varía. precisamente es VALOR.
#todos los datos iguales en los id repetidos excepto en valor. Vamos a quedarnos con los que FRAUDE=1,
#y vamos a quedarnos con la fecha mínima , el primer fraude que ha cometido, para saber que en las
#posteriores nos pueden salir cosas raras y saber que venía de fraude
table(repetidos_id$id, repetidos_id$FRAUDE)
#vemos que en los resgistros duplicados se mantiene la clasificacion
#de Fraude. No hay ninguno que sea en uno Fraude = 1 y en otro =0
table(train$SEGMENTO, useNA = 'always') #hay valores vacíos (24)) Y EN QUE SEGMENTO ESTA
data_max_h_1 <- repetidos_id[FRAUDE==1, .(HORA_AUX=min(HORA_AUX)), by= id]

#Fraude no está repetida en los repetidos_id. Quiero ver en qué variables se repite. 
#y veo que lo que varía es el VALOR, nos quedamnos con la transaccion que han hecho primero
data_max_h_1 <- merge(data_max_h_1, repetidos_id)##unimos las 2 matrices 
fraude_1 <- data_max_h_1[,.SD[sample(.N, min(1,1))], by = id]
#cojemos aleatoriamente nos quedamos con uno de los valores repetidos de la hora mas temprana de cometer fraude
#de los dos registros en los que para id identico, la hora está duplicada
data_max_h_0 <- repetidos_id[FRAUDE==0,.SD[sample(.N, 1)], by=id]
fraude_0 <- merge(repetidos_id, data_max_h_0)
fraude_01 <- rbind(fraude_1, fraude_0)
no_repetidos_id <- train_x[!train_x$id %in% train_id$id,]
fraude_ok <- rbind(no_repetidos_id, fraude_01)
fraude_ok$id <-  NULL##eliminamos la variable id

##################################################################################################
#ELIMINACION DE NA´S, VALORES MISSING, TRANSFORMACION DE VARIABLES A CATEGORICAS, A FORMATO FECHA, OTRAS 
#TRANSFORMACIONES COMO LOGARITMICAS, NORMALIZACION DE VARIABLES, ALGUNOS GRAFICOS EXPLICATIVOS

########################################################################################################
#CREAR UNA NUEVA CATEGORIA PARA LAS VARIABLES SEXO Y SEGMENTO PARA QUITARNOS LOS VALORES AUSENTES
####################################################################################################
fraude_ok$SEXO<- factor(fraude_ok$SEXO, levels = c('F', 'M', 'OTRO'))
fraude_ok$SEXO[is.na(fraude_ok$SEXO)] <- 'OTRO'

fraude_ok$SEGMENTO<- factor(fraude_ok$SEGMENTO, levels = c('Emprendedor', 'Empresarial', 
                                                           'PYME','Personal', 'Personal Plus', 'Preferencial',
                                                           'OTRO'))
fraude_ok$SEGMENTO[is.na(fraude_ok$SEGMENTO)] <- 'OTRO'
###########################################################################################################
#las variable edad,HORA_AUX y NROCIUDADES: la convertimos en una categorica, para trabajarla mejor
#y quitarnos los NA´s
########################################################################################################3
table(fraude_ok$EDAD)
fraude_ok$EDAD[is.na(fraude_ok$EDAD)] <- median(fraude_ok$EDAD, na.rm = T)

gedad <- vector()
gedad[fraude_ok$EDAD <= 10] <- 1
gedad[fraude_ok$EDAD >= 11 & fraude_ok$EDAD < 16] <- 2
gedad[fraude_ok$EDAD >= 16 & fraude_ok$EDAD< 20] <- 3
gedad[fraude_ok$EDAD >= 20 & fraude_ok$EDAD< 25] <- 4
gedad[fraude_ok$EDAD >= 25 & fraude_ok$EDAD< 30] <- 5
gedad[fraude_ok$EDAD >= 30 & fraude_ok$EDAD< 35] <- 6
gedad[fraude_ok$EDAD >= 35 & fraude_ok$EDAD< 40] <- 7
gedad[fraude_ok$EDAD >= 40 & fraude_ok$EDAD< 45] <- 8
gedad[fraude_ok$EDAD >= 45 & fraude_ok$EDAD< 50] <- 9
gedad[fraude_ok$EDAD >= 50 & fraude_ok$EDAD< 55] <- 10
gedad[fraude_ok$EDAD >= 55 & fraude_ok$EDAD< 60] <- 11
gedad[fraude_ok$EDAD >= 60 & fraude_ok$EDAD< 65] <- 12
gedad[fraude_ok$EDAD >= 65 & fraude_ok$EDAD< 70] <- 13
gedad[fraude_ok$EDAD >= 70 & fraude_ok$EDAD< 75] <- 14
gedad[fraude_ok$EDAD >= 75 & fraude_ok$EDAD< 80] <- 15
gedad[fraude_ok$EDAD >= 80 & fraude_ok$EDAD< 85] <- 16
gedad[fraude_ok$EDAD >= 85] <- 17


summary(fraude_ok$EDAD)



fraude_ok$EDAD <- as.factor(gedad)#la convertimos a factor, con las etiquitas de abajo

levels(fraude_ok$EDAD) <- c("0 a 15", "16 a 19", "20 a 24", "25 a 29", '30 a 34', '35 a 39', '40 a 44', '45 a 49',
                           '50 a 54', '55 a 59', '60 a 64', '65 a 69', '70 a 74', '75 a 79', '80 a 84', 'Mas de 85')
table(fraude_ok$EDAD)##verificamos que todo esta ok

#######################################################################################################


table(fraude_ok$HORA_AUX)
class(fraude_ok$HORA_AUX)

gHORA_AUX <- vector()
gHORA_AUX[fraude_ok$HORA_AUX <=2] <- 1
gHORA_AUX[fraude_ok$HORA_AUX >= 3 & fraude_ok$HORA_AUX< 5] <- 2
gHORA_AUX[fraude_ok$HORA_AUX >= 5 & fraude_ok$HORA_AUX< 7] <- 3
gHORA_AUX[fraude_ok$HORA_AUX >= 7 & fraude_ok$HORA_AUX< 9] <- 4
gHORA_AUX[fraude_ok$HORA_AUX >= 9 & fraude_ok$HORA_AUX< 11] <- 5
gHORA_AUX[fraude_ok$HORA_AUX >= 11 & fraude_ok$HORA_AUX< 13] <- 6
gHORA_AUX[fraude_ok$HORA_AUX >= 13 & fraude_ok$HORA_AUX< 15] <- 7
gHORA_AUX[fraude_ok$HORA_AUX >= 15 & fraude_ok$HORA_AUX< 17] <- 8
gHORA_AUX[fraude_ok$HORA_AUX >= 17 & fraude_ok$HORA_AUX< 19] <- 9
gHORA_AUX[fraude_ok$HORA_AUX >= 19 & fraude_ok$HORA_AUX< 21] <- 10
gHORA_AUX[fraude_ok$HORA_AUX >= 21 & fraude_ok$HORA_AUX< 23] <- 11
gHORA_AUX[fraude_ok$HORA_AUX >= 23] <- 12


summary(fraude_ok$HORA_AUX)

fraude_ok$HORA_AUX <- as.factor(gHORA_AUX)
levels(fraude_ok$HORA_AUX) <- c("0 a 2", "3 a 4", "5 a 6", "7 a 8", '9 a 10', '11 a 12', '13 a 14', '15 a 16','17 a 18',
                                '19 a 20', '21 a 22', 'Mas de 23')

table(fraude_ok$HORA_AUX)

plot(fraude_ok$HORA_AUX, col=14,
     main= 'HORA EN QUE SE PRODUCE EL FRAUDE',
     xlab= 'Franjas Horarias',
     ylab= 'Frecuencias')##a la hora donde mas se comete fraude es entre las 17 y 18 horas



########################################################################################
table(fraude_ok$NROCIUDADES)
class(fraude_ok$NROCIUDADES)


gNROCIUDADES <- vector()  
gNROCIUDADES[fraude_ok$NROCIUDADES == 1] <- 1
gNROCIUDADES[fraude_ok$NROCIUDADES == 2] <- 2
gNROCIUDADES[fraude_ok$NROCIUDADES == 3] <- 3
gNROCIUDADES[fraude_ok$NROCIUDADES == 4] <- 4
gNROCIUDADES[fraude_ok$NROCIUDADES == 5] <- 5
gNROCIUDADES[fraude_ok$NROCIUDADES == 6] <- 6
gNROCIUDADES[fraude_ok$NROCIUDADES == 7] <- 7
gNROCIUDADES[fraude_ok$NROCIUDADES == 8] <- 8
gNROCIUDADES[fraude_ok$NROCIUDADES == 9] <- 9
gNROCIUDADES[fraude_ok$NROCIUDADES == 10] <- 10
gNROCIUDADES[fraude_ok$NROCIUDADES == 11] <- 11
gNROCIUDADES[fraude_ok$NROCIUDADES == 12] <- 12
gNROCIUDADES[fraude_ok$NROCIUDADES == 13] <- 13
gNROCIUDADES[fraude_ok$NROCIUDADES >= 14 & fraude_ok$NROCIUDADES <= 19] <- 14
gNROCIUDADES[fraude_ok$NROCIUDADES >= 20] <- 15




summary(fraude_ok$NROCIUDADES)

fraude_ok$NROCIUDADES <- as.factor(gNROCIUDADES)
levels(fraude_ok$NROCIUDADES) <- c("1 CIU", "2 CIU", "3 CIU", "4 CIU", '5 CIU', '6 CIU', '7 CIU', '8 CIU','9 CIU',
                                '10 CIU', '11 CIU', '12 CIU','13 CIU','ENTRE 14 Y 19 CIU','20 CIU')


barplot(table(fraude_ok$EDAD, fraude_ok$NROCIUDADES))

###########################################################################################33
#elimimanos los NAs de estas variables , FECHA_VIN, INGRESOS Y EGRESOSy las sustituimos por su mediana
################################################################################################3
fraude_ok$FECHA_VIN[is.na(fraude_ok$FECHA_VIN)] <- median(fraude_ok$FECHA_VIN, na.rm = T)
fraude_ok$INGRESOS[is.na(fraude_ok$INGRESOS)] <- median(fraude_ok$INGRESOS, na.rm = T)
fraude_ok$EGRESOS[is.na(fraude_ok$EGRESOS)] <- median(fraude_ok$EGRESOS, na.rm = T)
#################################################################################
#sustituimos los valores 0 de estas variables por su mediana, porque despues quiero aplicarle
# ogaritmo ya que tienen valores muy extremos
#########################################################################
fraude_ok$VALOR[fraude_ok$VALOR == '0'] <- median(fraude_ok$VALOR, na.rm = T)
fraude_ok$INGRESOS[fraude_ok$INGRESOS == '0'] <- median(fraude_ok$INGRESOS, na.rm = T)
fraude_ok$EGRESOS[fraude_ok$EGRESOS == '0'] <- median(fraude_ok$EGRESOS, na.rm = T)



fraude_ok$VALOR_LOG<- log(fraude_ok$VALOR)
fraude_ok$INGRESOS_LOG<- log(fraude_ok$INGRESOS)
fraude_ok$EGRESOS_LOG<- log(fraude_ok$EGRESOS)
###########################################################################################
#CONVERTIMOS A FORMATO FECHA, LAS VARIABLES QUE INDICAN FECHA
##############################################################################################3
str(fraude_ok$FECHA)#INTEGER
str(fraude_ok$FECHA_VIN)#NUMERICO
fraude_ok$FECHA_VIN <- as.integer(fraude_ok$FECHA_VIN)
str(fraude_ok$FECHA_VIN)

ymd(fraude_ok$FECHA)
ymd(fraude_ok$FECHA_VIN)
#############################################################################################
#CREAMOS UN VECTOR DE VALORES CONSTANTES CON LOS PAISES QUE FUERON EN 2015 PARAISOS FISCALES
#CREAMOS UNA NUEVA VARIABLE FACTOR SI LA VARIABLE CODIGO_PAIS ESTA DENTRO DE ESTA CONSTANTE
##############################################################################################
PAISES_PARAISOS = c('AG','AN','AW','BB','BM','BH','AE','FI','GI','LU', 'GD','HK', 'IM','KY', 'CC',	
                    'FK', 'MP',	'SB', 'TC', 'VG', 'VI',	'JM', 'BS',	'MO', 'MU', 'MS', 'AD',	'LI',	
                    'MC', 'JO',	'CY', 'DO',	'LR', 'MT',	'NR', 'PA',	'SM', 'SC',	'SG', 'TT',	'VU',
                    'LY', 'VC',	'LC', 'BN',	'OM')

fraude_ok$INDICADOR_PARAISO <- fraude_ok$COD_PAIS %in% PAISES_PARAISOS
fraude_ok$INDICADOR_PARAISO <- as.factor(fraude_ok$INDICADOR_PARAISO)

##############################################################################################
#Seguimos trabajando con las variables distancias, sustiruimos NA`s por mediana
################################################################################################
fraude_ok$Dist_max_NAL[is.na(fraude_ok$Dist_max_NAL)] <- median(fraude_ok$Dist_max_NAL, na.rm = T)
fraude_ok$Dist_Mean_NAL[is.na(fraude_ok$Dist_Mean_NAL)] <- median(fraude_ok$Dist_Mean_NAL, na.rm = T) 
fraude_ok$Dist_Mean_INTER[is.na(fraude_ok$Dist_Mean_INTER)] <- median(fraude_ok$Dist_Mean_INTER, na.rm = T)
fraude_ok$Dist_Sum_INTER[is.na(fraude_ok$Dist_Sum_INTER)] <- median(fraude_ok$Dist_Sum_INTER, na.rm = T)
###############################################################################################3
table(apply(fraude_ok, 1, function(x) sum(is.na(x))))###VERIFICAMOS QUE NO HAY NAS
#summary(fraude_ok)
###################################################################################################
#SUSTITUCION DE OUTLIERS, CREAMOS UNA FUNCION QUE POR DEBAJO DEL QUANTIL 0.05 SUSTITUYE EL VALOR POR LA MEDIA, 
#Y POR ENCIMA DEL QUANTIL 0.95 SUSTITUYE EL VALOR POR LA MEDIANA Y LO APLICO A LAS VARIABLES FECHA_VIN, VALOR, INGRESOS, 
#○EGRESOS, Y NROPAISES. HAGO GRAFICOS BOXPLOTS ANTES Y DESDEPUES DE QUITAR OUTLIERS
###################################################################################################3

ELIMINAR_outliers <- function(x, removeNA = TRUE){
    quantiles <- quantile(x, c(0.05, 0.95), na.rm = removeNA)
    x[x<quantiles[1]] <- mean(x, na.rm = removeNA) ##sustituimos por la media
    x[x>quantiles[2]] <- median(x, na.rm = removeNA)##sustituimos por la mediana
    x
}



fraude_ok$FECHA_VIN <- ELIMINAR_outliers(fraude_ok$FECHA_VIN)



par(mfrow=c(1,2))#PARA CREAR DOS BOXPLOT EN LA PANTALLA, ASI PODEMOS COMPARAR MEJOR VISULAMENTE

fraude_ok$VALOR_SINOUT <- ELIMINAR_outliers(fraude_ok$VALOR)
boxplot(fraude_ok$VALOR)
boxplot(fraude_ok$VALOR_SINOUT)





############################################################3
####################################################################33

boxplot(fraude_ok$INGRESOS)
fraude_ok$INGRESOS_SINOUT <- ELIMINAR_outliers(fraude_ok$INGRESOS)
boxplot(fraude_ok$INGRESOS_SINOUT)


fraude_ok$EGRESOS_SINOUT<- ELIMINAR_outliers(fraude_ok$EGRESOS)
boxplot(fraude_ok$EGRESOS)#CON OUTLIERS
boxplot(fraude_ok$EGRESOS_SINOUT)#SIN OUTLIERS


fraude_ok$NROPAISES_SINOUT <- ELIMINAR_outliers(fraude_ok$NROPAISES)
boxplot(fraude_ok$NROPAISES)
boxplot(fraude_ok$NROPAISES_SINOUT)###SIGUE HABIENDO OUTIERS!!!!!

par(mfrow=c(1,1))##VOLVER A DEJAR LA PANTALLA PARA QUE APARAEZCA UN SOLO GRAFICO

#################################################################################################
#VAMOS A ESCALAR LAS DISTANCIAS,ES DECIR, NORMALIZARLAS, PARA MEJORAR SUS DISTRIBUCION
###############################################################################################


fraude_ok$Dist_Sum_INTER_Z <- scale(fraude_ok$Dist_Sum_INTER)
fraude_ok$Dist_Mean_INTER_Z <- scale(fraude_ok$Dist_Mean_INTER)
fraude_ok$Dist_Mean_NAL_Z <- scale(fraude_ok$Dist_Mean_NAL)
fraude_ok$Dist_HOY_Z <- scale(fraude_ok$Dist_HOY)
fraude_ok$Dist_sum_NAL_Z <- scale(fraude_ok$Dist_sum_NAL)

###############################MODELO DE REGRESION LOGISTICA######################################
str(fraude_ok)
#####################################################################################################3
#GENERAR PARTICION DE fraude_ok en dos, uno con el 80% y el otro el 20. Creamos el modelo en el 80%
#y entrenamos dicho modelo en el 20% restante
############################################################################################
set.seed(100)
num_train1 <- nrow(fraude_ok)*0.8
random_filas1 <- sample(1:nrow(fraude_ok), size= num_train1)
fraude_ok_train1 <- fraude_ok[random_filas1,]
fraude_ok_test1 <- fraude_ok[-random_filas1,]
###########################################################################################
#CREAMOS EL MODELO LINEAL LOGISTICO, USANDO STEP#######################################
#######################################################################################
mod1_ninguna <- glm(FRAUDE~1, fraude_ok_train1, family = binomial(link = 'logit'))
mod1_todas <-  glm(FRAUDE~.,fraude_ok_train1, family = binomial(link = 'logit')) 
modelo1 <- step(mod1_todas, direction = 'both')


summary(modelo1)




###############################################################################3
################################################################################

#############################################################################################################
# #Step:  AIC=1568.52
# FRAUDE ~ VALOR + HORA_AUX + COD_PAIS + CANAL + DIASEM + SEXO + 
#     EDAD + NROPAISES + Dist_Sum_INTER + NROCIUDADES + Dist_Mean_NAL + 
#     VALOR_LOG + INGRESOS_LOG + EGRESOS_LOG + VALOR_SINOUT + INGRESOS_SINOUT

############################################################################################
###########################################################################################3
############################################################################################

prediction_modelo_total <- predict(modelo1, newdata = fraude_ok, type = 'response')
prediction_modelo_total

fraude_ok <- cbind(fraude_ok, prediction_modelo_total)
fraude_ok$FRAUDE_PREDICCION_TOTAL <- fraude_ok$prediction_modelo_total > 0.5

MATRIZ <- table(fraude_ok$FRAUDE_PREDICCION_TOTAL, fraude_ok$FRAUDE, dnn = c("Actual", "Predicho"))
MATRIZ

round(prop.table(table(fraude_ok$FRAUDE_PREDICCION_TOTAL, fraude_ok$FRAUDE, dnn = c("Actual", "Predicho"))),2)

MATRIZ[1,1] #TP
MATRIZ[2,2] #TN
MATRIZ[1,2] #FN
MATRIZ[2,1] #FP

#RECALL=TP/(TP+FN)
RECALL <-  MATRIZ[1,1]/(MATRIZ[1,1] + MATRIZ[2,1])#[1] 0.9407475
RECALL


#PRECISION=TP/(TP+FP)
PRECISION <- MATRIZ[1,1]/(MATRIZ[1,1]+MATRIZ[1,2])#[1] 0.8820513
PRECISION


#ACCUARACY=TP+TN/(TP+TN+FP+FN)
ACCU <- (MATRIZ[1,1]+MATRIZ[2,2])/(MATRIZ[1,1]+MATRIZ[2,2]+MATRIZ[1,2]+MATRIZ[2,1])#[1] 0.8605769
ACCU

###################################CURVA ROC Y AIC ###############################################33

roc_curva_TOTAL <- roc(fraude_ok$FRAUDE, fraude_ok$prediction_modelo_total)
auc(roc_curva_TOTAL)#Area under the curve: 0.9114
plot(1-roc_curva_TOTAL$specificities,roc_curva_TOTAL$sensitivities, 'l')
lines(par()$usr[1:2], par()$usr[3:4])



###################ANALISIS DE REIDUOS EN EL MODELO TOTAL####################
residuos_TOTALES <- modelo1$residuals
shapiro.test(residuos_TOTALES)#p-value < 2.2e-16 no existe normalidad en los residuos

bptest(modelo1)#p-value < 2.2e-16 no existe homocedasticidad

dwtest(modelo1)#p-value = 0.6486 los resoduos no estan correlados!!!





############################################################################
prediction_TRAIN<- predict(modelo1, newdata = fraude_ok_train1, type='response')
prediction_TRAIN

###cramos 2 nuevas columnas con la prediccion en probabilidad y true/false
fraude_ok_train1 <- cbind(fraude_ok_train1,prediction_TRAIN)
fraude_ok_train1$FRAUDE_PREDICTION_TRAIN <- fraude_ok_train1$prediction_TRAIN> 0.5##NOS DEVUEVE TRUE O FLASE DEPENDIENDO DEL VALOR 


names(fraude_ok_train1)


MATRIZ_TRAIN<- table(fraude_ok_train1$FRAUDE_PREDICTION_TRAIN, fraude_ok_train1$FRAUDE,dnn = c("Actual", "Predicho"))
MATRIZ_TRAIN
round(prop.table(table(fraude_ok_train1$FRAUDE_PREDICTION_TRAIN, fraude_ok_train1$FRAUDE, dnn = c("Actual", "Predicho"))),2)

RECALL_TRAIN<-  MATRIZ_TRAIN[1,1]/(MATRIZ_TRAIN[1,1] + MATRIZ_TRAIN[2,1])#0.943662
RECALL_TRAIN

PRECISION_TRAIN<- MATRIZ_TRAIN[1,1]/(MATRIZ_TRAIN[1,1]+MATRIZ_TRAIN[1,2])#0.8900106
PRECISION_TRAIN

ACCU_TRAIN <- (MATRIZ_TRAIN[1,1]+MATRIZ_TRAIN[2,2])/(MATRIZ_TRAIN[1,1]+MATRIZ_TRAIN[2,2]+MATRIZ_TRAIN[1,2]+MATRIZ_TRAIN[2,1])
ACCU_TRAIN#0.8681838

roc_curva_TRAIN<- roc(fraude_ok_train1$FRAUDE, fraude_ok_train1$prediction_TRAIN)
auc(roc_curva_TRAIN)#Area under the curve: 0.9195
plot(1-roc_curva_TRAIN$specificities,roc_curva_TRAIN$sensitivities, 'l')
lines(par()$usr[1:2], par()$usr[3:4])##DIBUJAMOS LA LINEA QUE UNE POR LA MITAD EL RECUADRO


str(fraude_ok_train1)

##########################################################



######################################################################################################
#########################################################################################################
#############################APLICAR EL MODELO EN TEST#######################################################
prediction_TEST<- predict(modelo1, newdata = fraude_ok_test1, type='response')
prediction_TEST

###cramos 2 nuevas columnas con la prediccion en probabilidad y true/false
fraude_ok_test1 <- cbind(fraude_ok_test1,prediction_TEST)
fraude_ok_test1$FRAUDE_PREDICTION_TEST<- fraude_ok_test1$prediction_TEST > 0.5

names(fraude_ok_test1)


MATRIZ_TEST <- table(fraude_ok_test1$FRAUDE_PREDICTION_TEST, fraude_ok_test1$FRAUDE,dnn = c("Actual", "Predicho"))
MATRIZ_TEST

round(prop.table(table(fraude_ok_test1$FRAUDE_PREDICTION_TEST, fraude_ok_test1$FRAUDE, dnn = c("Actual", "Predicho"))),2)


RECALL_TEST<-  MATRIZ_TEST[1,1]/(MATRIZ_TEST[1,1] + MATRIZ_TEST[2,1])#0.928401
RECALL_TEST

PRECISION_TEST<- MATRIZ_TEST[1,1]/(MATRIZ_TEST[1,1]+MATRIZ_TEST[1,2])#0.849345
PRECISION_TEST

ACCU_TEST <- (MATRIZ_TEST[1,1]+MATRIZ_TEST[2,2])/(MATRIZ_TEST[1,1]+MATRIZ_TEST[2,2]+MATRIZ_TEST[1,2]+MATRIZ_TEST[2,1])
ACCU_TEST#0.8301887





roc_curva_TEST<- roc(fraude_ok_test1$FRAUDE, fraude_ok_test1$prediction_TEST)
auc(roc_curva_TEST)#Area under the curve: 0.8827
plot(1-roc_curva_TEST$specificities,roc_curva_TEST$sensitivities, 'l')
lines(par()$usr[1:2], par()$usr[3:4])


#####################################################################################################
#####################################################################################################
##################################APLICAR EL MODELO EN EL CONJUNTO DE DATOS: TEST###########################################


test <- fread("C:/Users/LOS_MORALCA/Desktop/DATAHACK/MODULO1/PRACTICA/test.csv") 


train <- fread("C:/Users/LOS_MORALCA/Desktop/DATAHACK/MODULO1/PRACTICA/train.csv", 
               stringsAsFactors = T)

###########################################################################################
#CREAMOS 2 TABLAS DUPLICADOS DE TEST, MANIPUPAMOS TEST_DUPLICADOS QUE ES DONDE 
#LE APLICAMOS NUESTRO MODELO Y EN TEST LE AÑADIMOS NUESTRAS PREDICCIONES EN LA COLUMNA FRAUDE
#Y HACEMO LAS SUSTITUCIONES Y TRASNFORMACIONES NECESIARAS QUE SE EXPLICAN A CPNTINUACION
############################################################################################






test_duplicado <- test 


test_duplicado$VALOR <- as.integer(test_duplicado$VALOR)
test_duplicado$FRAUDE <- as.numeric(test_duplicado$FRAUDE)
test_duplicado$DIASEM <- as.factor(test_duplicado$DIASEM)

str(test_duplicado)
test_duplicado$NROPAISES <- as.integer(test_duplicado$NROPAISES)
################################################
table(apply(train, 1, function(x) sum(is.na(x))))##UNA FUNCION PARA AVERIGUAR CUANTOS NAS HAY DENTRO DEL traiN, por frecuencias
nrow(train)-length(unique(train$id))#elimino ids duplicados. Los localizamos con la ID
train_x <- train[!duplicated(train)]#eliminamos las filas iguales con todas las variables iguales, es decir, quitamos 5 
nrow(train_x)-length(unique(train_x$id))####AVERIGUAMOS CUANTAS FILAS NOS QUEDAN CON LOS ID REPETIDO
train_id <- train_x[duplicated(train_x$id)]
#son el conjunto de duplicados, pero pueden tener distintos registros
#la misma persona que ha hecho transacciones diferentes

repetidos_id <- train_x[train_x$id %in% train_id$id,]
#quiero ver dónde están duplicados. me quedo con todos
#los id que tienen id duplicadas,vemos qué valor varía. precisamente es VALOR.
#todos los datos iguales en los id repetidos excepto en valor. Vamos a quedarnos con los que FRAUDE=1,
#y vamos a quedarnos con la fecha mínima , el primer fraude que ha cometido, para saber que en las
#posteriores nos pueden salir cosas raras y saber que venía de fraude
table(repetidos_id$id, repetidos_id$FRAUDE)
#vemos que en los resgistros duplicados se mantiene la clasificacion
#de Fraude. No hay ninguno que sea en uno Fraude = 1 y en otro =0
table(train$SEGMENTO, useNA = 'always') #hay valores vacíos (24)) Y EN QUE SEGMENTO ESTA
data_max_h_1 <- repetidos_id[FRAUDE==1, .(HORA_AUX=min(HORA_AUX)), by= id]

#Fraude no está repetida en los repetidos_id. Quiero ver en qué variables se repite. 
#y veo que lo que varía es el VALOR, nos quedamnos con la transaccion que han hecho primero
data_max_h_1 <- merge(data_max_h_1, repetidos_id)##unimos las 2 matrices 
fraude_1 <- data_max_h_1[,.SD[sample(.N, min(1,1))], by = id]
#cojemos aleatoriamente nos quedamos con uno de los valores repetidos de la hora mas temprana de cometer fraude
#de los dos registros en los que para id identico, la hora está duplicada
data_max_h_0 <- repetidos_id[FRAUDE==0,.SD[sample(.N, 1)], by=id]
fraude_0 <- merge(repetidos_id, data_max_h_0)
fraude_01 <- rbind(fraude_1, fraude_0)
no_repetidos_id <- train_x[!train_x$id %in% train_id$id,]
fraude_ok <- rbind(no_repetidos_id, fraude_01)
fraude_ok$id <-  NULL##eliminamos la variable id










summary(test_duplicado$FRAUDE)
str(test_duplicado$FRAUDE)
###############################################################################################
##ELIMINAMIS ESTAS VARIABLES QUE NO APORTAN NADA A NUESTRO MODELO######
test_duplicado$id <- NULL
test_duplicado$OFICINA_VIN <-  NULL
test_duplicado$DIAMES <- NULL
test_duplicado$Canal1 <- NULL
#################################################################################################
summary(test_duplicado)
####################################################################################################
##TENEMOS VALORES MISSING Y CREAMOS UN NUEVO FACTOR QUE INCLUYAN ESOS VALORES MISSING#########
test_duplicado$SEXO<- factor(test_duplicado$SEXO, levels = c('F', 'M', 'OTRO'))
test_duplicado$SEXO[is.na(test_duplicado$SEXO)] <- 'OTRO'
#################################################################################################################
test_duplicado$EDAD[is.na(test_duplicado$EDAD)] <- median(test_duplicado$EDAD, na.rm = T)

gedad <- vector()
gedad[test_duplicado$EDAD <= 10] <- 1
gedad[test_duplicado$EDAD >= 11 & test_duplicado$EDAD < 16] <- 2
gedad[test_duplicado$EDAD >= 16 & test_duplicado$EDAD< 20] <- 3
gedad[test_duplicado$EDAD >= 20 & test_duplicado$EDAD< 25] <- 4
gedad[test_duplicado$EDAD >= 25 & test_duplicado$EDAD< 30] <- 5
gedad[test_duplicado$EDAD >= 30 & test_duplicado$EDAD< 35] <- 6
gedad[test_duplicado$EDAD >= 35 & test_duplicado$EDAD< 40] <- 7
gedad[test_duplicado$EDAD >= 40 & test_duplicado$EDAD< 45] <- 8
gedad[test_duplicado$EDAD >= 45 & test_duplicado$EDAD< 50] <- 9
gedad[test_duplicado$EDAD >= 50 & test_duplicado$EDAD< 55] <- 10
gedad[test_duplicado$EDAD >= 55 & test_duplicado$EDAD< 60] <- 11
gedad[test_duplicado$EDAD >= 60 & test_duplicado$EDAD< 65] <- 12
gedad[test_duplicado$EDAD >= 65 & test_duplicado$EDAD< 70] <- 13
gedad[test_duplicado$EDAD >= 70 & test_duplicado$EDAD< 75] <- 14
gedad[test_duplicado$EDAD >= 75 & test_duplicado$EDAD< 80] <- 15
gedad[test_duplicado$EDAD >= 80 & test_duplicado$EDAD< 85] <- 16
gedad[test_duplicado$EDAD >= 85] <- 17


summary(test_duplicado$EDAD)



test_duplicado$EDAD <- as.factor(gedad)#la convertimos a factor, con las NUEVAS ETIQUETAS

levels(test_duplicado$EDAD) <- c("0 a 15", "16 a 19", "20 a 24", "25 a 29", '30 a 34', '35 a 39', '40 a 44', '45 a 49',
                                 '50 a 54', '55 a 59', '60 a 64', '65 a 69', '70 a 74', '75 a 79', '80 a 84', 'Mas de 85')
#####################################################################################################################
###MODIFICAMOS LA VARIABLE HORA_AUX EN CATEGORICA, PARA MEJORAR NUESTRO MODELO##################
gHORA_AUX <- vector()
gHORA_AUX[test_duplicado$HORA_AUX <=2] <- 1
gHORA_AUX[test_duplicado$HORA_AUX >= 3 & test_duplicado$HORA_AUX< 5] <- 2
gHORA_AUX[test_duplicado$HORA_AUX >= 5 & test_duplicado$HORA_AUX< 7] <- 3
gHORA_AUX[test_duplicado$HORA_AUX >= 7 & test_duplicado$HORA_AUX< 9] <- 4
gHORA_AUX[test_duplicado$HORA_AUX >= 9 & test_duplicado$HORA_AUX< 11] <- 5
gHORA_AUX[test_duplicado$HORA_AUX >= 11 & test_duplicado$HORA_AUX< 13] <- 6
gHORA_AUX[test_duplicado$HORA_AUX >= 13 & test_duplicado$HORA_AUX< 15] <- 7
gHORA_AUX[test_duplicado$HORA_AUX >= 15 & test_duplicado$HORA_AUX< 17] <- 8
gHORA_AUX[test_duplicado$HORA_AUX >= 17 & test_duplicado$HORA_AUX< 19] <- 9
gHORA_AUX[test_duplicado$HORA_AUX >= 19 & test_duplicado$HORA_AUX< 21] <- 10
gHORA_AUX[test_duplicado$HORA_AUX >= 21 & test_duplicado$HORA_AUX< 23] <- 11
gHORA_AUX[test_duplicado$HORA_AUX >= 23] <- 12




test_duplicado$HORA_AUX <- as.factor(gHORA_AUX)###ETIQUETAS DE LA NUEVA VARIABLE
levels(test_duplicado$HORA_AUX) <- c("0 a 2", "3 a 4", "5 a 6", "7 a 8", '9 a 10', '11 a 12', '13 a 14', '15 a 16','17 a 18',
                                     '19 a 20', '21 a 22', 'Mas de 23')


###################################################################################################################
##VAMOS A CREAR UNA VARIABLE NROCIUDADES QUE SEA CATEGORIACA PARA MEJORAR EL MODELO#############
gNROCIUDADES <- vector()  
gNROCIUDADES[test_duplicado$NROCIUDADES == 1] <- 1
gNROCIUDADES[test_duplicado$NROCIUDADES == 2] <- 2
gNROCIUDADES[test_duplicado$NROCIUDADES == 3] <- 3
gNROCIUDADES[test_duplicado$NROCIUDADES == 4] <- 4
gNROCIUDADES[test_duplicado$NROCIUDADES == 5] <- 5
gNROCIUDADES[test_duplicado$NROCIUDADES == 6] <- 6
gNROCIUDADES[test_duplicado$NROCIUDADES == 7] <- 7
gNROCIUDADES[test_duplicado$NROCIUDADES == 8] <- 8
gNROCIUDADES[test_duplicado$NROCIUDADES == 9] <- 9
gNROCIUDADES[test_duplicado$NROCIUDADES == 10] <- 10
gNROCIUDADES[test_duplicado$NROCIUDADES == 11] <- 11
gNROCIUDADES[test_duplicado$NROCIUDADES == 12] <- 12
gNROCIUDADES[test_duplicado$NROCIUDADES == 13] <- 13
gNROCIUDADES[test_duplicado$NROCIUDADES >= 14 & test_duplicado$NROCIUDADES <= 19] <- 14
gNROCIUDADES[test_duplicado$NROCIUDADES >= 20] <- 15




summary(test_duplicado$NROCIUDADES)

test_duplicado$NROCIUDADES <- as.factor(gNROCIUDADES)#ETIQUETAS DE LA NUEVA VARIABLE
levels(test_duplicado$NROCIUDADES) <- c("1 CIU", "2 CIU", "3 CIU", "4 CIU", '5 CIU', '6 CIU', '7 CIU', '8 CIU','9 CIU',
                                        '10 CIU', '11 CIU', '12 CIU','13 CIU','ENTRE 14 Y 19 CIU','20 CIU')
###################################################################################################################3
#####SUSTITUIMOS LOS VALORES NA`S POR SU MEDINA######################################
test_duplicado$FECHA_VIN[is.na(test_duplicado$FECHA_VIN)] <- median(test_duplicado$FECHA_VIN, na.rm = T)
test_duplicado$INGRESOS[is.na(test_duplicado$INGRESOS)] <- median(test_duplicado$INGRESOS, na.rm = T)
test_duplicado$EGRESOS[is.na(test_duplicado$EGRESOS)] <- median(test_duplicado$EGRESOS, na.rm = T)
####################################################################################################
#2.3.- sustituimos los valores 0 de estas variables por su mediana, porque despues quiero aplicarle
# logaritmo ya que tienen valores muy extremos, mejormaos su ditribucion y el modelo.
#########################################################################@#####################
test_duplicado$VALOR[test_duplicado$VALOR == '0'] <- median(test_duplicado$VALOR, na.rm = T)
test_duplicado$INGRESOS[test_duplicado$INGRESOS == '0'] <- median(test_duplicado$INGRESOS, na.rm = T)
test_duplicado$EGRESOS[test_duplicado$EGRESOS == '0'] <- median(test_duplicado$EGRESOS, na.rm = T)



test_duplicado$VALOR_LOG<- log(test_duplicado$VALOR)
test_duplicado$INGRESOS_LOG<- log(test_duplicado$INGRESOS)
test_duplicado$EGRESOS_LOG<- log(test_duplicado$EGRESOS)
############################################################################################################
#CON LAS VARIABLES FECHAS, GARANTIZAMOS QUE SEAN INTEGER Y DESPUES LE CAMBIAMOS EL FORMATO A FECHA
str(test_duplicado$FECHA)#INTEGER
str(test_duplicado$FECHA_VIN)#NUMERICO
test_duplicado$FECHA_VIN <- as.integer(test_duplicado$FECHA_VIN)
str(test_duplicado$FECHA_VIN)

ymd(test_duplicado$FECHA)
ymd(test_duplicado$FECHA_VIN)
#####################################################################################################################
#####CREAMOS UN VECTOR CON VALORES CONSTANTES CON UNOS PAISES QUE FUERON PARAISO FISCAL EN 2015 QUE ES CUANDO
###SE REALIZO EL ESTUDIO Y VEMOS CUANTOS COINCIDEN DE NUESTRO CONJUNTO DE DATOS. DESPUES CREAMOS UN FACTOR CON TRUE SI ESTA DENTRA DE LA LISTA 
#Y FALSE SI ESTA FUERA DE LA LISTA DE PARAISOS FISCALES
PAISES_PARAISOS = c('AG','AN','AW','BB','BM','BH','AE','FI','GI','LU', 'GD','HK', 'IM','KY', 'CC',	
                    'FK', 'MP',	'SB', 'TC', 'VG', 'VI',	'JM', 'BS',	'MO', 'MU', 'MS', 'AD',	'LI',	
                    'MC', 'JO',	'CY', 'DO',	'LR', 'MT',	'NR', 'PA',	'SM', 'SC',	'SG', 'TT',	'VU',
                    'LY', 'VC',	'LC', 'BN',	'OM')

test_duplicado$INDICADOR_PARAISO <- test_duplicado$COD_PAIS %in% PAISES_PARAISOS
test_duplicado$INDICADOR_PARAISO <- as.factor(test_duplicado$INDICADOR_PARAISO)
#####################################################################################################################
##############SUSTITUCION DE PODIBLES NA´S POR LA MEDIANA##############################################################
test_duplicado$Dist_max_NAL[is.na(test_duplicado$Dist_max_NAL)] <- median(test_duplicado$Dist_max_NAL, na.rm = T)
test_duplicado$Dist_Mean_NAL[is.na(test_duplicado$Dist_Mean_NAL)] <- median(test_duplicado$Dist_Mean_NAL, na.rm = T) 
test_duplicado$Dist_Mean_INTER[is.na(test_duplicado$Dist_Mean_INTER)] <- median(test_duplicado$Dist_Mean_INTER, na.rm = T)
test_duplicado$Dist_Sum_INTER[is.na(test_duplicado$Dist_Sum_INTER)] <- median(test_duplicado$Dist_Sum_INTER, na.rm = T)
test_duplicado$Dist_Max_INTER[is.na(test_duplicado$Dist_Max_INTER)] <- median(test_duplicado$Dist_Max_INTER, na.rm = T)
############################################################
table(apply(test_duplicado, 1, function(x) sum(is.na(x))))##VERIFICAMOS QUE NO TENEMOS NA`S

################################################################################################################
m_corr_test_duplicado<- test_duplicado[, c('Dist_Sum_INTER', 'Dist_Mean_INTER', 
                                           'Dist_Max_INTER', 'Dist_Mean_NAL','Dist_HOY',
                                           'Dist_sum_NAL', 'Dist_max_NAL')]
cor(m_corr_test_duplicado, use='pairwise.complete.obs') #obtenemos la matriz de correlaciones
m.cor_test_duplicado <- cor(m_corr_test_duplicado, use='pairwise.complete.obs')
corrplot.mixed(m.cor_test_duplicado)
#EXISTE CORRELACION LINEAL POSITVA ENTRE LAS VARIABLES Dist_Max_INTER Y DIST_Mean_INTER, ELIMINAMOS 1
test_duplicado$Dist_Max_INTER <- NULL
###########################################################################################################
##CREAMOS UNA FUNCION PARA ELIMINAR POSIBLES OUTLIERS, BASANDONOS EN LOS CUANTILES 0.05 Y 0.95.
#####EN EL PRIMER CASO SE SUSTITUYE POR LA MEDIA Y EN EL SEGUNDO POR LA MEDIANA.######
ELIMINAR_outliers <- function(x, removeNA = TRUE){
    quantiles <- quantile(x, c(0.05, 0.95), na.rm = removeNA)
    x[x<quantiles[1]] <- mean(x, na.rm = removeNA) ##sustituimos por la media
    x[x>quantiles[2]] <- median(x, na.rm = removeNA)##sustituimos por la mediana
    x
}
test_duplicado$FECHA_VIN <- ELIMINAR_outliers(test_duplicado$FECHA_VIN)
test_duplicado$VALOR_SINOUT <- ELIMINAR_outliers(test_duplicado$VALOR)
test_duplicado$INGRESOS_SINOUT <- ELIMINAR_outliers(test_duplicado$INGRESOS)
test_duplicado$EGRESOS_SINOUT<- ELIMINAR_outliers(test_duplicado$EGRESOS)
test_duplicado$NROPAISES_SINOUT <- ELIMINAR_outliers(test_duplicado$NROPAISES)
#################################################################################################################
#########NORMALIZAMOS LAS DISTANCIAS, PARA OBTENER MEJOR DISTRIBUCIONES Y MEJOR MODELO
test_duplicado$Dist_Sum_INTER_Z <- scale(test_duplicado$Dist_Sum_INTER)
test_duplicado$Dist_Mean_INTER_Z <- scale(test_duplicado$Dist_Mean_INTER)
test_duplicado$Dist_Mean_NAL_Z <- scale(test_duplicado$Dist_Mean_NAL)
test_duplicado$Dist_HOY_Z <- scale(test_duplicado$Dist_HOY)
test_duplicado$Dist_sum_NAL_Z <- scale(test_duplicado$Dist_sum_NAL)
###########################################################################################
################LE APLICAMOS EL MODELO Y SACAMOS NUESTRAS PROBABILIDADES O PREDCCIONES####
prediction_modelo_duplicado <- predict(modelo1, newdata = test_duplicado, type = 'response')
test_duplicado <- cbind(test_duplicado, prediction_modelo_duplicado)
test_duplicado$FRAUDE_PREDICCION_MODELO_DUPLICADO <- test_duplicado$prediction_modelo_duplicado > 0.5
#NOS DEVUELVE UNA VARIABLE CON TRUE O FALSE, SI ES MAYOR LA PROBABILIDAD ES MAYOR QUE 0.5 ES TRUE.

table(test_duplicado$FRAUDE_PREDICCION_MODELO_DUPLICADO)
test_del_test <- test_duplicado$prediction_modelo_duplicado[1:10]



######ASIGNAMOS A LA VARIABLE FRAUDE, LA PREDCCION DE NUESTRO MODELO
test$FRAUDE <- test_duplicado$prediction_modelo_duplicado

summary(test$FRAUDE)
names(test)
dim(test)

###REDONDEAMOS LA VARIABLE FRAUDE A 3 DIGITOS, PARA NUESTRA COMODIDAD
test$FRAUDE <- round(test$FRAUDE, 3)

summary(test)
###################SUSTUTIUIR LOS NA`S
test$Dist_Sum_INTER <- test_duplicado$Dist_Sum_INTER
test$Dist_Mean_INTER <- test_duplicado$Dist_Mean_INTER
test$Dist_Max_INTER[is.na(test$Dist_Max_INTER)] <- median(test$Dist_Max_INTER, na.rm = T)
test$Dist_Mean_NAL[is.na(test$Dist_Mean_NAL)] <- median(test$Dist_Mean_NAL, na.rm = T)

test$SEXO<- factor(test$SEXO, levels = c('F', 'M', 'OTRO'))
test$SEXO[is.na(test$SEXO)] <- 'OTRO'
summary(test)

head(test)
dim(test)

###############################################################################################
#############GUARDAMOS EL ARCHIVO DEFINITIVO CON LA PREDICCION SOLICITADA#####################
write.csv(test, file = 'test_evaluado.csv')

##### LO RECUPERAMOS PARA VER QUE TODO ESTA CORRECTO
test.evaluado <- fread('c:/Users/LOS_MORALCA/Desktop/DATAHACK/MODULO1/PRACTICA/test_evaluado.csv')
head(datata)

test.evaluado$V1 <- NULL

#NOTA: A LA HORA DE GRABARLO, ES EL ARCHIVO DESEADO, CON 26 VARIABLES EN EL ORDEN PEDIDO, PERO AL RECUPERRALO
# NOS INTRUDUCE UNA NUEVA VARIABLE V1, SE ELIMINA Y YA ESTA!!!
