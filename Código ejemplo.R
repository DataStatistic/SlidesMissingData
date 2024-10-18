
##################################
# Un problema de datos faltantes #
##################################

# Importando el conjunto de datos

library(readxl)
library(dplyr)

# Conjunto de datos
url = "http://www.ime.usp.br/~jmsinger/Dados/Fernandes2002.xls"
temp = tempfile(fileext = ".xls")
download.file(url, destfile = temp, mode = "wb")
datos = read_excel(temp) %>%
  select(-c(1, 4:6, 8:13, 15, 20)) %>%
  rename(Edad = IDADE, Sexo = SEXO, IMC = IMC,
         Hospitalización = DIAS, Intervención = T_IC,
         Anestesia = T_A, Recuperación = T_REC1,
         Perfusión = T_P, Tipo = `TIPO DE PACIENTE`,
         Protocolo = `MÉTODO DE RECUPERAÇÃO`) %>%
  mutate(Sexo = factor(ifelse(Sexo == 0, "Femenino", "Masculino")),
         Tipo = factor(ifelse(Tipo == 0, "Congénito","Coronario")),
         Protocolo = factor(ifelse(Protocolo == 0, "Convencional", "Acelerada"), 
                            labels = c("Convencional", "Acelerada")),
         IMC = ifelse(IMC==0, NA, IMC))

## Descripción de los datos faltantes

# Porcentaje de faltantes

library(DataExplorer)
library(ggplot2)

# Frecuencia de faltantes por variable
datos %>%
  plot_missing(
    group = list(`0-5` = 0.05, `5-15` = 0.15,
                 `15-30` = 0.3, `> 30` = 1),
    group_color = list(`0-5` = "#1B9E77", `5-15` = "#E6AB02",
                       `15-30` = "#D95F02", `> 30` = "#E41A1C")) +
  labs(y = "Cantidad de Faltantes", x = "Variable", fill = "% de faltantes")

# Patrones de datos faltantes

library(mice)

# Patrones de faltantes 
datos %>% 
  md.pattern(rotate.names = TRUE, plot = TRUE)

library(naniar)

# Otra forma
datos %>%
  gg_miss_upset(nsets = 7)

# Test de Little

library(naniar)

# Prueba MCAR de Little
datos %>% 
  select(where(is.numeric)) %>%
  mcar_test()

## Tratamiento de datos faltantes

# Métodos de eliminación

# Vectores de promedios
datos %>%
  select(where(is.numeric)) %>%
  na.omit() %>%
  colMeans()

datos %>%
  select(where(is.numeric)) %>%
  colMeans(na.rm = TRUE)

# Matrices de correlación

library(ggcorrplot)

Rcc = datos %>% 
  select(where(is.numeric)) %>%
  cor(use = "complete.obs")

ggcorrplot(Rcc, type = "lower", colors = c("#E46726", "white", "#6D9EC1"))

Rcd = datos %>% 
  select(where(is.numeric)) %>%
  cor(use = "pairwise.complete.obs")

ggcorrplot(Rcd, type = "lower", colors = c("#E46726", "white", "#6D9EC1"))

# Relación entre variables

Estado = ifelse(ici(datos), "Faltantes", "Observados")

ggplot(datos, aes(x = Intervención,
                  y = Hospitalización, 
                  colour = Estado)) +
  geom_jitter(size = 2.5, alpha = 0.8)

# Imputación

# Imputación por media incondicional o aleatoria

library(Hmisc)

datos.imp = datos %>%
  select(where(is.numeric)) %>%
  mutate(across(everything(), \(x) impute(x, fun = mean))) # media incondicional
# mutate(across(everything(), \(x) impute(x, "random")) # aleatoria

ggplot(data.frame(
   Tipo = rep(c("Observados", "Imputados"), rep(nrow(datos), 2)),
   Intervención = c(datos$Intervención, datos.imp$Intervención)),
   aes(x = Intervención, color = Tipo)) +
  geom_density()

# Imputación por regresión determinística y estocástica

datos.imp = datos
patrones = data.frame(md.pattern(datos, plot = FALSE))[,1:ncol(datos)]
variables = names(patrones)
k = nrow(patrones) - 1 ; k
set.seed(123)
for(j in 2:k){
  resp = variables[patrones[j,]==0]
  expl = variables[patrones[j,]==1]
  for(v in resp){
    if(class(datos[[v]])=="numeric"){
      mod = lm(paste(v, "~", paste(expl, collapse = " + ")), data = datos)
      sig = sigma(mod)
      # pred = predict(mod, datos) # determinística
      pred = predict(mod, datos) + rnorm(nrow(datos), 0, sig) # estocástica
      datos.imp[is.na(datos.imp[,v]),v] = pred[is.na(datos.imp[,v])]
    }
  } 
}

# El método Hot-Deck

library(VIM)

datos.imp = hotdeck(datos,
                    domain_var = "Tipo", # variable de grupo de imputación
                    imp_var = FALSE
)

# El método kNN

# Emplea la distancia de Gower
datos.imp = kNN(datos,
                k = 5, # número de vecinos más cercanos 
                imp_var = FALSE)

# Coincidencia media predictiva (pmm)

# Emplea regresión estocástica
datos.imp = datos
patrones = data.frame(md.pattern(datos, plot = FALSE))[,1:ncol(datos)]
variables = names(patrones) ; k = nrow(patrones) - 1
K = 5 # vecinos más cercanos
set.seed(123)
for (j in 2:k) {
  resp = variables[patrones[j, ] == 0] ; expl = variables[patrones[j, ] == 1]
  for(v in resp){
    if(class(datos[[v]]) == "numeric"){
      mod = lm(paste(v, "~", paste(expl, collapse = " + ")), data = datos)
      sig = sigma(mod) ; pred = predict(mod, datos)
      for(i in which(is.na(datos[[v]]))){
        if(!is.na(pred[i])){
          distancias = abs(pred - (pred[i]+rnorm(1, 0, sig)))
          orden = order(distancias)
          cercanos = orden[!is.na(datos[[v]][orden])][1:K]
          donante = sample(datos[[v]][cercanos], 1)
          datos.imp[i, v] = donante
        }
      }
    }
  }
}

# Algoritmo MICE

library(mice)

# Proceso de imputación 
proc.imp = mice(datos, # conjunto de datos con faltantes 
                m = 5, # número de imputaciones
                maxit = 15, # número de iteraciones
                defaultMethod = c("pmm", "logreg", "polyreg", "polr"), # métodos
                seed = 12345, # semilla
                printFlag = FALSE
)
# Conjuntos de datos imputados
complete(proc.imp, 1)
complete(proc.imp, 2)
complete(proc.imp, 3)
complete(proc.imp, 4)
complete(proc.imp, 5)

# Ajuste de una distribución

library(MASS, exclude = "select")

# Estimaciones por imputación
the.imp = with(proc.imp, fitdistr(Anestesia, "gamma"))

# Combinación de resultados
the.comb = pool(the.imp)

# Intervalos de confianza y pruebas de hipótesis

# Test de Levene para la comparación de varianzas
library(miceafter)

proc.imp.ml <- mids2milist(proc.imp)
lev.imp = with(proc.imp.ml, expr=levene_test(Perfusión ~ Tipo))
pool_levenetest(lev.imp, method="D2")

# t.test para la comparación de medias
t.imp = with(proc.imp.ml,
             expr = t_test(Perfusión ~ Tipo, var_equal=FALSE, paired=FALSE))
pool_t_test(t.imp, statistic=TRUE)

# Análisis de componentes principales

# Imputación
proc.imp = mice(
  datos, m = 50, maxit = 15, seed = 123, printFlag = FALSE,
  defaultMethod = c("pmm", "logreg", "polyreg", "polr")
)

library(FactoMineR)
library(factoextra)

# ACP con cada base imputada
acp.m = with(proc.imp, 
             PCA(data.frame(
               Sexo, Tipo, Protocolo, # ilustrativas
               Edad, IMC, Hospitalización, Intervención,
               Anestesia, Perfusión, Recuperación), 
               quali.sup = 1:3,
               graph = FALSE))

# Resultados a combinar
acp.m$analyses

# Agrupamiento

library(miclust)

datos.imp = list(set0 = select(datos, where(is.numeric)))
for(i in 1:proc.imp$m) {
  datos.imp[[paste("set", i, sep = "")]] <- select(complete(proc.imp, i), where(is.numeric))
}
datos.imp = lapply(datos.imp, as.data.frame)
datos.imp = getdata(datos.imp)

# k-means
mc = miclust(datos.imp,
             ks = 2:5, # número de grupos
             method = "kmeans", # método
             initcl = "hc" # jerárquico + kmeans
) 

summary(mc)

# Modelos de regresión

# Imputación
proc.imp = mice(
  datos, m = 5, maxit = 15, seed = 123, printFlag = FALSE,
  defaultMethod = c("pmm", "logreg", "polyreg", "polr")
)

# Modelos ajustados con cada imputación
mod.imp = with(proc.imp,
               glm(Hospitalización ~ Edad + Sexo + IMC + Intervención +
                     Anestesia + Perfusión + Recuperación + Tipo + Protocolo, 
                   family = poisson(link = "log")))

# Combinación de resultados
comb = pool(mod.imp) ; comb

# Inferencias
summary(comb)

# Regresión paso a paso

# Imputación
proc.imp = mice(
  datos, m = 5, maxit = 15, seed = 123, printFlag = FALSE,
  defaultMethod = c("pmm", "logreg", "polyreg", "polr")
)

# Modelos ajustados con cada imputación
mod.imp = with(proc.imp,
               step(glm(Hospitalización ~ Edad + Sexo + IMC + Intervención +
                          Anestesia + Perfusión + Recuperación + Tipo + Protocolo, 
                        family = poisson(link = "log"))))

# Combinación de resultados
comb = pool(mod.imp) ; comb

# Inferencias
summary(comb)
