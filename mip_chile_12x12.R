####################################
# Importante
####################################

# Este archivo es reproducible. Por eso conviene abrir el archivo .rproj en RStudio y fijar la
# carpeta del projecto como carpeta de trabajo. Luego con ctrl+a y ctrl+enter se descargan los
# archivos directamente de la página del BCCh y se hacen los cálculos.

####################################
# Librerías necesarias
####################################

library(XLConnect)
library(xlsx)

####################################
# Leer datos
####################################

url.12 <- "http://si3.bcentral.cl/estadisticas/Principal1/Excel/CCNN/cdr/xls/CdeR2008_MIP_12x12.xlsx"
xlsx.12 <- "mip_2008_bcch_12x12.xlsx"

if(!file.exists(xlsx.12)) {
  print("descargando")
  download.file(url.12, xlsx.12, method="curl")
}

if(file.exists(xlsx.12)) {
  mip.2008.12 <- readWorksheetFromFile("mip_2008_bcch_12x12.xlsx", sheet = "12x12_2008", region = "C12:AC23", header = FALSE)
  # hay que usar lapply porque el archivo excel no está con un formato numérico adecuado
  mip.2008.12 <- as.data.frame(apply(mip.2008.12, 2, gsub, pattern = ",", replacement= ""), stringsAsFactors = FALSE)
  
  glosas.mip.2008.12 <- readWorksheetFromFile("mip_2008_bcch_12x12.xlsx", sheet = "Glosas", region = "C10:C120", header = FALSE)
  colnames(glosas.mip.2008.12) <- c("Glosas")
}

####################################
# Cálculos
####################################

# para la justificación de los cálculos consultar http://si3.bcentral.cl/estadisticas/Principal1/Metodologias/CCNN/cdr/serieestudios38.pdf
# http://www.cepal.org/deype/noticias/noticias/0/22350/redima2005_schuschny.pdf
# tener presente que el sistema es de la forma $x = Ax + b$

# matriz de insumo-producto
X <- data.matrix(mip.2008.12[,1:12])
X[is.na(X)] <- 0
colnames(X) <- t(glosas.mip.2008.12)
rownames(X) <- t(glosas.mip.2008.12)

# total demanda intermedia
Xi <- data.matrix(as.numeric(mip.2008.12[,12]))
Xi[is.na(Xi)] <- 0
colnames(Xi) <- c("Total demanda intermedia") 
rownames(Xi) <- t(glosas.mip.2008.12)

# total demanda final
Xf <- data.matrix(as.numeric(mip.2008.12[,27]))
Xf[is.na(Xf)] <- 0
colnames(Xf) <- c("Total demanda final") 
rownames(Xf) <- t(glosas.mip.2008.12)

# tabla de demanda
Xt <- cbind(X, Xi, Xf)
Xt[is.na(Xt)] <- 0

# matriz identidad
I <- diag(12)

# matriz de coeficientes directos
A <- X / t(matrix(Xf, 12, 12))
A[is.na(A)] <- 0
colnames(A) <- t(glosas.mip.2008.12)
rownames(A) <- t(glosas.mip.2008.12)

# matriz de coeficientes indirectos
B <- solve(I-A)
B[is.na(B)] <- 0
colnames(B) <- t(glosas.mip.2008.12)
rownames(B) <- t(glosas.mip.2008.12)

# la solución del sistema anterior es $x = (I-A)^{-1}b = Lb$
Xe <- solve(B,Xf)

# matriz de sensibilidad 
p = 0.01
W <- A * (t(B)*p + diag(diag(B))*t(matrix(Xf, 12, 12))/matrix(Xf, 12, 12))
W[is.na(W)] <- 0

####################################
# Guardar las matrices
####################################

save(X, file = "matriz_insumo_producto_12x12.RData")
save(A, file = "matriz_de_coeficientes_directos_12x12.RData")
save(B, file = "matriz_de_coeficientes_indirectos_12x12.RData")
save(W, file = "matriz_de_sensibilidad_12x12.RData")

####################################
# Indicadores
####################################

# encadenamientos directos hacia atrás
dlb <- data.matrix(colSums(A))
colnames(dlb) <- c("Encadenamientos directos hacia atrás") 
dlb <- rbind(dlb, colMeans(dlb))
rownames(dlb)[rownames(dlb) == ""] <- "Promedio"

# encadenamientos directos hacia adelante
dlf <- data.matrix(rowSums(A))
colnames(dlf) <- c("Encadenamientos directos hacia adelante") 
dlf <- rbind(dlf, colMeans(dlf))

dl <- as.data.frame(cbind(dlb,dlf))

n = 12
dl$coeficiente_variacion_dlb <- n/dl[,1] * (1/(n-1) * (c(colSums(B),mean(colSums(B)))-dl[,1]/n)^2)^(1/2)
dl$coeficiente_variacion_dlf <- n/dl[,2] * (1/(n-1) * (c(rowSums(B),mean(rowSums(B)))-dl[,2]/n)^2)^(1/2)
colnames(dl)[colnames(dl) == "coeficiente_variacion_dlb"] <- "Coeficiente de variación de encadenamientos directos hacia atrás"
colnames(dl)[colnames(dl) == "coeficiente_variacion_dlf"] <- "Coeficiente de variación de encadenamientos directos hacia adelante"

dl$poder_dispersion <- dl[,1]/dl[13,1]
dl$sensibilidad_dispersion <- dl[,2]/dl[13,2]
colnames(dl)[colnames(dl) == "poder_dispersion"] <- "Poder de dispersión"
colnames(dl)[colnames(dl) == "sensibilidad_dispersion"] <- "Sensibilidad de dispersión"

dl$tipologia <- ifelse((dl[,1] < dl[13,1]) & (dl[,2] < dl[13,2]), "No manufacturera/destino final",
                          ifelse((dl[,1] < dl[13,1]) & (dl[,2] >= dl[13,2]), "No manufacturera/destino intermedio",
                                 ifelse((dl[,1] >= dl[13,1]) & (dl[,2] < dl[13,2]), "Manufacturera/destino final", "Manufacturera/destino intermedio")))
colnames(dl)[colnames(dl) == "tipologia"] <- "Tipología sectorial según multiplicadores directos"

dl$sectores_a <- ifelse((dl[,5] < 1) & (dl[,3] <= min(dl[,3])), "Sectores de bajo arrastre disperso",
                        ifelse((dl[,5] < 1) & (dl[,3] > min(dl[,3])), "Sectores de bajo arrastre y concentrado",
                               ifelse((dl[,5] >= 1) & (dl[,3] <= min(dl[,3])), "Sectores clave", "Sectores con arrastre concentrado")))
colnames(dl)[colnames(dl) == "sectores_a"] <- "Sectores clave (Tipo A)"

dl$sectores_b <- ifelse((dl[,5] < 1) & (dl[,6] >= 1), "Sectores estratégicos",
                      ifelse((dl[,5] < 1) & (dl[,6] < 1), "Sectores independientes",
                             ifelse((dl[,5] >= 1) & (dl[,6] >= 1), "Sectores clave", "Sectores impulsores")))
colnames(dl)[colnames(dl) == "sectores_b"] <- "Sectores clave (Tipo B)"

save(dl, file = "encadenamientos_directos_hacia_atras_y_adelante_12x12.RData")
write.xlsx(dl, "indicadores_sectoriales_12x12.xlsx")