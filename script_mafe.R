

library(RcmdrMisc)
library(abind)


enlace <- "https://raw.githubusercontent.com/mariafernandachingate/Tunjos/refs/heads/main/Tabla_tunjos.txt"

Tunjos <- read.table(enlace,
                     header = TRUE, row.names = 1)

str(Tunjos)



# funci?n de gr?fico de balas de proporciones para una variable

# Ingresar funci?n
BalasP <- function(x, tit, ymi,yma,las){  cat <- x
  tit = tit
  # Creamos una tabla con las frecuencias y calculamos los grados de libertad:
  LID <- data.frame(table(cat))
  DF <- sum(LID$Freq)-1
  # A?adimos a la tabla las proporciones, el error est?ndar y rangos de error:
  LID$p <- LID$Freq/sum(LID$Freq)
  LID$ser <- sqrt(LID$p*(1-LID$p)) / sqrt(sum(LID$Freq))
  LID$er80 <- qt(0.900,df=DF)*LID$ser
  LID$er95 <- qt(0.975,df=DF)*LID$ser
  LID$er99 <- qt(0.995,df=DF)*LID$ser
  print(LID)
  # Contar las categor?as:
  nc <- nrow(LID)
  # Creamos un gr?fico de las proporciones estimadas:
  plot(1:nc, LID$p, xlim=c(.5,nc+.5), pch=3, cex=2.5, ylim=c(ymi,yma),
       xaxt="n",xlab = "", ylab="Proporci?n",col=1,main=tit, las = las)
  # A?adimos las barras de rangos de error
  arrows(1:nc,LID$p-LID$er99,1:nc,LID$p+LID$er99,length=.0,angle=90,code=3,
         lend=1,lwd=3, col=1)
  arrows(1:nc,LID$p-LID$er95,1:nc,LID$p+LID$er95,length=.0,angle=90,code=3,
         lend=1,lwd=6, col=1)
  arrows(1:nc,LID$p-LID$er80,1:nc,LID$p+LID$er80,length=.0,angle=90,code=3,
         lend=1,lwd=12, col=1)
  # A?adimos los nombres de categor?as y un texto explicativo:
  axis(side=1,at=1:nc,labels=(LID$cat), las = las)
  mtext("Conf.: \u2590\u2588\ 80%  \u2588 95%  \u2590 99%",line=-1,cex=.7,col=1)
}

# Balas, proporciones y rangos de error de la variable Sexo

BalasP(Tunjos$Sexo, "Gráfico de balas de las categorías de sexo", 0, 0.4, 2)

# Balas, proporciones y rangos de error de la variable Sexo_2

s1 <- subset(Tunjos, subset = Tunjos$Sexo == "femenino")
s2 <- subset(Tunjos, subset = Tunjos$Sexo == "masculino")
FxM <- rbind(s1, s2)

par(mar = c(16,3,2,2))

BalasP(FxM$Sexo_2, "Gráfico de balas de las categorías de sexo", 0, 0.4, las = 2)

BalasP(s1$Sexo_2, "Gráfico de balas de las categorías de sexo", 0, 1, las = 2)

BalasP(s2$Sexo_2, "Gráfico de balas de las categorías de sexo", 0, 0.7, las = 2)
       