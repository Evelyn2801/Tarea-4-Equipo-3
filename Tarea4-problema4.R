library(readxl)
library(mFilter)
library(knitr)
Base <- read_excel("C:/Users/PROBOOK/Desktop/COLMEX/Segundo Semestre/Macroeconomía/Tarea 4/Base.xlsx", col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric","numeric", "numeric"))
View(Base)

#(a) Utilice datos del SIE/Valores en Circulación y de SIE/Finanzas públicas del Banco de México para describir la evolución a lo largo del
#tiempo de la composición de la deuda del gobierno mexicano por
#tipo de instrumento, madurez y moneda. Señale la implicación de lo
#que encuentre para el riesgo de impago.

# Convert to time series
ts_Base <- ts(Base[,2:ncol(Base)], start = c(2009,6), frequency = 12)

# Check the class of the object
class(ts_Base)

#VARIABLES
GPGF  <-ts_Base[,1]
IngGF  <-ts_Base[,2]
DeudaIFP <- ts_Base[,3]
DeudaIFPCP <- ts_Base[,4]
DeudaIFPLP <- ts_Base[,5]
DeudaIFPML <- ts_Base[,6]
DeudaIFPME <- ts_Base[,7]
DeudaGF<- ts_Base[,8]
DeudaGFCP<- ts_Base[,9]
DeudaGFLP<- ts_Base[,10]
DeudaGFML<- ts_Base[,11]
DeudaGFME<- ts_Base[,12]
Udibonos <- ts_Base[,13]
BONDESF <- ts_Base[,17]
BONDESG <- ts_Base[,18]
CETES <- ts_Base[,19]
BONDES <- ts_Base[,20]
BONDESD <- ts_Base[,21]
BONOS <- ts_Base[,22]


#Analizar las finanzas publicas del gobierno federal
Balance<-IngGF-GPGF
# Plot the first time series
plot.ts(Balance, ylim = c(-1000000, 10000000))
# Add the other time series to the same plot
lines(GPGF, col = "red")
lines(IngGF, col = "blue")
legend("topleft", legend = c("Balance", "Gasto Federal", "Ingreso Federal"),
       col = c("black", "red", "blue"), lty = 1)

plot.ts(DeudaGF, ylim = c(-100, 10000000))
lines(DeudaGFCP, col = "orange")
lines(DeudaGFLP, col = "brown")    
legend("topleft", legend = c("DeudaGF", "DeudaCortoplazo", "DeudaLargoPLazo"),
       col = c("black", "orange", "brown"), lty = 1)  

plot.ts(DeudaGFML, ylim = c(-100, 10000000))
lines(DeudaGFME, col = "green")    
legend("topleft", legend = c("MonedaLocal", "MonedaExtranjera"),
       col = c("black", "green"), lty = 1) 



ts.plot(ts_Base[,c(13,17:22)],ylim= c(-1000999, 3999999),  col = c("red", "blue", "green", "orange", "purple", "brown", "black"))
variable_names <- c("Udibonos", "BONDESF", "BONDESG", "CETES", "BONDES", "BONDESD", "BONOS")
colors <- c("red", "blue", "green", "orange", "purple", "brown", "black")
variable_colors <- data.frame(variable = variable_names, color = colors)
kable(variable_colors, row.names = FALSE)



#(b) Utilice datos del SIE/Valores en Circulación o para describir la composición a lo largo del tiempo de la deuda del del sector privado no Financiero mexicano por madurez y moneda. Señale la implicación
#de lo que encuentre para el riesgo de impago.
# Plot the first time series
plot.ts(DeudaIFP, ylim = c(0, 1000009))
lines(DeudaIFPCP, col = "red")
lines(DeudaIFPLP, col = "blue")
legend("topleft", legend = c("Deuda total", "Corto Plazo", "Largo Plazo"),
       col = c("black", "red", "blue"), lty = 1)


plot.ts(DeudaIFP, ylim = c(0, 1000009))
lines(DeudaIFPML, col = "orange")
lines(DeudaIFPME, col = "purple")
legend("topleft", legend = c("Deuda total", "Moneda Local", "Moneda Extranjera"),
       col = c("black", "orange", "purple"), lty = 1)

