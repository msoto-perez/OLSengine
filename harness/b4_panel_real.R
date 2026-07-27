suppressMessages(source("../R/OLS_engine.R"))
suppressMessages(library(AER))
suppressMessages(library(plm))

data("Fatalities")
Fatalities$fatal_rate <- Fatalities$fatal / Fatalities$pop * 10000

cat("=== B4: CASO REAL - Fatalities (Stock & Watson), panel_engine vs plm() ===\n")
cat("48 estados, 7 anios (1982-1988), N =", nrow(Fatalities), "\n\n")

# Referencia: plm, efectos fijos de entidad (estado)
pdata <- pdata.frame(Fatalities, index = c("state", "year"))
base_fit <- plm(fatal_rate ~ beertax, data = pdata, model = "within")

# OLSengine
eng_fit <- panel_engine(fatal_rate ~ beertax, data = Fatalities,
                         entity_id = "state", time_id = "year", method = "fe")

cat("Coeficiente beertax (plm, referencia):", coef(base_fit)["beertax"], "\n")
cat("Coeficiente beertax (panel_engine):   ", eng_fit$coefficients["beertax"], "\n")
cat("Diferencia absoluta:", abs(coef(base_fit)["beertax"] - eng_fit$coefficients["beertax"]), "\n\n")

cat("Referencia de literatura: el signo esperado es NEGATIVO (mayor impuesto a la\n")
cat("cerveza -> menor tasa de mortalidad), consistente con Stock & Watson (2015),\n")
cat("cap. 10 -- el ejemplo canonico de este dataset.\n\n")

cat("=== Seleccion automatica FE/RE del motor en datos reales ===\n")
eng_auto <- panel_engine(fatal_rate ~ beertax, data = Fatalities,
                          entity_id = "state", time_id = "year", method = "auto")
cat("Metodo seleccionado:", eng_auto$method, "\n")
cat("(Es razonable esperar FE: hay heterogeneidad no observada de estado muy\n")
cat(" plausible -- cultura de manejo, densidad de carreteras, etc.)\n")
