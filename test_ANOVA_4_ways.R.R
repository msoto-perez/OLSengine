# ========================================================
# VALIDATION TEST: ANOVA IN FOUR WAYS (The Customs in Action)
# ========================================================

# Limpiar el entorno de trabajo
rm(list = ls())

# 1. PROCESO DE ACTUALIZACIÓN DEL PAQUETE
cat(">> Actualizando documentación y cargando paquete...\n")
devtools::document()
devtools::load_all()

# ========================================================
# PREPARACIÓN DE DATOS SIMULADOS
# ========================================================
set.seed(123)
n_per_group <- 40

# DATASET A: Muestras Independientes (3 grupos experimentales)
grupo_indep <- factor(rep(c("Control", "Tratamiento 1", "Tratamiento 2"), each = n_per_group))
y_normal_indep <- c(rnorm(n_per_group, 50, 5), rnorm(n_per_group, 55, 5), rnorm(n_per_group, 50, 5))
# Introducimos un sesgo masivo exponencial para forzar la no-normalidad en la segunda variable
y_sesgado_indep <- c(rexp(n_per_group, 0.1), rexp(n_per_group, 0.2), rexp(n_per_group, 0.1))

data_indep <- data.frame(grupo_indep, y_normal_indep, y_sesgado_indep)

# DATASET B: Muestras Pareadas (2 momentos: Pre y Post test)
tiempo_pareado <- factor(rep(c("Pre-Test", "Post-Test"), each = n_per_group), levels = c("Pre-Test", "Post-Test"))
y_normal_pre <- rnorm(n_per_group, 40, 5)
y_normal_post <- y_normal_pre + rnorm(n_per_group, 10, 3) # El Post depende del Pre (Pareado)
y_normal_pareado <- c(y_normal_pre, y_normal_post)

y_sesgado_pre <- rexp(n_per_group, 0.1)
y_sesgado_post <- y_sesgado_pre + rexp(n_per_group, 0.05)
y_sesgado_pareado <- c(y_sesgado_pre, y_sesgado_post)

data_pareado <- data.frame(tiempo_pareado, y_normal_pareado, y_sesgado_pareado)


# ========================================================
# EJECUCIÓN DE LAS 4 VARIANTES
# ========================================================

cat("\n\n#########################################################")
cat("\n# VARIANTE 1: Independiente Paramétrica (One-Way ANOVA) #")
cat("\n#########################################################\n")
mod1 <- paper_engine(y_normal_indep ~ grupo_indep, data = data_indep, model = "anova")
print(mod1$tables$Table4_Mean_Differences)
cat("\nMensajes de la Aduana:\n")
print(mod1$messages)


cat("\n\n#########################################################")
cat("\n# VARIANTE 2: Independiente No Paramétrica (Auto-Pilot) #")
cat("\n#########################################################\n")
# Usamos los datos sesgados y dejamos que el software tome la decisión en "auto"
mod2 <- paper_engine(y_sesgado_indep ~ grupo_indep, data = data_indep, model = "anova", non_parametric = "auto")
print(mod2$tables$Table4_Mean_Differences)
cat("\nMensajes de la Aduana:\n")
print(mod2$messages)


cat("\n\n#########################################################")
cat("\n# VARIANTE 3: Pareada Paramétrica (Paired t-test)       #")
cat("\n#########################################################\n")
# Activamos explícitamente el parámetro 'paired'
mod3 <- paper_engine(y_normal_pareado ~ tiempo_pareado, data = data_pareado, model = "anova", paired = TRUE)
print(mod3$tables$Table4_Mean_Differences)
cat("\nMensajes de la Aduana:\n")
print(mod3$messages)


cat("\n\n#########################################################")
cat("\n# VARIANTE 4: Pareada No Paramétrica (Wilcoxon explícito)#")
cat("\n#########################################################\n")
# Forzamos la prueba no paramétrica explícitamente con non_parametric = TRUE
mod4 <- paper_engine(y_sesgado_pareado ~ tiempo_pareado, data = data_pareado, model = "anova", paired = TRUE, non_parametric = TRUE)
print(mod4$tables$Table4_Mean_Differences)
cat("\nMensajes de la Aduana:\n")
print(mod4$messages)

# ========================================================
# TEST VISUAL EXTRA
# ========================================================
cat("\n\n>> Generando gráfico para la Variante 1 (revisa tu panel de Plots)...\n")
plot_engine(mod1, y_label = "Puntaje Normal", x_label = "Grupos Independientes")

