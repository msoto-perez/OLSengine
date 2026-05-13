# ========================================================
# VALIDATION TEST 4: PANEL DATA MODEL
# ========================================================

# Clear environment for pure reproducibility
rm(list = ls())

# install directly from GitHub and load
devtools::install_github("msoto-perez/OLSengine")
library(OLSengine)

# 1. Simulate panel data
set.seed(123)
n_entities <- 50
n_time <- 3

entity_ids <- rep(1:n_entities, each = n_time)
time_ids <- rep(2018:2020, times = n_entities)

years_experience <- rep(5:7, times = n_entities) + rep(rnorm(n_entities, 0, 2), each = n_time)
publications <- rpois(n_entities * n_time, lambda = 2)

entity_ability <- rep(rnorm(n_entities, mean = 0, sd = 5), each = n_time)
entity_discipline <- rep(sample(c("Theory", "Applied"), n_entities, replace = TRUE), each = n_time)

salary_panel <- 60000 +
  entity_ability * 1000 +
  years_experience * 1200 +
  publications * 800 +
  ifelse(entity_discipline == "Applied", 5000, 0) +
  rnorm(n_entities * n_time, 0, 3000)

panel_test_data <- data.frame(
  professor_id = entity_ids,
  year = time_ids,
  salary = round(salary_panel, 0),
  years_experience = round(years_experience, 1),
  publications = publications,
  discipline = entity_discipline
)

# 2. Test Auto-Pilot (Hausman decides)
cat("\n======================================================\n")
cat("--- TEST 1: AUTO-PILOT (Hausman Test) ---\n")
cat("======================================================\n")
panel_auto <- paper_engine(
  salary ~ years_experience + publications,
  data = panel_test_data,
  model = "panel",
  entity_id = "professor_id",
  time_id = "year",
  method = "auto"
)

cat("\n>> TABLE: PANEL ESTIMATION\n")
print(panel_auto$tables[[1]])

cat("\n>> CUSTOMS MESSAGES\n")
print(panel_auto$messages)

# 3. Test Fixed Effects (explicit)
cat("\n\n======================================================\n")
cat("--- TEST 2: FIXED EFFECTS (Explicit) ---\n")
cat("======================================================\n")
panel_fe <- paper_engine(
  salary ~ years_experience + publications,
  data = panel_test_data,
  model = "panel",
  entity_id = "professor_id",
  time_id = "year",
  method = "fe"
)

cat("\n>> TABLE: FIXED EFFECTS\n")
print(panel_fe$tables$Table2_Panel_Fixed_Effects)

cat("\n>> CUSTOMS MESSAGES\n")
print(panel_fe$messages)

# 4. Test Random Effects (explicit)
cat("\n\n======================================================\n")
cat("--- TEST 3: RANDOM EFFECTS (Explicit) ---\n")
cat("======================================================\n")
panel_re <- paper_engine(
  salary ~ years_experience + publications,
  data = panel_test_data,
  model = "panel",
  entity_id = "professor_id",
  time_id = "year",
  method = "re"
)

cat("\n>> TABLE: RANDOM EFFECTS\n")
print(panel_re$tables$Table2_Panel_Random_Effects)

cat("\n>> CUSTOMS MESSAGES\n")
print(panel_re$messages)

# 5. Test plot (will fail until we add panel branch to plot_engine)
cat("\n>> GENERANDO GRÁFICO APA EN EL PANEL DE PLOTS...\n")
plot_engine(panel_fe)

