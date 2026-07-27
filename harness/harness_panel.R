# ==============================================================================
# HARNESS A2 - MOTOR PANEL
# Multi-scenario Monte Carlo validation: panel_engine vs. plm()
# Tests: (1) numerical parity given explicit method, (2) correctness of
#        Hausman-based auto-selection (method="auto") against known true DGP.
# ==============================================================================
suppressMessages(source("../R/OLS_engine.R"))
suppressMessages(library(plm))

make_panel_data <- function(n_entities, n_time, true_dgp, balanced, seed) {
  set.seed(seed)
  entity_effect <- rnorm(n_entities, 0, 5)

  if (balanced) {
    ids <- rep(1:n_entities, each = n_time)
    times <- rep(1:n_time, times = n_entities)
  } else {
    # unbalanced: each entity gets a random number of periods (2..n_time)
    obs_per_entity <- sample(2:n_time, n_entities, replace = TRUE)
    ids <- rep(1:n_entities, times = obs_per_entity)
    times <- unlist(lapply(obs_per_entity, function(k) 1:k))
  }

  n <- length(ids)
  x1 <- rnorm(n, 10, 3)

  if (true_dgp == "FE") {
    # entity effect correlated with x1 -> FE is the consistent estimator
    x1 <- x1 + entity_effect[ids] * 0.8
    y <- 5 + 1.2 * x1 + entity_effect[ids] + rnorm(n, 0, 2)
  } else {
    # RE case: entity effect uncorrelated with x1 -> RE is efficient & consistent
    y <- 5 + 1.2 * x1 + entity_effect[ids] + rnorm(n, 0, 2)
  }

  data.frame(id = ids, time = times, y = y, x1 = x1)
}

run_panel_scenario <- function(n_entities, n_time, true_dgp, balanced, seed) {
  df <- make_panel_data(n_entities, n_time, true_dgp, balanced, seed)

  # --- Explicit-method numerical parity check (FE) ---
  pdata <- plm::pdata.frame(df, index = c("id", "time"))
  base_fe <- tryCatch(plm::plm(y ~ x1, data = pdata, model = "within"),
                       error = function(e) NULL)
  eng_fe  <- tryCatch(panel_engine(y ~ x1, data = df, entity_id = "id",
                                    time_id = "time", method = "fe"),
                       error = function(e) NULL)
  fe_coef_diff <- if (!is.null(base_fe) && !is.null(eng_fe)) {
    abs(coef(base_fe)["x1"] - eng_fe$coefficients["x1"])
  } else NA

  # --- Auto-selection correctness ---
  eng_auto <- tryCatch(panel_engine(y ~ x1, data = df, entity_id = "id",
                                     time_id = "time", method = "auto"),
                        error = function(e) NULL)
  selected <- if (!is.null(eng_auto)) eng_auto$method else NA
  correct_selection <- if (!is.null(eng_auto)) {
    (true_dgp == "FE" && selected == "Fixed Effects") ||
    (true_dgp == "RE" && selected == "Random Effects")
  } else NA

  data.frame(n_entities, n_time, true_dgp, balanced, seed,
             fe_coef_diff = as.numeric(fe_coef_diff),
             selected = selected, correct_selection)
}

scenarios <- expand.grid(
  n_entities = c(20, 60),
  n_time = c(5, 10),
  true_dgp = c("FE", "RE"),
  balanced = c(TRUE, FALSE)
)

n_seeds <- 150
results <- list()
idx <- 1
for (s in seq_len(nrow(scenarios))) {
  for (seed in seq_len(n_seeds)) {
    results[[idx]] <- tryCatch(
      run_panel_scenario(scenarios$n_entities[s], scenarios$n_time[s],
                          scenarios$true_dgp[s], scenarios$balanced[s],
                          seed + 20000),
      error = function(e) data.frame(n_entities=scenarios$n_entities[s],
                                      n_time=scenarios$n_time[s],
                                      true_dgp=scenarios$true_dgp[s],
                                      balanced=scenarios$balanced[s],
                                      seed=seed+20000, fe_coef_diff=NA,
                                      selected=NA, correct_selection=NA)
    )
    idx <- idx + 1
  }
}
res <- do.call(rbind, results)

cat("=== PANEL ENGINE (FE): numerical parity vs plm(), n runs =", nrow(res), "===\n")
cat("Max FE coefficient difference:", max(res$fe_coef_diff, na.rm=TRUE), "\n")
cat("Mean FE coefficient difference:", mean(res$fe_coef_diff, na.rm=TRUE), "\n")
cat("NA/failed runs:", sum(is.na(res$fe_coef_diff)), "of", nrow(res), "\n\n")

cat("=== HAUSMAN AUTO-SELECTION: correctness by scenario ===\n")
tab <- aggregate(correct_selection ~ true_dgp + balanced + n_entities + n_time,
                  data = res, FUN = function(x) mean(x, na.rm = TRUE))
print(tab)

cat("\n=== Overall auto-selection accuracy ===\n")
cat(sprintf("%.3f (n=%d)\n", mean(res$correct_selection, na.rm=TRUE), sum(!is.na(res$correct_selection))))

saveRDS(res, "results_panel.rds")
