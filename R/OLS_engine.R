#################################################
# OLSengine - CORE MATHEMATICAL ENGINES & CUSTOMS
# Stage 3: Functional Prototype (English Version)
#################################################

#' @importFrom stats aggregate aov binomial coef cor fitted glm hatvalues kruskal.test ks.test lm median model.matrix na.omit pchisq pf pnorm pt qnorm qt quantile residuals sd setNames shapiro.test t.test wilcox.test
#' @importFrom graphics abline arrows axis box lines par points text
NULL

# =========================================================
# 1. PRE-ESTIMATION FILTER (Data Integrity Customs)
# =========================================================
# Internal function: Data integrity filter (not exported)
pre_estimation_filter <- function(formula, data, extra_vars = NULL) {

  # Extract only the variables used in the formula
  vars_in_model <- all.vars(formula)

  # Add extra variables (for panel data: entity_id and time_id)
  if (!is.null(extra_vars)) {
    vars_in_model <- unique(c(vars_in_model, extra_vars))
  }

  # Validate that variables exist in the dataset
  missing_vars <- setdiff(vars_in_model, names(data))
  if(length(missing_vars) > 0) {
    stop(paste("Structural Error: The following variables do not exist in the dataset:",
               paste(missing_vars, collapse = ", ")))
  }

  # Audit and clean NAs (Listwise deletion)
  model_data <- data[, vars_in_model, drop = FALSE]
  n_original <- nrow(model_data)
  clean_data <- na.omit(model_data)
  n_clean <- nrow(clean_data)

  n_dropped <- n_original - n_clean
  messages <- character(0)

  if (n_dropped > 0) {
    pct_dropped <- round((n_dropped / n_original) * 100, 1)
    msg <- sprintf("Pre-Estimation Customs: %d observations (%.1f%%) were removed due to missing values (NA). Final N = %d.",
                   n_dropped, pct_dropped, n_clean)
    messages <- c(messages, msg)

    # Critical alert if more than 10% of the sample is lost
    if (pct_dropped > 10) {
      messages <- c(messages, "CRITICAL WARNING: More than 10% of the sample was lost due to incomplete data. Consider potential bias in your results.")
    }
  }

  return(list(
    status = "ok",
    clean_data = clean_data,
    messages = messages,
    n_final = n_clean
  ))
}

# =========================================================
# 2. OLS ENGINE (Mathematical Core & Robust SE)
# =========================================================
# Internal function: Data integrity filter (not exported)
ols_engine <- function(formula, data, robust = FALSE) {

  # 1. Base OLS Estimation
  model <- lm(formula, data = data)
  sum_mod <- summary(model)
  n_obs <- nrow(data)

  # 2. Diagnostics: Breusch-Pagan Test (Heteroskedasticity in Base R)
  e <- residuals(model)
  X_matrix <- model.matrix(model)

  # Auxiliary regression for BP test (Koenker's version)
  aux_mod <- lm(I(e^2) ~ X_matrix - 1)
  LM_stat <- n_obs * summary(aux_mod)$r.squared
  bp_p_value <- pchisq(LM_stat, df = ncol(X_matrix) - 1, lower.tail = FALSE)
  heteroskedasticity_detected <- (bp_p_value < 0.05)

  # 3. Diagnostics: Normality Test (Base R)
  if (n_obs <= 5000) {
    norm_p <- shapiro.test(e)$p.value
    norm_method <- "Shapiro-Wilk"
  } else {
    norm_p <- ks.test(scale(e), "pnorm")$p.value
    norm_method <- "Kolmogorov-Smirnov"
  }

  # 4. Diagnostics: VIF calculation (Base R Matrix Algebra)
  X_preds <- X_matrix[, -1, drop = FALSE] # Remove intercept
  if(ncol(X_preds) > 1) {
    vif_vals <- diag(solve(cor(X_preds)))
  } else {
    vif_vals <- 1 # VIF is 1 for a single predictor
  }

  # 5. Diagnostics: Cohen's f2 for predictors (f2 = t^2 / df)
  t_stats <- sum_mod$coefficients[-1, "t value"]
  f2_vals <- (t_stats^2) / model$df.residual

  # 6. Customs Decision Logic (The 3 paths for Robust SE)
  use_robust <- FALSE
  aduana_msgs <- character(0)

  if (identical(robust, "auto")) {
    if (heteroskedasticity_detected) {
      use_robust <- TRUE
      aduana_msgs <- c(aduana_msgs, "Customs Auto-Pilot: Heteroskedasticity detected (p < .05). Robust Standard Errors (HC3) were applied automatically (Hayes & Cai, 2007).")
    } else {
      aduana_msgs <- c(aduana_msgs, "Customs Auto-Pilot: Homoskedasticity confirmed. Classic OLS Standard Errors were used.")
    }
  } else if (isTRUE(robust)) {
    use_robust <- TRUE
    aduana_msgs <- c(aduana_msgs, "Customs: Robust Standard Errors (HC3) applied per explicit user request.")
  } else {
    # robust = FALSE (Default)
    if (heteroskedasticity_detected) {
      aduana_msgs <- c(aduana_msgs, "CRITICAL WARNING: Heteroskedasticity detected (Breusch-Pagan p < .05). Reported classic standard errors are biased. It is highly recommended to run the model with 'robust = TRUE' or 'robust = \"auto\"'.")
    } else {
      aduana_msgs <- c(aduana_msgs, "Customs: The Homoskedasticity assumption is met (Breusch-Pagan p > .05).")
    }
  }

  # 7. Coefficient Extraction and HC3 Calculation
  coef_table <- as.data.frame(sum_mod$coefficients)
  names(coef_table) <- c("B", "SE", "t", "p")
  coef_table$Predictor <- rownames(coef_table) # Guardamos los nombres de las variables

  if (use_robust) {
    # HC3 Mathematics: V(b) = (X'X)^-1 X' diag(e_hc3^2) X (X'X)^-1
    h <- hatvalues(model)
    e_hc3 <- e / (1 - h)
    bread <- chol2inv(chol(crossprod(X_matrix)))
    meat <- crossprod(X_matrix * e_hc3)
    vcov_hc3 <- bread %*% meat %*% bread

    coef_table$SE <- sqrt(diag(vcov_hc3))
    coef_table$t <- coef_table$B / coef_table$SE
    coef_table$p <- 2 * pt(-abs(coef_table$t), df = model$df.residual)
  }

  diagnostics <- list(
    R2 = sum_mod$r.squared,
    Adj_R2 = sum_mod$adj.r.squared,
    F_stat = sum_mod$fstatistic[1],
    F_df1 = sum_mod$fstatistic[2],
    F_df2 = sum_mod$fstatistic[3],
    F_p = pf(sum_mod$fstatistic[1], sum_mod$fstatistic[2], sum_mod$fstatistic[3], lower.tail = FALSE),
    N = n_obs,
    bp_p_value = bp_p_value,
    norm_p_value = norm_p,
    norm_method = norm_method
  )

  return(list(
    coefficients = setNames(coef_table$B, coef_table$Predictor),
    se_classic = setNames(coef_table$SE, coef_table$Predictor),
    df_residual = model$df.residual,
    f2 = f2_vals,
    vif = vif_vals,
    diagnostics = diagnostics,
    aduana_msgs = aduana_msgs
  ))
}

# =========================================================
# 3. ANOVA / T-TEST ENGINE (Mathematical Core & 4 Variants)
# =========================================================
# Internal function: Data integrity filter (not exported)
anova_engine <- function(formula, data, non_parametric = FALSE, paired = FALSE) {

  y_name <- all.vars(formula)[1]
  x_name <- all.vars(formula)[2]
  y_val <- data[[y_name]]
  group_val <- as.factor(data[[x_name]])
  k_groups <- length(levels(group_val))
  n_total <- nrow(data)

  # 1. DIAGNOSTICS: Normality and Homogeneity
  fit_aov <- aov(formula, data = data)
  norm_p <- shapiro.test(residuals(fit_aov))$p.value

  # Levene's Test (Median-based, robust)
  medians <- tapply(y_val, group_val, median, na.rm = TRUE)
  abs_deviations <- abs(y_val - medians[group_val])
  fit_levene <- aov(abs_deviations ~ group_val)
  levene_p <- summary(fit_levene)[[1]]$`Pr(>F)`[1]

  # 2. CUSTOMS DECISION LOGIC (The 3 paths)
  use_np <- FALSE
  aduana_msgs <- character(0)

  if (identical(non_parametric, "auto")) {
    if (norm_p < 0.05) {
      use_np <- TRUE
      aduana_msgs <- c(aduana_msgs, "Customs Auto-Pilot: Severe non-normality detected (Shapiro p < .05). Transitioned automatically to Non-Parametric tests to protect validity.")
    } else {
      aduana_msgs <- c(aduana_msgs, "Customs Auto-Pilot: Normality assumption met. Parametric tests were used.")
    }
  } else if (isTRUE(non_parametric)) {
    use_np <- TRUE
    aduana_msgs <- c(aduana_msgs, "Customs: Non-Parametric test applied per explicit user request.")
  } else {
    # Default (FALSE)
    if (norm_p < 0.05) {
      aduana_msgs <- c(aduana_msgs, "CRITICAL WARNING: Non-normal distribution detected (Shapiro p < .05). Parametric results might be biased. Consider using 'non_parametric = TRUE' or 'non_parametric = \"auto\"'.")
    } else {
      aduana_msgs <- c(aduana_msgs, "Customs: Normality assumption of residuals is met (Shapiro p > .05).")
    }
  }

  # 3. EXECUTION OF THE 4 VARIANTS
  if (paired) {
    if (k_groups > 2) stop("Structural Error: Repeated measures (>2 paired groups) require subject IDs. This version supports 2 paired groups (Pre/Post).")

    # R Base doesn't support 'paired' in formula. We must split the data manually.
    levels_group <- levels(group_val)
    y_group1 <- y_val[group_val == levels_group[1]]
    y_group2 <- y_val[group_val == levels_group[2]]

    if (length(y_group1) != length(y_group2)) stop("Structural Error: Paired tests require equal number of observations in both groups.")

    if (use_np) {
      # Variant 4: Paired Non-Parametric (Wilcoxon)
      test_res <- wilcox.test(x = y_group1, y = y_group2, paired = TRUE, exact = FALSE)
      test_name <- "Wilcoxon Signed-Rank"
      stat_name <- "V"
      stat_val <- test_res$statistic
      p_val <- test_res$p.value
      df_val <- NA
      eff_name <- "Effect Size (r)"
      eff_val <- abs(qnorm(p_val / 2)) / sqrt(n_total) # R base math approx
    } else {
      # Variant 3: Paired Parametric (T-test)
      test_res <- t.test(x = y_group1, y = y_group2, paired = TRUE)
      test_name <- "Paired t-test"
      stat_name <- "t"
      stat_val <- test_res$statistic
      p_val <- test_res$p.value
      df_val <- test_res$parameter
      eff_name <- "Cohen's d (dz)"
      eff_val <- abs(stat_val) / sqrt(n_total / 2)
    }
  } else {
    if (use_np) {
      # Variant 2: Independent Non-Parametric (Kruskal/Mann-Whitney)
      test_res <- kruskal.test(formula, data = data)
      test_name <- ifelse(k_groups == 2, "Mann-Whitney U (via Kruskal)", "Kruskal-Wallis")
      stat_name <- "Chi-square"
      stat_val <- test_res$statistic
      p_val <- test_res$p.value
      df_val <- test_res$parameter
      eff_name <- "Eta-squared (H)"
      eff_val <- (stat_val - k_groups + 1) / (n_total - k_groups)
    } else {
      # Variant 1: Independent Parametric (ANOVA)
      aov_table <- summary(fit_aov)[[1]]
      test_name <- "One-Way ANOVA"
      stat_name <- "F"
      stat_val <- aov_table$`F value`[1]
      p_val <- aov_table$`Pr(>F)`[1]
      df_val <- aov_table$`Df`[1]
      ss_eff <- aov_table$`Sum Sq`[1]
      ss_res <- aov_table$`Sum Sq`[2]
      eff_name <- "Partial Eta-sq"
      eff_val <- ss_eff / (ss_eff + ss_res)

      if (levene_p < 0.05) {
        aduana_msgs <- c(aduana_msgs, "WARNING (Homogeneity): p < .05 in Levene's test. Unequal variances. Suggested: Welch's ANOVA (Lakens, 2013).")
      }
    }
  }

  # 4. ASSEMBLE RAW RESULTS
  effects_table <- data.frame(
    Test = test_name,
    Metric = stat_name,
    Statistic = stat_val,
    df = ifelse(is.na(df_val), "-", as.character(round(df_val, 2))),
    p_value = p_val,
    Effect_Metric = eff_name,
    Effect_Size = eff_val
  )

  list(
    effects_table = effects_table,
    diagnostics = list(n = n_total, levene_p_value = levene_p, norm_p_value = norm_p),
    aduana_msgs = aduana_msgs
  )
}

# =========================================================
# 4. LOGIT ENGINE (Mathematical Core for Binary Outcomes)
# =========================================================
# Internal function: Data integrity filter (not exported)
logit_engine <- function(formula, data) {

  fit <- glm(formula, data = data, family = binomial(link = "logit"))
  sum_fit <- summary(fit)

  coefs <- coef(fit)
  se <- sum_fit$coefficients[, "Std. Error"]
  z_stats <- sum_fit$coefficients[, "z value"]
  odds_ratios <- exp(coefs)

  null_dev <- fit$null.deviance
  res_dev <- fit$deviance
  n <- nrow(data)

  # McFadden Pseudo R² (original metric)
  pseudo_r2_mcfadden <- 1 - (res_dev / null_dev)

  # Nagelkerke Pseudo R² (normalized to [0,1] range for better interpretability)
  r2_cox_snell <- 1 - exp((res_dev - null_dev) / n)
  r2_max <- 1 - exp(-null_dev / n)
  pseudo_r2_nagelkerke <- r2_cox_snell / r2_max

  aduana_msgs <- character(0)

  # A. Check for Perfect or Quasi-Perfect Separation
  perfect_separation <- any(se > 50)

  # B. Hosmer-Lemeshow Goodness of Fit Test (Base R Manual Implementation)
  fitted_probs <- fitted(fit)
  y_actual <- fit$y

  decile_breaks <- unique(quantile(fitted_probs, probs = seq(0, 1, by = 0.1), na.rm = TRUE))
  if(length(decile_breaks) < 3) decile_breaks <- c(0, 0.5, 1)
  deciles <- cut(fitted_probs, breaks = decile_breaks, include.lowest = TRUE)

  obs_1 <- tapply(y_actual, deciles, sum)
  obs_0 <- tapply(1 - y_actual, deciles, sum)
  exp_1 <- tapply(fitted_probs, deciles, sum)
  exp_0 <- tapply(1 - fitted_probs, deciles, sum)

  hl_stat <- sum((obs_1 - exp_1)^2 / exp_1 + (obs_0 - exp_0)^2 / exp_0, na.rm = TRUE)
  g <- length(unique(deciles))
  hl_p_value <- pchisq(hl_stat, df = max(1, g - 2), lower.tail = FALSE)

  # Classification Accuracy (Threshold = 0.5)
  predicted_classes <- ifelse(fitted_probs > 0.5, 1, 0)
  accuracy <- sum(predicted_classes == y_actual) / length(y_actual)

  list(
    coefficients = coefs,
    se = se,
    z_stats = z_stats,
    odds_ratios = odds_ratios,
    pseudo_r2_mcfadden = pseudo_r2_mcfadden,
    pseudo_r2_nagelkerke = pseudo_r2_nagelkerke,
    diagnostics = list(
      perfect_separation = perfect_separation,
      hl_stat = hl_stat,
      hl_p_value = hl_p_value,
      accuracy = accuracy,
      n = n
    ),
    aduana_msgs = aduana_msgs
  )
}

# =========================================================
# 5. PANEL DATA ENGINE (Fixed & Random Effects + Hausman)
# =========================================================

# Internal function: Panel data estimation (not exported)
panel_engine <- function(formula, data, entity_id, time_id, method = "auto") {

  # Extract variables from formula
  vars_in_model <- all.vars(formula)
  y_name <- vars_in_model[1]
  x_names <- vars_in_model[-1]

  # Validate panel structure
  if (!entity_id %in% names(data)) stop("entity_id variable not found in data")
  if (!time_id %in% names(data)) stop("time_id variable not found in data")

  # Create panel structure
  data <- data[order(data[[entity_id]], data[[time_id]]), ]
  entities <- data[[entity_id]]
  time_periods <- data[[time_id]]
  y <- data[[y_name]]
  X <- as.matrix(data[, x_names, drop = FALSE])

  n_obs <- nrow(data)
  n_entities <- length(unique(entities))
  n_time <- length(unique(time_periods))
  k <- ncol(X)

  aduana_msgs <- character(0)

  # Check for balanced panel
  entity_counts <- table(entities)
  is_balanced <- all(entity_counts == n_time)
  if (!is_balanced) {
    aduana_msgs <- c(aduana_msgs, sprintf("INFO: Unbalanced panel detected. Entity observations range from %d to %d time periods.", min(entity_counts), max(entity_counts)))
  }

  # ============================================
  # A. FIXED EFFECTS (Within Estimator)
  # ============================================
  # Demean by entity (within transformation)
  y_entity_means <- tapply(y, entities, mean, na.rm = TRUE)[as.character(entities)]

  # Handle X demeaning carefully for single or multiple predictors
  if (k == 1) {
    X_entity_means <- tapply(X[,1], entities, mean, na.rm = TRUE)[as.character(entities)]
    X_entity_means <- matrix(X_entity_means, ncol = 1)
  } else {
    X_entity_means <- apply(X, 2, function(col) tapply(col, entities, mean, na.rm = TRUE)[as.character(entities)])
  }

  y_demeaned <- y - y_entity_means
  X_demeaned <- X - X_entity_means

  # OLS on demeaned data (within estimator)
  XX_inv <- solve(crossprod(X_demeaned))
  beta_fe <- as.vector(XX_inv %*% crossprod(X_demeaned, y_demeaned))
  names(beta_fe) <- x_names

  # Residuals and variance
  resid_fe <- y_demeaned - as.vector(X_demeaned %*% beta_fe)
  sigma2_fe <- sum(resid_fe^2) / (n_obs - n_entities - k)
  vcov_fe <- sigma2_fe * XX_inv
  se_fe <- sqrt(diag(vcov_fe))

  # R² within
  tss_within <- sum((y_demeaned - mean(y_demeaned))^2)
  rss_within <- sum(resid_fe^2)
  r2_within <- max(0, 1 - (rss_within / tss_within))

  # ============================================
  # B. RANDOM EFFECTS (GLS Estimator)
  # ============================================
  # Between estimator (entity-level means)
  entity_data <- aggregate(cbind(y, X), by = list(entities), FUN = mean, na.rm = TRUE)
  y_between <- entity_data[, 2]
  X_between <- as.matrix(entity_data[, -c(1, 2), drop = FALSE])

  XX_between_inv <- solve(crossprod(X_between))
  beta_between <- as.vector(XX_between_inv %*% crossprod(X_between, y_between))
  resid_between <- y_between - as.vector(X_between %*% beta_between)
  sigma2_between <- sum(resid_between^2) / (n_entities - k - 1)

  # Theta for GLS transformation (Swamy-Arora)
  sigma2_within <- sigma2_fe
  if (is_balanced) {
    theta <- 1 - sqrt(sigma2_within / (sigma2_within + n_time * sigma2_between))
  } else {
    # Average T for unbalanced panels
    theta <- 1 - sqrt(sigma2_within / (sigma2_within + mean(entity_counts) * sigma2_between))
  }

  # Quasi-demeaned data for RE
  y_quasi <- y - theta * y_entity_means
  X_quasi <- X - theta * X_entity_means

  # Add intercept for RE
  X_quasi_int <- cbind(1, X_quasi)
  XX_re_inv <- solve(crossprod(X_quasi_int))
  beta_re_full <- as.vector(XX_re_inv %*% crossprod(X_quasi_int, y_quasi))
  beta_re <- beta_re_full[-1]  # Remove intercept
  intercept_re <- beta_re_full[1]
  names(beta_re) <- x_names

  # RE standard errors
  resid_re <- y_quasi - as.vector(X_quasi_int %*% beta_re_full)
  sigma2_re <- sum(resid_re^2) / (n_obs - k - 1)
  vcov_re <- sigma2_re * XX_re_inv
  se_re_full <- sqrt(diag(vcov_re))
  se_re <- se_re_full[-1]

  # R² overall (for RE)
  y_grand_mean <- mean(y)
  tss_overall <- sum((y - y_grand_mean)^2)
  y_fitted_re <- intercept_re + as.vector(X %*% beta_re)
  rss_overall <- sum((y - y_fitted_re)^2)
  r2_overall <- max(0, 1 - (rss_overall / tss_overall))

  # ============================================
  # C. HAUSMAN TEST
  # ============================================
  # H0: RE is consistent and efficient (use RE)
  # H1: Only FE is consistent (use FE)
  beta_diff <- beta_fe - beta_re
  vcov_diff <- vcov_fe - vcov_re[-1, -1, drop = FALSE]

  # Check if vcov_diff is positive definite
  eigenvalues <- eigen(vcov_diff, only.values = TRUE)$values
  if (any(eigenvalues < -1e-10)) {
    hausman_stat <- NA
    hausman_p <- NA
    hausman_warning <- "WARNING: Hausman test covariance matrix is not positive definite. This can occur with highly collinear predictors or small between-entity variation. Test is inconclusive."
    aduana_msgs <- c(aduana_msgs, hausman_warning)
  } else {
    # Force symmetry and positive definiteness
    vcov_diff <- (vcov_diff + t(vcov_diff)) / 2
    vcov_diff <- vcov_diff + diag(1e-10, nrow(vcov_diff))

    tryCatch({
      hausman_stat <- as.numeric(t(beta_diff) %*% solve(vcov_diff) %*% beta_diff)
      hausman_p <- pchisq(hausman_stat, df = k, lower.tail = FALSE)
    }, error = function(e) {
      hausman_stat <<- NA
      hausman_p <<- NA
      aduana_msgs <<- c(aduana_msgs, "WARNING: Hausman test computation failed numerically. Consider using Fixed Effects as conservative default.")
    })
  }

  # ============================================
  # D. AUTO MODEL SELECTION
  # ============================================
  use_fe <- FALSE
  if (identical(method, "auto")) {
    if (!is.na(hausman_p) && hausman_p < 0.05) {
      use_fe <- TRUE
      aduana_msgs <- c(aduana_msgs, sprintf("Customs Auto-Pilot: Hausman test rejects random effects (p = %.3f). Fixed Effects estimator selected to control for entity-specific unobserved heterogeneity (Wooldridge, 2010).", hausman_p))
    } else if (!is.na(hausman_p)) {
      aduana_msgs <- c(aduana_msgs, sprintf("Customs Auto-Pilot: Hausman test supports random effects (p = %.3f). Random Effects estimator selected for efficiency gains (Wooldridge, 2010).", hausman_p))
    } else {
      use_fe <- TRUE
      aduana_msgs <- c(aduana_msgs, "Customs Auto-Pilot: Hausman test inconclusive. Fixed Effects selected as conservative default.")
    }
  } else if (method == "fe") {
    use_fe <- TRUE
    aduana_msgs <- c(aduana_msgs, "Customs: Fixed Effects applied per explicit user request.")
  } else if (method == "re") {
    aduana_msgs <- c(aduana_msgs, "Customs: Random Effects applied per explicit user request.")
  }

  # Return selected model
  if (use_fe) {
    return(list(
      coefficients = beta_fe,
      se = se_fe,
      method = "Fixed Effects",
      r2 = r2_within,
      sigma2 = sigma2_fe,
      n_obs = n_obs,
      n_entities = n_entities,
      n_time = n_time,
      df_residual = n_obs - n_entities - k,
      hausman_stat = hausman_stat,
      hausman_p = hausman_p,
      diagnostics = list(
        r2 = r2_within,
        n = n_obs,
        n_entities = n_entities,
        n_time = n_time
      ),
      aduana_msgs = aduana_msgs
    ))
  } else {
    return(list(
      coefficients = beta_re,
      se = se_re,
      intercept = intercept_re,
      method = "Random Effects",
      r2 = r2_overall,
      sigma2 = sigma2_re,
      n_obs = n_obs,
      n_entities = n_entities,
      n_time = n_time,
      df_residual = n_obs - k - 1,
      hausman_stat = hausman_stat,
      hausman_p = hausman_p,
      diagnostics = list(
        r2 = r2_overall,
        n = n_obs,
        n_entities = n_entities,
        n_time = n_time
      ),
      aduana_msgs = aduana_msgs
    ))
  }
}

# =========================================================
# 6. INSTRUMENTAL VARIABLES ENGINE (2SLS in Base R)
# =========================================================
# Internal function: IV estimation (not exported)
iv_engine <- function(formula, data, instruments) {

  # Parse formula: y ~ x1 + x2 + ... (endogenous predictors)
  vars_in_model <- all.vars(formula)
  y_name <- vars_in_model[1]
  x_names <- vars_in_model[-1]

  # Parse instruments formula: ~ z1 + z2 + ... (instruments)
  # Handle both formula and character vector
  if (inherits(instruments, "formula")) {
    z_names <- all.vars(instruments)
  } else {
    z_names <- instruments
  }

  # Validate that instrument variables exist in data
  missing_z <- setdiff(z_names, names(data))
  if (length(missing_z) > 0) {
    stop("Structural Error: The following instruments do not exist in the dataset: ",
         paste(missing_z, collapse = ", "))
  }

  # Extract data
  y <- data[[y_name]]
  X <- as.matrix(data[, x_names, drop = FALSE])
  Z <- as.matrix(data[, z_names, drop = FALSE])

  n <- nrow(data)
  k_x <- ncol(X)
  k_z <- ncol(Z)

  aduana_msgs <- character(0)

  # Check identification
  if (k_z < k_x) {
    stop("Structural Error: Under-identification. Number of instruments (", k_z,
         ") must be >= number of endogenous variables (", k_x, ").")
  }

  # Check for multicollinearity in instruments
  if (k_z > 1) {
    z_cor <- cor(Z)
    max_z_cor <- max(abs(z_cor[upper.tri(z_cor)]))
    if (max_z_cor > 0.95) {
      aduana_msgs <- c(aduana_msgs, sprintf("WARNING: High correlation (%.3f) detected between instruments. This may indicate redundancy.", max_z_cor))
    }
  }

  # ============================================
  # STAGE 1: First-Stage Regressions (X ~ Z)
  # ============================================

  # Add intercept to Z
  Z_int <- cbind(1, Z)

  # First stage: regress each endogenous variable on all instruments
  X_fitted <- matrix(NA, nrow = n, ncol = k_x)
  first_stage_r2 <- numeric(k_x)
  first_stage_fstat <- numeric(k_x)

  for (j in 1:k_x) {
    # OLS: X_j = Z * gamma + error
    ZZ_inv <- solve(crossprod(Z_int))
    gamma_j <- as.vector(ZZ_inv %*% crossprod(Z_int, X[, j]))
    X_fitted[, j] <- as.vector(Z_int %*% gamma_j)

    # R² and F-stat for first stage
    resid_fs <- X[, j] - X_fitted[, j]
    tss_fs <- sum((X[, j] - mean(X[, j]))^2)
    rss_fs <- sum(resid_fs^2)
    first_stage_r2[j] <- 1 - (rss_fs / tss_fs)

    # F-stat for joint significance of instruments (excluding intercept)
    # F = (R²/(k_z)) / ((1-R²)/(n - k_z - 1))
    first_stage_fstat[j] <- (first_stage_r2[j] / k_z) / ((1 - first_stage_r2[j]) / (n - k_z - 1))
  }

  # Weak instruments test (Stock & Yogo, 2005)
  min_fstat <- min(first_stage_fstat)
  if (min_fstat < 10) {
    aduana_msgs <- c(aduana_msgs, sprintf("CRITICAL WARNING (Weak Instruments): Minimum first-stage F-statistic = %.2f (< 10). Instruments may be too weak for reliable causal inference (Stock & Yogo, 2005).", min_fstat))
  } else {
    aduana_msgs <- c(aduana_msgs, sprintf("INFO: First-stage F-statistic = %.2f (> 10). Instruments pass relevance threshold (Stock & Yogo, 2005).", min_fstat))
  }

  # ============================================
  # STAGE 2: Second-Stage Regression (Y ~ X̂)
  # ============================================

  # Add intercept to fitted X
  X_fitted_int <- cbind(1, X_fitted)

  # OLS: Y = X̂ * beta + error
  XX_inv <- solve(crossprod(X_fitted_int))
  beta_2sls <- as.vector(XX_inv %*% crossprod(X_fitted_int, y))

  # Residuals (using ORIGINAL X, not fitted)
  X_int <- cbind(1, X)
  resid_2sls <- y - as.vector(X_int %*% beta_2sls)

  # Variance estimation (robust to heteroskedasticity)
  sigma2_2sls <- sum(resid_2sls^2) / (n - k_x - 1)

  # Standard errors (need to account for first stage uncertainty)
  # Use Z as instrument matrix for variance
  PZ <- Z_int %*% solve(crossprod(Z_int)) %*% t(Z_int)
  X_tilde <- PZ %*% X_int
  vcov_2sls <- sigma2_2sls * solve(crossprod(X_tilde))
  se_2sls <- sqrt(diag(vcov_2sls))

  # Coefficients (remove intercept for output)
  beta_iv <- beta_2sls[-1]
  se_iv <- se_2sls[-1]
  names(beta_iv) <- x_names
  names(se_iv) <- x_names

  # R² (warning: R² can be negative or > 1 in IV, so report with caution)
  tss <- sum((y - mean(y))^2)
  rss <- sum(resid_2sls^2)
  r2_iv <- 1 - (rss / tss)

  # ============================================
  # OVERIDENTIFICATION TEST (Sargan/Hansen)
  # ============================================

  sargan_stat <- NA
  sargan_p <- NA

  if (k_z > k_x) {
    # Overidentified: more instruments than endogenous variables
    # Sargan test: regress residuals on all instruments
    Z_int_resid <- Z_int
    ZZ_inv_resid <- solve(crossprod(Z_int_resid))
    gamma_resid <- as.vector(ZZ_inv_resid %*% crossprod(Z_int_resid, resid_2sls))
    resid_fitted <- as.vector(Z_int_resid %*% gamma_resid)

    # Sargan stat = n * R² from auxiliary regression
    tss_aux <- sum((resid_2sls - mean(resid_2sls))^2)
    rss_aux <- sum((resid_2sls - resid_fitted)^2)
    r2_aux <- 1 - (rss_aux / tss_aux)
    sargan_stat <- n * r2_aux
    sargan_p <- pchisq(sargan_stat, df = k_z - k_x, lower.tail = FALSE)

    if (sargan_p < 0.05) {
      aduana_msgs <- c(aduana_msgs, sprintf("WARNING (Overidentification): Sargan test p = %.3f (< .05). Some instruments may be invalid (correlated with error term).", sargan_p))
    } else {
      aduana_msgs <- c(aduana_msgs, sprintf("INFO: Sargan test p = %.3f (> .05). Overidentifying restrictions are satisfied.", sargan_p))
    }
  } else if (k_z == k_x) {
    aduana_msgs <- c(aduana_msgs, "INFO: Exact identification (# instruments = # endogenous variables). Sargan test not applicable.")
  }

  # ============================================
  # RETURN RESULTS
  # ============================================

  return(list(
    coefficients = beta_iv,
    se = se_iv,
    first_stage_fstat = first_stage_fstat,
    first_stage_r2 = first_stage_r2,
    r2 = r2_iv,
    sargan_stat = sargan_stat,
    sargan_p = sargan_p,
    n_obs = n,
    n_instruments = k_z,
    n_endogenous = k_x,
    df_residual = n - k_x - 1,
    diagnostics = list(
      r2 = r2_iv,
      n = n,
      first_stage_fstat = min(first_stage_fstat),
      sargan_p = sargan_p
    ),
    aduana_msgs = aduana_msgs
  ))
}
# =========================================================
# 7. DIFFERENCE-IN-DIFFERENCES ENGINE (DiD in Base R)
# =========================================================
# Internal function: DiD estimation (not exported)
did_engine <- function(formula, data, treatment_var, time_var, treatment_level = NULL, post_level = NULL) {

  # Parse formula: y ~ controls (if any)
  vars_in_model <- all.vars(formula)
  y_name <- vars_in_model[1]
  control_vars <- if (length(vars_in_model) > 1) vars_in_model[-1] else NULL

  # Validate treatment and time variables exist
  if (!treatment_var %in% names(data)) stop("treatment_var not found in data")
  if (!time_var %in% names(data)) stop("time_var not found in data")

  # Extract variables
  y <- data[[y_name]]
  treatment <- as.factor(data[[treatment_var]])
  time <- as.factor(data[[time_var]])

  # Auto-detect levels if not provided
  if (is.null(treatment_level)) {
    treatment_level <- levels(treatment)[2]  # Assume second level is treated
  }
  if (is.null(post_level)) {
    post_level <- levels(time)[2]  # Assume second level is post
  }

  # Create binary indicators
  treated <- as.integer(treatment == treatment_level)
  post <- as.integer(time == post_level)
  did_interaction <- treated * post

  n <- nrow(data)
  aduana_msgs <- character(0)

  # Check for balanced design
  group_counts <- table(treatment, time)
  is_balanced <- length(unique(as.vector(group_counts))) == 1
  if (!is_balanced) {
    aduana_msgs <- c(aduana_msgs, "INFO: Unbalanced design detected. DiD estimates may be sensitive to sample composition.")
  }

  # ============================================
  # DiD REGRESSION: Y ~ Treated + Post + (Treated × Post) + Controls
  # ============================================

  # Build design matrix
  if (is.null(control_vars)) {
    X <- cbind(1, treated, post, did_interaction)
    colnames(X) <- c("(Intercept)", "Treated", "Post", "Treated:Post")
  } else {
    X_controls <- as.matrix(data[, control_vars, drop = FALSE])
    X <- cbind(1, treated, post, did_interaction, X_controls)
    colnames(X) <- c("(Intercept)", "Treated", "Post", "Treated:Post", control_vars)
  }

  # OLS estimation
  XX_inv <- solve(crossprod(X))
  beta_did <- as.vector(XX_inv %*% crossprod(X, y))
  names(beta_did) <- colnames(X)

  # Residuals and variance
  resid_did <- y - as.vector(X %*% beta_did)
  k <- ncol(X)
  df_residual <- n - k
  sigma2 <- sum(resid_did^2) / df_residual
  vcov_did <- sigma2 * XX_inv
  se_did <- sqrt(diag(vcov_did))

  # DiD coefficient is the interaction term
  did_coef <- beta_did["Treated:Post"]
  did_se <- se_did["Treated:Post"]
  did_t <- did_coef / did_se
  did_p <- 2 * pt(-abs(did_t), df = df_residual)

  # R²
  tss <- sum((y - mean(y))^2)
  rss <- sum(resid_did^2)
  r2_did <- 1 - (rss / tss)

  # ============================================
  # PARALLEL TRENDS TEST (Pre-period placebo test)
  # ============================================

  # For parallel trends, we need pre-period data with multiple time points
  # Simplified version: check if pre-period means differ between groups
  pre_data <- data[time != post_level, ]

  if (nrow(pre_data) > 0) {
    y_pre_treated <- pre_data[pre_data[[treatment_var]] == treatment_level, y_name]
    y_pre_control <- pre_data[pre_data[[treatment_var]] != treatment_level, y_name]

    if (length(y_pre_treated) > 1 && length(y_pre_control) > 1) {
      # T-test for pre-period difference
      pre_test <- t.test(y_pre_treated, y_pre_control)
      pre_diff_p <- pre_test$p.value

      if (pre_diff_p < 0.05) {
        aduana_msgs <- c(aduana_msgs, sprintf("WARNING (Parallel Trends): Significant pre-treatment difference detected (p = %.3f). The parallel trends assumption may be violated. Consider including covariates or using alternative identification strategies (Roth et al., 2023).", pre_diff_p))
      } else {
        aduana_msgs <- c(aduana_msgs, sprintf("INFO: No significant pre-treatment difference (p = %.3f). Parallel trends assumption is plausible but should be verified with visual inspection of trends.", pre_diff_p))
      }
    } else {
      aduana_msgs <- c(aduana_msgs, "INFO: Insufficient pre-period data for formal parallel trends test. Interpret with caution.")
    }
  } else {
    aduana_msgs <- c(aduana_msgs, "INFO: No pre-period data available. Parallel trends assumption cannot be tested. Interpret DiD estimate as a conditional difference.")
  }

  # ============================================
  # GROUP MEANS FOR VISUALIZATION
  # ============================================

  # Calculate means for each group × time combination
  mean_treated_pre <- mean(y[treated == 1 & post == 0], na.rm = TRUE)
  mean_treated_post <- mean(y[treated == 1 & post == 1], na.rm = TRUE)
  mean_control_pre <- mean(y[treated == 0 & post == 0], na.rm = TRUE)
  mean_control_post <- mean(y[treated == 0 & post == 1], na.rm = TRUE)

  group_means <- data.frame(
    Group = rep(c("Control", "Treated"), each = 2),
    Time = rep(c("Pre", "Post"), times = 2),
    Mean = c(mean_control_pre, mean_control_post, mean_treated_pre, mean_treated_post)
  )

  # ============================================
  # RETURN RESULTS
  # ============================================

  return(list(
    coefficients = beta_did,
    se = se_did,
    did_estimate = did_coef,
    did_se = did_se,
    did_t = did_t,
    did_p = did_p,
    r2 = r2_did,
    n_obs = n,
    df_residual = df_residual,
    group_means = group_means,
    diagnostics = list(
      r2 = r2_did,
      n = n,
      did_estimate = did_coef,
      did_p = did_p
    ),
    aduana_msgs = aduana_msgs
  ))
}

# =========================================================
# 8. WRAPPER FUNCTION (Paper Engine & Customs router)
# =========================================================

#' Transparent and Assisted Linear Modeling Engine
#'
#' @description Estimates OLS regression, ANOVA/t-tests, binary logistic
#'   regression, panel data models, instrumental variables, or difference-in-differences
#'   using pure base R matrix algebra. Automatically audits statistical assumptions
#'   through an integrated methodological customs layer and returns publication-ready
#'   APA-formatted tables. Designed for applied researchers and early-career academics
#'   who need a single, transparent workflow from estimation to reporting.
#'
#' @param formula A \code{formula} object specifying the model (e.g., \code{y ~ x1 + x2}).
#' @param data A data frame containing all variables referenced in \code{formula}.
#' @param model A character string indicating the estimation engine.
#'   One of \code{"ols"} (default), \code{"anova"}, \code{"logit"}, \code{"panel"},
#'   \code{"iv"}, or \code{"did"}.
#' @param robust Logical or \code{"auto"}. Controls heteroskedasticity-robust
#'   standard errors (HC3) for OLS models. If \code{TRUE}, HC3 SEs are always
#'   applied. If \code{"auto"}, they are applied only when the Breusch-Pagan
#'   test detects heteroskedasticity (p < .05). Default is \code{FALSE}.
#' @param non_parametric Logical or \code{"auto"}. Controls non-parametric
#'   fallback for ANOVA/t-test models. If \code{TRUE}, Kruskal-Wallis or
#'   Wilcoxon tests are used. If \code{"auto"}, transition occurs when
#'   Shapiro-Wilk detects non-normality (p < .05). Default is \code{FALSE}.
#' @param paired Logical. If \code{TRUE}, assumes paired/dependent samples
#'   for ANOVA/t-test models (pre-post designs). Default is \code{FALSE}.
#' @param entity_id Character string. Name of the entity/individual identifier
#'   variable for panel data models. Required when \code{model = "panel"}.
#' @param time_id Character string. Name of the time period identifier variable
#'   for panel data models. Required when \code{model = "panel"}.
#' @param method Character string for panel data. One of \code{"auto"} (default,
#'   uses Hausman test to select between FE and RE), \code{"fe"} (Fixed Effects),
#'   or \code{"re"} (Random Effects). Only used when \code{model = "panel"}.
#' @param instruments A \code{formula} specifying instrumental variables for IV models
#'   (e.g., \code{~ z1 + z2}). Required when \code{model = "iv"}. Instruments must
#'   satisfy relevance (correlated with endogenous X) and exogeneity (uncorrelated
#'   with error term).
#' @param treatment_var Character string. Name of the treatment group variable for
#'   DiD models. Required when \code{model = "did"}.
#' @param time_var Character string. Name of the time period variable (pre/post) for
#'   DiD models. Required when \code{model = "did"}.
#' @param treatment_level Character string. Which level of \code{treatment_var}
#'   represents the treated group. If \code{NULL}, the second level is used.
#' @param post_level Character string. Which level of \code{time_var} represents
#'   the post-treatment period. If \code{NULL}, the second level is used.
#' @param digits Integer. Number of decimal places in output tables.
#'   Default is \code{2}.
#'
#' @return An object of class \code{basic_model}, which is a list containing:
#'   \describe{
#'     \item{tables}{A list of formatted data frames with estimation results.}
#'     \item{diagnostics}{A list of raw diagnostic statistics (p-values, fit indices).}
#'     \item{messages}{A character vector of methodological guidance messages from the customs layer.}
#'     \item{method}{A character string indicating the engine used (\code{"ols"},
#'       \code{"anova"}, \code{"logit"}, \code{"panel"}, \code{"iv"}, or \code{"did"}).}
#'     \item{data}{The cleaned data frame used for estimation (after listwise deletion).}
#'   }
#'
#' @examples
#' # OLS example
#' set.seed(42)
#' df <- data.frame(y = rnorm(100), x1 = rnorm(100), x2 = rnorm(100))
#' result <- paper_engine(y ~ x1 + x2, data = df, model = "ols")
#' print(result$tables)
#' print(result$messages)
#'
#' # ANOVA example
#' df2 <- data.frame(score = c(rnorm(30, 5), rnorm(30, 7)),
#'                   group = rep(c("A", "B"), each = 30))
#' result2 <- paper_engine(score ~ group, data = df2, model = "anova")
#' print(result2$tables)
#'
#' # Logit example
#' df3 <- data.frame(y = rbinom(100, 1, 0.5), x = rnorm(100))
#' result3 <- paper_engine(y ~ x, data = df3, model = "logit")
#' print(result3$tables)
#'
#' @export
paper_engine <- function(formula, data, model = "ols", robust = FALSE,
                         non_parametric = FALSE, paired = FALSE,
                         entity_id = NULL, time_id = NULL, method = "auto",
                         instruments = NULL,
                         treatment_var = NULL, time_var = NULL,
                         treatment_level = NULL, post_level = NULL,
                         digits = 2) {

  model <- match.arg(tolower(model), choices = c("ols", "anova", "logit", "panel", "iv", "did"))

  # Validate panel data parameters
  if (model == "panel") {
    if (is.null(entity_id) || is.null(time_id)) {
      stop("Panel data models require both 'entity_id' and 'time_id' parameters.")
    }
  }

  # Validate IV parameters
  if (model == "iv") {
    if (is.null(instruments)) {
      stop("Instrumental variables models require 'instruments' parameter (a formula like ~ z1 + z2).")
    }
  }

  # Validate DiD parameters
  if (model == "did") {
    if (is.null(treatment_var) || is.null(time_var)) {
      stop("Difference-in-differences models require both 'treatment_var' and 'time_var' parameters.")
    }
  }

  # Pass extra variables for panel data, IV, and DiD
  extra_vars <- NULL
  if (model == "panel") {
    extra_vars <- c(entity_id, time_id)
  } else if (model == "iv") {
    # Extract instrument variable names from formula
    if (inherits(instruments, "formula")) {
      extra_vars <- all.vars(instruments)
    } else {
      extra_vars <- instruments
    }
  } else if (model == "did") {
    extra_vars <- c(treatment_var, time_var)
  }

  filter_res <- pre_estimation_filter(formula, data, extra_vars = extra_vars)

  clean_data <- filter_res$clean_data
  aduana_messages <- filter_res$messages

  # --- OLS BRANCH ---
  if (model == "ols") {
    raw <- ols_engine(formula, clean_data, robust = robust)
    aduana_messages <- c(aduana_messages, raw$aduana_msgs)

    coefs <- raw$coefficients
    se <- raw$se_classic
    t_stats <- coefs / se
    p_vals <- 2 * pt(abs(t_stats), df = raw$df_residual, lower.tail = FALSE)
    p_formatted <- ifelse(p_vals < 0.001, "< .001", sprintf(paste0("%.", digits, "f"), p_vals))

    t_crit <- qt(0.975, df = raw$df_residual)

    out_table <- data.frame(
      Predictor = names(coefs),
      B = round(coefs, digits),
      SE = round(se, digits),
      t = round(t_stats, digits),
      p = p_formatted,
      CI_95_Low = round(coefs - t_crit * se, digits),
      CI_95_High = round(coefs + t_crit * se, digits),
      f2 = round(c(NA, raw$f2), digits)
    )
    table_name <- "Table2_OLS_Estimation"

    # Methodological Customs: VIF Interpretation with pedagogical nuance
    max_vif <- max(raw$vif, na.rm = TRUE)
    if (max_vif > 5) {
      vif_warning <- sprintf(
        "WARNING (Multicollinearity): Maximum VIF = %.2f exceeds conventional threshold of 5 (O'Brien, 2007). High collinearity inflates standard errors and may obscure true predictor effects. Note: VIF thresholds are disciplinary conventions, not absolute cutoffs—interpret in context of your research design.",
        max_vif
      )
      aduana_messages <- c(aduana_messages, vif_warning)
    } else if (max_vif > 2.5) {
      vif_info <- sprintf(
        "INFO (Multicollinearity): Maximum VIF = %.2f. Moderate collinearity detected but below critical threshold. Standard errors may be slightly inflated (O'Brien, 2007).",
        max_vif
      )
      aduana_messages <- c(aduana_messages, vif_info)
    }

    if (raw$diagnostics$norm_p_value < 0.05) {
      aduana_messages <- c(aduana_messages, sprintf("WARNING (Normality): p < .05 in %s. With N=%d, OLS may be robust due to CLT (Lumley et al., 2002).", raw$diagnostics$norm_method, raw$diagnostics$N))
    }
  }

  # --- ANOVA BRANCH ---
  else if (model == "anova") {
    raw <- anova_engine(formula, clean_data, non_parametric = non_parametric, paired = paired)
    aduana_messages <- c(aduana_messages, raw$aduana_msgs)

    y_var <- all.vars(formula)[1]
    x_var <- all.vars(formula)[2]
    means <- aggregate(formula, data = clean_data, FUN = mean)
    sds <- aggregate(formula, data = clean_data, FUN = sd)

    p_vals <- raw$effects_table$p_value
    p_formatted <- ifelse(p_vals < 0.001, "< .001", sprintf(paste0("%.", digits, "f"), p_vals))

    out_table <- data.frame(
      Test = raw$effects_table$Test,
      Metric = raw$effects_table$Metric,
      Statistic = round(raw$effects_table$Statistic, digits),
      df = raw$effects_table$df,
      p = p_formatted,
      Effect_Metric = raw$effects_table$Effect_Metric,
      Effect_Size = round(raw$effects_table$Effect_Size, digits)
    )

    means_table <- data.frame(Group = means[,1], Mean = round(means[,2], digits), SD = round(sds[,2], digits))
    table_name <- "Table4_Mean_Differences"
  }

  # --- LOGIT BRANCH ---
  else if (model == "logit") {
    raw <- logit_engine(formula, clean_data)
    aduana_messages <- c(aduana_messages, raw$aduana_msgs)

    coefs <- raw$coefficients
    se <- raw$se
    z_stats <- raw$z_stats
    or <- raw$odds_ratios

    p_vals <- 2 * pnorm(abs(z_stats), lower.tail = FALSE)
    p_formatted <- ifelse(p_vals < 0.001, "< .001", sprintf(paste0("%.", digits, "f"), p_vals))

    z_crit <- qnorm(0.975)
    ci_low <- exp(coefs - z_crit * se)
    ci_high <- exp(coefs + z_crit * se)

    out_table <- data.frame(
      Predictor = names(coefs),
      B = round(coefs, digits),
      OR = round(or, digits),
      SE = round(se, digits),
      z = round(z_stats, digits),
      p = p_formatted,
      OR_CI_95_Low = round(ci_low, digits),
      OR_CI_95_High = round(ci_high, digits)
    )
    table_name <- "Table2_Logit_Estimation"

    if (raw$diagnostics$perfect_separation) {
      w_sep <- "CRITICAL WARNING (Perfect Separation): Astronomically high standard errors detected. The model perfectly predicts the outcome. Odds Ratios are invalid."
      aduana_messages <- c(aduana_messages, w_sep)
      warning(w_sep, call. = FALSE)
    }
    if (!is.na(raw$diagnostics$hl_p_value) && raw$diagnostics$hl_p_value < 0.05) {
      w_hl <- "WARNING (Goodness of Fit): p < .05 in Hosmer-Lemeshow test. The model shows poor structural fit to the data (Hosmer et al., 2013)."
      aduana_messages <- c(aduana_messages, w_hl)
      warning(w_hl, call. = FALSE)
    }

    aduana_messages <- c(aduana_messages, sprintf("INFO: Classification Accuracy (Threshold 0.5) = %.1f%%", raw$diagnostics$accuracy * 100))
    aduana_messages <- c(aduana_messages, sprintf("INFO: McFadden Pseudo R² = %.3f | Nagelkerke Pseudo R² = %.3f", raw$pseudo_r2_mcfadden, raw$pseudo_r2_nagelkerke))
  }

  # --- PANEL DATA BRANCH ---
  else if (model == "panel") {
    raw <- panel_engine(formula, clean_data, entity_id = entity_id, time_id = time_id, method = method)
    aduana_messages <- c(aduana_messages, raw$aduana_msgs)

    coefs <- raw$coefficients
    se <- raw$se
    t_stats <- coefs / se
    p_vals <- 2 * pt(abs(t_stats), df = raw$df_residual, lower.tail = FALSE)
    p_formatted <- ifelse(p_vals < 0.001, "< .001", sprintf(paste0("%.", digits, "f"), p_vals))

    t_crit <- qt(0.975, df = raw$df_residual)

    out_table <- data.frame(
      Predictor = names(coefs),
      B = round(coefs, digits),
      SE = round(se, digits),
      t = round(t_stats, digits),
      p = p_formatted,
      CI_95_Low = round(coefs - t_crit * se, digits),
      CI_95_High = round(coefs + t_crit * se, digits)
    )

    table_name <- paste0("Table2_Panel_", gsub(" ", "_", raw$method))

    # Panel-specific diagnostics
    aduana_messages <- c(aduana_messages, sprintf("INFO: Panel structure - N entities = %d, T periods = %d, Total obs = %d", raw$n_entities, raw$n_time, raw$n_obs))
    aduana_messages <- c(aduana_messages, sprintf("INFO: R² (%s) = %.3f", ifelse(raw$method == "Fixed Effects", "within", "overall"), raw$r2))

    if (!is.na(raw$hausman_p)) {
      aduana_messages <- c(aduana_messages, sprintf("INFO: Hausman test statistic = %.2f (p = %.3f)", raw$hausman_stat, raw$hausman_p))
    }
  }

  # --- INSTRUMENTAL VARIABLES BRANCH ---
  else if (model == "iv") {
    raw <- iv_engine(formula, clean_data, instruments = instruments)
    aduana_messages <- c(aduana_messages, raw$aduana_msgs)

    coefs <- raw$coefficients
    se <- raw$se
    t_stats <- coefs / se
    p_vals <- 2 * pt(abs(t_stats), df = raw$df_residual, lower.tail = FALSE)
    p_formatted <- ifelse(p_vals < 0.001, "< .001", sprintf(paste0("%.", digits, "f"), p_vals))

    t_crit <- qt(0.975, df = raw$df_residual)

    out_table <- data.frame(
      Predictor = names(coefs),
      B = round(coefs, digits),
      SE = round(se, digits),
      t = round(t_stats, digits),
      p = p_formatted,
      CI_95_Low = round(coefs - t_crit * se, digits),
      CI_95_High = round(coefs + t_crit * se, digits)
    )

    table_name <- "Table2_IV_2SLS"

    # IV-specific diagnostics
    aduana_messages <- c(aduana_messages, sprintf("INFO: Sample size N = %d, Number of instruments = %d", raw$n_obs, raw$n_instruments))

    if (raw$r2 < 0 || raw$r2 > 1) {
      aduana_messages <- c(aduana_messages, sprintf("INFO: R² = %.3f (can be negative or > 1 in IV models due to endogeneity correction)", raw$r2))
    } else {
      aduana_messages <- c(aduana_messages, sprintf("INFO: R² = %.3f", raw$r2))
    }
  }

  # --- DIFFERENCE-IN-DIFFERENCES BRANCH ---
  else if (model == "did") {
    raw <- did_engine(formula, clean_data, treatment_var = treatment_var, time_var = time_var,
                      treatment_level = treatment_level, post_level = post_level)
    aduana_messages <- c(aduana_messages, raw$aduana_msgs)

    coefs <- raw$coefficients
    se <- raw$se
    t_stats <- coefs / se
    p_vals <- 2 * pt(abs(t_stats), df = raw$df_residual, lower.tail = FALSE)
    p_formatted <- ifelse(p_vals < 0.001, "< .001", sprintf(paste0("%.", digits, "f"), p_vals))

    t_crit <- qt(0.975, df = raw$df_residual)

    out_table <- data.frame(
      Predictor = names(coefs),
      B = round(coefs, digits),
      SE = round(se, digits),
      t = round(t_stats, digits),
      p = p_formatted,
      CI_95_Low = round(coefs - t_crit * se, digits),
      CI_95_High = round(coefs + t_crit * se, digits)
    )

    table_name <- "Table2_DiD_Estimation"

    # DiD-specific diagnostics
    aduana_messages <- c(aduana_messages, sprintf("INFO: DiD Effect (Treated:Post interaction) = %.3f (SE = %.3f, p %s)", raw$did_estimate, raw$did_se, p_formatted[names(coefs) == "Treated:Post"]))
    aduana_messages <- c(aduana_messages, sprintf("INFO: Sample size N = %d", raw$n_obs))
    aduana_messages <- c(aduana_messages, sprintf("INFO: R² = %.3f", raw$r2))
  }

  # --- WRAP-UP ---
  if(length(aduana_messages) == 0) {
    aduana_messages <- "Success: The model meets all diagnosed assumptions and there was no severe data loss."
  }

  tables_list <- list()
  tables_list[[table_name]] <- out_table
  if(model == "anova") tables_list$Descriptive_Means <- means_table
  if(model == "did") tables_list$Group_Means <- raw$group_means

  final_model <- list(
    tables = tables_list,
    diagnostics = raw$diagnostics,
    messages = aduana_messages,
    method = model,
    data = clean_data
  )

  class(final_model) <- "basic_model"
  return(final_model)
}

# =========================================================
# 9. VISUALIZATION ENGINE (Paper-Ready Plots in Base R)
# =========================================================

#' Generate Publication-Ready Plots for Basic Models
#'
#' @description Produces minimalist APA-style plots from a \code{basic_model}
#'   object returned by \code{\link{paper_engine}}. The plot type is selected
#'   automatically based on the estimation method: a forest plot of coefficients
#'   with 95% CI for OLS, a group means plot with 95% CI error bars for ANOVA,
#'   and a logistic probability curve for logistic regression.
#'
#' @param model_object An object of class \code{basic_model} generated by
#'   \code{\link{paper_engine}}.
#' @param y_label A character string for the Y-axis label. If \code{NULL}
#'   (default), a label is generated automatically from the model type.
#' @param x_label A character string for the X-axis label. If \code{NULL}
#'   (default), a label is generated automatically from the model type.
#'
#' @return A base R plot rendered in the active graphics device. The function
#'   is called for its side effect (the plot) and returns \code{NULL} invisibly.
#'
#' @examples
#' set.seed(42)
#' df <- data.frame(y = rnorm(100), x1 = rnorm(100), x2 = rnorm(100))
#' result <- paper_engine(y ~ x1 + x2, data = df, model = "ols")
#' plot_engine(result, y_label = "Outcome", x_label = "Predictors")
#'
#' @export
plot_engine <- function(model_object, y_label = NULL, x_label = NULL) {

  if (!inherits(model_object, "basic_model")) {
    stop("Input must be an object of class 'basic_model' generated by paper_engine().")
  }

  method <- model_object$method

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mar = c(5, 6, 4, 2) + 0.1, family = "sans", bg = "white", fg = "black")

  # =======================================================
  # 1. ANOVA PLOT: Means with 95% CI
  # =======================================================
  if (method == "anova") {
    means_data <- model_object$tables$Descriptive_Means

    n_total <- model_object$diagnostics$n
    if(is.null(n_total)) n_total <- 100
    n_group <- n_total / nrow(means_data)

    se <- means_data$SD / sqrt(n_group)
    ci_margin <- qt(0.975, df = n_group - 1) * se

    upper_ci <- means_data$Mean + ci_margin
    lower_ci <- means_data$Mean - ci_margin

    y_min <- min(lower_ci) - (sd(means_data$Mean) * 0.8)
    y_max <- max(upper_ci) + (sd(means_data$Mean) * 0.8)

    ylab_text <- ifelse(is.null(y_label), "Group Mean", y_label)
    xlab_text <- ifelse(is.null(x_label), "Experimental Group", x_label)

    plot(1:nrow(means_data), means_data$Mean, type = "n",
         ylim = c(y_min, y_max), xlim = c(0.5, nrow(means_data) + 0.5),
         axes = FALSE, xlab = xlab_text, ylab = ylab_text,
         main = "Main Effect: Group Means (95% CI)")

    axis(1, at = 1:nrow(means_data), labels = means_data$Group, tick = FALSE)
    axis(2, las = 1)
    box(bty = "l")

    arrows(x0 = 1:nrow(means_data), y0 = lower_ci, x1 = 1:nrow(means_data), y1 = upper_ci,
           code = 3, angle = 90, length = 0.1, lwd = 1.5, col = "gray30")

    points(1:nrow(means_data), means_data$Mean, pch = 19, cex = 1.8, col = "black")
    lines(1:nrow(means_data), means_data$Mean, lty = 3, lwd = 1.5, col = "black")

    text(x = 1:nrow(means_data), y = means_data$Mean, labels = sprintf("%.2f", means_data$Mean),
         pos = 4, offset = 0.8, cex = 0.9, col = "black")
  }

  # =======================================================
  # 2. LOGIT PLOT: Probability Curve
  # =======================================================
  else if (method == "logit") {
    coefs <- model_object$tables$Table2_Logit_Estimation$B
    names(coefs) <- model_object$tables$Table2_Logit_Estimation$Predictor

    if(length(coefs) < 2) stop("Logit plot requires at least one predictor.")

    main_pred_name <- names(coefs)[2]
    beta_0 <- coefs["(Intercept)"]
    beta_1 <- coefs[2]

    x_data <- model_object$data[[main_pred_name]]
    x_seq <- seq(min(x_data, na.rm=TRUE), max(x_data, na.rm=TRUE), length.out = 100)

    z_seq <- beta_0 + (beta_1 * x_seq)
    prob_seq <- 1 / (1 + exp(-z_seq))

    ylab_text <- ifelse(is.null(y_label), "Predicted Probability P(Y=1)", y_label)
    xlab_text <- ifelse(is.null(x_label), main_pred_name, x_label)

    plot(x_seq, prob_seq, type = "l", lwd = 2, col = "black",
         ylim = c(0, 1), axes = FALSE, xlab = xlab_text, ylab = ylab_text,
         main = paste("Logistic Probability Curve:", main_pred_name))

    axis(1)
    axis(2, las = 1, at = seq(0, 1, by = 0.2))
    box(bty = "l")
    abline(h = 0.5, lty = 2, col = "gray60")
  }

  # =======================================================
  # 3. OLS PLOT: Forest Plot (Coefficients & CI)
  # =======================================================
  else if (method == "ols") {
    est_table <- model_object$tables$Table2_OLS_Estimation

    # Exclude Intercept to focus the plot on predictors
    plot_data <- est_table[est_table$Predictor != "(Intercept)", ]

    # NEW: Visual filter of max 10 variables to avoid visual chaos
    if (nrow(plot_data) > 10) {
      message("\n>> Visual Note: Plotting only the first 10 predictors to maintain APA clarity. See numeric table for full model.")
      plot_data <- plot_data[1:10, ]
    }

    # Reverse order so the first variable appears at the top of the plot
    plot_data <- plot_data[nrow(plot_data):1, ]

    y_pos <- 1:nrow(plot_data)

    # Give slightly more margin on X to fit the numbers
    x_min <- min(plot_data$CI_95_Low) - abs(min(plot_data$CI_95_Low) * 0.3)
    x_max <- max(plot_data$CI_95_High) + abs(max(plot_data$CI_95_High) * 0.3)

    plot(plot_data$B, y_pos, type = "n",
         xlim = c(x_min, x_max), ylim = c(0.5, nrow(plot_data) + 0.5),
         axes = FALSE, ylab = "", xlab = "Coefficient Estimate (95% CI)",
         main = "OLS Regression: Predictor Effects")

    # Solid and connected axes APA style (L-shape)
    axis(1)
    axis(2, at = y_pos, labels = plot_data$Predictor, las = 1, tick = TRUE)
    box(bty = "l")

    # "Zero Effect" line (Vertical dashed gray)
    abline(v = 0, lty = 2, col = "gray60")

    # Horizontal error bars (Confidence Intervals)
    arrows(x0 = plot_data$CI_95_Low, y0 = y_pos, x1 = plot_data$CI_95_High, y1 = y_pos,
           code = 3, angle = 90, length = 0.05, lwd = 1.5, col = "gray30")

    # Estimator points (Betas as black squares)
    points(plot_data$B, y_pos, pch = 15, cex = 1.5, col = "black")

    # Explicit numeric values (Betas) just above each square
    text(x = plot_data$B, y = y_pos,
         labels = sprintf("%.2f", plot_data$B),
         pos = 3, offset = 0.8, cex = 0.9, col = "black", font = 2)
  }

  # =======================================================
  # 4. PANEL DATA PLOT: Forest Plot with Method Label
  # =======================================================
  else if (method == "panel") {
    # Get the table (name varies by FE or RE)
    table_name <- names(model_object$tables)[1]
    est_table <- model_object$tables[[1]]

    # Reverse order so the first variable appears at the top
    plot_data <- est_table[nrow(est_table):1, ]

    y_pos <- 1:nrow(plot_data)

    # Margin on X to fit the numbers
    x_min <- min(plot_data$CI_95_Low) - abs(min(plot_data$CI_95_Low) * 0.3)
    x_max <- max(plot_data$CI_95_High) + abs(max(plot_data$CI_95_High) * 0.3)

    # Determine method for title
    panel_method <- gsub("Table2_Panel_", "", table_name)
    panel_method <- gsub("_", " ", panel_method)

    plot(plot_data$B, y_pos, type = "n",
         xlim = c(x_min, x_max), ylim = c(0.5, nrow(plot_data) + 0.5),
         axes = FALSE, ylab = "", xlab = "Coefficient Estimate (95% CI)",
         main = paste0("Panel Data (", panel_method, ")"))

    # Axes
    axis(1)
    axis(2, at = y_pos, labels = plot_data$Predictor, las = 1, tick = TRUE)
    box(bty = "l")

    # Zero line
    abline(v = 0, lty = 2, col = "gray60")

    # Confidence intervals
    arrows(x0 = plot_data$CI_95_Low, y0 = y_pos, x1 = plot_data$CI_95_High, y1 = y_pos,
           code = 3, angle = 90, length = 0.05, lwd = 1.5, col = "gray30")

    # Coefficients
    points(plot_data$B, y_pos, pch = 15, cex = 1.5, col = "black")

    # Values
    text(x = plot_data$B, y = y_pos,
         labels = sprintf("%.2f", plot_data$B),
         pos = 3, offset = 0.8, cex = 0.9, col = "black", font = 2)
  }

  # =======================================================
  # 5. INSTRUMENTAL VARIABLES PLOT: Forest Plot with 2SLS
  # =======================================================
  else if (method == "iv") {
    # Get the IV table
    est_table <- model_object$tables$Table2_IV_2SLS

    # Reverse order so the first variable appears at the top
    plot_data <- est_table[nrow(est_table):1, ]

    y_pos <- 1:nrow(plot_data)

    # Margin on X to fit the numbers
    x_min <- min(plot_data$CI_95_Low) - abs(min(plot_data$CI_95_Low) * 0.3)
    x_max <- max(plot_data$CI_95_High) + abs(max(plot_data$CI_95_High) * 0.3)

    plot(plot_data$B, y_pos, type = "n",
         xlim = c(x_min, x_max), ylim = c(0.5, nrow(plot_data) + 0.5),
         axes = FALSE, ylab = "", xlab = "Coefficient Estimate (95% CI)",
         main = "Instrumental Variables (2SLS)")

    # Axes
    axis(1)
    axis(2, at = y_pos, labels = plot_data$Predictor, las = 1, tick = TRUE)
    box(bty = "l")

    # Zero line
    abline(v = 0, lty = 2, col = "gray60")

    # Confidence intervals
    arrows(x0 = plot_data$CI_95_Low, y0 = y_pos,
           x1 = plot_data$CI_95_High, y1 = y_pos,
           code = 3, angle = 90, length = 0.05, lwd = 1.5, col = "gray30")

    # Coefficients
    points(plot_data$B, y_pos, pch = 15, cex = 1.5, col = "black")

    # Values
    text(x = plot_data$B, y = y_pos,
         labels = sprintf("%.2f", plot_data$B),
         pos = 3, offset = 0.8, cex = 0.9, col = "black", font = 2)
  }
}
