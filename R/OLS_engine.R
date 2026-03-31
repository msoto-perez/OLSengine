#################################################
# OLSengine - CORE MATHEMATICAL ENGINES & CUSTOMS
# Stage 3: Functional Prototype (English Version)
#################################################

# =========================================================
# 1. PRE-ESTIMATION FILTER (Data loss & Variance check)
# =========================================================
pre_estimation_filter <- function(formula, data) {

  vars_in_formula <- all.vars(formula)

  if (!all(vars_in_formula %in% colnames(data))) {
    stop("CRITICAL ERROR: One or more variables in the formula do not exist in the dataframe.")
  }

  data_subset <- data[, vars_in_formula, drop = FALSE]

  n_original <- nrow(data_subset)
  clean_data <- na.omit(data_subset)
  n_final <- nrow(clean_data)

  missing_cases <- n_original - n_final
  missing_percentage <- (missing_cases / n_original) * 100

  zero_var <- sapply(clean_data, function(x) {
    if (is.numeric(x)) {
      var(x, na.rm = TRUE) == 0
    } else {
      length(unique(x)) < 2
    }
  })

  if (any(zero_var)) {
    bad_vars <- paste(names(zero_var)[zero_var], collapse = ", ")
    stop(sprintf("CRITICAL ERROR: Model cannot be computed. The following variables have zero variance (constants): %s", bad_vars))
  }

  messages_pre <- character()

  if (missing_percentage > 10) {
    w_na <- sprintf("WARNING (Data Loss): %.1f%% of original observations were removed due to missing values (NAs). Justify this listwise deletion.", missing_percentage)
    messages_pre <- c(messages_pre, w_na)
    warning(w_na, call. = FALSE)
  }

  list(
    clean_data = clean_data,
    messages = messages_pre
  )
}

# =========================================================
# 2. OLS ENGINE (Mathematical Core)
# =========================================================
ols_engine <- function(formula, data) {

  fit <- lm(formula, data = data)
  sum_fit <- summary(fit)

  X <- model.matrix(fit)
  y <- fit$model[, 1]
  res <- fit$residuals

  n <- nrow(X)
  k <- ncol(X)

  # VIF Calculation
  vif_vals <- numeric(k - 1)
  names(vif_vals) <- colnames(X)[-1]

  if(k > 1) {
    for (i in 2:k) {
      y_vif <- X[, i]
      X_vif <- X[, -i, drop = FALSE]
      fit_vif <- lm.fit(x = as.matrix(X_vif), y = y_vif)

      ss_tot <- sum((y_vif - mean(y_vif))^2)
      ss_res <- sum(fit_vif$residuals^2)
      r2_vif <- 1 - (ss_res / ss_tot)

      vif_vals[i - 1] <- ifelse(r2_vif == 1, Inf, 1 / (1 - r2_vif))
    }
  }

  # Robust Standard Errors (HC3)
  hat_diag <- hatvalues(fit)
  omega <- (res^2) / ((1 - hat_diag)^2)
  Sigma <- diag(omega)
  XX_inv <- solve(crossprod(X))
  cov_HC3 <- XX_inv %*% (t(X) %*% Sigma %*% X) %*% XX_inv
  se_HC3 <- sqrt(diag(cov_HC3))

  # Effect Size (Cohen's f2)
  r2_inclusive <- sum_fit$r.squared
  f2_vals <- numeric(k - 1)
  names(f2_vals) <- colnames(X)[-1]

  if(k > 1) {
    for (i in 2:k) {
      X_excl <- X[, -i, drop = FALSE]
      fit_excl <- lm.fit(x = X_excl, y = y)

      ss_tot_excl <- sum((y - mean(y))^2)
      ss_res_excl <- sum(fit_excl$residuals^2)
      r2_exclusive <- 1 - (ss_res_excl / ss_tot_excl)

      f2_vals[i - 1] <- ifelse(r2_inclusive >= 1, NA,
                               (r2_inclusive - r2_exclusive) / (1 - r2_inclusive))
    }
  }

  # Diagnostics (Breusch-Pagan & Normality)
  res_sq <- res^2
  bp_fit <- lm.fit(x = X, y = res_sq)
  ss_tot_bp <- sum((res_sq - mean(res_sq))^2)
  ss_res_bp <- sum(bp_fit$residuals^2)
  r2_bp <- 1 - (ss_res_bp / ss_tot_bp)

  lm_stat <- n * r2_bp
  p_val_bp <- pchisq(lm_stat, df = k - 1, lower.tail = FALSE)

  if (n <= 5000) {
    norm_test <- shapiro.test(res)
    p_val_norm <- norm_test$p.value
    norm_method <- "Shapiro-Wilk"
  } else {
    norm_test <- ks.test(scale(res), "pnorm")
    p_val_norm <- norm_test$p.value
    norm_method <- "Kolmogorov-Smirnov"
  }

  list(
    coefficients = coef(fit),
    se_classic   = sum_fit$coefficients[, "Std. Error"],
    se_robust    = se_HC3,
    r_squared    = r2_inclusive,
    adj_r_squared = sum_fit$adj.r.squared,
    f_stat       = sum_fit$fstatistic,
    vif          = vif_vals,
    f2           = f2_vals,
    residuals    = res,
    df_residual  = fit$df.residual,
    diagnostics  = list(
      n = n,
      bp_stat = lm_stat,
      bp_p_value = p_val_bp,
      norm_method = norm_method,
      norm_p_value = p_val_norm
    )
  )
}

# =========================================================
# 3. ANOVA ENGINE (Mathematical Core)
# =========================================================
anova_engine <- function(formula, data) {

  fit_aov <- aov(formula, data = data)
  aov_table <- summary(fit_aov)[[1]]

  ss_effects <- aov_table$`Sum Sq`[-nrow(aov_table)]
  ss_residuals <- aov_table$`Sum Sq`[nrow(aov_table)]

  df_effects <- aov_table$`Df`[-nrow(aov_table)]
  df_residuals <- aov_table$`Df`[nrow(aov_table)]

  f_stats <- aov_table$`F value`[-nrow(aov_table)]
  p_vals <- aov_table$`Pr(>F)`[-nrow(aov_table)]

  effect_names <- trimws(rownames(aov_table)[-nrow(aov_table)])

  # Effect Size (Partial Eta-Squared)
  eta_sq_partial <- ss_effects / (ss_effects + ss_residuals)

  # Levene's Test (Robust - Median based)
  y_name <- all.vars(formula)[1]
  x_name <- all.vars(formula)[2]

  y_val <- data[[y_name]]
  group_val <- as.factor(data[[x_name]])

  medians <- tapply(y_val, group_val, median, na.rm = TRUE)
  abs_deviations <- abs(y_val - medians[group_val])

  fit_levene <- aov(abs_deviations ~ group_val)
  p_val_levene <- summary(fit_levene)[[1]]$`Pr(>F)`[1]

  list(
    effects_table = data.frame(
      Effect = effect_names,
      df = df_effects,
      SS = ss_effects,
      F_stat = f_stats,
      p_value = p_vals,
      eta_sq_partial = eta_sq_partial
    ),
    df_residual = df_residuals,
    ss_residual = ss_residuals,
    diagnostics = list(
      levene_p_value = p_val_levene
    )
  )
}

# =========================================================
# 4. LOGIT ENGINE (Mathematical Core for Binary Outcomes)
# =========================================================
logit_engine <- function(formula, data) {

  # 1. Fit the logistic regression model
  fit <- glm(formula, data = data, family = binomial(link = "logit"))
  sum_fit <- summary(fit)

  coefs <- coef(fit)
  se <- sum_fit$coefficients[, "Std. Error"]
  z_stats <- sum_fit$coefficients[, "z value"]

  # 2. Calculate Odds Ratios
  odds_ratios <- exp(coefs)

  # 3. Calculate Pseudo R-Squared (McFadden)
  null_dev <- fit$null.deviance
  res_dev <- fit$deviance
  pseudo_r2 <- 1 - (res_dev / null_dev)

  # ---------------------------------------------------------
  # 4. CUSTOMS DIAGNOSTICS
  # ---------------------------------------------------------

  # A. Check for Perfect or Quasi-Perfect Separation
  # If any standard error is abnormally huge (e.g., > 50), separation occurred.
  perfect_separation <- any(se > 50)

  # B. Hosmer-Lemeshow Goodness of Fit Test (Base R Manual Implementation)
  fitted_probs <- fitted(fit)
  y_actual <- fit$y

  # Create 10 probability deciles for the H-L test
  decile_breaks <- unique(quantile(fitted_probs, probs = seq(0, 1, by = 0.1), na.rm = TRUE))
  # Fallback if quantiles don't produce enough bins (e.g., small datasets)
  if(length(decile_breaks) < 3) {
    decile_breaks <- c(0, 0.5, 1)
  }

  deciles <- cut(fitted_probs, breaks = decile_breaks, include.lowest = TRUE)

  # Calculate observed and expected frequencies per decile
  obs_1 <- tapply(y_actual, deciles, sum)
  obs_0 <- tapply(1 - y_actual, deciles, sum)
  exp_1 <- tapply(fitted_probs, deciles, sum)
  exp_0 <- tapply(1 - fitted_probs, deciles, sum)

  # H-L Chi-Square Statistic
  hl_stat <- sum((obs_1 - exp_1)^2 / exp_1 + (obs_0 - exp_0)^2 / exp_0, na.rm = TRUE)
  g <- length(unique(deciles))
  hl_p_value <- pchisq(hl_stat, df = max(1, g - 2), lower.tail = FALSE)

  # 5. ASSEMBLE RAW RESULTS
  list(
    coefficients = coefs,
    se = se,
    z_stats = z_stats,
    odds_ratios = odds_ratios,
    pseudo_r2 = pseudo_r2,
    diagnostics = list(
      perfect_separation = perfect_separation,
      hl_stat = hl_stat,
      hl_p_value = hl_p_value
    )
  )
}

# =========================================================
# 5. WRAPPER FUNCTION (Paper Engine & Customs router)
# =========================================================

#' Transparent and Assisted Linear Modeling Engine
#'
#' @description A wrapper function that routes data to specific mathematical engines
#' (OLS, ANOVA, Logit), performs automatic methodological diagnostics (the "Customs"),
#' and returns publication-ready formatted tables.
#'
#' @param formula An object of class \code{formula} (e.g., \code{y ~ x1 + x2}).
#' @param data A data frame containing the variables in the model.
#' @param model A character string specifying the engine to use. Options are \code{"ols"}, \code{"anova"}, or \code{"logit"}. Default is \code{"ols"}.
#' @param digits An integer specifying the number of decimal places for the output. Default is \code{2}.
#'
#' @return A list of class \code{basic_model} containing formatted tables, raw diagnostics, and methodological guidance messages.
#' @export

paper_engine <- function(formula, data, model = "ols", digits = 2) {

  model <- match.arg(tolower(model), choices = c("ols", "anova", "logit"))

  filter_res <- pre_estimation_filter(formula, data)
  clean_data <- filter_res$clean_data
  aduana_messages <- filter_res$messages

  # --- OLS BRANCH ---
  if (model == "ols") {
    raw <- ols_engine(formula, clean_data)

    coefs <- raw$coefficients
    se <- raw$se_classic
    t_stats <- coefs / se
    p_vals <- 2 * pt(abs(t_stats), df = raw$df_residual, lower.tail = FALSE)
    p_formatted <- ifelse(p_vals < 0.001, "< .001", sprintf("%.3f", p_vals))

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

    if (max(raw$vif) > 5) {
      aduana_messages <- c(aduana_messages, sprintf("WARNING (Multicollinearity): Max VIF is %.2f (> 5). Consider mean-centering (Hayes, 2022).", max(raw$vif)))
    }
    if (raw$diagnostics$bp_p_value < 0.05) {
      aduana_messages <- c(aduana_messages, "WARNING (Heteroskedasticity): p < .05 in Breusch-Pagan. Use Robust SE (Hayes & Cai, 2007).")
    }
    if (raw$diagnostics$norm_p_value < 0.05) {
      aduana_messages <- c(aduana_messages, sprintf("WARNING (Normality): p < .05 in %s. With N=%d, OLS may be robust due to CLT (Lumley et al., 2002).", raw$diagnostics$norm_method, raw$diagnostics$n))
    }
  }

  # --- ANOVA BRANCH ---
  else if (model == "anova") {
    raw <- anova_engine(formula, clean_data)

    y_var <- all.vars(formula)[1]
    x_var <- all.vars(formula)[2]
    means <- aggregate(formula, data = clean_data, FUN = mean)
    sds <- aggregate(formula, data = clean_data, FUN = sd)

    p_formatted <- ifelse(raw$effects_table$p_value < 0.001, "< .001", sprintf("%.3f", raw$effects_table$p_value))

    out_table <- data.frame(
      Effect = raw$effects_table$Effect,
      F_stat = round(raw$effects_table$F_stat, digits),
      df = raw$effects_table$df,
      p = p_formatted,
      Partial_Eta_sq = round(raw$effects_table$eta_sq_partial, digits)
    )

    means_table <- data.frame(Group = means[,1], Mean = round(means[,2], digits), SD = round(sds[,2], digits))
    table_name <- "Table4_ANOVA"

    if (raw$diagnostics$levene_p_value < 0.05) {
      w_levene <- "WARNING (Homogeneity): p < .05 in Levene's test. Unequal variances. Suggested: Welch's ANOVA (Lakens, 2013)."
      aduana_messages <- c(aduana_messages, w_levene)
      warning(w_levene, call. = FALSE)
    }
  }

  # --- LOGIT BRANCH ---
  else if (model == "logit") {
    raw <- logit_engine(formula, clean_data)

    coefs <- raw$coefficients
    se <- raw$se
    z_stats <- raw$z_stats
    or <- raw$odds_ratios

    # Gretl-style p-values using the Z-distribution (Wald test)
    p_vals <- 2 * pnorm(abs(z_stats), lower.tail = FALSE)
    p_formatted <- ifelse(p_vals < 0.001, "< .001", sprintf("%.3f", p_vals))

    # Confidence Intervals for Odds Ratios
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

    # Logit Customs
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

    # Agregamos el Pseudo R2 a los mensajes como información útil
    aduana_messages <- c(aduana_messages, sprintf("INFO: McFadden's Pseudo R-Squared = %.3f", raw$pseudo_r2))
  }

  # --- WRAP-UP ---
  if(length(aduana_messages) == 0) {
    aduana_messages <- "Success: The model meets all diagnosed assumptions and there was no severe data loss."
  }

  tables_list <- list()
  tables_list[[table_name]] <- out_table
  if(model == "anova") tables_list$Descriptive_Means <- means_table

  final_model <- list(
    tables = tables_list,
    diagnostics = raw$diagnostics,
    messages = aduana_messages,
    method = model
  )

  class(final_model) <- "basic_model"
  return(final_model)
}
