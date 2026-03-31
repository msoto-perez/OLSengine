#################################################
# OLSengine - CORE MATHEMATICAL ENGINES & CUSTOMS
# Stage 3: Functional Prototype (English Version)
#################################################

# =========================================================
# 1. PRE-ESTIMATION FILTER (Data Integrity Customs)
# =========================================================
pre_estimation_filter <- function(formula, data) {

  # Extract only the variables used in the formula
  vars_in_model <- all.vars(formula)

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
logit_engine <- function(formula, data) {

  fit <- glm(formula, data = data, family = binomial(link = "logit"))
  sum_fit <- summary(fit)

  coefs <- coef(fit)
  se <- sum_fit$coefficients[, "Std. Error"]
  z_stats <- sum_fit$coefficients[, "z value"]
  odds_ratios <- exp(coefs)

  null_dev <- fit$null.deviance
  res_dev <- fit$deviance
  pseudo_r2 <- 1 - (res_dev / null_dev)

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
    pseudo_r2 = pseudo_r2,
    diagnostics = list(
      perfect_separation = perfect_separation,
      hl_stat = hl_stat,
      hl_p_value = hl_p_value,
      accuracy = accuracy,
      n = nrow(data)
    ),
    aduana_msgs = aduana_msgs
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
#' @param robust Logical or "auto". If TRUE, applies HC3 robust SE for OLS. Default is FALSE.
#' @param non_parametric Logical or "auto". Integrates ANOVA/t-test non-parametric equivalents. Default is FALSE.
#' @param paired Logical. Assumes paired/dependent data for ANOVA/t-tests. Default is FALSE.
#' @param digits An integer specifying the number of decimal places for the output. Default is \code{2}.
#'
#' @return A list of class \code{basic_model} containing formatted tables, raw diagnostics, and methodological guidance messages.
#' @export
paper_engine <- function(formula, data, model = "ols", robust = FALSE, non_parametric = FALSE, paired = FALSE, digits = 2) {

  model <- match.arg(tolower(model), choices = c("ols", "anova", "logit"))

  filter_res <- pre_estimation_filter(formula, data)
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
      f2 = round(c(NA, raw$f2), digits) # NAs for the intercept
    )
    table_name <- "Table2_OLS_Estimation"

    if (max(raw$vif) > 5) {
      aduana_messages <- c(aduana_messages, sprintf("WARNING (Multicollinearity): Max VIF is %.2f (> 5.0). Consider mean-centering (Hayes, 2022).", max(raw$vif)))
    }
    if (raw$diagnostics$norm_p_value < 0.05) {
      aduana_messages <- c(aduana_messages, sprintf("WARNING (Normality): p < .05 in %s. With N=%d, OLS may be robust due to CLT (Lumley et al., 2002).", raw$diagnostics$norm_method, raw$diagnostics$N))
    }
  }

  # --- ANOVA BRANCH ---
  else if (model == "anova") {
    # Pass the main function parameters to the engine
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

    # Gretl-style p-values using the Z-distribution (Wald test)
    p_vals <- 2 * pnorm(abs(z_stats), lower.tail = FALSE)
    p_formatted <- ifelse(p_vals < 0.001, "< .001", sprintf(paste0("%.", digits, "f"), p_vals))

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

    aduana_messages <- c(aduana_messages, sprintf("INFO: Classification Accuracy (Threshold 0.5) = %.1f%%", raw$diagnostics$accuracy * 100))
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
    method = model,
    data = clean_data
  )

  class(final_model) <- "basic_model"
  return(final_model)
}

# =========================================================
# 6. VISUALIZATION ENGINE (Paper-Ready Plots in Base R)
# =========================================================

#' Generate Publication-Ready Plots for Basic Models
#'
#' @description Automatically generates minimalist, APA-style plots based on the estimated model.
#' OLS: Forest plot of coefficients with 95% CI.
#' ANOVA: Means plot with 95% CI error bars.
#' Logit: Logistic probability curve for the main predictor.
#'
#' @param model_object An object of class \code{basic_model} generated by \code{paper_engine}.
#' @param y_label A character string for the Y-axis label.
#' @param x_label A character string for the X-axis label.
#'
#' @return A base R plot rendered in the active graphics device.
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
}
