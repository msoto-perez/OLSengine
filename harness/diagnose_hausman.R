suppressMessages(source("../R/OLS_engine.R"))
suppressMessages(library(plm))

check_one <- function(n_entities, n_time, corr_strength, seed) {
  set.seed(seed)
  ent_eff <- rnorm(n_entities, 0, 5)
  ids <- rep(1:n_entities, each=n_time); times <- rep(1:n_time, times=n_entities)
  x1 <- rnorm(n_entities*n_time, 10, 3) + corr_strength * ent_eff[ids]
  y <- 5 + 1.2*x1 + ent_eff[ids] + rnorm(n_entities*n_time, 0, 2)
  df <- data.frame(id=ids, time=times, y, x1)

  pdata <- pdata.frame(df, index=c("id","time"))
  fe <- plm(y~x1, data=pdata, model="within")
  re <- plm(y~x1, data=pdata, model="random")
  ht <- phtest(fe, re)

  eng <- panel_engine(y~x1, data=df, entity_id="id", time_id="time", method="auto")

  data.frame(n_entities, n_time, corr_strength,
             plm_chisq = as.numeric(ht$statistic), plm_p = ht$p.value,
             engine_chisq = eng$hausman_stat, engine_p = eng$hausman_p,
             ratio = as.numeric(ht$statistic) / eng$hausman_stat)
}

res <- rbind(
  check_one(48, 7, 0.8, 1),
  check_one(20, 5, 0.8, 2),
  check_one(100, 10, 0.8, 3),
  check_one(48, 7, 0.3, 4),
  check_one(200, 15, 1.2, 5)
)
print(res, row.names = FALSE)
