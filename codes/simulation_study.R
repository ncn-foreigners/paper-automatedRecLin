library(automatedRecLin)
library(data.table)
library(readr)
library(phonics)
library(tidyr)

census <- read_csv("data-raw/census.csv")
cis <- read_csv("data-raw/cis.csv")
prd <- read_csv("data-raw/prd.csv")

census <- as.data.frame(lapply(census, as.character))
census[is.na(census)] <- ""
cis <- as.data.frame(lapply(cis, as.character))
cis[is.na(cis)] <- ""
prd <- as.data.frame(lapply(prd, as.character))
prd[is.na(prd)] <- ""

census_soundex <- copy(census)
cis_soundex <- copy(cis)
prd_soundex <- copy(prd)
set(census_soundex, j = "PERNAME1", value = soundex(census_soundex[["PERNAME1"]]))
set(census_soundex, j = "PERNAME2", value = soundex(census_soundex[["PERNAME2"]]))
census_soundex <- separate(census_soundex, PERNAME1,
                           into = c("P11", "P12", "P13", "P14"), sep = 1:3)
census_soundex <- separate(census_soundex, PERNAME2,
                           into = c("P21", "P22", "P23", "P24"), sep = 1:3)
set(cis_soundex, j = "PERNAME1", value = soundex(cis_soundex[["PERNAME1"]]))
set(cis_soundex, j = "PERNAME2", value = soundex(cis_soundex[["PERNAME2"]]))
cis_soundex <- separate(cis_soundex, PERNAME1,
                        into = c("P11", "P12", "P13", "P14"), sep = 1:3)
cis_soundex <- separate(cis_soundex, PERNAME2,
                        into = c("P21", "P22", "P23", "P24"), sep = 1:3)
set(prd_soundex, j = "PERNAME1", value = soundex(prd_soundex[["PERNAME1"]]))
set(prd_soundex, j = "PERNAME2", value = soundex(prd_soundex[["PERNAME2"]]))
prd_soundex <- separate(prd_soundex, PERNAME1,
                        into = c("P11", "P12", "P13", "P14"), sep = 1:3)
prd_soundex <- separate(prd_soundex, PERNAME2,
                        into = c("P21", "P22", "P23", "P24"), sep = 1:3)

setDT(census)
setDT(cis)
setDT(prd)
setDT(census_soundex)
setDT(cis_soundex)
setDT(prd_soundex)

n_A <- 500
n_B <- 1000
ids <- census$PERSON_ID
variables <- c("PERNAME1", "PERNAME2", "SEX",
               "DOB_DAY", "DOB_MON", "DOB_YEAR")
variables_soundex <- c("P11", "P12", "P13", "P14",
                       "P21", "P22", "P23", "P24", "SEX",
                       "DOB_DAY", "DOB_MON", "DOB_YEAR")
comparators <- list(
  "PERNAME1" = jarowinkler_complement(),
  "PERNAME2" = jarowinkler_complement()
)
methods_cpar <- list(
  "PERNAME1" = "continuous_parametric",
  "PERNAME2" = "continuous_parametric"
)
methods_cnonpar <- list(
  "PERNAME1" = "continuous_nonparametric",
  "PERNAME2" = "continuous_nonparametric"
)

# p_A = 0.8

set.seed(1)
p_A_8 <- 0.8
n_0_8 <- n_B / p_A_8
ids_sample_8 <- sample(ids, n_0_8)

ids_prd_8 <- sample(ids_sample_8, n_A)
ids_cis_8 <- sample(ids_sample_8, n_B)

prd_sample_8 <- prd[PERSON_ID %in% ids_prd_8, ]
cis_sample_8 <- cis[PERSON_ID %in% ids_cis_8, ]
prd_soundex_sample_8 <- prd_soundex[PERSON_ID %in% ids_prd_8, ]
cis_soundex_sample_8 <- cis_soundex[PERSON_ID %in% ids_cis_8, ]

matches_8 <- merge(x = prd_sample_8[, .(x=1:.N, PERSON_ID)],
                   y = cis_sample_8[, .(y = 1:.N, PERSON_ID)],
                   by = "PERSON_ID")
setnames(matches_8, c("x", "y"), c("a", "b"))
set(matches_8, j = "PERSON_ID", value = NULL)

# set_construction = "size"
set.seed(1)
result_8_b <- mec(prd_sample_8, cis_sample_8,
                  variables = variables,
                  true_matches = matches_8)
result_soundex_8_b <- mec(prd_soundex_sample_8, cis_soundex_sample_8,
                          variables = variables_soundex,
                          true_matches = matches_8)
result_8_cpar <- mec(prd_sample_8, cis_sample_8,
                     variables = variables,
                     true_matches = matches_8,
                     comparators = comparators,
                     methods = methods_cpar)
result_8_cnonpar <- mec(prd_sample_8, cis_sample_8,
                        variables = variables,
                        true_matches = matches_8,
                        comparators = comparators,
                        methods = methods_cnonpar)
result_8_cnonpar_hurdle <- mec(prd_sample_8, cis_sample_8,
                               variables = variables,
                               true_matches = matches_8,
                               comparators = comparators,
                               methods = methods_cnonpar,
                               nonpar_hurdle = TRUE)

# set_construction = "flr"
set.seed(1)
result_8_b_flr_03 <- mec(prd_sample_8, cis_sample_8,
                         variables = variables,
                         true_matches = matches_8,
                         set_construction = "flr")
result_soundex_8_b_flr_03 <- mec(prd_soundex_sample_8, cis_soundex_sample_8,
                                 variables = variables_soundex,
                                 true_matches = matches_8,
                                 set_construction = "flr")
result_8_cpar_flr_03 <- mec(prd_sample_8, cis_sample_8,
                            variables = variables,
                            true_matches = matches_8,
                            comparators = comparators,
                            methods = methods_cpar,
                            set_construction = "flr")
result_8_cnonpar_flr_03 <- mec(prd_sample_8, cis_sample_8,
                               variables = variables,
                               true_matches = matches_8,
                               comparators = comparators,
                               methods = methods_cnonpar,
                               set_construction = "flr")
result_8_cnonpar_hurdle_flr_03 <- mec(prd_sample_8, cis_sample_8,
                                      variables = variables,
                                      true_matches = matches_8,
                                      comparators = comparators,
                                      methods = methods_cnonpar,
                                      nonpar_hurdle = TRUE,
                                      set_construction = "flr")

set.seed(1)
result_8_b_flr_05 <- mec(prd_sample_8, cis_sample_8,
                         variables = variables,
                         true_matches = matches_8,
                         set_construction = "flr",
                         target_flr = 0.05)
result_soundex_8_b_flr_05 <- mec(prd_soundex_sample_8, cis_soundex_sample_8,
                                 variables = variables_soundex,
                                 true_matches = matches_8,
                                 set_construction = "flr",
                                 target_flr = 0.05)
result_8_cpar_flr_05 <- mec(prd_sample_8, cis_sample_8,
                            variables = variables,
                            true_matches = matches_8,
                            comparators = comparators,
                            methods = methods_cpar,
                            set_construction = "flr",
                            target_flr = 0.05)
result_8_cnonpar_flr_05 <- mec(prd_sample_8, cis_sample_8,
                               variables = variables,
                               true_matches = matches_8,
                               comparators = comparators,
                               methods = methods_cnonpar,
                               set_construction = "flr",
                               target_flr = 0.05)
result_8_cnonpar_hurdle_flr_05 <- mec(prd_sample_8, cis_sample_8,
                                      variables = variables,
                                      true_matches = matches_8,
                                      comparators = comparators,
                                      methods = methods_cnonpar,
                                      nonpar_hurdle = TRUE,
                                      set_construction = "flr",
                                      target_flr = 0.05)

# results
metrics_8_b <- unlist(c(n_M_est = result_8_b$n_M_est / 100, flr_est = result_8_b$flr_est,
                        mmr_est = result_8_b$mmr_est, unlist(result_8_b$eval_metrics)))
metrics_soundex_8_b <- unlist(c(n_M_est = result_soundex_8_b$n_M_est / 100, flr_est = result_soundex_8_b$flr_est,
                                mmr_est = result_soundex_8_b$mmr_est, unlist(result_soundex_8_b$eval_metrics)))
metrics_8_cpar <- unlist(c(n_M_est = result_8_cpar$n_M_est / 100, flr_est = result_8_cpar$flr_est,
                           mmr_est = result_8_cpar$mmr_est, unlist(result_8_cpar$eval_metrics)))
metrics_8_cnonpar <- unlist(c(n_M_est = result_8_cnonpar$n_M_est / 100, flr_est = result_8_cnonpar$flr_est,
                              mmr_est = result_8_cnonpar$mmr_est, unlist(result_8_cnonpar$eval_metrics)))
metrics_8_cnonpar_hurdle <- unlist(c(n_M_est = result_8_cnonpar_hurdle$n_M_est / 100, flr_est = result_8_cnonpar_hurdle$flr_est,
                                     mmr_est = result_8_cnonpar_hurdle$mmr_est, unlist(result_8_cnonpar_hurdle$eval_metrics)))

results_8_size <- list(
  "binary" = metrics_8_b,
  "binary_soundex" = metrics_soundex_8_b,
  "cpar" = metrics_8_cpar,
  "cnonpar" = metrics_8_cnonpar,
  "cnonpar_hurdle" = metrics_8_cnonpar_hurdle
)
table_8_size <- data.frame(round(do.call(rbind, results_8_size) * 100, 4))

metrics_8_b_flr_03 <- unlist(c(n_M_est = result_8_b_flr_03$n_M_est / 100, flr_est = result_8_b_flr_03$flr_est,
                               mmr_est = result_8_b_flr_03$mmr_est, unlist(result_8_b_flr_03$eval_metrics)))
metrics_soundex_8_b_flr_03 <- unlist(c(n_M_est = result_soundex_8_b_flr_03$n_M_est / 100, flr_est = result_soundex_8_b_flr_03$flr_est,
                                       mmr_est = result_soundex_8_b_flr_03$mmr_est, unlist(result_soundex_8_b_flr_03$eval_metrics)))
metrics_8_cpar_flr_03 <- unlist(c(n_M_est = result_8_cpar_flr_03$n_M_est / 100, flr_est = result_8_cpar_flr_03$flr_est,
                                  mmr_est = result_8_cpar_flr_03$mmr_est, unlist(result_8_cpar_flr_03$eval_metrics)))
metrics_8_cnonpar_flr_03 <- unlist(c(n_M_est = result_8_cnonpar_flr_03$n_M_est / 100, flr_est = result_8_cnonpar_flr_03$flr_est,
                                     mmr_est = result_8_cnonpar_flr_03$mmr_est, unlist(result_8_cnonpar_flr_03$eval_metrics)))
metrics_8_cnonpar_hurdle_flr_03 <- unlist(c(n_M_est = result_8_cnonpar_hurdle_flr_03$n_M_est / 100, flr_est = result_8_cnonpar_hurdle_flr_03$flr_est,
                                            mmr_est = result_8_cnonpar_hurdle_flr_03$mmr_est, unlist(result_8_cnonpar_hurdle_flr_03$eval_metrics)))

results_8_flr_03 <- list(
  "binary" = metrics_8_b_flr_03,
  "binary_soundex" = metrics_soundex_8_b_flr_03,
  "cpar" = metrics_8_cpar_flr_03,
  "cnonpar" = metrics_8_cnonpar_flr_03,
  "cnonpar_hurdle" = metrics_8_cnonpar_hurdle_flr_03
)
table_8_flr_03 <- data.frame(round(do.call(rbind, results_8_flr_03) * 100, 4))

metrics_8_b_flr_05 <- unlist(c(n_M_est = result_8_b_flr_05$n_M_est / 100, flr_est = result_8_b_flr_05$flr_est,
                               mmr_est = result_8_b_flr_05$mmr_est, unlist(result_8_b_flr_05$eval_metrics)))
metrics_soundex_8_b_flr_05 <- unlist(c(n_M_est = result_soundex_8_b_flr_05$n_M_est / 100, flr_est = result_soundex_8_b_flr_05$flr_est,
                                       mmr_est = result_soundex_8_b_flr_05$mmr_est, unlist(result_soundex_8_b_flr_05$eval_metrics)))
metrics_8_cpar_flr_05 <- unlist(c(n_M_est = result_8_cpar_flr_05$n_M_est / 100, flr_est = result_8_cpar_flr_05$flr_est,
                                  mmr_est = result_8_cpar_flr_05$mmr_est, unlist(result_8_cpar_flr_05$eval_metrics)))
metrics_8_cnonpar_flr_05 <- unlist(c(n_M_est = result_8_cnonpar_flr_05$n_M_est / 100, flr_est = result_8_cnonpar_flr_05$flr_est,
                                     mmr_est = result_8_cnonpar_flr_05$mmr_est, unlist(result_8_cnonpar_flr_05$eval_metrics)))
metrics_8_cnonpar_hurdle_flr_05 <- unlist(c(n_M_est = result_8_cnonpar_hurdle_flr_05$n_M_est / 100, flr_est = result_8_cnonpar_hurdle_flr_05$flr_est,
                                            mmr_est = result_8_cnonpar_hurdle_flr_05$mmr_est, unlist(result_8_cnonpar_hurdle_flr_05$eval_metrics)))

results_8_flr_05 <- list(
  "binary" = metrics_8_b_flr_05,
  "binary_soundex" = metrics_soundex_8_b_flr_05,
  "cpar" = metrics_8_cpar_flr_05,
  "cnonpar" = metrics_8_cnonpar_flr_05,
  "cnonpar_hurdle" = metrics_8_cnonpar_hurdle_flr_05
)
table_8_flr_05 <- data.frame(round(do.call(rbind, results_8_flr_05) * 100, 4))
