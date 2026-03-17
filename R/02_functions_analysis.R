#' Run multilevel meta-analysis using metafor::rma.mv
#' 
#' Conducts a multilevel random-effects meta-analysis with
#' correlation estimates nested within studies.
#' 
#' @param data Data frame with z, var.z, author, and cor_id columns
#' @return rma.mv object with meta-analysis results
run_multilevel_meta <- function(data) {
  model <- rma.mv(
    yi = z,
    V = var.z,
    slab = author,
    data = data,
    random = ~ 1 | author/cor_id,
    test = "t",
    method = "REML"
  )
  return(model)
}

#' Run sensitivity meta-analysis using meta::metacor
#' 
#' Conducts a conventional random-effects meta-analysis for sensitivity.
#' 
#' @param data Data frame with cor, n, author, and cluster columns
#' @param title Title for the analysis
#' @return metacor object with meta-analysis results
run_sensitivity_meta <- function(data, title = "Sensitivity meta-analysis using metacor") {
  model <- metacor(
    cor = cor,
    n = n,
    studlab = author,
    data = data,
    cluster = cluster,
    common = FALSE,
    random = TRUE,
    method.tau = "REML",
    method.random.ci = "HK",
    title = title
  )
  return(model)
}

#' Run subgroup analysis using metafor
#' 
#' Conducts subgroup meta-analysis with a moderator variable.
#' 
#' @param data Data frame with meta-analysis data
#' @param moderator Name of the moderator variable (character)
#' @return rma.mv object with subgroup analysis results
run_subgroup_analysis_metafor <- function(data, moderator) {
  data[[moderator]] <- as.factor(data[[moderator]])
  
  formula <- as.formula(paste("~", moderator))
  
  model <- rma.mv(
    yi = z,
    V = var.z,
    slab = author,
    data = data,
    random = ~ 1 | author/cor_id,
    test = "t",
    method = "REML",
    mods = formula
  )
  return(model)
}



#' Run equivalence test using TOST
#' 
#' Conducts two one-sided tests for equivalence testing.
#' 
#' @param effect_size Point estimate (Fisher's z)
#' @param se Standard error
#' @param sesoi_r Smallest effect size of interest in r (default: 0.1)
#' @param alpha Significance level (default: 0.05)
#' @return TOST results
run_equivalence_test <- function(effect_size, se, sesoi_r = 0.1, alpha = 0.05) {
  low_eq_z <- atanh(-sesoi_r)
  high_eq_z <- atanh(sesoi_r)
  
  result <- TOSTmeta(
    ES = effect_size,
    se = se,
    low_eqbound = low_eq_z,
    high_eqbound = high_eq_z,
    alpha = alpha
  )
  return(result)
}

#' Run Egger's test for publication bias
#' 
#' Conducts Egger's regression test for funnel plot asymmetry
#' using standard errors as predictor in meta-regression.
#' 
#' @param data Data frame with z, var.z, author, and cor_id columns
#' @return rma.mv object with Egger's test results
run_eggers_test <- function(data) {
  result <- rma.mv(
    z,
    var.z,
    mod = ~ sqrt(var.z),
    random = ~ 1 | author/cor_id,
    data = data,
    test = "t"
  )
  return(result)
}


#' Convert Fisher's z to Pearson's r
#' 
#' Wrapper for esc::convert_z2r for convenience.
#' 
#' @param z Fisher's z value
#' @param digits Number of decimal places (default: 2)
#' @return Rounded Pearson's r
z_to_r <- function(z, digits = 2) {
  r <- convert_z2r(z)
  return(round(r, digits))
}
