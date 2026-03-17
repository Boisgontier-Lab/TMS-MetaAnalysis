
options(repos = c(CRAN = "https://cloud.r-project.org/"))

if (!require("remotes")) {
  install.packages("remotes")
}

# Downgrade ggplot2 to 3.5.2 for compatibility with dmetar::var.comp
if (packageVersion("ggplot2") >= "4.0.0") {
  remotes::install_version("ggplot2", version = "3.5.2", upgrade = "never")
}

if (!require("devtools")) {
  install.packages("devtools")
}

devtools::install_github("MathiasHarrer/dmetar")
library(dmetar)

# Patch var.comp function to fix plot issue
patched_var.comp <- function(x) {
  m <- x
  if (!(class(m)[1] %in% c("rma.mv", "rma")))
    stop("x must be of class 'rma.mv'.")
  if (m$sigma2s != 2)
    stop("The model does not seem to be a three-level model.")
  if (sum(grepl("/", as.character(m$random[[1]]))) < 1)
    stop("Model must contain nested random effects.")

  n <- m$k.eff
  vector.inv.var <- 1/(diag(m$V))
  sum.inv.var <- sum(vector.inv.var)
  sum.sq.inv.var <- (sum.inv.var)^2
  vector.inv.var.sq <- 1/(diag(m$V)^2)
  sum.inv.var.sq <- sum(vector.inv.var.sq)
  num <- (n - 1) * sum.inv.var
  den <- sum.sq.inv.var - sum.inv.var.sq
  est.samp.var <- num/den

  level1 <- (est.samp.var / (m$sigma2[1] + m$sigma2[2] + est.samp.var)) * 100
  level2 <- (m$sigma2[2]  / (m$sigma2[1] + m$sigma2[2] + est.samp.var)) * 100
  level3 <- (m$sigma2[1]  / (m$sigma2[1] + m$sigma2[2] + est.samp.var)) * 100

  Level    <- c("Level 1", "Level 2", "Level 3")
  Variance <- c(level1, level2, level3)
  df.res   <- data.frame(`% of total variance` = Variance, check.names = FALSE)
  rownames(df.res) <- Level
  I2 <- c("---", round(Variance[2:3], 2))
  df.res <- as.data.frame(cbind(df.res, I2))
  totalI2 <- Variance[2] + Variance[3]

  df1 <- data.frame(Level = c("Sampling Error", "Total Heterogeneity"),
                    Variance = c(df.res[1, 1], df.res[2, 1] + df.res[3, 1]),
                    Type = rep(1, 2))
  df2 <- data.frame(Level = rownames(df.res),
                    Variance = df.res[, 1],
                    Type = rep(2, 3))
  df <- as.data.frame(rbind(df1, df2))

  g <- ggplot(df, aes(fill = Level, y = Variance, x = as.factor(Type))) +
    coord_cartesian(ylim = c(0, 1), clip = "off") +
    geom_bar(stat = "identity", position = "fill", width = 1, color = "black") +
    scale_y_continuous(labels = scales::percent) +
    theme(axis.title.x = element_blank(),
          axis.text.y = element_text(color = "black"),
          axis.line.y = element_blank(), axis.title.y = element_blank(),
          axis.line.x = element_blank(), axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_line(lineend = "round"),
          legend.position = "none",
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.background = element_rect(linetype = "solid", colour = "black"),
          legend.title = element_blank(),
          legend.key.size = unit(0.75, "cm"),
          axis.ticks.length = unit(0.25, "cm"),
          plot.margin = unit(c(1, 3, 1, 1), "lines")) +
    scale_fill_manual(values = c("darkseagreen3", "deepskyblue3",
                                 "darkseagreen2", "deepskyblue1", "deepskyblue2")) +
    annotate("text", x = 1.5, y = 1.05,
             label = paste("Total Variance:", round(m$sigma2[1] + m$sigma2[2] + est.samp.var, 3))) +
    annotate("text", x = 1, y = (df[1, 2]/2 + df[2, 2])/100,
             label = paste("Sampling Error Variance: \n", round(est.samp.var, 3)), size = 3) +
    annotate("text", x = 1, y = ((df[2, 2])/100)/2 - 0.02,
             label = paste0("'Total'~italic(I)^2*':'~", round(df[2, 2], 2), "*'%'"),
             parse = TRUE, size = 3) +
    annotate("text", x = 1, y = ((df[2, 2])/100)/2 + 0.05,
             label = paste("Variance not attributable \n to sampling error: \n",
                           round(m$sigma2[1] + m$sigma2[2], 3)), size = 3) +
    annotate("text", x = 2, y = (df[1, 2]/2 + df[2, 2])/100,
             label = paste("Level 1: \n", round(df$Variance[3], 2), "%", sep = ""), size = 3) +
    annotate("text", x = 2, y = (df[5, 2] + (df[4, 2]/2))/100,
             label = paste0("italic(I)[Level2]^2*':'~", round(df[4, 2], 2), "*'%'"),
             parse = TRUE, size = 3) +
    annotate("text", x = 2, y = (df[5, 2]/2)/100,
             label = paste0("italic(I)[Level3]^2*':'~", round(df[5, 2], 2), "*'%'"),
             parse = TRUE, size = 3)

  returnlist <- list(results = df.res, totalI2 = totalI2, plot = g)
  class(returnlist) <- c("mlm.variance.distribution", "list")
  invisible(returnlist)
  returnlist
}
assignInNamespace("var.comp", patched_var.comp, ns = "dmetar")

if (!require(tidyverse)) {
  install.packages("tidyverse")
}
library(tidyverse)

if (!require(meta)) {
  install.packages("meta")
}
library(meta)

if (!require(metafor)) {
  install.packages("metafor")
}
library(metafor)

if (!require(esc)) {
  install.packages("esc")
}
library(esc)

if (!require(TOSTER)) {
  install.packages("TOSTER")
}
library(TOSTER)

if (!require("tinytex")) {
  install.packages("tinytex")
}
library(tinytex)
if (!tinytex::is_tinytex()) {
  message("TinyTeX not detected — installing TinyTeX (this may take several minutes)...")
  tinytex::install_tinytex()
}

if (!require(svglite)) {
  install.packages("svglite")
}
library(svglite)
