library(rprojroot)
library(fs)
ROOT <- path(find_root(has_file(".gitignore")))

source(path(ROOT, "lib", "common.R"))

################################### UTILITIES ##################################


# See https://github.com/suyusung/arm/blob/master/R/sim.R
setOldClass("negbin")
setMethod("sim", signature(object = "negbin"),
  function (object, n.sims = 100) {
    object.class <- class(object)[[1]]
    summ <- summary(object, correlation = TRUE, dispersion = object$dispersion)
    coef <- summ$coefficients[, 1:2, drop = FALSE]
    dimnames(coef)[[2]] <- c("coef.est", "coef.sd")
    beta.hat <- coef[, 1, drop = FALSE]
    sd.beta <- coef[, 2, drop = FALSE]
    corr.beta <- summ$correlation
    n <- summ$df[1] + summ$df[2]
    k <- summ$df[1]
    V.beta <- corr.beta * array(sd.beta, c(k, k)) * t(array(sd.beta, 
                                                            c(k, k)))
    beta <- MASS::mvrnorm(n.sims, beta.hat, V.beta)
    beta2 <- array(0, c(n.sims, length(coefficients(object))))
    dimnames(beta2) <- list(NULL, names(coefficients(object)))
    beta2[, dimnames(beta2)[[2]] %in% dimnames(beta)[[2]]] <- beta
    sigma <- rep(sqrt(summ$dispersion), n.sims)
    ans <- new("sim", coef = beta2, sigma = sigma)
    return(ans)
  }
)
