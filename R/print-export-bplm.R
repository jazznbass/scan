#' @describeIn bplm Print results
#' @inheritParams print.sc
#' @order 2
#' @param x An object returned by [bplm()]
#' @export
print.sc_bplm <- function(x, digits = 3, ...) {
  cat("Bayesian Piecewise Linear Regression\n\n")
  cat("Contrast model: ", 
      x$model$interaction.method, " (", 
      paste0(names(x$contrast), ": ",x$contrast, collapse = ", "), 
      ")\n", sep = "")
  
  if (x$N > 1) cat(x$N, "Cases\n\n")
  
  out <- .output_bplm(x)
  
  cat("Deviance Information Criterion:", out$dic, "\n\n")
  
  cat("B-structure - Fixed effects (", out$formula$fixed, ")\n\n", sep = "")
  print(round_numeric(out$fixed, digits))
  
  if (!is.null(out$random)) {
    cat("\nG-Structure - Random effects (", out$formula$random, ")\n\n", sep = "")
    print(round_numeric(out$random, digits), row.names = FALSE, ...)
  }
  
  if (!is.null(out$correlation)) {
    cat("\nCorrelation\n")
    print(round_numeric(out$correlation, digits), row.names = FALSE, ...)
  }
  
  cat("\nR-Structure - Residuals\n\n")
  print(round(out$residuals, digits))
  
  
}

#' @describeIn bplm Export results as html table (see [export()])
#' @order 3
#' @inheritParams export
#' @param nice If set TRUE (default) output values are rounded and optimized for
#' publication tables.
#' @export
export.sc_bplm <- function(object, 
                           caption = NA, 
                           footnote = NA, 
                           filename = NA,
                           kable_styling_options = list(), 
                           kable_options = list(), 
                           round = 2,
                           nice = TRUE,
                           ...) {
  
  kable_options <- .join_kabel(kable_options)
  kable_styling_options <- .join_kabel_styling(kable_styling_options)
  
  if (is.na(caption)) {
    if (object$N == 1) {
      caption <- paste0(
        "Bayesian Piecewise Linear Regression predicting variable '", 
        attr(object, opt("dv")),  "'"
      )
    } else {
      caption <- paste0(
        "Bayesian Multilevel Piecewise Linear Regression predicting variable '", 
        attr(object, opt("dv")),  "'"
      )
    }
  }
  
  footnote <- c(
    paste0("Slope estimation method: ", object$model$interaction.method),
    paste0(names(x$contrast), ": ",x$contrast, collapse = ", "),
    paste0(object$N, " cases")
  )

  results <- .output_bplm(object)
  out <- results$fixed
  dat_g <- results$random
 
  dat_r <- cbind(
    data.frame(" " = "Residuals", check.names = FALSE),
    t(round(results$residuals, round))
  )
  
  nrow_g <- nrow(dat_g)
  nrow_b <- nrow(out)
  nrow_r <- nrow(dat_r)
  
  
  if (nice) {
    out$p <- .nice_p(out$p)
    if (!is.null(dat_g$p)) dat_g$p <- .nice_p(dat_g$p)
  }
  
  # B-structure -----
  out[, ] <- lapply(out[, ], function(x)
    if (inherits(x, "numeric")) as.character(round(x, round)) else x
  )
  out <- cbind(
    Predictors = rownames(out), 
    out, 
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL

  # add G-structure -----
  
  if (!is.null(results$random)) {
    names(dat_g)[1] <- " "
    dat_g[, ] <- lapply(dat_g, function(x)
      if (inherits(x, "numeric")) as.character(round(x, round)) else x
    )
    
    tmp_row <- (nrow_b + 1):(nrow_b + nrow_g + 1)
    out[tmp_row, ] <- ""
    
    out[tmp_row, 1:ncol(dat_r)] <- rbind(
      colnames(dat_g), 
      dat_g, 
      stringsAsFactors = FALSE
    )
  }
 
  # add R-structure -----

  tmp_row <- (nrow(out) + 1):(nrow(out) + nrow_r + 1)
  out[tmp_row, ] <- ""
  
  out[tmp_row, 1:ncol(dat_r)] <- rbind(
    colnames(dat_r), 
    dat_r, 
    stringsAsFactors = FALSE
  )
  
  # model ---
  
  new_row <- (nrow(out) + 1)
  out[new_row, ] <- ""
  
  out[new_row, 1:2] <- c(
    "DIC", as.character(round(results$dic, 1))
  )

  row_group <- if (!is.null(results$random)) {
    list(
      "Fixed effects (B-Structure)" = 1:nrow_b,
      "Random effects (G-Structure)" = (nrow_b + 1) : (nrow_b + nrow_g + 1),
      "Residuals (R-Structure)" = (nrow_b + nrow_g + 2) : (nrow_b + nrow_g + nrow_r + 2),
      "Model" = (nrow_b + nrow_g + nrow_r + 2) : nrow(out)
    )
  } else {
    list(
      "Fixed effects (B-Structure)" = 1:nrow_b,
      "Residuals (R-Structure)" = (nrow_b + 1) : (nrow_b + nrow_r + 1),
      "Model" = (nrow_b + nrow_r + 1) : nrow(out)
    )
  }
  
  
  table <- .create_table(
    out,
    kable_options,
    kable_styling_options,
    caption = caption,
    footnote = footnote,
    row_group = row_group
  )

  if (getOption("scan.export.engine") == "kable") {
    if (!is.null(results$random)) {
      table <- table |>
        #pack_rows("Fixed effects", 1, nrow_out, indent = FALSE) |>
        pack_rows("\nRandom effects (G-Structure)", nrow_b + 1, nrow_b + nrow_g  + 1, indent = FALSE) |>
        pack_rows("\nResiduals (R-Structure)", nrow_b + nrow_g + 2, nrow_b + nrow_g + nrow_r + 2, indent = FALSE) |>
        pack_rows("\nModel", nrow(out), nrow(out), indent = FALSE) |>
        #row_spec(nrow_out + nrow(dat_g) + 1, hline_after = TRUE) |>
        row_spec(nrow_b, hline_after = TRUE) |> 
        row_spec(nrow_b + nrow_g + 1, hline_after = TRUE)
    }
    
    if (is.null(results$random)) {
      table <- table |>
        #pack_rows("\nRandom effects (G-Structure)", nrow_b + 1, nrow_b + nrow_g  + 1, indent = FALSE) |>
        pack_rows("\nResiduals (R-Structure)", nrow_b + 1, nrow_b + nrow_r + 1, indent = FALSE) |>
        pack_rows("\nModel", nrow(out), nrow(out), indent = FALSE) |>
        #row_spec(nrow_out + nrow(dat_g) + 1, hline_after = TRUE) |>
        row_spec(nrow_b, hline_after = TRUE) 
    }
    
  }
  
  if (!is.na(filename)) .save_export(table, filename)
  
  table
  
}


.output_bplm <- function(x) {
  
  out <- list()
  
  model_summary <- summary(x$mcmcglmm)
  out$dic <- model_summary$DIC
  
  # B-structure -----
  
  out$formula$fixed <- deparse(model_summary$fixed.formula)
  
  md <- as.data.frame(model_summary$solutions)
  colnames(md) <- c("B", "lower 95% CI", "upper 95% CI", "sample size", "p")
  row.names(md) <- rename_predictors(row.names(md), x)
  out$fixed <- md
  
  # G-structure -----
  
  if (!is.null(x$model$random)) {
    
    out$formula$random <- deparse(model_summary$random.formula)
    
    #vcv <- x$mcmc$VCV[, grep("case", colnames(x$mcmc$VCV))]
    #G_structure <- apply(vcv, 2, function(x) {
    #  c(mean = mean(x), lower = quantile(x, 0.025), upper = quantile(x, 0.975))
    #})
    #G_structure <- as.data.frame(t(G_structure))
    #G_matrix <- posterior.mode(vcv) |> as.data.frame()
    G_structure <- model_summary$Gcovariances |> as.data.frame()
    row.names(G_structure) <- rename_predictors(row.names(G_structure), x)
    row.names(G_structure) <- gsub(".case", "", row.names(G_structure))
    row.names(G_structure) <- gsub("case", "Intercept", row.names(G_structure))
    G_structure <- cbind(Parameter = row.names(G_structure), G_structure)
    row.names(G_structure) <- NULL
    
    G_structure <- G_structure[, -5]
    
    if (nrow(G_structure) > 1) {
      matching_elements <- grepl("^(.*):\\1$", G_structure$Parameter)
      G_structure_variance <- G_structure[matching_elements, ]
      G_structure_covariance <- G_structure[!matching_elements, ]
      
      G_structure_variance$Parameter <- sub(":.*", "", G_structure_variance$Parameter)
      
      G_structure_covariance$Parameter <- sapply(
        strsplit(G_structure_covariance$Parameter, ":", fixed = TRUE), 
        function(y) paste(sort(y), collapse = ":")
      )
      
      G_structure_covariance <- 
        G_structure_covariance[duplicated(G_structure_covariance$Parameter), ]
      
      names(G_structure_variance) <- c("Parameter", "SD", "lower 95% CI", "upper 95% CI") 
      names(G_structure_covariance) <- c("Parameter", "Correlation", "lower 95% CI", "upper 95% CI") 
      
      #cat("Standard deviation\n")
      G_structure_variance[, 2:4] <- sqrt(G_structure_variance[, 2:4])
      
      out$random <- G_structure_variance
      
      cor_vars <- strsplit(G_structure_covariance$Parameter, ":", fixed = TRUE)
      
      std_covar <- sapply(cor_vars, function(x) {
        var1 <- which(G_structure_variance$Parameter == x[1])
        var2 <- which(G_structure_variance$Parameter == x[2])
        G_structure_variance$SD[var1] * G_structure_variance$SD[var2]
      })
      
      for (i in 1:nrow(G_structure_covariance)) {
        G_structure_covariance[i, 2:4] <- G_structure_covariance[i,2:4] / std_covar[i]  
      }
      
      out$correlation <- G_structure_covariance
      
    } else {
      names(G_structure) <- c("Parameter", "SD", "lower 95% CI", "upper 95% CI") 
      G_structure[, 2:4] <- sqrt(G_structure[, 2:4])
      #cat("Standard deviation\n")
      
      out$random <- G_structure
      
    }
    
  }
  
  R_structure <- model_summary$Rcovariances[, -4]
  names(R_structure) <- c("SD", "lower 95% CI", "upper 95% CI") 
  out$residuals <- sqrt(R_structure)
  
  out
}
