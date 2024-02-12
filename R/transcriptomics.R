#' Add column with
#'
#' Adds a new column with those rows that meet the preference
#'
#' @param x A data.frame or matrix with differential results (DEG or other bioinformatics data)
#' @param p The name of the column with the p-values.
#' @param fc The name of the column with the fold change.
#' @param fdr The name of the column with the p-values corrected by multiple comparison (usually FDR).
#' @param fdr_threshold The value to decide when to classify them.
#' @param p_threshold The value to decide when to consider a row
#' @note limma recommends using the B value instead of this *ad-hoc* test (but it is popular among labs).
#' @return The same matrix or data.frame with a new column "sign" with 5 possible values:
#' `c("UUP", "UP", "", "DW", "DDW")`.
#' This new column allows to filter and easily find relevant rows.
#' @export
llrs_diff <- function(x, p = "pval", fc = "diff", fdr = "fdr", fdr_threshold = 0.05,
                      p_threshold = 0.05) {
  if (any(!c(p, fc, fdr) %in% colnames(x))) {
    return(x)
  }
  out <- rep("UP", nrow(x))
  out[x[[p]] > p_threshold] <- ""
  s <- sign(x[[fc]])
  f <- x[[fdr]] < fdr_threshold
  out[s < 0 & x[[p]] < p_threshold] <- "DW"
  out[s < 0 & f] <- "DDW"
  out[s > 0 & f] <- "UUP"
  out[is.na(x[[p]])] <- NA
  cbind(x, sign = out)
}

# In 0.9009 it was renamed to follow the patterns:
# quant_diff <- llrs_diff
