
  
  
  
  myshap <- function (object, X, bg_X, pred_fun = stats::predict, feature_names = colnames(X), 
                      bg_w = NULL, exact = length(feature_names) <= 8L, hybrid_degree = 1L + 
                        length(feature_names) %in% 4:16, paired_sampling = TRUE, 
                      m = 2L * length(feature_names) * (1L + 3L * (hybrid_degree == 
                                                                     0L)), tol = 0.005, max_iter = 100L, parallel = FALSE, 
                      parallel_args = NULL, verbose = TRUE, ...) {
    
    stopifnot(is.matrix(X) || is.data.frame(X), is.matrix(bg_X) || 
                is.data.frame(bg_X), is.matrix(X) == is.matrix(bg_X), 
              dim(X) >= 1L, dim(bg_X) >= 1L, !is.null(colnames(X)), 
              !is.null(colnames(bg_X)), (p <- length(feature_names)) >= 
                1L, all(feature_names %in% colnames(X)), all(feature_names %in% 
                                                               colnames(bg_X)), all(colnames(X) %in% colnames(bg_X)), 
              is.function(pred_fun), exact %in% c(TRUE, FALSE), p == 
                1L || exact || hybrid_degree %in% 0:(p/2), paired_sampling %in% 
                c(TRUE, FALSE), `m must be even` = trunc(m/2) == 
                m/2)
    n <- nrow(X)
    bg_n <- nrow(bg_X)
    if (!is.null(bg_w)) {
      stopifnot(length(bg_w) == bg_n, all(bg_w >= 0), !all(bg_w == 
                                                             0))
    }
    if (is.matrix(X) && !identical(colnames(X), feature_names)) {
      stop("If X is a matrix, feature_names must equal colnames(X)")
    }
    v1 <- kernelshap:::check_pred(pred_fun(object, X, ...), n = n)
    bg_preds <- kernelshap:::check_pred(pred_fun(object, bg_X[, colnames(X), 
                                                 drop = FALSE], ...), n = bg_n)
    v0 <- kernelshap:::weighted_colMeans(bg_preds, bg_w)
    if (p == 1L) {
      return(kernelshap:::case_p1(n = n, nms = feature_names, v0 = v0, v1 = v1, 
                     X = X, verbose = verbose))
    }
    if (!identical(colnames(bg_X), feature_names)) {
      bg_X <- bg_X[, feature_names, drop = FALSE]
    }
    if (exact || hybrid_degree >= 1L) {
      precalc <- if (exact) 
        kernelshap:::input_exact(p)
      else kernelshap:::input_partly_exact(p, hybrid_degree)
      m_exact <- nrow(precalc[["Z"]])
      prop_exact <- sum(precalc[["w"]])
      precalc[["bg_X_exact"]] <- bg_X[rep(seq_len(bg_n), times = m_exact), 
                                      , drop = FALSE]
    }
    else {
      precalc <- list()
      m_exact <- 0L
      prop_exact <- 0
    }
    if (!exact) {
      precalc[["bg_X_m"]] <- bg_X[rep(seq_len(bg_n), times = m), 
                                  , drop = FALSE]
    }
    txt <- kernelshap:::summarize_strategy(p, exact = exact, deg = hybrid_degree)
    if (verbose) {
      message(txt)
    }
    if (max(m, m_exact) * bg_n > 2e+05) {
      warning("\nPredictions on large data sets with ", max(m, 
                                                            m_exact), "x", bg_n, " observations are being done.\n", 
              "Consider reducing the computational burden (e.g. use smaller X_bg)")
    }
    if (isTRUE(parallel)) {
      parallel_args <- c(list(i = seq_len(n)), parallel_args)
      res <- do.call(foreach::foreach, parallel_args) %dopar% 
        kernelshap:::kernelshap_one(x = X[i, , drop = FALSE], v1 = v1[i, 
                                                         , drop = FALSE], object = object, pred_fun = pred_fun, 
                       feature_names = feature_names, bg_w = bg_w, exact = exact, 
                       deg = hybrid_degree, paired = paired_sampling, 
                       m = m, tol = tol, max_iter = max_iter, v0 = v0, 
                       precalc = precalc, ...)
    }
    else {
      if (verbose && n >= 2L) {
        pb <- utils::txtProgressBar(1L, n, style = 3)
      }
      res <- vector("list", n)
      for (i in seq_len(n)) {
        res[[i]] <- kernelshap:::kernelshap_one(x = X[i, , drop = FALSE], 
                                   v1 = v1[i, , drop = FALSE], object = object, 
                                   pred_fun = pred_fun, feature_names = feature_names, 
                                   bg_w = bg_w, exact = exact, deg = hybrid_degree, 
                                   paired = paired_sampling, m = m, tol = tol, max_iter = max_iter, 
                                   v0 = v0, precalc = precalc, ...)
        if (verbose && n >= 2L) {
          utils::setTxtProgressBar(pb, i)
        }
      }
    }
    
    return(res)
    
    converged <- vapply(res, `[[`, "converged", FUN.VALUE = logical(1L))
    
    if (verbose && !all(converged)) {
      warning("\nNon-convergence for ", sum(!converged), " rows.")
    }
    out <- list(S = reorganize_list(lapply(res, `[[`, "beta"), 
                                    nms = feature_names), X = X, baseline = as.vector(v0), 
                SE = reorganize_list(lapply(res, `[[`, "sigma"), nms = feature_names), 
                n_iter = vapply(res, `[[`, "n_iter", FUN.VALUE = integer(1L)), 
                converged = converged, m = m, m_exact = m_exact, prop_exact = prop_exact, 
                exact = exact || trunc(p/2) == hybrid_degree, txt = txt, 
                predictions = v1)
    class(out) <- "kernelshap"
    out
  }
  