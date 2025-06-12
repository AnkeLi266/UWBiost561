#' Simulation study for maximal partial clique implementations, with progress bar
#'
#' @param n_values Vector of node counts (e.g., c(10, 25, 50))
#' @param alpha_values Vector of alpha thresholds (e.g., c(0.7, 0.9))
#' @param clique_fraction Fraction of nodes in clique (default 0.5)
#' @param clique_edge_density Density within planted clique (default 0.9)
#' @param n_trials Number of trials per setting (default 5)
#' @param time_limit Max seconds per method (default 30)
#' @return Data frame with simulation results
#' 
#'  @examples
#' Run a quick simulation study (small for demo)
#' sim_results <- simulate_partial_clique_study(
#'   n_values = c(10),
#'   alpha_values = c(0.7),
#'   n_trials = 1,
#'   time_limit = 3
#' )
#' head(sim_results)
#' 
#' @export
simulate_partial_clique_study <- function(
    n_values = c(10, 25, 50),
    alpha_values = c(0.7, 0.9),
    clique_fraction = 0.5,
    clique_edge_density = 0.9,
    n_trials = 5,
    time_limit = 30
) {
  results <- list()
  set.seed(42) # for reproducibility
  
  total_steps <- length(n_values) * length(alpha_values) * n_trials
  step <- 0
  
  for (n in n_values) {
    for (alpha in alpha_values) {
      for (trial in 1:n_trials) {
        step <- step + 1
        percent <- round(100 * step / total_steps, 1)
        # 打印进度到控制台和SLURM日志
        message(sprintf(
          "[%s] Progress: %d/%d (%.1f%%) | n=%d, alpha=%.2f, trial=%d",
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          step, total_steps, percent, n, alpha, trial
        ))
        
        # 如有需要，也可写到本地文件
        # cat(sprintf(
        #   "[%s] Progress: %d/%d (%.1f%%) | n=%d, alpha=%.2f, trial=%d\n",
        #   format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        #   step, total_steps, percent, n, alpha, trial
        # ), file = "simulation_progress.log", append = TRUE)
        
        # Try generating adjacency matrix
        adj_mat <- tryCatch({
          sim <- UWBiost561::generate_partial_clique(
            n = n,
            clique_fraction = clique_fraction,
            clique_edge_density = clique_edge_density
          )
          sim$adj_mat
        }, error = function(e) NULL)
        
        for (method in 1:15) {
          if (is.null(adj_mat)) {
            results[[length(results) + 1]] <- data.frame(
              n = n,
              alpha = alpha,
              trial = trial,
              method = method,
              timed_out = NA,
              error = TRUE,
              clique_size = NA,
              reported_density = NA,
              correct_density = NA,
              time = NA
            )
          } else {
            res <- tryCatch(
              {
                UWBiost561::compute_maximal_partial_clique_master(
                  adj_mat = adj_mat,
                  alpha = alpha,
                  number = method,
                  time_limit = time_limit
                )
              },
              error = function(e) list(
                timed_out = NA,
                error = TRUE,
                clique_idx = NA,
                edge_density = NA,
                time = NA,
                message = e$message
              )
            )
            is_error <- isTRUE(res$error) ||
              is.null(res$clique_idx) || any(is.na(res$clique_idx)) ||
              is.null(res$edge_density) || any(is.na(res$edge_density))
            correct_density <- if (!is.null(res$clique_idx) && !is.null(adj_mat) && !any(is.na(res$clique_idx))) {
              tryCatch(UWBiost561::compute_correct_density(adj_mat, res$clique_idx), error=function(e) NA)
            } else {
              NA
            }
            results[[length(results) + 1]] <- data.frame(
              n = n,
              alpha = alpha,
              trial = trial,
              method = method,
              timed_out = ifelse(is.null(res$timed_out), NA, res$timed_out),
              error = is_error,
              clique_size = ifelse(is.null(res$clique_idx) || any(is.na(res$clique_idx)), NA, length(res$clique_idx)),
              reported_density = ifelse(is.null(res$edge_density) || any(is.na(res$edge_density)), NA, res$edge_density),
              correct_density = correct_density,
              time = ifelse(is.null(res$time), NA, res$time)
            )
          }
        }
      }
    }
  }
  dplyr::bind_rows(results)
}
