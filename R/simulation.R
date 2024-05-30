simulation <- function(num_func, num_trials) {
    rm(list=ls())
    set.seed(10)
    library(UWBiost561)

    imp_numbers <- 1:num_func
    trials <- num_trials
    alpha_vec <- c(0.5, 0.95)

    # loop over the levels
    level_trial_list <- lapply(alpha_vec, function(alpha){
      print(paste("Value of alpha:", alpha))

      # loop over the different trials for this level
      trial_list <- lapply(1:trials, function(trial){
        print(paste("Working on trial:", trial))
        # to freeze the randomness of adj_mat
        set.seed(trial)

        # generate the data
        data <- generate_partial_clique(n=10)
        adj_mat <- data$adj_mat

        # loop over the methods for this trial
        result_lis <- lapply(imp_numbers, function(imp_number){

          # to freeze the randomness of the method
          set.seed(trial)
          cat('*')
          result <- compute_maximal_partial_clique_master(
            adj_mat = adj_mat,
            alpha = alpha,
            number = imp_number,
            time_limit = 30
          )

          return(result)
        })
        names(result_list) <- paste("Implementation:", imp_numbers)
        cat("\n")

        return(result_list)
      })
      names(trial_list) <- paste("Trial:", 1:trials)
      print("====")

      return(trial_list)
    })

    names(level_trial_list) <- paste0("alpha:", alpha_vec)

    # it's always useful to save the date and R session info
    date_of_run <- Sys.time()
    session_info <- devtools::session_info()

    save(level_trial_list, # save your results
         alpha_vec, # save which alphas you used
         date_of_run, session_info,
         file = "~/simulation.RData")
}
