context("Testing partial_clique_simulation")

test_that("partial_clique_simulation runs without errors for basic input", {
  # Run the function with minimal inputs
  result <- partial_clique_simulation(num_func = 1, num_trials = 1)

  # Check that the result is a list
  expect_type(result, "list")

  # Check that the list has two elements (one for each alpha)
  expect_equal(length(result), 2)

  # Check that the names of the list elements are correct
  expect_equal(names(result), c("alpha:0.5", "alpha:0.95"))
})

test_that("partial_clique_simulation returns correctly structured output", {
  # Run the function with 2 functions and 2 trials
  result <- partial_clique_simulation(num_func = 2, num_trials = 2)

  # Check that the result is a list
  expect_type(result, "list")

  # Check that the list has two elements (one for each alpha)
  expect_equal(length(result), 2)

  # Check that each alpha level contains the correct number of trials
  expect_equal(length(result[["alpha:0.5"]]), 2)
  expect_equal(length(result[["alpha:0.95"]]), 2)

  # Check that each trial contains the correct number of implementations
  for (alpha in names(result)) {
    for (trial in names(result[[alpha]])) {
      expect_equal(length(result[[alpha]][[trial]]), 2)
    }
  }
})

test_that("partial_clique_simulation handles different input sizes", {
  # Run the function with different numbers of functions and trials
  result_1 <- partial_clique_simulation(num_func = 3, num_trials = 1)
  result_2 <- partial_clique_simulation(num_func = 1, num_trials = 3)

  # Check that the result is a list
  expect_type(result_1, "list")
  expect_type(result_2, "list")

  # Check the length of the results
  expect_equal(length(result_1), 2)
  expect_equal(length(result_2), 2)
})

test_that("partial_clique_simulation saves results correctly", {
  # Temporary file to save results
  temp_file <- tempfile(fileext = ".RData")

  # Save function modified to save to temp file
  partial_clique_simulation_temp <- function(num_func, num_trials) {
    set.seed(10)
    imp_numbers <- 1:num_func
    trials <- num_trials
    alpha_vec <- c(0.5, 0.95)
    level_trial_list <- lapply(alpha_vec, function(alpha) {
      trial_list <- lapply(1:trials, function(trial) {
        data <- UWBiost561::generate_partial_clique(n = 10, clique_fraction = 0.5, clique_edge_density = 0.9)
        adj_mat <- data$adj_mat
        result_list <- lapply(imp_numbers, function(imp_number) {
          result <- UWBiost561::compute_maximal_partial_clique_master(
            adj_mat = adj_mat,
            alpha = alpha,
            number = imp_number,
            time_limit = 30
          )
          return(result)
        })
        names(result_list) <- paste("Implementation:", imp_numbers)
        return(result_list)
      })
      names(trial_list) <- paste("Trial:", 1:trials)
      return(trial_list)
    })
    names(level_trial_list) <- paste0("alpha:", alpha_vec)
    date_of_run <- Sys.time()
    session_info <- devtools::session_info()
    save(level_trial_list, alpha_vec, date_of_run, session_info, file = temp_file)
    return(level_trial_list)
  }

  # Run the modified function
  result <- partial_clique_simulation_temp(num_func = 2, num_trials = 1)

  # Check if the file is created and non-empty
  expect_true(file.exists(temp_file))
  expect_true(file.size(temp_file) > 0)

  # Clean up
  unlink(temp_file)
})
