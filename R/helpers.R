# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

go_parallel <- function(data, function_to_apply, bind_method, n_cores = parallel::detectCores() - 1, ...) {
  #' Wrapper to easily apply a function to an object in parallel
  #'
  #' @description
  #' This function takes takes a dataframe and performs an operation on it in parallel.
  #' The workflow is as follows: (1) take a dataframe and divide into equal segments based on the number of cores;
  #' (2) use mclapply to loop through the divided dataframes and apply the function and all other arguments;
  #' (3) take the results and bind back together with the defined method.
  #'
  #' @param data Data set to get split up and analysed
  #' @param function_to_apply The function to analyse the data set by
  #' @param n_cores The number of cores to use. The default is pc number of cores - 1.
  #' @param bind_method Is the method to bind the results back togetehr with e.g. if the output of the function is a dataframe,
  #' then you want to use the "rbind" method. If it is a list, then use "c". To find out, check the output of the function being used.
  #' @param ... The other named arguments that function will use to analyse the data set.
  #'
  #' @return It returns whatever is expected to be returned by the function passed.
  #'
  #' @examples
  #' # Basic example
  #' x <- 1:10000000
  #' da_function <- function(x, y) {x * y}
  #' output <- go_parallel(x, da_function, bind_method = "c", n_cores = 1, y = 10)
  #'
  #' # Simple features example
  #' # To be completed...

  # Create a vector to split the data set up by.
  if (bind_method == ("c")) data_length <- length(data)
  else data_length <- nrow(data)

  split_vector <- rep(1:n_cores, each = data_length / n_cores, length.out = data_length)
  split_data <- split(data, split_vector)

  # Perform analysis
  split_results <- parallel::mclapply(split_data, function(x) function_to_apply(x, ...), mc.cores = n_cores)

  result <- do.call(bind_method, split_results)

  return(result)
}
