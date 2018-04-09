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

go_parallel <- function(data, function_to_apply, bind_method, length_counter, n_cores = parallel::detectCores() - 1, ...) {
  #' Wrapper to easily apply a function to an object in parallel
  #'
  #' @description
  #' This function takes takes an array (vector, list, matrix, dataframe) and performs an operation on it in parallel.
  #' The workflow is as follows: (1) take a array (eg. dataframe) and divide into equal segments based on the number of cores;
  #' (2) use mclapply to loop through the divided dataframes and apply the function and all other arguments;
  #' (3) take the results and bind back together with the defined method.
  #'
  #' Only works on MacOSX and Linux!
  #'
  #' @param data Data set to get split up and analysed
  #' @param function_to_apply The function to analyse the data set by
  #' @param n_cores The number of cores to use. The default is pc number of cores - 1.
  #' @param bind_method Is the method to bind the results back togetehr with e.g. if the output of the function is a dataframe,
  #' @param length_counter Is the method in which to count the length of an object e.g. length for vector or list, and nrow for matric or dataframe.
  #' then you want to use the "rbind" method. If it is a list, then use "c". To find out, check the output of the function being used.
  #' @param ... The other named arguments that function will use to analyse the data set.
  #'
  #' @return It returns whatever is expected to be returned by the function passed.
  #'
  #' @examples
  #' # Basic example
  #' x <- 1:10000000
  #' multiplier <- function(x, y) {x * y}
  #' output <- go_parallel(x, multiplier, bind_method = "c", length_counter = "length", n_cores = 1, y = 10)
  #'
  #' # Simple features example
  #' library(sf)
  #' nc <- st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
  #' nc_union_par <- go_parallel(nc, st_union, bind_method = "c", length_counter = "nrow", n_cores = 3)
  #'
  #' # Note that st_union is expected to return one feature, yet it returns three in this case. To solve this issue, you just need to
  #' # rerun the function on the remaining polygons.
  #' nc_union <- st_union(nc_union_par)

  # Create a vector to split the data set up by.
  if (length_counter == "length") data_length <- length(data)
  else if (length_counter == "nrow") data_length <- nrow(data)
  else stop("Do not know how to deal with length counter: ", length_counter)

  split_vector <- rep(1:n_cores, each = data_length / n_cores, length.out = data_length)
  split_data <- split(data, split_vector)

  # Perform analysis
  split_results <- parallel::mclapply(split_data, function(x) function_to_apply(x, ...), mc.cores = n_cores)

  # Bind together using bind method
  result <- do.call(bind_method, split_results)

  return(result)
}
