#'This method prints to the command line the confidence interval for muX and muY.
#'
#' @param x list from myttest function
#' @param ... extra parameters
#'
#' @return This function will return a list of summary data with the type of the t-test,
#' confidence interval of Rttest.
#' @export print.Rttest
#' @export
#'
#'
print.Rttest = function(x, ...) {

  ci = x$conf_inter  # store the confidence interval
  test_type = x$test_type   # store the type of test

  # create a list of summary data
  print_list = list(ci = ci, test_type = test_type)

  return(print_list)
}
