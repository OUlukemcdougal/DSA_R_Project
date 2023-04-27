#' myttest function will look for a difference in the population variances and see whether or not the data.
#' If the data supports the conculusion that the variances are different or if the variances are paired
#' then the t-test will be ran and will return a list.
#'
#'- The one following type of test could be ran: "Welch", " T-test", "Paired"
#'- A decision on whether or not to reject the null of equality of the mean at the alpha level of significance.
#'- A summary of the chosen t test.
#'
#'
#' @param x data
#' @param y data
#' @param alpha ci
#' @param paired paired parameter
#'
#' @importFrom dplyr '%>%' filter
#' @importFrom stats 't.test' 'sd' 'var'
#'
#' @return A data frame
#' @export
myttest = function(x, y, alpha, paired){

  # create a data frame
  df = data.frame(x = x, y = y)

  # finding the population variance
  sd1 = sd(x)
  sd2 = sd(y)

  if(paired){
    print("Paired"); test_type = "Paired"; v = FALSE
  }
  else if (sd1 - sd2 <= 6) {
    print("T-test"); test_type = "T-test"; v = TRUE
  }
  else{
    print("WELCH "); test_type = "WELCH "; v = FALSE
  }

  # make t-test assuming that the variances in the population are the same
  test = t.test(x = x, y = y, var.equal = v, paired = paired, conf.level = 1 - alpha)

  # find the confidence intervals and p-value
  conf_inter = test$conf.int

  p_value = test$p.value

  if(p_value < alpha) {
    print("Y")
  }
  else{
    print("N")
  }

  # make a named list of function
  function_list = list(data = df, alpha = alpha, paired = paired,
                  conf_inter = conf_inter, p_value = p_value,
                  test_type = test_type)

  # make list to be of class "Rttest"
  class(function_list) = "Rttest"

  return(function_list)
}
