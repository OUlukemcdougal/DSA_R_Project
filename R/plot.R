#'This function will return a a boxplot of the data from the Rttest class, it will return multiple boxplots
#'One when the non-paired data is analyzed
#'Second one with the differences in data when the paired is True.
#'Third one is the ci for the difference of means?
#' @param x data from myttest function
#' @param ... Extra parameters
#'
#' @return Multiple plots of the categories and samples
#' @importFrom ggplot2 'ggplot' 'aes' 'stat_summary' 'scale_fill_discrete' 'labs' 'geom_boxplot'
#'
#'
#' @export plot.Rttest
#' @export
#'
plot.Rttest = function(x, ...){

  if(x$paired){

    data_var = NULL
    value    = NULL

    # merge the differences in data of x object
    m_data = data.frame(value = x$data$x - x$data$y, data_var = rep(LETTERS[24]))

    # create the boxplot when paired is True
    ggplot(m_data, aes(x = data_var , y= value , fill=data_var )) +
      geom_boxplot() +
      stat_summary(fun=mean, geom="point", shape=23, size=4, fill="white") +
      scale_fill_discrete(name = "categories") +
      labs(x = "Differences", y = "samples differences",
           title ="Boxplot of the samples")
  }
  else{

    data_var = NULL
    value    = NULL

    # create separate datasets of x and y
    x_data = data.frame(value = x$data$x, data_var = rep(LETTERS[24]))
    y_data = data.frame(value = x$data$y, data_var = rep(LETTERS[25]))

    # merge the x and y data into new data set
    merge_data = merge(x = x_data, y = y_data, all = TRUE)

    # create the boxplot
    ggplot(merge_data, aes(x = data_var , y= value , fill=data_var )) +
      scale_fill_discrete(name = "categories") +
      geom_boxplot() +
      labs(x = "category", y = "samples",
           title ="Boxplot of the samples")
  }
}
