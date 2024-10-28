#' Function 2
#'
#' This function calculates statistics over all of the DRG codes for average Medicare payments.
#' It extracts the DRG code from the 'DRG Definition' column and groups the data by the DRG code.
#'
#' @param data A dataframe containing the DRG data.
#' @param statistic The statistic to calculate for each DRG code ("mean", "median", or "sd").
#'
#' @return A dataframe with the DRG codes and the corresponding statistic.
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr %>%

#'
#' @examples
#' # Assuming `drg_data` is a data frame with a column "Average.Medicare.Payments"
#' calculate_drg_stats(drg_data, statistic = "mean")
#' calculate_drg_stats(drg_data, statistic = "sd")


calculate_drg_stats <- function(data, statistic = "mean") {
  if (!statistic %in% c("mean", "median", "sd")) {
    stop("Invalid statistic. Please choose 'mean', 'median', or 'sd'.")
  }
  
  # Extract the DRG code
  data <- data %>%
    mutate(DRG_Code = sub("^(\\d+)\\s-.*$", "\\1", DRG.Definition))
  
  # Group by DRG_Code and calculate the specified statistic
  result <- data %>%
    group_by(DRG_Code) %>%
    summarise(Statistic = if (statistic == "mean") {
      mean(Average.Medicare.Payments, na.rm = TRUE)
    } else if (statistic == "median") {
      median(Average.Medicare.Payments, na.rm = TRUE)
    } else {
      sd(Average.Medicare.Payments, na.rm = TRUE)
    })
  
  # Rename the column based on the selected statistic
  colnames(result)[2] <- statistic
  
  return(result)
}




