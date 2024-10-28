#' Function 1
#'
#'This function creates a boxplot based on options: "average Medicare payments", "average total payment",
# "average covered charges"

#' @param data a dataframe
#' @param payment_type user options ("average Medicare payments", "average total payment",
#' "average covered charges")
#'
#' @return A boxplot of payments based on users options
#' @export
#'
#' @importFrom dplyr case_when
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme
#' @importFrom rlang sym()
#' @importFrom rlang !!
#'
#' @examples
#' Assuming df is your data frame with columns "Average.Medicare.Payments", "Average.Total.Payments", and "Average.Covered.Charges"
#' User can input the above valid options
#' Function1(df, payment_type = "average covered charges")


Function1 <- function(data, payment_type = "average Medicare payments") {

  # Define valid options for payment types
  valid_payment_options <- c("average Medicare payments", "average total payment", "average covered charges")

  # Check if the user-provided payment_type is valid
  if (!payment_type %in% valid_payment_options) {
    stop("Invalid payment type. Choose from 'average Medicare payments', 'average total payment', or 'average covered charges'.")
  }

  # Create a mapping from user input to the actual column names in the data
  payment_type_map <- list(
    "average Medicare payments" = "Average.Medicare.Payments",
    "average total payment" = "Average.Total.Payments",
    "average covered charges" = "Average.Covered.Charges"
  )

  # Retrieve the correct column name
  payment_type_act <- payment_type_map[[payment_type]]


  # Determine Y-axis label based on user options
  y_axis_label <- case_when(
    payment_type == "average Medicare payments" ~ "Average Medicare Payments",
    payment_type == "average total payment" ~ "Average Total Payment",
    payment_type == "average covered charges" ~ "Average Covered Charges"
  )

  # Convert for dplyr/ggplot2
  payment_col <- sym(payment_type_act)

  # Create the boxplot
  ggplot(data, aes(y = !!payment_col)) +
    geom_boxplot() +
    labs(y = y_axis_label,  # Use the determined y-axis label
         title = "Boxplot") +  # Set title
    theme(plot.title = element_text(hjust = 0.5))  # Center align the title
}
