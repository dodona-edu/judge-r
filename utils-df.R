round_df <- function(x) {
    # round all numeric variables
    # x: data frame
    # signif rounds to 6 significant digits by default
    x %>%
       dplyr::mutate_if(is.numeric, signif)
}
