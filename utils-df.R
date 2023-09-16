round_df <- function(x) {
    # round all numeric variables
    # x: data frame
    # signif rounds to 6 significant digits by default
    x %>%
        dplyr::mutate_if(is.numeric, signif)
}

# Sorting and then just checking for equality isn't good enough. If
# there are near-equal floating point numbers, floating point errors
# can make it so some rows are sorted differently in the expected
# dataframe and the generated dataframe. We fix this by looking in the
# vicinity of the rows after sorting. Note that this vicinity is
# implemented quite naively at the moment, in that we just store the
# index below the first row that wasn't matched yet to determine where
# we start looking, and we don't check any stop condition other than
# getting to the end of the dataframe. This could theoretically result
# in poor performance (in that we would need n^2 expensive
# comparisons), but in practice once we get too far from the original
# row, we won't be able to match the row and the process will stop
# since we will have found a mistake.
compare_data_frames_row_independent <- function(expected, generated, ...) {
    expected <- dplyr::arrange(expected, dplyr::across(everything()))
    generated <- dplyr::arrange(generated, dplyr::across(everything()))

    mistakes_found <- !isTRUE(all.equal(nrow(expected), nrow(generated)))
    used_rows <- c()
    definitely_used <- 0
    i <- 1
    while(i <= nrow(generated) && !mistakes_found) {
        j <- definitely_used + 1
        while (j <= nrow(expected) && (is.element(j, used_rows) || !isTRUE(all.equal(generated[i,], expected[j,], ...)))) {
            j <- j + 1
        }
        mistakes_found <- isTRUE(all.equal(j, nrow(expected) + 1))
        used_rows <- c(used_rows, j)
        while (is.element(definitely_used + 1, used_rows)) {
            definitely_used <- definitely_used + 1
        }
        i <- i + 1
    }
    return(!mistakes_found)
}


#df1 is the dataframe from which the order of collumns and rows will be preserved when ignore_col_order or ignore_row_order is set to TRUE
dataframe_all_equal <- function(df1, df2, ignore_col_order = TRUE, ignore_row_order = FALSE, after_error = 2, before_error = 2) {
    # rounding to avoid identical testing
    df1 <- round_df(df1)
    df2 <- round_df(df2)

    # change the column order of df2 to match df1 if needed
    if (ignore_col_order){
        df2 <- df2[intersect(names(df1), names(df2))]
    }

    error <- FALSE
    textual_feedback <- ""
    if (ignore_row_order){
        no_match_count <- 0
        df2_matches <- c()
        past_error <- 0

        # loop over every df1 row and find its match
        df1_row <- 0
        while((df1_row < nrow(df1) || length(df2_matches) < nrow(df2)) &&
              (past_error < after_error + no_match_count || df1_row <= after_error + before_error + no_match_count)){

            df1_row <- df1_row + 1
            df2_row_options <- c(1:nrow(df2))[!c(1:nrow(df2)) %in% df2_matches]

            if(error){
                past_error <- past_error + 1
            }

            if(df1_row > nrow(df1)){# df1 is missing a row
                # if we reach this case we are sure that df2_row_options contains at least 1 element
                df2_matches <- c(df2_matches, df2_row_options[1])
                if(!error){
                    error <- TRUE
                    textual_feedback <- "Your dataframe is missing at least one row."
                }
            } else if (length(df2_row_options) == 0){# no possible matches left, df1 has a row too much
                df2_matches <- c(df2_matches, NA)
                if(!error){
                    error <- TRUE
                    textual_feedback <- "Your dataframe contains at least one row too much."
                }
            } else {# search a match for this row
                df2_row_ind <- 1
                while(df2_row_ind <= length(df2_row_options) &&
                      #TODO write a function that can also test for row.names so that row names can be displayed in the future
                      #look into: !isTRUE(all.equal(df1[df1_row,], df2[df2_row_options[df2_row_ind],], use.names = TRUE,check.attributes = FALSE)))
                      !isTRUE(dplyr::all_equal(df1[df1_row,], df2[df2_row_options[df2_row_ind],], ignore_col_order = FALSE, ignore_row_order = FALSE)))
                {

                    df2_row_ind <- df2_row_ind + 1
                }
                if(df2_row_ind > length(df2_row_options)){ # no match found
                    no_match_count <- no_match_count + 1
                    if(!error){
                        error <- TRUE
                        textual_feedback <- "Your dataframe contains at least one row which is not found in the solution."
                    }
                } else { # match found!!
                    df2_matches <- c(df2_matches, df2_row_options[df2_row_ind])
                }
            }
        }

        # add unmatched rows from df2 to df2_matches if needed
        df2_row_options <- c(1:nrow(df2))[!c(1:nrow(df2)) %in% df2_matches]
        to_add <- min(after_error - (past_error - no_match_count), nrow(df1) - length(df2_matches))
        if(to_add > 0){
            df2_matches <- c(df2_matches, df2_row_options[1:to_add])
        }

        eind <- length(df2_matches)
        begin <- max(1, eind - (before_error + after_error))
        return(list(
                    'equal' = !error,
                    'df1_rows' = begin:eind,
                    'df2_rows' = tail(df2_matches, after_error + before_error + 1),
                    'df2_cols' = names(df2),
                    "feedback" = textual_feedback
                    ))

    } else {
        df_row <- 0
        past_error <- 0
        while(df_row < max(nrow(df1), nrow(df2)) && (past_error < after_error || df_row <= after_error + before_error + 1)){
            df_row <- df_row + 1
            if(error){
                past_error <- past_error + 1
            }
            if (!identical(df1[df_row,], df2[df_row,]) && !error){
                textual_feedback <- paste0("unexpected row with rownumber: ", df_row)
                error <- TRUE
            }
        }
        eind <- df_row
        begin <- max(1, eind - (after_error + before_error))
        return(list(
                    'equal' = !error,
                    'df1_rows' = begin:eind,
                    'df2_rows' = begin:eind,
                    'df2_cols' = names(df2),
                    'feedback' = textual_feedback
                    ))
    }
}
