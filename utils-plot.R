# These functions were inspired by the check_ggplot functions of the testwhat package https://github.com/datacamp/testwhat

test_data_layer <- function(sol_data, stud_data) {
    feedback <- "You did not add the correct data."
    list('equal' = isTRUE(all.equal(sol_data, stud_data, check.attributes = FALSE)), 'feedback' = feedback)
}

test_aes_layer <- function(sol_mapping, stud_mapping) {
    for (map in names(sol_mapping)) {
        feedback_msg <- c(paste0("Have you specified the `", map, "` aesthetic?"),
                          paste0("Found `", stud_mapping[map], "` for the `", map, "` aesthetic. Expected `", sol_mapping[map], "`."))

        if (is.null(stud_mapping[map][[1]])){
            return(list('equal' = FALSE, 'feedback' = feedback_msg[1]))
        }
        if(!isTRUE(all.equal(stud_mapping[map], sol_mapping[map], check.attributes = FALSE))){
            return(list('equal' = FALSE, 'feedback' = feedback_msg[2]))
        }
    }
    return(list('equal' = TRUE, 'feedback' = ""))
}

test_geom_layer <- function(sol_layers, stud_layers) {
    nb_sol_layers <- length(sol_layers)

    if (!(nb_sol_layers > 0)) {
        return(list('equal' = TRUE, 'feedback' = ""))
    }

    for (i in 1:nb_sol_layers) {
        sol_layer <- sol_layers[[i]]

        found_geom <- FALSE
        found_geom_with_params <- FALSE

        sol_params <- get_params(sol_layer)

        sol_geom_type <- get_type(sol_layer$geom)

        nb_stud_layers <- length(stud_layers)
        if (nb_stud_layers > 0) {
            for (j in 1:nb_stud_layers) {
                stud_layer <- stud_layers[[j]]

                stud_geom_type <- get_type(stud_layer$geom)
                if (sol_geom_type == stud_geom_type) {
                    found_geom <- TRUE
                    found_params <- TRUE

                    stud_params <- get_params(stud_layer)

                    for (sol_param in names(sol_params)) {
                        if (!(sol_param %in% names(stud_params))) {
                            found_params <- FALSE
                            break
                        } else {
                            sol_value <- sol_params[[sol_param]]
                            stud_value <- stud_params[[sol_param]]

                            if (!isTRUE(all.equal(sol_value, stud_value))) {
                                found_params <- FALSE
                                break
                            }
                        }
                    }

                    if (found_params) {
                        found_geom_with_params <- TRUE
                    }

                    if (found_geom_with_params) {
                        stud_layers[[j]] <- NULL
                        break
                    }
                }
            }
        }
        geom_base_feedback <- paste0("Have you added a `", as.character(sol_geom_type),"` layer")
        feedback_msg <- c(paste0(geom_base_feedback, " with a `+` operator?"),
                          paste0(geom_base_feedback, " with the correct parameters?"))

        if (!found_geom) {
            return(list('equal' = FALSE, 'feedback' = feedback_msg[1]))
        }
        if (!found_geom_with_params) {
            return(list('equal' = FALSE, 'feedback' = feedback_msg[2]))
        }
    }
    return(list('equal' = TRUE, 'feedback' = ""))
}

get_params <- function(geom_layer) {
    params <- geom_layer$geom_params
    stat_params <- geom_layer$stat_params
    params[names(stat_params)] <- stat_params
    mapping_params <- lapply(geom_layer$mapping, function(x) structure(x, aes = TRUE))
    params[names(mapping_params)] <- mapping_params
    aes_params <- geom_layer$aes_params
    params[names(aes_params)] <- aes_params
    return(params)
}

get_type <- function(object) {
    return(sub("^_", "", tolower(gsub("([A-Z])", "_\\1", class(object)[1]))))
}
