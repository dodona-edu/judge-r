# These functions were inspired by the check_ggplot functions of the testwhat package https://github.com/datacamp/testwhat

test_data_layer <- function(sol_data, stud_data) {
    equal_names <- names(sol_data) %in% names(stud_data)
    equal <- all(equal_names)
    feedback <- paste0("You did not add the correct data, missing ", ifelse(sum(equal_names)==1, "column", "columns"), " named: ", names(sol_data)[!equal_names])
    if(equal){
        equal <- isTRUE(all.equal(dplyr::select(sol_data, names(sol_data)), dplyr::select(stud_data, names(sol_data))))
        feedback <- "The data you used in your ggplot contains the right column names but is wrong"
    }
    list('equal' = equal, 'feedback' = feedback)
}

test_facet_layer <- function(sol_facet, stud_facet, ignore_facet_type = TRUE) {
    sol_type <- class(sol_facet)[1]
    stud_type <- class(stud_facet)[1]
    
    if (sol_type == "FacetNull") {
        return(list('equal' = TRUE, 'feedback' = ""))
    }
    if (stud_type == "FacetNull") {
        return(list('equal' = FALSE, 'feedback' = "Did you define a multipanel plot?"))
    }
    equal <- FALSE
    
    if (ignore_facet_type) {
        equal <- setequal(sol_facet$vars(), stud_facet$vars())
        return(list('equal' = equal, 'feedback' = ifelse(equal, "", paste0("Did you make a multipanel plot with rows or columns at least divided by (", paste0(sol_facet$vars(), collapse = ", "), ")?")))) 
    } else {
        if (sol_type == "FacetWrap") {
            if (stud_type  == "FacetWrap") {
                equal <- setequal(sol_facet$vars(), stud_facet$vars())
                return(list('equal' = equal, 'feedback' = ifelse(equal, "", paste0("Did you make a multipanel plot with rows or columns at least divided by (", paste0(sol_facet$vars(), collapse = ", "), ")?")))) 
            } else {
                return(list('equal' = FALSE, `feedback` = "Did you use the facet_wrap layer?"))
            }
        } else {
            if (stud_type  == "FacetGrid") {
                
                sol_rows <- names(sol_facet$params$rows)
                sol_cols <- names(sol_facet$params$cols)
                
                stud_rows <- names(stud_facet$params$rows)
                stud_cols <- names(stud_facet$params$cols)
                
                #Cols and rows can be interchanged
                equal <- (setequal(sol_rows, stud_rows) && setequal(sol_cols, stud_cols)) || (setequal(sol_rows, stud_cols) && setequal(sol_cols, stud_rows))  
                return(list('equal' = equal, 'feedback' = ifelse(equal, "", paste0("Did you make a multipanel plot with rows or columns at least divided by (", paste0(sol_facet$vars(), collapse = ", "), ")?")))) 
            } else {
                return(list('equal' = FALSE, `feedback` = "Did you use the facet_grid layer?"))
            }
        }
    } 
}

test_geom_layer <- function(sol_gg, stud_gg){
    sol_layers <- sol_gg$layers
    stud_layers <- stud_gg$layers
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

        for (j in seq_along(stud_layers)) {
            stud_layer <- stud_layers[[j]]

            stud_geom_type <- get_type(stud_layer$geom)
            if (sol_geom_type == stud_geom_type) {

                # Testing aes for every layer, we will check if every aes component of a layer in the solution-plot is
                # also specified in the corresponding layer of the student-plot or in the aes of the base ggplot function
                mapping_keys <- unique(c(names(sol_layer$mapping), names(sol_gg$mapping)))
                for(key in mapping_keys){

                    stud_map_component <- stud_layer$mapping[key]
                    if (is.null(stud_map_component[[1]])){
                        stud_map_component <- stud_gg$mapping[key]
                        if (is.null(stud_map_component[[1]])){
                            return(list('equal' = FALSE, 
                                        'feedback' = paste0("Have you specified the `", key, "` aesthetic needed in the `", as.character(sol_geom_type), "` layer?"))
                            )
                        }
                    }

                    sol_map_component <- sol_layer$mapping[key]
                    if (is.null(sol_map_component[[1]])){
                        sol_map_component <- sol_gg$mapping[key]
                    }

                    if(!isTRUE(all.equal(stud_map_component, sol_map_component, check.attributes = FALSE))){
                        return(list('equal' = FALSE, 
                                    'feedback' = paste0("Found `", stud_map_component, "` for the `", key, "` aesthetic. Expected `", sol_map_component, "`."))
                        )
                    }
                }

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
    aes_params <- geom_layer$aes_params
    params[names(aes_params)] <- aes_params
    return(params)
}

get_type <- function(object) {
    return(sub("^_", "", tolower(gsub("([A-Z])", "_\\1", class(object)[1]))))
}
