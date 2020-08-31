# These functions were adapted from the check_ggplot module of the testwhat package in the Datacamp repository

test_data_layer <- function(sol_data, stud_data, data_fail_msg = NULL) {
  if (!is.null(data_fail_msg)) {
    feedback_msg <- data_fail_msg
  } else {
    feedback_msg <- "You didn't get the data layer right."
  }
  list('equal' = isTRUE(all.equal(sol_data, stud_data, check.attributes = FALSE)), 'feedback' = feedback_msg)
}

test_aes_layer <- function(sol_mapping, stud_mapping, aes_fail_msg = NULL) {
  for (map in names(sol_mapping)) {
    if (!is.null(aes_fail_msg)) {
      feedback_msg <- rep_len(aes_fail_msg, 3)
    } else {
      feedback_msg <- c(paste0("Have you mapped something on the `", map, "` aesthetic?"),
                        paste0("Have you mapped `", sol_mapping[map] ,"` on the `", map, "` aesthetic? Instead, you got `", stud_mapping[map], "`."),
                        paste0("Have you mapped exactly what is asked on the aesthetics layer, no more and no less?"))
    }
    
    if (is.null(stud_mapping[map][[1]])){
        return(list('equal' = FALSE, 'feedback' = feedback_msg[1]))
    }
    if(!isTRUE(all.equal(stud_mapping[map], sol_mapping[map], check.attributes = FALSE))){
        return(list('equal' = FALSE, 'feedback' = feedback_msg[2]))
    }
  }
  return(list('equal' = TRUE, 'feedback' = ""))
}

#' @importFrom stats na.omit
test_geom_layer <- function(sol_layers, stud_layers, geom_fail_msg = NULL, exact_geom = FALSE, check_geom_params = NULL) {
  nb_sol_layers <- length(sol_layers)
  
  exact_geom <- rep_len(exact_geom, nb_sol_layers)
  
  if (!(nb_sol_layers > 0)) {
    return(list('equal' = TRUE, 'feedback' = ""))
  }
  
  if (!is.null(geom_fail_msg)) {
    geom_fail_msg <- rep_len(geom_fail_msg, 5)
  }
  
  for (i in 1:nb_sol_layers) {
    sol_layer <- sol_layers[[i]]
    
    found_geom_name <- FALSE
    found_geom_with_params <- FALSE
    found_geom_with_exact_params <- FALSE
    found_geom_with_correct_position <- FALSE
    
    sol_params <- get_geom_params(sol_layer)
    if (!is.null(check_geom_params)) {
      sol_params <- sol_params[check_geom_params]
      sol_params <- sol_params[na.omit(names(sol_params))]
    }
    
    sol_position <- extract_type_from_object(sol_layer$position)
    sol_geom_type <- extract_type_from_object(sol_layer$geom)
    
    nb_stud_layers <- length(stud_layers)
    if (nb_stud_layers > 0) {
      for (j in 1:nb_stud_layers) {
        stud_layer <- stud_layers[[j]]
        
        sol_geom_type <- extract_type_from_object(sol_layer$geom)
        stud_geom_type <- extract_type_from_object(stud_layer$geom)
        if (sol_geom_type == stud_geom_type) {
          found_geom_name <- TRUE
          found_params <- TRUE
          
          stud_params <- get_geom_params(stud_layer)
          if (!is.null(check_geom_params)) {
            stud_params <- stud_params[check_geom_params]
            stud_params <- stud_params[na.omit(names(stud_params))]
          }
          
          stud_position <- extract_type_from_object(stud_layer$position)
          
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
          
          if (found_geom_with_params && (!exact_geom[i] || length(sol_params) == length(stud_params))) {
            found_geom_with_exact_params <- TRUE
          }
          
          if (found_geom_with_exact_params && compare_positions(sol_layer, stud_layer)) {
            found_geom_with_correct_position <- TRUE
          }
          
          if (found_geom_with_correct_position) {
            stud_layers[[j]] <- NULL
            break
          }
        }
        
      }
    }
    
    if (!is.null(geom_fail_msg)) {
      feedback_msg <- geom_fail_msg
    } else {
      geom_base_feedback <- paste0("Have you correctly added a `", as.character(sol_geom_type),"` layer")
      if (!is.null(check_geom_params)) {
        filtered_geom_params <- names(sol_params)
      } else {
        filtered_geom_params <- names(filter_standard_geom_params(as.character(sol_geom_type), sol_params))
      }
      param_strings <- vapply(filtered_geom_params, 
                              function(x) {
                                gen_fb <- ""
                                if (isTRUE(attr(sol_params[[x]], "aes"))) {
                                  attr(sol_params[[x]], "aes") <- NULL
                                  gen_fb <- "aesthetic "
                                }
                                return(paste0(gen_fb,"`", x, "` set to `", paste0(deparse(sol_params[[x]]), collapse = " "), "`"))
                              }, character(1))
      nb_param_strings <- length(param_strings)
      if (nb_param_strings > 1) {
        param_feedback <- paste0(paste(param_strings[1:(nb_param_strings - 1)], collapse = ", "), " and ", param_strings[nb_param_strings])
      } else {
        param_feedback <- param_strings
      }
      feedback_msg <- c(paste0(geom_base_feedback, " with a `+` operator?"),
                        paste0(geom_base_feedback, " with ", param_feedback, "?"),
                        paste0(geom_base_feedback, " with ", param_feedback, "?", " It seems like you have defined too much attributes or aesthetics for this geom."),
                        paste0(geom_base_feedback, " with the `position` set correctly? Have another look at the instructions."))
      
    }
    
    if (!found_geom_name) {
        return(list('equal' = FALSE, 'feedback' = feedback_msg[1]))
    } 
    if (!found_geom_with_params) {
        return(list('equal' = FALSE, 'feedback' = feedback_msg[2]))
    } 
    if (!found_geom_with_exact_params) {
        return(list('equal' = FALSE, 'feedback' = feedback_msg[3]))
    } 
    if (!found_geom_with_correct_position) {
        return(list('equal' = FALSE, 'feedback' = feedback_msg[4]))
    } 
  }
  
  if (isTRUE(exact_geom)) {
    if (!is.null(geom_fail_msg)) {
      feedback_msg <- geom_fail_msg[5]
    } else {
      feedback_msg <- "Have you added only the geom layers that are asked for? Nothing more."
    }
    if (length(stud_layers) != 0) {
        return(list('equal' = FALSE, 'feedback' = feedback_msg))
    }
  }

  return(list('equal' = TRUE, 'feedback' = ""))
}

get_geom_params <- function(geom_layer) {
  params <- geom_layer$geom_params
  stat_params <- geom_layer$stat_params
  params[names(stat_params)] <- stat_params
  mapping_params <- lapply(geom_layer$mapping, function(x) structure(x, aes = TRUE))
  params[names(mapping_params)] <- mapping_params
  aes_params <- geom_layer$aes_params
  params[names(aes_params)] <- aes_params
  return(params)
}

extract_type_from_object <- function(object) {
  return(sub("^_", "", tolower(gsub("([A-Z])", "_\\1", class(object)[1]))))
}

compare_positions <- function(sol_layer, stud_layer) {
   sol_position <- sol_layer$position
   stud_position <- stud_layer$position

   return(isTRUE(all.equal(sol_position, stud_position, check.attributes = FALSE)))
}

filter_standard_geom_params <- function(geom_call, params) {
  standard_layer <- eval(call(geom_call))
  standard_params <- get_geom_params(standard_layer)
  ov <- base::intersect(names(params), names(standard_params))
  eq <- mapply(function(x,y) { isTRUE(all.equal(x,y, check.attributes = FALSE))}, standard_params[ov], params[ov])
  if (any(eq)) {
    params[names(eq[eq])] <- NULL
  } 
  return(params)
}