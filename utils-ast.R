is_function_used_in_var <- function(func_name, var_paths, var, main_code) {
    var_paths_c <- var_paths
    var_paths_c[[var]] <- NULL
    return(some(var_paths[[var]], function(x) is_function_used(func_name, var_paths_c, get_node(x, main_code), main_code)))
}

is_function_used <- function(func_name, var_paths, sub_tree_root, main_code) {
    if (isTRUE(all.equal(sub_tree_root, sym("")))) {
        return(FALSE)
    }
    switch(expr_type(sub_tree_root),
           constant = { return(FALSE) },
           symbol = {
               var_paths_c <- var_paths
               var_paths_c[[sub_tree_root]] <- NULL
               return(some(var_paths[[sub_tree_root]], function(x) is_function_used(func_name, var_paths_c, get_node(x, main_code), main_code)))
           },
           call = {
               if (is_call(sub_tree_root, func_name)) {
                   return(TRUE)
               }
               children <- as.list(sub_tree_root)
               # No need to check assignation branches of parameters before assignation symbol
               if((is_call(sub_tree_root, "<-") || is_call(sub_tree_root, "=")) && is_symbol(children[[2]])){
                   children[[2]] <- NULL
               }
               return(some(children, function(x) is_function_used(func_name, var_paths, x, main_code)))
           },
           expression = {
               return(some(sub_tree_root, function(x) is_function_used(func_name, var_paths, x, main_code)))
           },
           pairlist = {
               return(some(sub_tree_root, function(x) {
                               !(is.symbol(x) && identical("", as.character(x))) && is_function_used(func_name, var_paths, x, main_code)
}))
           },
           stop("Don't know how to handle type ", typeof(sub_tree_root), call. = FALSE)
    )
}

get_node <- function(path, code){
    element <- code
    for (direction in path){
        element <- as.list(element)[[direction]]
    }
    return(element)
}

expr_type <- function(x) {
    if (rlang::is_syntactic_literal(x)) {
        "constant"
    } else if (is.symbol(x)) {
        "symbol"
    } else if (is.call(x)) {
        "call"
    } else if (is.pairlist(x)) {
        "pairlist"
    } else if (is.expression(x)){
        "expression"
    } else {
        typeof(x)
    }
}

concat_lists <- function(list1, list2){
    if(identical(list1, NULL)){
        return(list2)
    }
    if(identical(list2, NULL)){
        return(list1)
    }
    keys <- unique(c(names(list1), names(list2)))
    for (key in keys){
        if(identical(list1[[key]], NULL)){
            list1[[key]] <- list()
        }
        for (index in seq_along(list2[[key]])){
            list1[[key]][[length(list1[[key]])+1]] <- list2[[key]][[index]]
            list1[[key]] <- unique(list1[[key]])
        }
    }
    return(list1)
}

find_assign <- function(x, start_path=c()) {
    switch(expr_type(x),
           constant = {
               return(NULL)
           },
           symbol = {
               return(NULL)
           },
           call = {
               assignation_path <- NULL
               if ((is_call(x, "<-") || is_call(x, "=")) && is_symbol(x[[2]])) {
                   assignation_path <- list()
                   assignation_path[[as_string(x[[2]])]] <- list(start_path)
               }
               return(Reduce(concat_lists, lapply(seq_along(x), function(index) find_assign(x[[index]], c(start_path, index))), assignation_path))
           },
           pairlist = {
               return(NULL)
           },
           expression = {
               return(Reduce(concat_lists, lapply(seq_along(x), function(index) find_assign(x[[index]], c(start_path, index))), NULL))
           },
           stop("Don't know how to handle type ", typeof(x), call. = FALSE)
    )
}
