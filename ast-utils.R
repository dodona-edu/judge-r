is_function_used_in_var <- function(func_name, var_paths, var, main_code) {
  var_paths_c <- var_paths
  var_paths_c[[var]] <- NULL
  any(
    lapply(var_paths[[var]], 
           function(x) is_function_used(func_name, var_paths_c, get_node(x, main_code), main_code)
    ) == TRUE
  )
}

is_function_used <- function(func_name, var_paths, sub_tree_root, main_code){
    switch(expr_type(sub_tree_root),
      constant = {
        FALSE
      },
      symbol = {
        var_paths_c <- var_paths
        var_paths_c[[sub_tree_root]] <- NULL
        any(
          lapply(var_paths[[sub_tree_root]], 
                 function(x) is_function_used(func_name, var_paths_c, get_node(x, main_code), main_code)
          ) == TRUE
        )
      },
      call = {
        if (is_call(sub_tree_root, func_name)) {
          return(TRUE)
        } 
        children <- as.list(sub_tree_root)
        if(is_call(sub_tree_root, "<-")){
          children[[2]] <- NULL
        } 
        any(
          lapply(children, 
                 function(x) is_function_used(func_name, var_paths, x, main_code)
          ) == TRUE
        )
      },
      expression = {
          any(
          lapply(as.list(sub_tree_root), 
                 function(x) is_function_used(func_name, var_paths_c, x, main_code)
          ) == TRUE
        )
      },
      pairlist = {
        any(
          lapply(as.list(sub_tree_root), 
                 function(x) is_function_used(func_name, var_paths_c, x, main_code)
          ) == TRUE
        )
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
  keys <- unique(c(names(list1), names(list2)))
  for (key in keys){
    for (index in c(1:length(list2[[key]]))){
      list1[[key]][[length(list1[[key]])+1]] <- list2[[key]][[index]]
      list1[[key]] <- unique(list1[[key]])
    }
  }
  list1
}
concat_lists1 <- function(lists){
  result = lists[1]
  for(index in 2:length(lists)){
    result = concat_lists(result, lists[index])
  }
  result
}


find_assign <- function(x, start_path=c()) {
  assignation_paths <- list()
  switch(expr_type(x),
         constant = {
           list()
         },
         symbol = {
           list()
         },
         call = {
           if (is_call(x, "<-") && is_symbol(x[[2]])) {
             lhs <- as_string(x[[2]])
             assignation_paths[[lhs]] <- list(start_path)
           }
           res <- c(assignation_paths)
           for(child_index in seq_along(x)){
             path <- c(start_path, child_index)
             res <- c(res, find_assign(x[[child_index]], path))
           }
           return(concat_lists1(res))
         },
         pairlist = {
           res <- c()
           for (index in seq_along(x)){
             path <- c(start_path, index)
             res <- c(res, find_assign(x[[index]], path))
           }
           return(concat_lists1(res))
         },
         expression = {
            res <- c()
            for (index in seq_along(x)){
                path <- c(start_path, index)
                res <- c(res, find_assign(x[[index]], path))
            }
            return(concat_lists1(res))
         },
         stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  )
}
