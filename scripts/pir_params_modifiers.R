# Library to operate on pir_params

set_right_path <- function(
  filename,
  root_folder = file.path("home", "p274829", "pirouette", "test")
) {
  file.path(root_folder, basename(filename))
}

is_non_beast_path <- function(variable) {
  out <- FALSE
  if (is.character(variable)) {
    if (
      dirname(variable) != "." &&
      dirname(variable) != "" &&
      !grepl(pattern = "BEAST", x = variable) &&
      !grepl(pattern = "beast", x = variable)
      ) {
      out <- TRUE
    }
  }
  out
}

print_no_change <- function(x) {
  y <- set_right_path(x); print(x); print(y); return(x)
}

print_and_change <- function(x) {
  y <- set_right_path(x); print(y); return(y)
}

modify_pir_params <- function(
  pir_params,
  function_to_detect = is_non_beast_path,
  function_to_apply = print_no_change
) {
  for (i in 1:length(pir_params)) {
    if (is.list(x <- pir_params[[i]])) {
      for (j in 1:length(pir_params[[i]])) {
        if (is.list(x <- pir_params[[i]][[j]])) {
          for (k in 1:length(pir_params[[i]][[j]])) {
            if (is.list(x <- pir_params[[i]][[j]][[k]])) {
              for (l in 1:length(pir_params[[i]][[j]][[k]])) {
                if (is.list(x <- pir_params[[i]][[j]][[k]][[l]])) {
                  for (m in 1:length(pir_params[[i]][[j]][[k]][[l]])) {
                    if (is.list(x <- pir_params[[i]][[j]][[k]][[l]][[m]])) {
                      for (n in 1:length(pir_params[[i]][[j]][[k]][[l]][[m]])) {
                        x <- pir_params[[i]][[j]][[k]][[l]][[m]][[n]]
                        if (function_to_detect(x)) {
                          pir_params[[i]][[j]][[k]][[l]][[m]][[n]] <- function_to_apply(x)
                        }
                      }
                    } else {
                      if (function_to_detect(x)) {
                        pir_params[[i]][[j]][[k]][[l]][[m]] <- function_to_apply(x)
                      }
                    }
                  }
                } else {
                  if (function_to_detect(x)) {
                    pir_params[[i]][[j]][[k]][[l]] <- function_to_apply(x)
                  }
                }
              }
            } else {
              if (function_to_detect(x)) {
                pir_params[[i]][[j]][[k]] <- function_to_apply(x)
              }
            }
          }
        } else {
          if (function_to_detect(x)) {
            pir_params[[i]][[j]] <- function_to_apply(x)
          }
        }
      }
    } else {
      if (function_to_detect(x)) {
        pir_params[[i]] <- function_to_apply(x)
      }
    }
  }
  pir_params
}
# test1 <- modify_pir_params(pir_params, function_to_apply = print_no_change)
# test2 <- modify_pir_params(pir_params, function_to_apply = print_and_change)
