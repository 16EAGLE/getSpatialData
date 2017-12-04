#' Outputs errors, warnings and messages
#'
#' @param input character
#' @param type numeric, 1 = message/cat, 2 = warning, 3 = error and stop
#' @param msg logical. If \code{TRUE}, \code{message} is used instead of \code{cat}. Default is \code{FALSE}.
#' @param sign character. Defines the prefix string.
#'
#' @keywords internal
#' @noRd

out <- function(input,type = 1, ll = 1, msg = FALSE, sign = ""){
  if(type == 2 & ll <= 2){warning(paste0(sign,input), call. = FALSE, immediate. = TRUE)}
  else{if(type == 3){stop(input,call. = FALSE)}else{if(ll == 1){
    if(msg == FALSE){cat(paste0(sign,input),sep="\n")
    }else{message(paste0(sign,input))}}}}
}


#' Loads and/or installs python libraries using pip
#'
#' @param lib character, library name
#' @param get.auto logical, activates or deactivates automatic download, if not available
#' @param install.only logical, if \code{TRUE}, library will be installed but not loaded.
#'
#' @keywords internal
#' @noRd

py_load <- function(lib, get.auto = TRUE, install.only = FALSE){ #returns list of imports
  if(class(lib) != "character"){out("'lib' has to be a 'character' vector.", type=3)}
  imports <- lapply(lib, function(x){
    out(paste0("Loading package '",x,"'..."),type=1)
    from <- F
    if(length(grep("[$]",x)) == 1){
      y <- unlist(strsplit(x, "[$]"))[2]
      x <- unlist(strsplit(x, "[$]"))[1]
      from <- T
    }
    lib.try <- try(reticulate::import(x), silent = TRUE)
    if(class(lib.try)[1] == "try-error"){
      if(get.auto == TRUE){
        system(paste0("pip install ",x))
        re <- reticulate::import(x)
      }else{
        out(paste0("Module '",x,"' is not installed. Auto-install is not available. Please install modules."),type=3)
      }
    }else{re <- lib.try}
    if(from){
      g <- parse(text = paste0("re$",y))
      g <- list(eval(g)); names(g) <- y
    }else{
      g <- list(re); names(g) <- x
    }
    return(g)
  })
  if(length(imports) == 1){imports <- imports[[1]]
  }else{imports <- unlist(imports)}
  if(install.only == TRUE){return(TRUE)}else{return(imports)}
}


#' Simplifies check of variables being FALSE
#'
#' @param evaluate variable or expression to be evaluated
#'
#' @keywords internal
#' @noRd
is.FALSE <- function(evaluate){if(evaluate == FALSE){return(TRUE)}else{return(FALSE)}}


#' Simplifies check of variables being TRUE
#'
#' @param evaluate variable or expression to be evaluated
#'
#' @keywords internal
#' @noRd
is.TRUE <- function(evaluate){if(evaluate == TRUE){return(TRUE)}else{return(FALSE)}}


#' Checks, if specific command is available
#'
#' @param cmd command
#'
#' @keywords internal
#' @noRd
check.cmd <- function(cmd){
  sc <- try(devtools::system_check(cmd, quiet = TRUE),silent = TRUE)
  if(class(sc) == "try-error"){return(FALSE)}else{return(TRUE)}
}


#' On package startup
#' @noRd
.onLoad <- function(libname, pkgname){
  sat <<- reticulate::import("sentinelsat", delay_load = TRUE)
}
