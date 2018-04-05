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


#' gSD.get
#' @param q.url query url
#' @param q.user query user
#' @param q.pass query pass
#' @importFrom httr GET stop_for_status warn_for_status message_for_status progress
#' @keywords internal
#' @noRd
gSD.get <- function(q.url, q.user, q.pass, dir.file = NULL, prog = F){
  if(is.null(dir.file)){
    x <- GET(q.url, authenticate(q.user, q.pass))
  } else{
    if(is.FALSE(prog)) x <- GET(q.url, authenticate(q.user, q.pass), write_disk((dir.file)))
    if(is.TRUE(prog)) x <- GET(q.url, authenticate(q.user, q.pass), progress(), write_disk((dir.file)))
  }
  stop_for_status(x, "connect to Copernicus Open Access API. Please retry.")
  warn_for_status(x)
  #message_for_status(x); cat("\n")
  return(x)
}



#' get API url from user input
#'
#' @param x API keyword or URL
#' @param p platform
#' @param user user name
#' @param pw password
#' @keywords internal
#' @noRd
access_API <- function(x, p, user, pw){
  if(x == "auto"){
    if(p == "Sentinel-1" | p == "Sentinel-2"){x <- "operational"
    }else{x <- "pre-ops"}
  }
  if(x == "operational"){x <- 'https://scihub.copernicus.eu/dhus'}
  if(x == "pre-ops"){
    x <- 'https://scihub.copernicus.eu/s3'
    user <- "s3guest"
    pw <- "s3guest"
  }
  return(c(user, pw, x))
}


#' On package startup
#' @keywords internal
#' @noRd
.onLoad <- function(libname, pkgname){

  op <- options()
  op.gSD <- list(
    gSD.cophub_user = FALSE,
    gSD.cophub_pass = FALSE,
    gSD.cophub_set = FALSE,
    gSD.archive = FALSE,
    gSD.archive_set = FALSE
  )
  toset <- !(names(op.gSD) %in% names(op))
  if(any(toset)) options(op.gSD[toset])

  invisible()
}

