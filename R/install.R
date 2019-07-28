#' Install and uninstall support for remembr by appending / removing one line of code to the
#' \file{~/.Rprofile} file.
#'
#' Note this code is largely borrowed from https://github.com/HenrikBengtsson/startup/tree/master/R
#'
#' @param path The path where to create / update the \file{.Rprofile} file.
#'
#' @param backup If `TRUE`, a timestamped backup copy of the original file is
#' created before modifying / overwriting it, otherwise not.  If the backup
#' fails, then an error is produced and the R startup file is unmodified.
#'
#' @param overwrite If the R startup file already exist, then `FALSE` (default)
#' appends the startup code to the end of the file. is overwritten.  If `TRUE`,
#' any pre-existing R startup file is overwritten.
#'
#' @param quiet If `FALSE` (default), detailed messages are generated,
#' otherwise not.
#'
#' @return The pathname of the R startup file modified.
#'
#' @describeIn install injects a `try(remembr::initRemembr())` call to the
#' \file{.Rprofile} file (created if missing).
#'
#'
#' @export
install <- function(path = "~", backup = TRUE, overwrite = FALSE,
                    quiet = FALSE) {
  notef = message
  if (quiet) notef <- function(...) NULL

  file <- file.path(path, ".Rprofile")
  if (is_installed(file)) {
    msg <- sprintf("remembr::initRemembr() already installed: %s", sQuote(file))
    notef(msg)
    warning(msg)
    return(file)
  }


  file_exists <- file.exists(file)
  if (backup && file_exists) backup(file, quiet = quiet)

  code <- "try(remembr::initRemembr())\n"

  ## If the .Rprofile file does not have a newline at the end, which is
  ## a mistake, make sure that the appended startup code is on its own line
  if (file_exists && !eof_ok(file)) code <- paste("\n", code, sep = "")

  cat(code, file = file, append = !overwrite)
  if (file_exists) {
    notef("%s 'try(remembr::initRemembr())' to already existing R startup file: %s",
          if (overwrite) "Appended" else "Added", sQuote(file))
  } else {
    notef("Created new R startup file with 'try(remembr::initRemembr())': %s",
          sQuote(file))
  }

  file
}


#' @describeIn install Remove calls to `startup::startup()` and similar.
#' @export
uninstall <- function(path = "~", backup = TRUE, quiet = FALSE) {
  if (quiet) notef <- function(...) NULL

  file <- file.path(path, ".Rprofile")
  if (!is_installed(file)) {
    msg <- sprintf("remember::initRemembr() not installed: %s", sQuote(file))
    notef(msg)
    warning(msg)
    return(file)
  }

  bfr <- readLines(file, warn = FALSE)
  pattern <- "remembr::initRemembr[(].*[)]"
  bfr2 <- grep(pattern, bfr, value = TRUE, invert = TRUE)
  ## Nothing to do?
  if (isTRUE(all.equal(bfr2, bfr))) {
    msg <- sprintf("remembr::initRemembr() not installed: %s", sQuote(file))
    notef(msg)
    warning(msg)
    return(file)
  }
  if (backup) backup(file, quiet = quiet)
  writeLines(bfr2, con = file)
  notef("R startup file updated: %s", sQuote(file))

  file
}


is_installed <- function(file = file.path("~", ".Rprofile")) {
  if (!file.exists(file)) return(FALSE)
  bfr <- readLines(file, warn = FALSE)
  bfr <- gsub("#.*", "", bfr)
  pattern <- "remembr::initRemembr[(].*[)]"
  res <- any(grepl(pattern, bfr))
  attr(res, "file") <- file
  res
}

backup <- function(file, quiet = FALSE) {
  if (quiet) notef <- function(...) NULL

  stop_if_not(file.exists(file))
  size <- file.size(file)

  timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  backup_file <- sprintf("%s.bak.%s", file, timestamp)
  ## Was another backup file created just before during the same second?
  if (file.exists(backup_file)) {
    timestamp <- format(Sys.time(), "%Y%m%d-%H%M%OS3")
    backup_file <- sprintf("%s.bak.%s", file, timestamp)
  }
  stop_if_not(!file.exists(backup_file))
  res <- file.copy(file, backup_file, overwrite = FALSE)

  backup_size <- file.size(backup_file)
  notef("Backed up R startup file: %s (%d bytes) -> %s (%d bytes)",
        sQuote(file), size, sQuote(backup_file), backup_size)
  stop_if_not(file.exists(backup_file), identical(backup_size, size), res)

  backup_file
}


stop_if_not <- function(...) {
  res <- list(...)
  n <- length(res)
  if (n == 0L) return()

  for (ii in 1L:n) {
    res_ii <- .subset2(res, ii)
    if (length(res_ii) != 1L || is.na(res_ii) || !res_ii) {
        mc <- match.call()
        call <- deparse(mc[[ii + 1]], width.cutoff = 60L)
        if (length(call) > 1L) call <- paste(call[1L], "...")
        stop(sQuote(call), " is not TRUE", call. = FALSE, domain = NA)
    }
  }
}

