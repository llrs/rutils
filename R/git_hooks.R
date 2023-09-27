# Add a git hook that checks if the pkgdown website can be built before pushing!
#

#' Insert hook
#'
#' Insert a new hook (It doesn't replace hooks or expand them).
#'
#' @param file Name of the file to be used as a hook.
#' @param path Path to the git repository.
#'
#' @return A logical value if the hook can be placed there.
#' @references <https://git-scm.com/docs/githooks>
#' @seealso [llrs_hooks_list()]
#' @export
#' @examples
#' \dontrun{
#' hooks <- llrs_hooks_list()
#' llrs_hook(hooks[1], "projects/git_something")
#' }
llrs_hook <- function(file, path = getwd()) {
  stopifnot(length(file) == 1 && is.character(file))
  if (!check_git(path)) {
    stop("Doesn't seem to be a git repository!")
  }
  stopifnot("Only one hook at the time is allowed" = length(file) == 1)
  if (!file.exists(file)) {
    message("Using internal hooks")
    hooks <- llrs_hooks_list(TRUE)
    file <- hooks[which(basename(hooks) == file)]
  }

  hooks_path <- file.path(path, ".git/hooks")
  dir.create(hooks_path, recursive = TRUE, showWarnings = FALSE)
  out <- file.copy(file, hooks_path)
  if (isFALSE(out)) {
    out
  }
  Sys.chmod(file.path(hooks_path, basename(file)), mode = "777")
  invisible(out)
}


check_git <- function(path) {
  dir.exists(file.path(path, ".git"))
}

hooks_client <- c("pre-commit", "repare-commit-msg", "commit-msg", "post-commit",
                  "pre-applypatch", "applypatch-msg", "post-applypatch",
                  "pre-rebase", "post-rewrite",
                  "pre-push", "post-checkout",
                  "post-merge",
                  "pre-auto-gc")
hooks_server <- c("pre-receive", "update", "proc-receive", "post-receive", "post-update",
                  "reference-transaction", "push-to-checkout",
                  "sendemail-validate", "fsmonitor-watchman",
                  "p4-changelist", "p4-prepare-changelist", "p4-post-changelist", "p4-pre-submit",
                  "post-index-change")

hook_names <- function(path) {
  file <- basename(path)

  if (file %in% hooks_client) {
    message("Setting a client hook.")
  } else if (file %in% hooks_server) {
    message("Setting a server hook.")
  } else {
    stop("Hook name ", sQuote(file)," not recognized!")
  }
  TRUE
}

#' List existing hooks
#'
#' Check which hooks are in the package.
#' @param full.names A logical value to be passed to list.files
#' @return A character vector with the name of the hooks.
#' @export
#' @seealso [llrs_hook()]
#' @examples
#' llrs_hooks_list()
llrs_hooks_list <- function(full.names = FALSE) {
  stopifnot(isFALSE(full.names) || isTRUE(full.names))
  lf <- list.files(system.file("hooks", package = "rutils", mustWork = TRUE),
                   full.names = full.names)
  lf[basename(lf) %in% c(hooks_client, hooks_server)]
}
