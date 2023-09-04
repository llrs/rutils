# Add a git hook that checks if the pkgdown website can be built before pushing!
#

#' Insert hook
#'
#' Insert a new hook (It doesn't replace hooks or expand them).
#'
#' @param file Name of the file to be used as a hook
#' @param path Path to the git repository
#'
#' @return A logical value if the hook can be placed there.
#' @references <https://git-scm.com/docs/githooks>
#' @seealso [llrs_hooks_list()]
#' @export
#' @examples
#' \dontrun{
#' hook <- system.file("post-receive", package = "rutils")
#' llrs_hook(hook, "projects/git_something")
#' }
llrs_hook <- function(file, path = getwd()) {
  if (!check_git(path)) {
    stop("Doesn't seem to be a git repository!")
  }
  stopifnot("Only one hook at the time is allowed" = length(file) == 1)
  hook_names(file)
  hooks_path <- file.path(path, ".git/hooks")
  dir.create(hooks_path, recursive = TRUE, showWarnings = FALSE)
  out <- file.copy(file, hooks_path)
  if (isFALSE(out)) {
    out
  }
  Sys.chmod(hooks_path, mode = "0777")
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
#' @return A character vector with the name of the hooks.
#' @export
#' @seealso [llrs_hook()]
#' @examples
#' llrs_hooks_list()
llrs_hooks_list <- function() {
  lf <- list.files("inst", full.names = FALSE)
  lf[lf %in% c(hooks_client, hooks_server)]
}
