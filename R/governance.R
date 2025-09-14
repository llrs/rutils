#' Add the governance
#'
#' To make clear expectations and how people can join projects state how their
#' contributors can lead to higher rights (and responsibilities).
#' @param path Path to the directory where the file will be added.
#' @returns A logical value if the file has been added.
#' @export
#' @references \url{https://antonin.delpeuch.eu/posts/off-the-shelf-governance-models-for-small-foss-projects/}
#' @examples
#' \dontrun{
#' llrs_use_governance()
#' }
llrs_use_governance <- function(path = ".") {
  file_name <- "governance.md"
  governance <- system.file(file_name, package = "rutils")
  path_out <- file.path(path, "governance.md")
  if (dir.exists(path_out)) {
    stop("There is a directory with this governance.md name")
  }
  out <- file.copy(governance, to = ".", overwrite = file.exists(path_out))
  add2buildignore(file_name, path = path)
  if (isTRUE(out)) {
    message("Added governance.md")
  }
  add2buildignore(file_name, path = path)
  out
}


#' Add a contributor to the registry
#'
#' @param name Character name of the contributor.
#' @param role Either Collaborator or Publisher.
#' @param path Path to the project.
#'
#' @returns `NULL`
#' @export
#' @examples
#' \dontrun{
#' llrs_use_governance()
#' llrs_add_member(NULL)
#' }
llrs_add_member <- function(name = NULL, role = "Collaborator", path = ".") {
  role <- match.arg(role, c("Collaborator", "Publisher"))
  if (!file.exists(file.path(path, "governance.md"))) {
    warning("No governance file detected")
    llrs_use_governance(path)
  }
  if (is.null(name) && !is.null(usethis_name <- getOption("usethis.full_name"))) {
    name <- usethis_name
  } else if(is.null(name)) {
    stop("Missing name of contributor.", call. = TRUE)
  }

  file_name <- "members.csv"
  path_file <- file.path(path, file_name)
  out <- data.frame(Name = name,
                    Role = role,
                    `Date added` = as.character(Sys.Date()),
                    `Date removed` = "",
                    check.names = FALSE)
  if (file.exists(path_file)) {
    write.table(out,
                sep = ",",
                file = path_file,
                row.names = FALSE,
                append = TRUE, col.names = FALSE
                )
  } else {
    add2buildignore(file_name, path = path)
    write.table(out,
                sep = ",",
                file = path_file,
                append = FALSE,
                row.names = FALSE
                )
  }
}

#' Remove role of a contributor
#'
#' Change a contributor role on the project.
#' @inheritParams llrs_add_member
#' @param role Either User, Collaborator or Publisher.
#'
#' @returns `NULL`
#' @export
#' @importFrom utils read.csv write.table
#' @examples
#' \dontrun{
#' llrs_use_governance()
#' llrs_add_member(NULL)
#' llrs_change_member(NULL, "Publisher")
#' }
llrs_change_role <- function(name = NULL, role = "User", path = ".") {
  role <- match.arg(role, c("User", "Collaborator", "Publisher"))
  file_name <- "members.csv"
  path_file <- file.path(path, file_name)
  if (!file.exists(path_file)) {
    stop("No members.csv exists at ", sQuote(path), call. = FALSE)
  }
  members <- read.csv(file = path_file,
                      row.names = NULL, header = TRUE,
                      check.names = FALSE)

  if (is.null(name) && !is.null(usethis_name <- getOption("usethis.full_name"))) {
    name <- usethis_name
  } else if(is.null(name)) {
    stop("Missing name of contributor.", call. = TRUE)
  }

  if (!name %in% members$Name) {
    stop(sQuote(name),  " wasn't found on the list of members")
  }

  w <- which(members$Name %in% name)
  current_role <- members[w[length(w)], 2]
  date_removed <- members[w[length(w)], 4]
  current_date <- as.character(Sys.Date())

  if (role  == "User") {
    if (!nzchar(date_removed)) {
      members[w[length(w)], 4] <- current_date
    } else {
      stop("Contributor is present but not with an active role.", call. = FALSE)
    }
    write.csv(members, file = path_file, row.names = FALSE)
    return(invisible(NULL))
  }

  if (current_role == role) {
    out <- askYesNo(paste0("Collaborator ", sQuote(name), " is already ", role,
                    ".\nDid you meant a differnet role?"))
    if (!isTRUE(out)) {
      return(invisible(NULL))
    }
  }
  # Add remove date
  if (!nzchar(date_removed)) {
    members[w[length(w)], 4] <- current_date
  } else {
    stop("Contributor is present but not with an active role.", call. = FALSE)
  }

  # Add new line to new role with current date
  members <- rbind(members,
                   data.frame(Name = name, Role = role,
                              "Date added" = current_date,
                              "Date removed" = "", check.names = FALSE))
  write.csv(members, file = path_file, row.names = FALSE)
  return(invisible(NULL))
}


