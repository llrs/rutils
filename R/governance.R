llrs_use_governance <- function(path = ".") {
  file_name <- "governance.md"
  governance <- system.file("inst", file_name, package = "rutils")
  path_out <- file.path(path, "governance.md")
  if (dir.exists(path_out)) {
    stop("There is a directory with this governance.md name")
  }
  out <- file.copy(governance, to = ".", overwrite = file.exists(path_out))
  add2buildignore(file_name, path = path)
  if (isTRUE(out)) {
    message("Added governance.md")
  }
  out
}


llrs_add_member <- function(name, role = "Collaborator", path = ".") {
  role <- match.arg(role, c("Collaborator", "Publisher"))
  file_name <- "members.csv"
  out <- data.frame(Name = name,
                    Role = role,
                    `Date added` = as.character(Sys.Date()),
                    `Date removed` = "",
                    check.names = FALSE)
  write.csv(out, append = TRUE, row.names = FALSE)
}

llrs_change_role <- function(name, role = "User", path = ".") {
  role <- match.arg(role, c("User", "Collaborator", "Publisher"))
  file_name <- "members.csv"
  path_file <- file.path(path, file_name)
  members <- read.csv(file = path_file, row.names = NULL, header = TRUE)
  if (!name %in% members$Name) {
    stop(name,  " wasn't found on the list of members")
  }

  write.csv(out)
}
