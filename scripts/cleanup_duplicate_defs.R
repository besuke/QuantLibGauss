targets <- list(
  "R/curve_builder.R" = c(
    "qlg_yield_curve_handle"
  ),
  "R/swap_analytics.R" = c(
    "qlg_swap_cashflow_schedule",
    "qlg_ois_cashflow_schedule",
    "qlg_apply_discount_factors",
    "qlg_apply_fixings",
    "qlg_cashflow_leg_summary"
  ),
  "R/swap_factory.R" = c(
    "qlg_trade_value",
    "qlg_trade_period",
    "qlg_make_ois_from_trade"
  )
)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
backup_dir <- file.path("backup", paste0("before_duplicate_cleanup_", timestamp))
dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)

read_file <- function(path) {
  readLines(path, warn = FALSE, encoding = "UTF-8")
}

write_file <- function(path, lines) {
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(lines, con, useBytes = TRUE)
}

find_function_starts <- function(lines, fun) {
  rx <- paste0("^\\s*", fun, "\\s*<-\\s*function\\s*\\(")
  which(grepl(rx, lines))
}

find_roxygen_start <- function(lines, start_line) {
  i <- start_line - 1
  while (i >= 1 && grepl("^\\s*#'", lines[[i]])) {
    i <- i - 1
  }
  i + 1
}

find_function_end <- function(lines, start_line) {
  for (end_line in seq(start_line, length(lines))) {
    candidate <- paste(lines[start_line:end_line], collapse = "\n")
    ok <- tryCatch({
      parse(text = candidate)
      TRUE
    }, error = function(e) FALSE)

    if (ok) {
      return(end_line)
    }
  }

  stop("Could not find function end from line ", start_line)
}

remove_range <- function(lines, from, to) {
  if (from <= 1 && to >= length(lines)) {
    return(character())
  }

  lines[-seq(from, to)]
}

for (path in names(targets)) {
  if (!file.exists(path)) {
    warning("File not found: ", path)
    next
  }

  backup_path <- file.path(backup_dir, basename(path))
  file.copy(path, backup_path, overwrite = TRUE)

  lines <- read_file(path)
  changed <- FALSE

  for (fun in targets[[path]]) {
    starts <- find_function_starts(lines, fun)

    if (length(starts) <= 1) {
      cat("OK:", path, fun, "has", length(starts), "definition(s).\n")
      next
    }

    # Keep the last definition because that is what R currently uses.
    remove_starts <- starts[-length(starts)]

    # Remove from bottom to top so line numbers remain valid.
    for (s in rev(remove_starts)) {
      from <- find_roxygen_start(lines, s)
      to <- find_function_end(lines, s)

      cat("REMOVE:", path, fun, "lines", from, "-", to, "\n")
      lines <- remove_range(lines, from, to)
      changed <- TRUE
    }
  }

  if (changed) {
    write_file(path, lines)
    cat("UPDATED:", path, "\n")
  } else {
    cat("UNCHANGED:", path, "\n")
  }
}

cat("\nBackup created at: ", backup_dir, "\n", sep = "")