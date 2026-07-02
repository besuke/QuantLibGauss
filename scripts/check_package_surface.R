files <- list.files("R", pattern = "\\.R$", full.names = TRUE)

read_file <- function(path) {
  readLines(path, warn = FALSE, encoding = "UTF-8")
}

extract_defs <- function(path) {
  x <- read_file(path)

  # Only detect real package-style assignments:
  # qlg_xxx <- function(...)
  # helper_name <- function(...)
  # Do not detect tryCatch(error = function(e) ...)
  rx <- "^\\s*([A-Za-z.][A-Za-z0-9._]*)\\s*<-\\s*function\\s*\\("

  out <- list()

  for (i in seq_along(x)) {
    m <- regexec(rx, x[[i]])
    r <- regmatches(x[[i]], m)[[1]]

    if (length(r) > 0) {
      j <- i - 1
      roxy <- character()

      while (j >= 1 && grepl("^\\s*#'", x[[j]])) {
        roxy <- c(x[[j]], roxy)
        j <- j - 1
      }

      out[[length(out) + 1]] <- data.frame(
        file = path,
        line = i,
        name = r[[2]],
        roxygen_export = any(grepl("@export", roxy, fixed = TRUE)),
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(out) == 0) {
    data.frame(
      file = character(),
      line = integer(),
      name = character(),
      roxygen_export = logical(),
      stringsAsFactors = FALSE
    )
  } else {
    do.call(rbind, out)
  }
}

defs <- do.call(rbind, lapply(files, extract_defs))
defs <- defs[!is.na(defs$name), ]

ns <- if (file.exists("NAMESPACE")) readLines("NAMESPACE", warn = FALSE) else character()
ns_exports <- sub("^export\\((.*)\\)$", "\\1", grep("^export\\(", ns, value = TRUE))

cat("\n==============================\n")
cat("1. Function definitions by file\n")
cat("==============================\n")
print(defs[order(defs$file, defs$line), ], row.names = FALSE)

cat("\n==============================\n")
cat("2. Duplicate definitions\n")
cat("==============================\n")
dup_names <- names(which(table(defs$name) > 1))

if (length(dup_names) == 0) {
  cat("OK: duplicate definitions not found.\n")
} else {
  print(
    defs[defs$name %in% dup_names, ][order(defs$name, defs$file, defs$line), ],
    row.names = FALSE
  )
}

cat("\n==============================\n")
cat("3. NAMESPACE exports missing from R/ definitions\n")
cat("==============================\n")
missing_from_r <- setdiff(ns_exports, defs$name)

if (length(missing_from_r) == 0) {
  cat("OK: all NAMESPACE exports are defined in R/.\n")
} else {
  print(missing_from_r)
}

cat("\n==============================\n")
cat("4. @export functions missing from NAMESPACE\n")
cat("==============================\n")
roxy_exports <- defs$name[defs$roxygen_export]
missing_from_ns <- setdiff(roxy_exports, ns_exports)

if (length(missing_from_ns) == 0) {
  cat("OK: all @export functions appear in NAMESPACE.\n")
} else {
  print(missing_from_ns)
}

cat("\n==============================\n")
cat("5. qlg_ functions not exported\n")
cat("==============================\n")
qlg_defs <- defs$name[grepl("^qlg_", defs$name)]
unexported_qlg <- setdiff(qlg_defs, ns_exports)

if (length(unexported_qlg) == 0) {
  cat("OK: all qlg_ functions are exported.\n")
} else {
  print(unexported_qlg)
}

cat("\n==============================\n")
cat("6. Responsibility map\n")
cat("==============================\n")

for (f in unique(defs$file)) {
  cat("\n", f, "\n", sep = "")
  print(defs$name[defs$file == f])
}