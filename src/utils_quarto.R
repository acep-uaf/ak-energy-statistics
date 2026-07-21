library(fs)
library(dplyr)
library(yaml)
library(knitr)
library(rlang)

#' Generate a Markdown Download Table for Repository Data Files
#'
#' @param data_dir Local relative directory path containing the data files.
#' @param github_subpath Relative path inside the GitHub repository.
#' @param file_pattern Regex pattern matching files to include.
#' @param repo_base Base GitHub raw content URL.
generate_download_table <- function(
    data_dir,
    github_subpath,
    file_pattern = "\\.(csv|xlsx|parquet|zip)$",
    repo_base = "https://raw.githubusercontent.com/acep-uaf/ak-energy-statistics/main/"
) {

  if (!dir.exists(data_dir)) {
    cat("*No data directory found at:", data_dir, "*\n")
    return(invisible(NULL))
  }

  # Fetch files with full local paths to extract metadata (size)
  file_paths <- sort(list.files(data_dir, pattern = file_pattern, full.names = TRUE), decreasing = TRUE)

  if (length(file_paths) == 0) {
    cat("*No matching data files found.*\n")
    return(invisible(NULL))
  }

  # Clean GitHub subpath
  subpath_clean <- if (github_subpath != "") paste0(gsub("/+$", "", github_subpath), "/") else ""

  # Build Markdown Table Header
  cat("| File Name | Size | Direct Download |\n")
  cat("|---|---|---|\n")

  for (fp in file_paths) {
    f_name <- path_file(fp)

    # Extract human-readable file size (e.g., "1.2MB", "450KB")
    f_size <- as.character(fs::file_size(fp))

    # Construct GitHub raw URL
    full_github_url <- paste0(repo_base, subpath_clean, f_name)

    # Print clean Markdown row
    cat(sprintf("| `%s` | %s | [Download %s](%s) |\n", f_name, f_size, path_ext(f_name) %>% toupper(), full_github_url))
  }
}





render_quality_rules <- function(yaml_path, config_key) {
  if (!file.exists(yaml_path)) {
    cat("\n\n*Config file not found at:", yaml_path, "*\n\n")
    return(invisible(NULL))
  }

  cfg <- read_yaml(yaml_path)[[config_key]]

  if (is.null(cfg)) {
    cat("\n\n*Key '", config_key, "' not found in config.*\n\n", sep = "")
    return(invisible(NULL))
  }

  # 1. Identify ONLY columns with active constraints or bounds
  not_null_cols <- cfg$constraints$not_null %||% character(0)
  bounded_cols  <- names(cfg$bounds %||% list())

  target_cols <- unique(c(not_null_cols, bounded_cols))

  out_str <- ""

  # 2. Build summary table
  if (length(target_cols) > 0) {
    rules_df <- tibble(Column = target_cols) %>%
      mutate(
        `Required` = if_else(Column %in% not_null_cols, "Yes", "Optional"),
        `Allowed Values / Bounds` = sapply(Column, function(col) {
          b <- cfg$bounds[[col]]
          if (is.null(b)) return("Any value")

          parts <- c()
          if (!is.null(b$min) && !is.null(b$max)) {
            parts <- c(parts, paste0(b$min, " to ", b$max))
          } else if (!is.null(b$min)) {
            parts <- c(parts, paste0("≥ ", b$min))
          } else if (!is.null(b$max)) {
            parts <- c(parts, paste0("≤ ", b$max))
          }

          if (isFALSE(b$allow_zero)) {
            parts <- c(parts, "(No Zeros)")
          }

          paste(parts, collapse = " ")
        })
      ) %>%
      mutate(Column = paste0("`", Column, "`"))

    # Force kable to output plain markdown strings explicitly
    table_md <- as.character(knitr::kable(rules_df, format = "pipe"))

    out_str <- paste0(
      out_str,
      "\n\n**Active Data Quality Rules**\n\n",
      paste(table_md, collapse = "\n"),
      "\n\n"
    )
  }

  # 3. Compact Category Mapping List
  if (!is.null(cfg$category_mappings)) {
    out_str <- paste0(out_str, "**Standardized Categories**\n\n")
    for (cat_col in names(cfg$category_mappings)) {
      unique_targets <- unique(unlist(cfg$category_mappings[[cat_col]]))
      target_str <- paste(paste0("`", unique_targets, "`"), collapse = ", ")
      out_str <- paste0(out_str, sprintf("* **`%s`**: Maps inputs to %s\n", cat_col, target_str))
    }
    out_str <- paste0(out_str, "\n\n")
  }

  # Single cat() call ensures Pandoc gets contiguous raw markdown
  cat(out_str)
}
