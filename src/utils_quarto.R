library(fs)
library(dplyr)
library(yaml)
library(knitr)
library(rlang)



generate_download_table <- function(
    data_dir,
    file_pattern = "\\.(csv|xlsx|parquet|zip)$"
) {

  if (!dir.exists(data_dir)) {
    cat("*No data directory found at:* `", data_dir, "`\n", sep = "")
    return(invisible(NULL))
  }

  file_paths <- sort(list.files(data_dir, pattern = file_pattern, full.names = TRUE), decreasing = TRUE)

  if (length(file_paths) == 0) {
    cat("*No matching data files found.*\n")
    return(invisible(NULL))
  }

  cat("| File Name | Size | Download |\n")
  cat("|---|---|---|\n")

  for (fp in file_paths) {
    f_name <- fs::path_file(fp)
    f_size <- as.character(fs::file_size(fp))

    web_path <- gsub("\\\\", "/", fp)

    download_link <- sprintf(
      '<a href="%s" download="%s">Download %s</a>',
      web_path,
      f_name,
      fs::path_ext(f_name) %>% toupper()
    )

    cat(sprintf("| `%s` | %s | %s |\n", f_name, f_size, download_link))
  }
}


#' Render Data Quality Bounds as a Clean Markdown Table
#'
#' @param yaml_path Path to the YAML configuration file.
#' @param config_key Key inside the YAML (e.g., "l0_pce_header").
render_quality_rules <- function(yaml_path, config_key) {
  if (!file.exists(yaml_path)) {
    cat("\n\n*Config file not found at:", yaml_path, "*\n\n")
    return(invisible(NULL))
  }

  cfg <- read_yaml(yaml_path)[[config_key]]

  if (is.null(cfg) || is.null(cfg$bounds)) {
    return(invisible(NULL))
  }

  # Extract ONLY columns defined in bounds
  bounded_cols <- names(cfg$bounds)

  if (length(bounded_cols) == 0) {
    return(invisible(NULL))
  }

  # Build 2-column bounds summary
  rules_df <- tibble(Column = bounded_cols) %>%
    mutate(
      `Allowed Range / Bounds` = sapply(Column, function(col) {
        b <- cfg$bounds[[col]]

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

        if (length(parts) == 0) return("Any value")
        paste(parts, collapse = " ")
      }),
      Column = paste0("`", Column, "`")
    )

  # Format markdown table
  table_md <- as.character(knitr::kable(rules_df, format = "pipe"))

  out_str <- paste0(
    "\n\n**Data Quality Bounds**\n\n",
    paste(table_md, collapse = "\n"),
    "\n\n"
  )

  cat(out_str)
}



#' Render Outlier Detection Rules as a Clean Markdown Table
#'
#' @param yaml_path Path to the YAML configuration file.
#' @param config_key Optional key inside the YAML (e.g., "outlier_checks"). Leave NULL if top-level.
render_outlier_rules <- function(yaml_path, config_key = NULL) {
  if (!file.exists(yaml_path)) {
    cat("\n\n*Config file not found at:* `", yaml_path, "`\n\n", sep = "")
    return(invisible(NULL))
  }

  cfg <- yaml::read_yaml(yaml_path)

  if (!is.null(config_key)) {
    cfg <- cfg[[config_key]]
  }

  if (is.null(cfg)) {
    return(invisible(NULL))
  }

  # Extract settings and columns
  mad_thresh <- cfg$settings$mad_threshold %||% "N/A"
  cols_to_check <- unlist(cfg$columns_to_check)

  if (length(cols_to_check) == 0) {
    cat("\n\n*No columns configured for outlier detection.*\n\n")
    return(invisible(NULL))
  }

  # Create a clean table listing target columns
  outlier_df <- tibble::tibble(
    `Columns checked for outliers` = paste0("`", cols_to_check, "`")
  )

  table_md <- as.character(knitr::kable(outlier_df, format = "pipe"))

  # Assemble header summary + markdown table
  out_str <- paste0(
    "\n\n**Outlier Detection (MAD)**\n\n",
    "* **Method:** Median Absolute Deviation (MAD)\n",
    "* **Threshold Multiplier:** `", mad_thresh, "` MADs from the median\n\n",
    paste(table_md, collapse = "\n"),
    "\n\n"
  )

  cat(out_str)
}
