
src_dir  <- "../data"
dest_dir <- "data"

if (dir.exists(src_dir)) {
  # Recursively list all relative file paths inside ../data
  src_files <- list.files(src_dir, recursive = TRUE, full.names = FALSE)

  for (f in src_files) {
    src_path  <- file.path(src_dir, f)
    dest_path <- file.path(dest_dir, f)

    # Create subdirectories if they don't exist yet
    target_dir <- dirname(dest_path)
    if (!dir.exists(target_dir)) {
      dir.create(target_dir, recursive = TRUE)
    }

    # Copy if file is missing or if source file is newer
    if (!file.exists(dest_path) || file.info(src_path)$mtime > file.info(dest_path)$mtime) {
      file.copy(src_path, dest_path, overwrite = TRUE)
    }
  }
}
