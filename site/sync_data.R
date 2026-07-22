library(fs)

src_dir <- "../data"
dest_dir <- "data"

# Only perform copy if destination folder doesn't exist yet
if (!dir.exists(dest_dir)) {
  fs::dir_copy(src_dir, dest_dir)
} else {
  # Optionally sync only files that do not exist in destination
  src_files <- fs::dir_ls(src_dir, recurse = TRUE, type = "file")
  rel_paths <- fs::path_rel(src_files, start = src_dir)
  dest_files <- fs::path(dest_dir, rel_paths)

  missing_mask <- !fs::file_exists(dest_files)
  if (any(missing_mask)) {
    fs::file_copy(src_files[missing_mask], dest_files[missing_mask], overwrite = TRUE)
  }
}
