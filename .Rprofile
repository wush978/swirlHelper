local({
  R_LIBS <- file.path(".lib", sprintf("%s.%s", R.version$major, R.version$minor))
  if (!dir.exists(R_LIBS)) dir.create(R_LIBS, recursive = TRUE)
  .libPaths(new = R_LIBS)
  Sys.setenv("R_LIBS" = paste(R_LIBS, collapse = ":"))
})
