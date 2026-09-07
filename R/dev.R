# Interactive-development helpers. Not part of the targets pipeline -- source()
# this in a console session when working on a stage document by hand.

# Build any out-of-date targets a report stage depends on, then load its inputs
# into the global environment. The dependency list is read from targets' own
# network, so it matches exactly what that document's tar_load() call binds.
# Does not touch the report target itself, so it is safe to run while editing
# that .qmd.
dev_prep <- function(report) {
  edges <- targets::tar_network(targets_only = TRUE)$edges
  deps <- edges$from[edges$to == report]
  targets::tar_make(names = tidyselect::all_of(deps))
  targets::tar_load(tidyselect::all_of(deps), envir = globalenv())
}
