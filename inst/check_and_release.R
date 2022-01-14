devtools::check_rhub(interactive = FALSE)

# For windows if it fails with i.e. "there is no package called 'utf8'"
# rhub::check(
#   platform="windows-x86_64-devel",
#   env_vars=c(R_COMPILE_AND_INSTALL_PACKAGES = "always")
# )

# rhub tests on windows (?) but builds locally, `check_win()` also builds on
# windows
devtools::check_win_release()

# reverse dependency check ----

#remotes::install_github("r-lib/revdepcheck")
library(revdepcheck)
revdep_reset()
revdep_check(num_workers = 8)

# submit to cran ----

devtools::release()
