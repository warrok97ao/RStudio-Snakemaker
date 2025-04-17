.onAttach <- function(libname, pkgname) {
  if (interactive() && rstudioapi::isAvailable()) {
    my_addin()
  }
}


