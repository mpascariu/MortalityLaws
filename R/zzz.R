
#' onAttach
#' @param lib lib
#' @param pkg pkg
#' @name onAttach
#' @keywords internal
".onAttach" <- function(lib, pkg){
  packageStartupMessage("\nMortalityLaws: Parametric Mortality Models, Life Tables and HMD",
                        "\nAuthor       : Marius D. Pascariu",
                        "\nLast Update  : August 19, 2018\n")
}