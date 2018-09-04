#' Calcates the mean number of species under protracted birth-death model of
#' diversification. This is a checked version with a more explicit interface
#' than \code{\link{pbd_numspec_mean}}.
#' @param sir speciation initiation rate. This function assumes the
#'   speciation initiation rates of good and incipient species are equal.
#' @param erg extinction rate of a good species
#' @param scr speciation completion rate
#' @param eri extinction rate of an incipient species
#' @param crown_age the crown age
#' @return The expected number of representative species
#' @seealso
#'   \code{\link{pbd_numspec_mean}} provides for setting a time-dependence
#'   in the parameters and/or specify a stem age.
#'   \code{\link{pbd_numspec_median_checked}} calculates the median number of species.
#' @examples
#'  mean_n_species <- pbd_numspec_mean_checked(
#'    erg = 0.12,
#'    eri = 0.23,
#'    scr = 0.34,
#'    sir = 0.45,
#'    crown_age = 0.56
#'  )
#'  expected_mean_n_species <- 1.046121595
#'  testthat::expect_equal(mean_n_species, expected_mean_n_species)
#' @author Richel J.C. Bilderbeek
#' @export
pbd_numspec_mean_checked <- function(
  erg,
  eri,
  scr,
  sir,
  crown_age
) {
  if (sir < 0.0) stop("'sir' must be positive")
  if (erg < 0.0) stop("'erg' must be positive")
  if (scr < 0.0) stop("'scr' must be positive")
  if (eri < 0.0) stop("'eri' must be positive")
  if (crown_age < 0.0) stop("'crown_age' must be positive")
  pbd_numspec_mean(pars = c(sir, erg, scr, eri), age = crown_age, soc = 2)
}

#' Calcates the median number of species under protracted birth-death model of
#' diversification. This is a checked version with a more explicit interface
#' than \code{\link{pbd_numspec_median}}.
#' @param sir speciation initiation rate. This function assumes the
#'   speciation initiation rates of good and incipient species are equal.
#' @param erg extinction rate of a good species
#' @param scr speciation completion rate
#' @param eri extinction rate of an incipient species
#' @param crown_age the crown age
#' @return The median number of representative species
#' @seealso
#'   \code{\link{pbd_numspec_median}} provides for setting a time-dependence
#'   in the parameters and/or specify a stem age.
#'   \code{\link{pbd_numspec_mean_checked}} calculates the mean number of species.
#' @examples
#'   median_n_species <- pbd_numspec_median_checked(
#'     erg = 1.2,
#'     eri = 2.3,
#'     scr = 3.4,
#'     sir = 4.5,
#'     crown_age = 6.7
#'   )
#'   median_n_species_expected <- 99408712
#'   expect_equal(median_n_species, median_n_species_expected)
#' @author Richel J.C. Bilderbeek
#' @export
pbd_numspec_median_checked <- function(
  erg,
  eri,
  scr,
  sir,
  crown_age
){
  if (sir < 0.0) stop("'sir' must be positive")
  if (erg < 0.0) stop("'erg' must be positive")
  if (scr < 0.0) stop("'scr' must be positive")
  if (eri < 0.0) stop("'eri' must be positive")
  if (crown_age < 0.0) stop("'crown_age' must be positive")
  pbd_numspec_median(
    pars = c(sir, erg, scr, eri),
    age = crown_age,
    soc = 2
  )
}
