# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-10-05 23:38:09
# --------------------------------------------

#' Check the Available Mortality Laws
#'
#' The law catalogue. It lists every parametric model that
#' \code{\link{MortalityLaw}} can fit, with the formula and the code to pass
#' through \code{law}, and says where each model applies. Use it to choose a
#' law before fitting; there is no need to know the functional form, only its
#' code and the age range it is meant for. For a comprehensive review of the
#' mortality laws themselves, Tabeau (2001) is a good starting point.
#'
#' The \code{TYPE} column says where on the lifespan the law belongs, read off
#' the legend in the second component of the result: a law covering the whole
#' range (6) is fitted over all ages, while a law for old age (5) is fitted
#' from the adult ages up. The \code{FIT} column says whether the law
#' describes a hazard, \code{mu[x]}, or a death probability, \code{q[x]};
#' \code{\link{MortalityLaw}} and \code{\link{LawTable}} handle both. The
#' \code{SCALE_X} column flags the laws whose age vector is rescaled during
#' fitting for numerical stability, which matters when the fitted
#' coefficients are reused outside the fitted age range (see
#' \code{\link{LawTable}}).
#'
#' A law is normally reached through the \code{law} argument of
#' \code{\link{MortalityLaw}}, but each one is also a function that can be
#' called by name for a plain hazard or death probability curve; those help
#' pages exist for reference and are kept out of the help index. A law that is
#' not in the catalogue can still be fitted by passing it as a function
#' through the \code{custom.law} argument; see the examples on the
#' \code{\link{MortalityLaw}} page.
#'
#' A few laws carry a caveat worth knowing before choosing them, all
#' documented in the model catalogue and their catalogue entries:
#' \itemize{
#'   \item \code{"scholey"}: its truncation parameter is identified only on
#'         day- or week-level data over the first year of life; on
#'         single-year ages it collapses and the fit reduces to
#'         \code{"scholey_shifted_power"}, with a warning.
#'   \item \code{"opperman"}: the published middle term has a free sign; the
#'         package uses the negative branch the log-scale engine permits,
#'         which is the branch mortality data occupy.
#'   \item \code{"steffensen"}: the formula is attributed to Steffensen
#'         (1930), but the attribution is not verified against the paywalled
#'         source.
#'   \item \code{"HP"}, \code{"HP2"}, \code{"HP3"}, \code{"HP4"} and
#'         \code{"kostaki"}: high-parameter models; fit them with
#'         \code{opt.method = "LF2"}.
#'   \item \code{"gompertz_logquad"} and \code{"makeham_logquad"}: the sign of
#'         the quadratic term is fixed to the decelerating branch that
#'         mortality data occupy; the accelerating branch cannot be fitted.
#'   \item \code{"beard_makeham"} and \code{"perks"}: the same four-parameter
#'         Perks-Beard logistic written two ways, so they fit identical
#'         curves; \code{"beard"}, \code{"kannisto"} and
#'         \code{"kannisto_makeham"} are its two- and three-parameter cases.
#'         Pick one of them, not several.
#'   \item \code{"demoivre"}: the 1725 baseline, kept for completeness. Its
#'         hazard is defined only below a limiting age, so it must not be
#'         extrapolated and the fit warns every time.
#'   \item \code{"weibull"}: not defined at birth, so age 0 is reported as
#'         missing and takes no part in the fit; start the fit at age 1.
#' }
#'
#' Two entries in the reference list are background for the infant laws
#' rather than the source of a catalogue code: Harper (1936), and de Beer and
#' Janssen (2016), whose infancy term is the fitted \code{pareto_2}.
#'
#' @param law Optional. Default: \code{NULL}. One can extract details about
#' a certain model by specifying its codename.
#' @return The output is of the \code{"availableLaws"} class with the following
#' components:
#'  \item{table}{Table with mortality models and codes to be used in \code{\link{MortalityLaw}}, the model formula, the lifespan section (\code{TYPE}), the code (\code{CODE}), whether the law describes \code{mu[x]} or \code{q[x]} (\code{FIT}) and whether fitting rescales the ages (\code{SCALE_X}).}
#'  \item{legend}{Table with details about the section of the mortality curve.}
#' @seealso \code{\link{MortalityLaw}} to fit a law; \code{\link{LawTable}}
#'   to build a life table from fitted coefficients; \code{\link{availableLF}}
#'   for the loss functions.
#' @references
#' \enumerate{
#' \item{De Moivre, A. (1725). \emph{Annuities on Lives: or, the Valuation of
#' Annuities upon any Number of Lives}. London: William Pearson.}
#' \item{Gompertz, B. (1825). \href{https://www.jstor.org/stable/107756}{On the
#' Nature of the Function Expressive of the Law of Human Mortality, and on a
#' New Mode of Determining the Value of Life Contingencies.}
#' Philosophical Transactions of the Royal Society of London, 115, 513-583.}
#' \item{Makeham, W. (1860).
#' On the Law of Mortality and Construction of Annuity Tables.
#' The Assurance Magazine and Journal of the Institute of Actuaries, 8(6),
#' 301-310. \doi{10.1017/S204616580000126X}}
#' \item{Thiele, T. (1871). On a Mathematical Formula to express the
#' Rate of Mortality throughout the whole of Life, tested by a Series of
#' Observations made use of by the Danish Life Insurance Company of 1871.
#' Journal of the Institute of Actuaries and Assurance Magazine, 16(5), 313-329.
#' \doi{10.1017/S2046167400043688}}
#' \item{Lomax, K. S. (1954). Business Failures: Another Example of the
#' Analysis of Failure Data. Journal of the American Statistical Association,
#' 49(268), 847-852. \doi{10.1080/01621459.1954.10501239}}
#' \item{Vaupel, J. W. and Yashin, A. I. (1983). The Deviant Dynamics of Death
#' in Heterogeneous Populations. IIASA Research Report RR-83-1. Laxenburg,
#' Austria.}
#' \item{de Beer, J. and Janssen, F. (2016). A new parametric model to assess
#' delay and compression of mortality. Population Health Metrics, 14(1), 46.
#' \doi{10.1186/s12963-016-0113-1}}
#' \item{Scholey, J. (2019). The Age-Trajectory of Infant Mortality in the
#' United States: Parametric Models and Generative Mechanisms. PAA Annual
#' Conference, Austin.}
#' \item{Oppermann, L. H. F. (1870). On the graduation of life tables,
#' with special application to the rate of mortality in infancy and childhood.
#' The Insurance Record Minutes from a meeting in the Institute of Actuaries, 42.}
#' \item{Wittstein, T. and D. Bumsted. (1883).
#' \href{https://www.cambridge.org/core/journals/journal-of-the-institute-of-actuaries/article/the-mathematical-law-of-mortality/57A7403B578C84769A463EA2BC2F7ECD}{
#' The Mathematical Law of Mortality.}
#' Journal of the Institute of Actuaries and Assurance Magazine, 24(3), 153-173.}
#' \item{Steffensen, J. (1930). Infantile mortality from an actuarial point of
#' view. Skandinavisk Aktuarietidskrift 13, 272-286.
#' \doi{10.1080/03461238.1930.10416902}}
#' \item{Perks, W. (1932).
#' On Some Experiments in the Graduation of Mortality Statistics.
#' Journal of the Institute of Actuaries, 63(1), 12-57.
#' \doi{10.1017/S0020268100046680}}
#' \item{Harper, F. S. (1936). An actuarial study of infant mortality.
#' Scandinavian Actuarial Journal 1936 (3-4), 234-270.
#' \doi{10.1080/03461238.1936.10405113}}
#' \item{Weibull, W. (1951). A statistical distribution function of wide applicability.
#' Journal of applied mechanics 18, 293-297.
#'  \doi{10.1115/1.4010337}}
#' \item{Beard, R. E. (1971).
#' \href{http://longevity-science.org/Beard-1971.pdf}{
#' Some aspects of theories of mortality, cause of
#' death analysis, forecasting and stochastic processes.}
#' Biological aspects of demography 999, 57-68.}
#' \item{Vaupel, J., Manton, K.G., and Stallard, E. (1979).
#' The impact of heterogeneity in individual frailty on the dynamics of
#' mortality. Demography 16(3): 439-454.
#' \doi{10.2307/2061224}}
#' \item{Siler, W. (1979),
#' A Competing-Risk Model for Animal Mortality. Ecology, 60: 750-757.
#' \doi{10.2307/1936612}}
#' \item{Heligman, L., & Pollard, J. (1980). The age pattern of mortality.
#' Journal of the Institute of Actuaries, 107(1), 49-80.
#' \doi{10.1017/S0020268100040257}}
#' \item{Rogers A and Planck F (1983).
#' \href{https://pure.iiasa.ac.at/id/eprint/2210/1/WP-83-102.pdf}{
#' MODEL: A General Program for Estimating Parametrized Model Schedules of Fertility,
#' Mortality, Migration, and Marital and Labor Force Status Transitions.}
#' IIASA Working Paper. IIASA, Laxenburg, Austria: WP-83-102}
#' \item{Martinelle S. (1987). A generalized Perks formula for old-age mortality.
#' Stockholm, Sweden, Statistiska centralbyran, 1987. 55 p.
#' (R&D Report, Research-Methods-Development, U/STM No. 38)}
#' \item{Forfar, D. O., McCutcheon, J. J. and Wilkie, A. D. (1988).
#' On graduation by mathematical formula.
#' Journal of the Institute of Actuaries, 115(1), 1-149.}
#' \item{Carriere J.F. (1992). Parametric models for life tables.
#' Transactions of the Society of Actuaries. Vol.44}
#' \item{Kostaki A. (1992).
#' A nine-parameter version of the Heligman-Pollard formula.
#' Mathematical Population Studies. Vol. 3 277-288.
#' \doi{10.1080/08898489209525346}}
#' \item{Thatcher AR, Kannisto V and Vaupel JW (1998).
#' The force of mortality at ages 80 to 120. Odense Monographs on
#' Population Aging Vol. 5, Odense University Press, 1998. 104, 20 p.
#' Odense, Denmark}
#' \item{Tabeau E. (2001). A Review of Demographic Forecasting Models for
#' Mortality. In: Tabeau E., van den Berg Jeths A., Heathcote C. (eds)
#' Forecasting Mortality in Developed Countries.
#' European Studies of Population, vol 9. Springer, Dordrecht.
#' \doi{10.1007/0-306-47562-6_1}}
#' \item{Finkelstein M. (2012)
#' Discussing the Strehler-Mildvan model of mortality
#' Demographic Research, Vol. 26(9), 191-206.
#' \doi{10.4054/DemRes.2012.26.9}}
#' }
#' @seealso \code{\link{MortalityLaw}}
#' @author Marius D. Pascariu
#' @examples availableLaws()
#' @export
availableLaws <- function(law = NULL){

  if (is.null(law)) {

    law_table <- as.data.frame(
      matrix(
        ncol = 7,
        byrow = TRUE,
        data = c(
          1725, 'De Moivre', 'mu[x] = 1/[N - x]', 6, 'demoivre', 'mu[x]', FALSE,
          1825, 'Gompertz', 'mu[x] = A exp[Bx]', 3, 'gompertz', 'mu[x]', TRUE,
          NA, 'Gompertz', 'mu[x] = 1/sigma * exp[(x-M)/sigma]', 3, 'gompertz0', 'mu[x]', TRUE,
          NA, 'Inverse-Gompertz', 'mu[x] = 1/sigma * exp[-(x-M)/sigma] / (exp(exp[-(x-M)/sigma]) - 1)', 2, 'invgompertz', 'mu[x]', TRUE,
          1860, 'Makeham', 'mu[x] = A exp[Bx] + C', 3, 'makeham', 'mu[x]', TRUE,
          NA, 'Makeham', 'mu[x] = 1/sigma * exp[(x-M)/sigma] + C', 3, 'makeham0', 'mu[x]', TRUE,
          1870, 'Opperman', 'mu[x] = A/sqrt(x+1) - B + C*sqrt(x+1)', 1, 'opperman', 'mu[x]', FALSE,
          1871, 'Thiele', 'mu[x] = A exp(-Bx) + C exp[-.5D (x-E)^2] + F exp(Gx)', 6, 'thiele', 'mu[x]', FALSE,
          1871, 'Negative-Gompertz', 'mu[x] = A exp(-Bx)', 1, 'neggompertz', 'mu[x]', FALSE,
          1883, 'Wittstein', 'q[x] = (1/B) A^-[(Bx)^N] + A^-[(M-x)^N]', 6, 'wittstein', 'q[x]', FALSE,
          1930, 'Steffensen', 'mu[x] = [A + BC^x] / [BC^-x + 1 + DC^x]', 6, 'steffensen', 'mu[x]', TRUE,
          1932, 'Perks', 'mu[x] = [A + BC^x] / [1 + DC^x]', 3, 'perks', 'mu[x]', TRUE,
          1939, 'Weibull', 'mu[x] = 1/sigma * (x/M)^(M/sigma - 1)', 1, 'weibull', 'mu[x]', FALSE,
          1954, 'Pareto-II', 'mu[x] = A/(x + C)', 1, 'pareto_2', 'mu[x]', FALSE,
          NA, 'Inverse-Weibull', 'mu[x] = 1/sigma * (x/M)^[-M/sigma - 1] / [exp((x/M)^(-M/sigma)) - 1]', 2, 'invweibull', 'mu[x]', TRUE,
          1943, 'Van der Maen', 'mu[x] = A + Bx + Cx^2 + I/[N - x]', 4, 'vandermaen', 'mu[x]', TRUE,
          1943, 'Van der Maen', 'mu[x] = A + Bx + I/[N - x]', 5, 'vandermaen2', 'mu[x]', TRUE,
          1960, 'Strehler-Mildvan', 'mu[x] = A exp(Bx) exp[-(V/B)(1 - exp(-Bx))]', 3, 'strehler_mildvan', 'mu[x]', TRUE,
          NA, 'Quadratic', 'mu[x] = A + Bx + Cx^2', 5, 'quadratic', 'mu[x]', TRUE,
          1971, 'Beard', 'mu[x] = A exp(Bx) / [1 + KA exp(Bx)]', 4, 'beard', 'mu[x]', TRUE,
          1971, 'Beard-Makeham', 'mu[x] = A exp(Bx) / [1 + KA exp(Bx)] + C', 4, 'beard_makeham', 'mu[x]', TRUE,
          1979, 'Gamma-Gompertz', 'mu[x] = A exp(Bx) / (1 + AG/B * [exp(Bx) - 1])', 4, 'ggompertz', 'mu[x]', TRUE,
          1979, 'Siler', 'mu[x] = A exp(-Bx) + C + D exp(Ex)', 6, 'siler', 'mu[x]', FALSE,
          1980, 'Heligman-Pollard', 'q[x]/p[x] = A^[(x + B)^C] + D exp[-E log(x/F)^2] + G H^x', 6, 'HP', 'q[x]', FALSE,
          1980, 'Heligman-Pollard', 'q[x] = A^[(x + B)^C] + D exp[-E log(x/F)^2] + GH^x / [1 + GH^x]', 6, 'HP2', 'q[x]', FALSE,
          1980, 'Heligman-Pollard', 'q[x] = A^[(x + B)^C] + D exp[-E log(x/F)^2] + GH^x / [1 + KGH^x]', 6, 'HP3', 'q[x]', FALSE,
          1980, 'Heligman-Pollard', 'q[x] = A^[(x + B)^C] + D exp[-E log(x/F)^2] + GH^(x^K) / [1 + GH^(x^K)]', 6, 'HP4', 'q[x]', FALSE,
          1983, 'Rogers-Planck', 'q[x] = A0 + A1 exp[-Ax] + A2 exp[B(x - u) - exp(-C(x - u))] + A3 exp[Dx]', 6, 'rogersplanck', 'q[x]', FALSE,
          1987, 'Martinelle', 'mu[x] = [A exp(Bx) + C] / [1 + D exp(Bx)] + K exp(Bx)', 6, 'martinelle', 'mu[x]', FALSE,
          1988, 'Gompertz-Makeham', 'mu[x] = A0 + K exp[B1 x - B2 x^2]', 5, 'makeham_logquad', 'mu[x]', TRUE,
          1988, 'Gompertz-Makeham', 'mu[x] = K exp[B1 x - B2 x^2]', 5, 'gompertz_logquad', 'mu[x]', TRUE,
          1992, 'Carriere', 'l[x] = P1 l[x](weibull) + P2 l[x](invweibull) + P3 l[x](gompertz)', 6, 'carriere1', 'q[x]', TRUE,
          1992, 'Carriere', 'l[x] = P1 l[x](weibull) + P2 l[x](invgompertz) + P3 l[x](gompertz)', 6, 'carriere2', 'q[x]', TRUE,
          1992, 'Kostaki', 'q[x]/p[x] = A^[(x+B)^C] + D exp[-(E_i log(x/F_))^2] + G H^x', 6, 'kostaki', 'q[x]', FALSE,
          1998, 'Kannisto', 'mu[x] = A exp(Bx) / [1 + A exp(Bx)]', 5, 'kannisto', 'mu[x]', TRUE,
          1998, 'Kannisto-Makeham', 'mu[x] = A exp(Bx) / [1 + A exp(Bx)] + C', 5, 'kannisto_makeham', 'mu[x]', TRUE,
          2019, 'Scholey-Shifted-Power', 'mu[x] = A (x + C)^-B', 1, 'scholey_shifted_power', 'mu[x]', FALSE,
          2019, 'Scholey', 'mu[x] = A (x + C)^-B exp(-Dx)', 1, 'scholey', 'mu[x]', FALSE
          )
        )
      )

    colnames(law_table) <- c(
      'YEAR',
      'NAME',
      'MODEL',
      'TYPE',
      'CODE',
      'FIT',
      "SCALE_X"
      )

    law_legend <- as.data.frame(
      matrix(
        ncol = 2,
        byrow = TRUE,
        data = c(
          1, "Infant mortality",
          2, "Accident hump",
          3, "Adult mortality",
          4, "Adult and/or old-age mortality",
          5, "Old-age mortality",
          6, "Full age range"
          )
        )
      )

    colnames(law_legend) <- c("TYPE", "Coverage")
  }

  if (!is.null(law)) {
    A <- availableLaws()
    if (!(law %in% A$table$CODE)) {
      stop(
        "The specified 'law' is not available. ",
        "Run 'availableLaws()' to see the implemented models.",
        call. = FALSE)
    }
    law_table <- A$table[A$table$CODE %in% law, ]
    law_legend <- A$legend[A$legend$TYPE %in% unique(law_table$TYPE), ]
  }

  out <- structure(
    class = "availableLaws",
    list(table = law_table, legend = law_legend)
    )
  return(out)
}


#' Print the Available Mortality Laws
#'
#' Prints the catalogue of mortality laws (year, name, model formula, type
#' and code) followed by the legend that explains the type numbers.
#' @param x An object of class \code{"availableLaws"}.
#' @param ... Further arguments passed to or from other methods.
#' @return The object \code{x}, invisibly. Called for its printed output.
#' @seealso \code{\link{availableLaws}}.
#' @keywords internal
#' @export
print.availableLaws <- function(x, ...) {
  cat("\nMortality laws available in the package:\n\n")
  print(
    x$table[, 1:5],
    right = FALSE,
    row.names = FALSE
    )

  cat("\nLEGEND:\n")
  print(
    x$legend,
    right = FALSE,
    row.names = FALSE
    )

  return(invisible(x))
}

