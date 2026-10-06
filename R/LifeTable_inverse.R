# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-10-06 21:02:34
# --------------------------------------------

# Inverse life table: build a table from a vector of life expectancy e(x).
# Life expectancy is the one life-table input that is not a rate or a count,
# so it needs its own step backwards through the table. The recovery rests on
# the exact interval identity that ties e(x), the survivorship ratio and ax
# together (see ex_inverse).

#' Validate a vector of life expectancy before the inverse step
#'
#' The inverse sweep needs a complete curve of life expectancy: a missing or
#' non-finite value leaves a gap the sweep cannot cross, so it is an error
#' naming the affected ages. The curve is free to rise with age at the
#' youngest ages, which is the normal signature of a table over a population
#' with infant mortality (life expectancy at birth is lower than at age one);
#' the sweep does not require monotonicity, only that the implied death
#' probabilities stay in \code{[0, 1]}, which \code{ex_inverse} checks.
#' @inheritParams LifeTable
#' @return The life expectancy vector, or the matrix, with non-finite entries
#'   reported rather than repaired.
#' @noRd
lt_repair_ex <- function(x, ex) {

  bad <- !is.finite(ex)

  if (any(bad)) {
    rows <- if (is.null(dim(ex))) which(bad) else which(rowSums(bad) > 0)
    stop("'ex' contains missing or non-finite values at age(s) ",
         paste(x[rows], collapse = ", "), ". The inverse life table needs a ",
         "complete curve of life expectancy.", call. = FALSE)
  }

  return(ex)
}


#' Recover the survivorship from a vector of life expectancy and an ax rule
#'
#' Applies the exact interval identity that links life expectancy, the
#' survivorship ratio and ax. For each closed interval,
#' \deqn{e_x l_x - e_{x+n} l_{x+n} = nL_x = a_x l_x + (n - a_x) l_{x+n}}
#' so that the survivorship ratio
#' \deqn{r_x = l_{x+n}/l_x = (e_x - a_x) / (e_{x+n} + n - a_x)}
#' follows from ax alone. The open interval closes by the standard package
#' rule, \eqn{a_N = e_N} and \eqn{m_N = 1/e_N}, which keeps the row
#' identities of the table intact.
#'
#' When \code{ax} is a numeric value the ratio of every interval is one sweep.
#' When it is \code{NULL} the interval ratio is instead solved so that the ax
#' the method in force would assign to the recovered interval reproduces the
#' \eqn{r} it came from; each interval is a one-dimensional root of
#' \deqn{(e_x - a(r)) / (e_{x+n} + n - a(r)) - r = 0.}
#' The root is unique on \eqn{(0, 1)}: the left side is continuous and strictly
#' decreasing in \eqn{r} for every ax rule the package offers, so a bisection
#' finds it. The ax \eqn{a(r)} is the value the method's forward chain assigns
#' to the interval, expressed through \eqn{r} alone: on the default method a
#' one-year interval keeps the midpoint (with the Andreev-Kingkade value at
#' birth) and any other interval the constant-force value, and the plain
#' methods always the constant-force value.
#'
#' A zero \eqn{e_{x+n}} means the table has already closed at \eqn{x+n}:
#' nobody survives to that age, so the interval's ratio is zero and it takes
#' the closing row identity \eqn{a_x = e_x}, \eqn{m_x = 1/e_x}. The intervals
#' above it carry no survivors and reuse that rate.
#'
#' The Coale-Demeny childhood rule is the one non-local case: it sets the ax of
#' the second interval from the first interval's rate \eqn{m_0}, not from the
#' second interval's own rate. The first interval is therefore solved first,
#' \eqn{m_0} is recovered from it, and the second interval's ratio follows
#' directly from that value.
#'
#' Death probabilities that leave \code{[0, 1]}, or an interval with no root,
#' mean the input curve is not a feasible life table, for example a value of
#' \eqn{e_x} below its own \eqn{a_x}; these are an error naming the ages.
#'
#' @inheritParams LifeTable
#' @param nx Numeric vector of interval widths, one per age.
#' @param ax_method The ax method in force; one of \code{"andreev_kingkade"},
#'   \code{"cfm"}, \code{"preston"} or \code{"coale_demeny"}.
#' @return A list with \code{mx}, \code{qx} and the \code{ax} actually used.
#'   The \code{lx} and \code{dx} entries are \code{NULL} so that the caller
#'   rebuilds them from \code{qx} on the chosen radix.
#' @noRd
ex_inverse <- function(x, nx, ex, ax = NULL, sex = NULL,
                       ax_method = "andreev_kingkade") {

  N <- length(x)

  given_ax <- !is.null(ax)

  if (given_ax && length(ax) == 1L) {
    ax <- rep(ax, N)
  }

  # An interval whose end has e(x+n) = 0 ends the table: nobody survives it,
  # so its ratio is zero. Those intervals are read off, never solved.
  close_end <- ex[-1] == 0
  open_i    <- which(!close_end)

  if (given_ax) {
    # The open interval is fixed by the curve being inverted: a_N = e_N. This
    # is the one place a supplied value cannot be kept, so it is reported.
    if (!isTRUE(all.equal(ax[N], ex[N]))) {
      message("'ax' at the open age interval (age ", x[N], ") is set to ",
              "e(x) there, ", round(ex[N], 4), ", which is what a table ",
              "entered from 'ex' fixes it to. The value you supplied, ",
              round(ax[N], 4), ", applies to the closed intervals.")
    }

    ax[N]  <- ex[N]
    denom  <- ex[-1] + nx[-N] - ax[-N]

    if (any(denom[open_i] <= 0)) {
      stop("The 'ex' vector is not a feasible life table: the interval ",
           "starting at age ", x[which(denom <= 0)[1]], " has a non-positive ",
           "person-years balance. Check that e(x) stays above the ax values.",
           call. = FALSE)
    }

    r    <- numeric(N - 1)
    r[open_i] <- (ex[-N][open_i] - ax[-N][open_i]) / denom[open_i]

  } else {
    r <- numeric(N - 1)

    cd_child <- ax_method %in% c("preston", "coale_demeny") && !is.null(sex)

    if (!cd_child) {
      if (length(open_i)) {
        r[open_i] <- vapply(
          X   = open_i,
          FUN = function(i) ex_interval_ratio(x = x, nx = nx, ex = ex,
                                              sex = sex, ax_method = ax_method,
                                              m0 = NULL, i = i),
          FUN.VALUE = numeric(1)
          )
      }

    } else {
      # The first interval's ax is the Coale-Demeny a0, keyed on its own rate;
      # the second interval's a1 is keyed on the first interval's rate m0.
      if (1 %in% open_i) {
        r[1] <- ex_interval_ratio(x = x, nx = nx, ex = ex, sex = sex,
                                  ax_method = ax_method, m0 = NA_real_,
                                  i = 1)
      }

      m0    <- -log(r[1]) / nx[1]
      rest2 <- intersect(open_i, 2L)

      if (length(rest2)) {
        a2   <- coale_demeny_interval_ax(x = x, nx = nx, m0 = m0, sex = sex,
                                         ax_method = ax_method, i = 2)
        r[2] <- (ex[2] - a2) / (ex[3] + nx[2] - a2)
      }

      rest <- setdiff(open_i, 1:2)

      if (length(rest)) {
        r[rest] <- vapply(
          X   = rest,
          FUN = function(i) ex_interval_ratio(x = x, nx = nx, ex = ex,
                                              sex = sex, ax_method = ax_method,
                                              m0 = m0, i = i),
          FUN.VALUE = numeric(1)
          )
      }
    }

    mxa <- ex_mx_from_r(r = r, nx = nx[-N])
    ax  <- ex_ax_rule(x = x, nx = nx, mx = c(mxa, 1/ex[N]), qx = c(1 - r, 1),
                      sex = sex, ax_method = ax_method)
    ax[N] <- ex[N]

    # An interval that ends with no survivors (e(x+n) = 0) closes the table and
    # takes the row identity a = e(x). Everything above it has no survivors
    # either, so it is not identifiable from the curve; it carries the closing
    # row's value, which is what the forward build reports there too.
    if (any(close_end)) {
      close_rows <- which(close_end)
      ax[close_rows] <- ex[close_rows[1]]
      ax[N]          <- ex[close_rows[1]]
    }
  }

  qx  <- c(1 - r, 1)
  bad <- which(!is.finite(qx) | qx < -1e-8 | qx > 1 + 1e-8)

  if (length(bad)) {
    stop("The 'ex' vector is not a feasible life table: it implies death ",
         "probabilities outside [0, 1] at age(s) ",
         paste(x[bad], collapse = ", "), ". Check that e(x) stays above the ",
         "ax values and falls by no more than the interval width.", call. = FALSE)
  }

  # The reported rate is the one the pipeline would have entered: with a
  # numeric ax in hand, mx and qx are linked by the exact interval identity,
  # not by the constant-force form used inside the interval solve.
  mx <- qx/(ax * qx + nx * (1 - qx))
  mx[N] <- 1/ex[N]

  out <- list(mx = mx, qx = qx, lx = NULL, dx = NULL, ax = ax)
  return(out)
}


#' The survivorship ratio of one closed interval under an ax rule
#'
#' Solves \eqn{(e_x - a(r))/(e_{x+n} + n - a(r)) - r = 0} on \eqn{(0, 1)} by
#' bisection, with \eqn{a(r)} the ax rule in force expressed through the
#' ratio alone. The root is unique: the left side is continuous and strictly
#' decreasing in \eqn{r} for every ax rule the package offers.
#' @param x,nx,ex,sex,ax_method As in \code{ex_inverse}, the internal solver
#'   of the inverse life table.
#' @param m0 The first interval's rate for the Coale-Demeny childhood rule, or
#'   \code{NULL} when the rule does not apply. \code{NA} on the first interval
#'   means "solve this interval to the a0 rule of its own rate".
#' @param i The index of the interval to solve.
#' @return The survivorship ratio, or an error when the interval has no root.
#' @noRd
ex_interval_ratio <- function(x, nx, ex, sex, ax_method, m0, i) {

  g <- function(r) {
    a <- ex_interval_ax(x = x, nx = nx, r = r, sex = sex,
                        ax_method = ax_method, m0 = m0, i = i)
    (ex[i] - a)/(ex[i + 1] + nx[i] - a) - r
  }

  lo  <- 1e-12
  hi  <- 1 - 1e-12
  flo <- g(lo)
  fhi <- g(hi)

  if (!is.finite(flo) || !is.finite(fhi) || flo * fhi > 0) {
    stop("The 'ex' vector is not a feasible life table: the interval starting ",
         "at age ", x[i], " has no survivorship ratio in [0, 1]. Check that ",
         "e(x) stays above the ax values and falls by no more than the ",
         "interval width.", call. = FALSE)
  }

  for (k in seq_len(120)) {
    mid <- (lo + hi)/2
    fm  <- g(mid)

    if (flo * fm <= 0) {
      hi <- mid
    } else {
      lo <- mid
      flo <- fm
    }
  }

  out <- (lo + hi)/2
  return(out)
}


#' The ax rule of one interval, evaluated at a survivorship ratio
#'
#' The value the ax method in force assigns to interval \code{i} when the
#' interval's survivorship ratio is \code{r}. Mirrors the forward estimators
#' (\code{hmd_ax_vector}, \code{compute_ax}, \code{coale_demeny_ax}) so that
#' the recovered rates carry exactly the ax the forward pipeline would
#' compute. The Coale-Demeny childhood value of the second interval is a
#' function of the first interval's rate \code{m0}, not of \code{r}.
#' @param x,nx,sex,ax_method,m0,i As in \code{ex_interval_ratio}.
#' @param r The interval's survivorship ratio.
#' @return The value of ax for the interval.
#' @noRd
ex_interval_ax <- function(x, nx, r, sex, ax_method, m0, i) {

  n <- nx[i]

  if (ax_method %in% c("preston", "coale_demeny") && !is.null(sex) && i <= 2) {
    # The first interval's a0 keys on its own rate; the second interval's a1
    # keys on the first interval's rate, passed in as m0.
    m0_use <- if (is.na(m0)) -log(r)/n else m0

    return(coale_demeny_interval_ax(x = x, nx = nx, m0 = m0_use, sex = sex,
                                    ax_method = ax_method, i = i))
  }

  m <- -log(r)/n

  if (identical(ax_method, "andreev_kingkade")) {
    m_cfm <- -log(r)/n

    if (i == 1 && isTRUE(all.equal(x[1], 0)) && isTRUE(all.equal(n, 1))) {
      a <- ak_a0_coefs(m0 = m_cfm, sex = if (is.null(sex)) "total" else sex)
    } else if (isTRUE(all.equal(n, 1))) {
      a <- n/2
    } else {
      a <- n + 1/m_cfm - n/(1 - exp(-n * m_cfm))
    }

    # The cap is 1/m for the interval's own rate, and under the exact identity
    # that rate is m = q / (n (1 - q) + a q), not the constant-force -log(r)/n.
    # The two agree on wide intervals but differ on the fast single-year
    # intervals, where the cap binds.
    q   <- 1 - r
    m_r <- q / (n * (1 - q) + a * q)

    return(min(a, 1/m_r))
  }

  return(n + 1/m - n/(1 - r))
}


#' The Coale-Demeny childhood ax value of one interval
#'
#' The West separation factor of interval \code{i} for a given first-interval
#' rate \code{m0}, expressed as the person-years lived in the interval. Used
#' for the first two intervals when a sex is supplied.
#' @param x,nx,m0,sex,ax_method,i As in \code{ex_interval_ratio}.
#' @return The value of ax for the interval.
#' @noRd
coale_demeny_interval_ax <- function(x, nx, m0, sex, ax_method, i) {

  C   <- coale_demeny_ax_coefs(m0 = m0, method = ax_method)
  a02 <- switch(sex,
                male   = C$male,
                female = C$female,
                total  = (C$male + C$female)/2)

  out <- a02[i] * nx[i] / c(1, 4)[i]
  return(out)
}


#' Death rates from survivorship ratios
#'
#' The central rate of the interval under the constant-force-of-mortality
#' assumption, \eqn{m = -\log(r)/n}, which is the rate the forward pipeline
#' recovers from a probability input and the rate the ax rule is evaluated at.
#' @param r Numeric vector of survivorship ratios, one per closed interval.
#' @param nx Numeric vector of interval widths, one per closed interval.
#' @return A numeric vector of death rates.
#' @noRd
ex_mx_from_r <- function(r, nx) {

  out <- -log(r)/nx
  return(out)
}


#' Apply the ax rule in force to a recovered set of rates
#'
#' Dispatches to the same ax estimators the forward pipeline uses, so that
#' the ax a \code{ex}-entered table settles on is exactly the ax the method
#' would assign to that table had it been entered from rates. The
#' Andreev-Kingkade rule is its own vector; the other methods share the
#' constant-force identity with the Coale-Demeny childhood adjustment.
#' @inheritParams LifeTable
#' @param nx Numeric vector of interval widths, one per age.
#' @param ax_method The ax method name, see \code{\link{LifeTable}}.
#' @param mx Numeric vector of recovered death rates.
#' @param qx Numeric vector of recovered death probabilities.
#' @return A numeric vector the same length as \code{x}.
#' @noRd
ex_ax_rule <- function(x, nx, mx, qx, sex, ax_method) {

  if (identical(ax_method, "andreev_kingkade")) {
    ax <- hmd_ax_vector(x = x, mx = mx, qx = qx, sex = sex)

  } else {
    ax <- lt_ax(x = x, ax = NULL, mx = mx, qx = qx, nx = nx, sex = sex,
                user = FALSE, ax_method = ax_method)
  }

  return(ax)
}
