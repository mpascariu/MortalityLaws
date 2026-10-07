# <picture><source media="(prefers-color-scheme: dark)" srcset="man/figures/hex-mortalitylaws-lime.png"><img src="man/figures/hex-mortalitylaws-dark.png" align="right" width="175" height="202" alt="MortalityLaws hex logo"></picture> MortalityLaws: Parametric Mortality Models, Life Tables and HMD

[![CRAN version](https://www.r-pkg.org/badges/version/MortalityLaws)](https://cran.r-project.org/package=MortalityLaws)
[![R-CMD-check](https://github.com/mpascariu/MortalityLaws/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mpascariu/MortalityLaws/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/mpascariu/MortalityLaws/actions/workflows/pkgdown.yaml/badge.svg)](https://mpascariu.github.io/MortalityLaws/)
[![codecov](https://codecov.io/github/mpascariu/MortalityLaws/branch/master/graphs/badge.svg)](https://app.codecov.io/github/mpascariu/MortalityLaws)
[![downloads](https://cranlogs.r-pkg.org/badges/grand-total/MortalityLaws)](https://CRAN.R-project.org/package=MortalityLaws)
[![downloads (monthly)](https://cranlogs.r-pkg.org/badges/MortalityLaws)](https://CRAN.R-project.org/package=MortalityLaws)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![license](https://img.shields.io/badge/License-MIT-blue.svg)](https://github.com/mpascariu/MortalityLaws/blob/master/LICENSE)
[![issues](https://img.shields.io/github/issues-raw/mpascariu/MortalityLaws.svg)](https://github.com/mpascariu/MortalityLaws/issues)

A mortality law is a small parametric function that describes how a population dies
out with age: high mortality in infancy, a hump at young adult ages, and an
exponential climb from middle age onward. `MortalityLaws` fits these laws to observed
deaths, exposures and rates, turns any fit or any observed schedule into a full life
table, and pulls the underlying data straight from the Human Mortality Database and
its national siblings.

## The problem this solves

Mortality data rarely arrive in the shape the question needs. A statistical office
publishes deaths and exposures by age and year. The resulting curve is jagged where
deaths are few, thin at the oldest ages, and often closed at 85+ or 90+ with a single
wide interval. What you usually want is the opposite: a smooth description of the age
pattern of death that you can compare across populations and years, integrate into a
life table, or carry past the last observed age.

Parametric mortality laws are the classical answer, and there are many of them. Each
is a small formula with a handful of parameters, each is good at some part of the age
range and quietly wrong somewhere else. Doing this by hand means a spreadsheet of
starting values, one script per formula, and no two fits graduated quite the same
way. (Picking a law by eye is a time-honoured tradition, and reproducible only by
accident.)

`MortalityLaws` removes the assembly line. The package ships 38 laws, 8 fitting
objectives (two likelihoods and six losses) and 6 accepted life-table inputs. That is
38 x 8 x 6 = 1,824 combinations of law, loss and input, and every one of them is a
single function call.

## From one observed curve to a whole life table

![Age-specific death rates of England and Wales females in 1950 as black crosses on a log scale, with four fitted laws overlaid on the left (Heligman-Pollard and Siler over ages 0-100, Kannisto-Makeham over ages 60-100 and Gompertz over ages 40-80), and on the right the life table the fit implies: survivorship l(x) with radix 100,000 and life expectancy e(x) from a LawTable built on the Heligman-Pollard fit.](man/figures/README-hero.png)

## What it does

| capability | how |
|---|---|
| Fit a law to observed data | `MortalityLaw()`, from deaths and exposures, `mx`, or `qx` |
| Supply your own law | `custom.law`, any function of `x` and `par` you can write |
| Judge whether a fit deserves trust | `plot.MortalityLaw()`: observed versus fitted, plus four residual diagnostics |
| Build full and abridged life tables | `LifeTable()`: 6 input types, 4 `ax` methods, and `close` / `omega` for the tail |
| Convert between mortality indicators | `convertFx()` across `mx`, `qx`, `dx`, `lx`, `Lx`, `Tx`, `ex`; `LawTable()` turns a law and its parameters into a whole table |
| Download demographic data | `ReadHMD()`, `ReadJMD()`, `ReadCHMD()`, `ReadAHMD()`: 50 HMD countries, 6 interval formats from 1x1 to 5x10 |
| Look things up | `availableLaws()`, `availableLF()`, `availableHMD()`, `dispersion()` |

## Installation

Install the stable release from CRAN:

```r
install.packages("MortalityLaws")
```

The development version comes from GitHub. `pak` is the recommended installer, and
like any install from source it needs a working development toolchain:

```r
# install.packages("pak")
pak::pak("mpascariu/MortalityLaws")
```
Check that everything works:

```r
library(MortalityLaws)
availableLaws()   # the catalogue: 38 laws with formulas and lifespan types
```

## Updating

For the CRAN version, simply re-run `install.packages("MortalityLaws")` every so
often. For the development version, run `pak::pak("mpascariu/MortalityLaws")` again
to pull the latest commits.

## Documentation

- **Site:** <https://mpascariu.github.io/MortalityLaws/>
- **Intro (start here):** <https://mpascariu.github.io/MortalityLaws/articles/Intro.html>
- **Mortality models:** <https://mpascariu.github.io/MortalityLaws/articles/Mortality-models.html>
- **Life tables:** <https://mpascariu.github.io/MortalityLaws/articles/Life-tables.html>
- **Function reference:** <https://mpascariu.github.io/MortalityLaws/reference/index.html>
- **Offline:** `vignette("Intro", package = "MortalityLaws")`

## Citation

To cite `MortalityLaws` in publications use:

> Pascariu M (2026). *MortalityLaws: Parametric Mortality Models, Life Tables and
> HMD*. R package version 3.0.0, <https://github.com/mpascariu/MortalityLaws>.

A BibTeX entry for LaTeX users is:

```bibtex
  @Manual{,
    title = {MortalityLaws: Parametric Mortality Models, Life Tables and HMD},
    author = {Marius D. Pascariu},
    year = {2026},
    note = {R package version 3.0.0},
    url = {https://github.com/mpascariu/MortalityLaws},
  }
```

## Contributing

Issues and pull requests are welcome. If `MortalityLaws` misbehaves, please open an
issue with a minimal reproducible example at
<https://github.com/mpascariu/MortalityLaws/issues>, and see
[CONTRIBUTING.md](https://github.com/mpascariu/MortalityLaws/blob/master/CONTRIBUTING.md).
This project is released with a
[Contributor Code of Conduct](https://github.com/mpascariu/MortalityLaws/blob/master/CODE_OF_CONDUCT.md).
