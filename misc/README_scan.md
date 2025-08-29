<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/scan)](https://CRAN.R-project.org/package=scan)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/scan)](https://cranlogs.r-pkg.org/badges/grand-total/scan)
<!-- badges: end -->

# scan <img src="man/figures/logo.png" align="right" width="120" alt="scan logo" />

**Single‑Case Data Analysis in R** — a comprehensive toolkit for managing, analyzing, simulating, and visualizing single‑case experimental design (SCED) data.

- **Website:** https://jazznbass.github.io/scan  
- **Book (handbook):** https://jazznbass.github.io/scan-Book/  
- **Function reference:** https://jazznbass.github.io/scan/reference/  
- **Issues & development:** https://github.com/jazznbass/scan

---

## Key features

### Data management
- `scdf()` — create SCED data objects
- `as_scdf()`, `as.data.frame.scdf()` — import/export between formats
- `read_scdf()`, `write_scdf()` — file I/O
- `select_cases()`, `select_phases()`, `subset.scdf()` — filter & subset
- `add_l2()` — add level‑2 data
- Transform helpers: `transform.scdf()`, `across_cases()`, `rowwise()`, `moving_mean()`, `moving_median()`, `local_regression()`, `center_at()`, `set_na_at()`, `first_of()`, `rescale()`
- Utilities: `fetch()`, `na.omit.scdf()`, `is.scdf()`, `sample_names()`, `add_dummy_variables()`

### Analyses & modeling
- **Overlap & nonoverlap:** `pnd()`, `pem()`, `pet()`, `pand()`, `nap()`, `overlap()`
- **Effect sizes:** `tau_u()`, `corrected_tau()`, `smd()`, `between_smd()`
- **Models:** `plm()` (piecewise), `hplm()` (hierarchical), `mplm()` (multivariate), `bplm()` (Bayesian)
- **Other tools:** `autocorr()`, `describe()`, `trend()`, `outlier()`, `cdc()`, `ird()`, `rci()`
- **Randomization & power:** `rand_test()`, `power_test()`
- **ANOVA tables for PLM variants:** `anova.sc_plm()`, `anova.sc_hplm()`, `anova.sc_mplm()`

### Visualization & export
- Plotting single‑case data: `plot.scdf()` (see also the companion package **scplot**)
- Export helpers for tables/figures: `export()`; `plot_rand()`; printing and summaries: `print.scdf()`, `summary.scdf()`
- _Deprecated:_ `plotSC()`, `style_plot()`

### Simulation & design
- `design()`, `estimate_design()`, `random_scdf()` — construct and simulate SCEDs

### Interactive GUI
- `shinyscan()` — Shiny app for point‑and‑click workflows
- RStudio addin: `import_scdf()`

> For a structured walk‑through, see the Book’s chapters on **Create/Work with scdf**, **Overlapping indices**, **Piecewise regressions**, **Randomization tests**, and **Power analyses**.

---

## Installation

Stable (CRAN):
```r
install.packages("scan")
```

Development (GitHub):
```r
# install.packages("pak") # once
pak::pak("jazznbass/scan")
```

---

## Quick start

```r
library(scan)

# 1) Simulate a simple AB dataset
set.seed(1)
dat <- random_scdf(m = 1, phases = c(A = 10, B = 12))

# 2) Inspect & plot
dat
plot(dat)

# 3) Piecewise model (AB)
fit <- plm(dat)
summary(fit)

# 4) Effect size (Tau-U)
tau_u(dat)

# 5) Randomization test (if applicable)
rand_test(dat)

# 6) Launch the GUI (optional)
# shinyscan()
```

---

## Typical workflow snippets

### Data wrangling
```r
df <- data.frame(
  case = 1, session = 1:12,
  phase = c(rep("A", 6), rep("B", 6)),
  values = rnorm(12, 10, 2)
)
sc <- as_scdf(df, case = "case", session = "session", phase = "phase", dvar = "values")

# Transformations and selections
sc <- transform(sc, values_z = as.numeric(scale(values)))
sc_ab <- select_phases(sc, A, B)
```

### Modeling & reporting
```r
fit <- plm(sc_ab)            # piecewise level/slope model
es  <- tau_u(sc_ab)          # effect size
ovl <- overlap(sc_ab)        # overlap indices

# Export examples (adjust paths as needed)
# export(fit, file = "results/plm_summary.html")
# export(ovl, file = "results/overlap_table.tex")
```

---

## Learning resources

- **Start here:** Introduction & “The scan package” chapters in the Book  
- **Analyses:** Chapters on **Overlap indices**, **Piecewise regressions**, **Multilevel PLM**, **Multivariate/Bayesian PLM**, **Randomization tests**, **Power analyses**  
- **Export & GUI:** Chapters **Exporting scan results** and **Graphical user interface – Shinyscan**  
- **Function reference:** Full documentation at https://jazznbass.github.io/scan/reference/

---

## Citation

Please cite **scan** and (optionally) the Book:

```r
citation("scan")
```
Book: Wilbert, J. (2025). *Analyzing single-case data with R and scan*. https://doi.org/10.5281/zenodo.5713559

---

## Contributing

Issues and feature requests: https://github.com/jazznbass/scan/issues  
Pull requests welcome. Please include a minimal reproducible example (data + code).

---

## License

GPL-3 (see `LICENSE`).

---

## Acknowledgments

Developed by Jürgen Wilbert and Timo Lueke. Thanks to contributors and users of **scan** and **scplot**.
