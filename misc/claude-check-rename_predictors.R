# Check for rename_predictors() (Bug 22).  Read-only.  devtools::load_all(".")
hr <- function(n, txt) cat("\n\n===== [", n, "] ", txt, " =====\n", sep = "")
ok <- function(label, got, want) {
  pass <- isTRUE(all.equal(got, want))
  cat(sprintf("   %-4s %s\n", if (pass) "OK" else "FAIL", label))
  if (!pass) { cat("        got : "); print(got); cat("        want: "); print(want) }
  invisible(pass)
}

# a stand-in for a regression object: rename_predictors() only reads attr var.mt
obj <- structure(list(), class = "sc_plm")
attr(obj, scan:::opt("mt")) <- "mt"

hr(1, "unit test of the renaming itself, style = 'full'")
old <- options(scan.rename.predictors = "full")
rn <- c("(Intercept)", "mt", "phaseB", "interB", "phaseA2", "interA2")
res <- scan:::rename_predictors(rn, obj)
print(data.frame(input = rn, output = res))
ok("intercept",  res[1], "Intercept")
ok("trend",      res[2], "Trend (mt)")
ok("level B",    res[3], "Level phase B (phaseB)")
ok("slope B",    res[4], "Slope phase B (interB)")
ok("level A2",   res[5], "Level phase A2 (phaseA2)")
ok("slope A2",   res[6], "Slope phase A2 (interA2)")

hr(2, "the anchoring bug: covariates containing 'inter' or 'phase'")
rn2 <- c("winter_score", "phase_of_moon", "interest", "myphaseB", "xinterB")
res2 <- scan:::rename_predictors(rn2, obj)
print(data.frame(input = rn2, output = res2))
ok("no covariate is renamed", res2, rn2)
cat("   (before anchoring, 'winter_score' became 'wSlope _score')\n")

hr(3, "'Intercept' must not be caught by the slope pattern")
ok("Intercept survives a second pass",
   scan:::rename_predictors("Intercept", obj), "Intercept")
ok("lowercase 'interX' IS a slope dummy",
   scan:::rename_predictors("interX", obj), "Slope phase X (interX)")

hr(4, "style = 'concise'")
options(scan.rename.predictors = "concise")
res4 <- scan:::rename_predictors(rn, obj)
print(data.frame(input = rn, output = res4))
ok("concise", res4,
   c("Intercept", "Trend", "Level B", "Slope B", "Level A2", "Slope A2"))
ok("covariates untouched", scan:::rename_predictors(rn2, obj), rn2)

hr(5, "style = 'no' and FALSE return the input unchanged")
options(scan.rename.predictors = "no");   ok("no",    scan:::rename_predictors(rn, obj), rn)
options(scan.rename.predictors = FALSE);  ok("FALSE", scan:::rename_predictors(rn, obj), rn)
options(scan.rename.predictors = "nonsense")
cat("   invalid option: "); print(try(scan:::rename_predictors(rn, obj), silent = TRUE))

hr(6, "custom dummy prefixes are honoured")
options(scan.rename.predictors = "full",
        scan.string.dummy.phase = "LVL", scan.string.dummy.slope = "SLP")
ok("custom prefixes",
   scan:::rename_predictors(c("LVLB", "SLPB", "phaseB"), obj),
   c("Level phase B (LVLB)", "Slope phase B (SLPB)", "phaseB"))
options(scan.string.dummy.phase = "phase", scan.string.dummy.slope = "inter")

hr(7, "a non-default measurement-time variable")
obj2 <- structure(list(), class = "sc_plm")
attr(obj2, scan:::opt("mt")) <- "day"
ok("trend uses the object's mt name",
   scan:::rename_predictors(c("day", "mt"), obj2), c("Trend (day)", "mt"))

hr(8, "end to end: the real print output")
options(scan.rename.predictors = "full")
print(plm(exampleAB$Johanna))
print(plm(exampleA1B1A2B2$Pawel))
print(hplm(exampleAB_50))
print(mplm(exampleAB_add, dvar = c("wellbeing", "depression")))
coef(plm(exampleAB$Johanna))

hr(9, "end to end with a non-default mt variable and an extra covariate")
print(plm(exampleAB_add, dvar = "wellbeing"))
print(plm(exampleAB_add, dvar = "wellbeing",
          update.formula = ".~. + cigarrets"))

hr(10, "concise end to end, then restore")
options(scan.rename.predictors = "concise")
print(plm(exampleAB$Johanna))
options(old)
cat("\n-- scan.rename.predictors restored to: ")
print(getOption("scan.rename.predictors"))
