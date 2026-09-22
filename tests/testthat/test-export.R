# export(): the engine resolver, select, footnotes, decimals, missing cells,
# and the structural parity of the two engines

with_engine <- function(engine, code) {
  old <- getOption("scan.export.engine")
  on.exit(options(scan.export.engine = old), add = TRUE)
  options(scan.export.engine = engine)
  force(code)
}

render <- function(x) {
  if (inherits(x, "gt_tbl")) return(as.character(gt::as_raw_html(x)))
  paste(as.character(x), collapse = "\n")
}

body_of <- function(tab) {
  txt <- render(tab)
  b <- regmatches(txt, regexpr("<tbody[^>]*>.*</tbody>", txt))
  if (length(b) == 0) txt else b
}

# ---------------------------------------------------------------- the engine

test_that("gt is the default engine and the option decides", {
  expect_identical(getOption("scan.export.engine"), "gt")
  expect_identical(scan:::.export_engine(), "gt")
  expect_s3_class(export(nap(exampleAB)), "gt_tbl")

  with_engine("kable", {
    expect_identical(scan:::.export_engine(), "kable")
    expect_s3_class(export(nap(exampleAB)), "kableExtra")
  })
})

test_that("an unusable engine setting is rejected", {
  for (v in list("GT", "Kable", "", NULL, 1)) {
    with_engine(v, expect_error(scan:::.export_engine()))
  }
})

test_that("latex output falls back to kable, word output to gt", {
  old_fmt <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  on.exit(knitr::opts_knit$set("rmarkdown.pandoc.to" = old_fmt), add = TRUE)

  expect_identical(scan:::.export_engine(), "gt")

  knitr::opts_knit$set("rmarkdown.pandoc.to" = "latex")
  expect_identical(scan:::.export_engine(), "kable")
  expect_s3_class(export(nap(exampleAB)), "knitr_kable")
  with_engine("kable", expect_identical(scan:::.export_engine(), "kable"))

  knitr::opts_knit$set("rmarkdown.pandoc.to" = "html")
  expect_identical(scan:::.export_engine(), "gt")

  knitr::opts_knit$set("rmarkdown.pandoc.to" = "docx")
  expect_identical(scan:::.export_engine(), "gt")
  # kable can not produce usable word tables: the engine switches to gt
  with_engine("kable", expect_identical(scan:::.export_engine(), "gt"))
})

# ---------------------------------------------------------------- the select

test_that(".select keeps a data frame, renames and orders", {
  sel <- scan:::.select
  df <- data.frame(
    Case = c("a", "b"), NAP = c(90, 80), Rank = c(1, 2), P = c(0.01, 0.2),
    stringsAsFactors = FALSE
  )

  r <- sel(df, "Case")
  expect_true(is.data.frame(r))
  expect_identical(names(r), "Case")
  expect_equal(nrow(r), 2)

  expect_identical(names(sel(df, 2)), "NAP")
  expect_identical(names(sel(df, c(Effect = "NAP"))), "Effect")
  expect_equal(sel(df, c(Effect = "NAP"))[[1]], df$NAP)

  expect_identical(names(sel(df, c("Case", "P"))), c("Case", "P"))
  expect_identical(names(sel(df, c("P", "Case"))), c("P", "Case"))
  expect_identical(names(sel(df, c(A = "Case", B = "NAP"))), c("A", "B"))
  expect_identical(names(sel(df, c(A = "Case", "NAP"))), c("A", "NAP"))
  expect_identical(names(sel(df, c(A = 1, B = 3))), c("A", "B"))
  expect_identical(names(sel(df, c(A = 1, 3))), c("A", "Rank"))
  expect_equal(sel(df, c(A = 1, 3))[[2]], df$Rank)

  for (v in list("none", "", NA, NULL, FALSE)) {
    expect_identical(sel(df, v), df)
  }

  expect_error(sel(df, "Nap"))
  expect_error(sel(df, 9))
  expect_error(sel(df, 0))
  e <- tryCatch(sel(df, c("Case", "Nap", "Rnk")), error = conditionMessage)
  expect_true(grepl("Nap", e, fixed = TRUE) && grepl("Rnk", e, fixed = TRUE))
})

test_that("select works through export, also when it drops a p column", {
  res <- nap(exampleAB)
  for (eng in c("gt", "kable")) {
    with_engine(eng, {
      expect_no_error(export(res, select = "Case"))
      expect_no_error(export(res, select = c("Case", "NAP")))
      expect_no_error(export(res, select = c(Fall = "Case", "NAP")))
      expect_no_error(export(res, select = NULL))
      expect_no_error(export(res, select = c("NAP", "d")))
      # tau_u lists the cases as row groups, its columns start with Model
      expect_no_error(export(tau_u(exampleAB), select = c("Model", "Tau")))
      expect_no_error(export(pnd(exampleAB), select = c("Case", "PND")))
      expect_no_error(export(smd(exampleAB), select = c(Fall = "Case", "mA")))
      expect_error(export(res, select = "Nope"))
    })
  }
})

test_that(".nice_p formats p values and survives empty input", {
  expect_identical(scan:::.nice_p(numeric(0)), character(0))
  expect_identical(scan:::.nice_p(NA_real_), NA_character_)
  expect_identical(
    scan:::.nice_p(c(0.2, 0.04, 0.005, 0.0001, 1)),
    c(".20", "<.05", "<.01", "<.001", "1.00")
  )
})

# -------------------------------------------------------------- the footnote

test_that(".footnote picks between user input and default", {
  fn <- scan:::.footnote
  expect_identical(fn(c("a", "b"), "default"), c("a", "b"))
  expect_identical(fn(NA, "default"), "default")
  expect_identical(fn(NA_character_, "default"), "default")
  expect_null(fn(NULL, "default"))
  expect_null(fn(NA))
  expect_identical(fn(c("x", NA), "default"), c("x", NA))
})

test_that("footnotes reach the table in both engines", {
  objects <- list(
    scdf = exampleAB,
    summary = summary(exampleAB),
    describe = describe(exampleAB),
    nap = nap(exampleAB),
    smd = smd(exampleAB),
    tau_u = tau_u(exampleAB),
    pand = pand(exampleAB),
    outlier = outlier(exampleAB, method = "SD"),
    plm = plm(exampleAB$Johanna)
  )
  for (eng in c("gt", "kable")) {
    with_engine(eng, {
      for (nm in names(objects)) {
        obj <- objects[[nm]]
        expect_no_error(export(obj))
        expect_no_error(export(obj, footnote = ""))
        txt <- render(export(obj, footnote = c("first line", "second line")))
        expect_true(grepl("first line", txt, fixed = TRUE))
        expect_true(grepl("second line", txt, fixed = TRUE))
        txt <- render(export(obj, footnote = "single line"))
        expect_true(grepl("single line", txt, fixed = TRUE))
      }
      expect_false(grepl("Note", render(export(nap(exampleAB), footnote = NULL))))
      txt <- render(export(nap(exampleAB), footnote = NA_character_))
      expect_false(grepl("Note[^<]*NA", txt))
    })
  }
})

test_that("html tags in footnotes are rendered, not escaped", {
  for (eng in c("gt", "kable")) {
    with_engine(eng, {
      txt <- render(export(nap(exampleAB), footnote = "a <b>bold</b> word"))
      expect_true(grepl("<b", txt, fixed = TRUE))
      expect_false(grepl("&lt;b&gt;", txt, fixed = TRUE))
      txt <- render(export(summary(exampleAB)))
      expect_false(grepl("&lt;i&gt;", txt, fixed = TRUE))
      expect_false(grepl("&lt;br&gt;", txt, fixed = TRUE))
    })
  }
})

test_that("latex output keeps escaping the footnote", {
  d <- data.frame(a = 1:2, b = 3:4)
  k_html <- kableExtra::kable_styling(kableExtra::kbl(d, format = "html"))
  k_tex <- kableExtra::kable_styling(kableExtra::kbl(d, format = "latex"))
  t_html <- paste(
    as.character(scan:::.add_footnote(k_html, "bold <b>x</b>, 50%")),
    collapse = "\n"
  )
  t_tex <- paste(
    as.character(scan:::.add_footnote(k_tex, "bold <b>x</b>, 50%")),
    collapse = "\n"
  )
  expect_true(grepl("<b>x</b>", t_html, fixed = TRUE))
  expect_true(grepl("bold", t_tex, fixed = TRUE))
  expect_true(grepl("\\\\%", t_tex))
  expect_identical(scan:::.add_footnote(42, "x"), 42)
})

test_that("the kable engine keeps unicode and markup outside latex", {
  old <- getOption("knitr.table.format")
  on.exit(options(knitr.table.format = old), add = TRUE)

  options(knitr.table.format = "html")
  with_engine("kable", {
    expect_true(grepl("R\u00b2", render(export(plm(exampleAB$Johanna)))))
    txt <- render(export(nap(exampleAB), footnote = "a <b>bold</b> word"))
    expect_true(grepl("<b", txt, fixed = TRUE))
  })

  options(knitr.table.format = "latex")
  with_engine("kable", {
    txt <- render(export(plm(exampleAB$Johanna)))
    expect_false(grepl("R\u00b2", txt))
    expect_true(grepl("R-squared", txt, fixed = TRUE))
  })
})

test_that("unicode and markup are spelled out for latex", {
  plain <- scan:::.latex_plain
  expect_false(grepl("Φ", plain("Φ = .8")))
  expect_true(grepl("Phi", plain("Φ = .8")))
  expect_true(grepl("Phi-squared", plain("Φ² = .8")))
  expect_true(grepl("Chi-squared", plain("χ² = 4")))
  expect_true(grepl("R-squared", plain("R² = .5")))
  expect_false(grepl("<br>", plain("a<br>b"), fixed = TRUE))
  expect_false(grepl("<b>", plain("a <b>bold</b> word"), fixed = TRUE))
  expect_false(grepl("**", plain("a **bold** word"), fixed = TRUE))
})

# -------------------------------------------------------- numbers and blanks

test_that("the decimals argument reaches both engines", {
  n_dec <- function(v) nchar(sub("^[^.]*\\.", "", v))
  numbers <- function(tab) {
    b <- body_of(tab)
    cells <- regmatches(b, gregexpr("<t[dh][^>]*>.*?</t[dh]>", b))[[1]]
    v <- trimws(gsub("<[^>]*>|&nbsp;|−", "", cells))
    v <- v[grepl("^-?[0-9]+\\.[0-9]+$", v)]
    v[v != "1.00"]
  }
  objects <- list(
    tau_u = tau_u(exampleAB),
    smd = smd(exampleAB),
    overlap = overlap(exampleAB),
    trend = trend(exampleAB$Johanna)
  )
  for (d in c(1, 2, 4)) {
    for (eng in c("gt", "kable")) {
      with_engine(eng, {
        for (nm in names(objects)) {
          v <- numbers(export(objects[[nm]], decimals = d, round = d))
          expect_true(length(v) > 0)
          expect_true(all(n_dec(v) == d))
        }
      })
    }
  }
})

test_that("NA, NaN and Inf are shown as empty cells", {
  d <- data.frame(
    Case = c("a", "b", "c", "d"),
    Value = c(1.5, NA, NaN, Inf),
    Other = c(NA_real_, 2.5, 3.5, 4.5),
    check.names = FALSE
  )
  for (eng in c("gt", "kable")) {
    with_engine(eng, {
      b <- body_of(scan:::.create_table(
        d, caption = "test", footnote = "note", decimals = 2
      ))
      expect_false(grepl("NaN", b, fixed = TRUE))
      expect_false(grepl("Inf", b, fixed = TRUE))
      expect_false(grepl(">NA<", b, fixed = TRUE))
      expect_true(grepl("1.50", b, fixed = TRUE))
      expect_true(grepl("2.50", b, fixed = TRUE))
      expect_true(grepl("4.50", b, fixed = TRUE))
      for (v in c("a", "b", "c", "d")) {
        expect_true(grepl(paste0(">\\s*", v, "\\s*<"), b))
      }
    })
  }

  x <- suppressWarnings(tau_u(exampleAB$Johanna))
  for (eng in c("gt", "kable")) {
    with_engine(eng, {
      b <- body_of(export(x))
      expect_false(grepl("NaN", b, fixed = TRUE))
      expect_false(grepl(">NA<", b, fixed = TRUE))
    })
  }
})

# ------------------------------------------------------------- engine parity

# The set of column names of a table. gt names its cells
# "<row group>  <column>" and repeats them for every row group, kable lists
# the columns once; so both sides are reduced to the set of names.
parity_columns <- function(tab) {
  txt <- render(tab)
  strip <- function(x) {
    x <- gsub("<[^>]*>|&nbsp;", "", x)
    x <- gsub("&lt;", "<", x, fixed = TRUE)
    x <- gsub("&gt;", ">", x, fixed = TRUE)
    x <- gsub("&amp;", "&", x, fixed = TRUE)
    trimws(x)
  }
  if (inherits(tab, "gt_tbl")) {
    ids <- gsub(
      'headers="|"', "",
      regmatches(txt, gregexpr('headers="[^"]+"', txt))[[1]]
    )
    ids <- trimws(sub("^.*\\s\\s", "", ids))
    return(sort(unique(ids[nzchar(ids)])))
  }
  head <- regmatches(txt, regexpr("<thead>.*?</thead>", txt))
  rows <- regmatches(head, gregexpr("<tr[^>]*>.*?</tr>", head))[[1]]
  last <- rows[length(rows)]
  lab <- strip(regmatches(last, gregexpr("<th[^>]*>.*?</th>", last))[[1]])
  sort(unique(lab[nzchar(lab)]))
}

parity_spanners <- function(tab) {
  txt <- render(tab)
  if (inherits(tab, "gt_tbl")) {
    m <- regmatches(
      txt, gregexpr('class="gt_column_spanner"[^>]*>[^<]*', txt)
    )[[1]]
    return(sort(trimws(gsub(".*>", "", m))))
  }
  head <- regmatches(txt, regexpr("<thead>.*?</thead>", txt))
  rows <- regmatches(head, gregexpr("<tr[^>]*>.*?</tr>", head))[[1]]
  if (length(rows) < 2) return(character(0))
  lab <- trimws(gsub(
    "<[^>]*>|&nbsp;", "",
    regmatches(rows[1], gregexpr("<th[^>]*>.*?</th>", rows[1]))[[1]]
  ))
  sort(lab[nzchar(lab)])
}

parity_row_groups <- function(tab) {
  txt <- render(tab)
  if (inherits(tab, "gt_tbl")) {
    m <- regmatches(
      txt, gregexpr('class="gt_group_heading"[^>]*>[^<]*', txt)
    )[[1]]
    return(sort(trimws(gsub(".*>", "", m))))
  }
  # pack_rows() writes the label of a row group in bold into a spanning cell
  m <- regmatches(
    txt, gregexpr('<td colspan="[0-9]+"[^>]*><strong>[^<]*', txt)
  )[[1]]
  sort(trimws(gsub(".*<strong>", "", m)))
}

expect_engine_parity <- function(object, args = list()) {
  old <- getOption("scan.export.engine")
  on.exit(options(scan.export.engine = old), add = TRUE)
  res <- list()
  for (eng in c("gt", "kable")) {
    options(scan.export.engine = eng)
    tab <- do.call(export, c(list(object), args))
    res[[eng]] <- list(
      col = parity_columns(tab),
      sp = parity_spanners(tab),
      rg = parity_row_groups(tab)
    )
  }
  expect_identical(res$gt$col, res$kable$col)
  expect_identical(res$gt$sp, res$kable$sp)
  expect_equal(length(res$gt$rg), length(res$kable$rg))
}

test_that("both engines build the same table structure", {
  objects <- list(
    scdf = exampleAB,
    "scdf summary" = summary(exampleAB),
    describe = describe(exampleAB),
    overlap = overlap(exampleAB),
    nap = nap(exampleAB),
    pnd = pnd(exampleAB),
    pem = pem(exampleAB),
    pet = pet(exampleAB),
    ird = ird(exampleAB),
    smd = smd(exampleAB),
    tau_u = tau_u(exampleAB),
    corrected_tau = corrected_tau(exampleAB$Johanna),
    between_smd = between_smd(exampleAB),
    trend = trend(exampleAB$Johanna),
    autocorr = autocorr(exampleAB),
    cdc = cdc(exampleAB),
    outlier = outlier(exampleAB, method = "SD"),
    "pand sort" = pand(exampleAB, method = "sort"),
    "pand minimum" = pand(exampleAB, method = "minimum"),
    plm = plm(exampleAB$Johanna),
    "plm poisson" = plm(exampleAB$Johanna, family = "poisson")
  )
  for (nm in names(objects)) expect_engine_parity(objects[[nm]])
  expect_engine_parity(tau_u(exampleAB), list(meta = TRUE))
})

test_that("both engines agree for the slower models as well", {
  skip_on_cran()
  expect_engine_parity(hplm(exampleAB))
  expect_engine_parity(hplm(exampleAB), list(casewise = TRUE))
  expect_engine_parity(mplm(exampleAB_add, dvar = c("wellbeing", "cigarrets")))
  set.seed(1234)
  expect_engine_parity(rand_test(exampleAB, number = 100))
})
