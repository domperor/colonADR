.colonadr_nfkc <- function(x) {
  x <- stringi::stri_trans_nfkc(x)
  x <- gsub("[\u2010\u2011\u2012\u2013\u2014\u2015\u2212]", "-", x, perl = TRUE)
  x <- gsub("\\s+", " ", x, perl = TRUE)
  trimws(x)
}

.colonadr_normalize <- function(x) {
  stringi::stri_trans_tolower(.colonadr_nfkc(x))
}

.colonadr_contains_any <- function(x, terms) {
  grepl(paste(terms, collapse = "|"), x)
}

.colonadr_is_exact <- function(x, terms) {
  x %in% terms
}

#' Estimate colonoscopy quality indicators from structured diagnosis labels
#'
#' Each element of `x` represents one colonoscopy. Multiple findings within an
#' examination must be separated by a vertical bar (`|`). Japanese and English
#' structured diagnosis labels can be used in the same input vector.
#'
#' General diagnostic terms are matched after Unicode NFKC normalization and
#' lower-casing. Short morphology codes such as `Is` retain their case after
#' NFKC normalization so that the ordinary English word `is` is not counted.
#' Findings describing previous treatment, history, scars, or explicit absence
#' of a lesion are excluded at the finding level. Colonoscopies containing a
#' polyposis-associated diagnosis are excluded from the denominators and
#' numerators of all four indicators.
#' Missing and blank labels remain in the denominators as negative examinations.
#' Suspected lesions are included. Explicit residual or recurrent adenomas are
#' included unless their absence or previous treatment is recorded.
#'
#' @param x A character vector. Each element represents one colonoscopy.
#'   Multiple structured findings are separated by `|`.
#' @return A list containing `APC`, `ADR`, `PPC`, and `PDR`. 
#'
#' @examples
#' x <- c(
#'   "Tubular adenoma|Hyperplastic polyp",
#'   "No remarkable findings",
#'   "Post-polypectomy scar",
#'   "FAP",
#'   "Sessile serrated lesion"
#' )
#' apcadr(x)
#'
#' @export
apcadr <- function(x) {
  x[is.na(x)] <- ""
  nfkc_x <- .colonadr_nfkc(x)
  normalized_x <- .colonadr_normalize(x)

  polyposis_terms <- .colonadr_normalize(c(
    "\u5bb6\u65cf\u6027\u5927\u8178\u817a\u816b\u75c7",
    "familial adenomatous polyposis",
    "FAP",
    "\u30dd\u30ea\u30dd\u30fc\u30b7\u30b9",
    "polyposis",
    "Peutz-Jeghers",
    "Cronkhite-Canada",
    "Lynch",
    "HNPCC",
    "Cowden",
    "Gardner",
    "Turcot"
  ))

  is_polyposis <- .colonadr_contains_any(normalized_x, polyposis_terms)
  eligible <- !is_polyposis
  n_total <- length(normalized_x)
  n_eligible <- sum(eligible)

  if (n_eligible == 0L) {
    result <- list(
      APC = NA_real_, ADR = NA_real_, PPC = NA_real_, PDR = NA_real_
    )
    warning(
      "No eligible colonoscopies remained after excluding polyposis records.",
      call. = FALSE
    )
    return(result)
  }

  findings_by_exam <- strsplit(
    normalized_x[eligible],
    split = "\\|",
    perl = TRUE
  )
  findings_by_exam <- lapply(findings_by_exam, trimws)

  case_sensitive_findings_by_exam <- strsplit(
    nfkc_x[eligible],
    split = "\\|",
    perl = TRUE
  )
  case_sensitive_findings_by_exam <- lapply(
    case_sensitive_findings_by_exam,
    trimws
  )

  # Active adenomas are also counted as polyps below.
  polyp_substring_terms <- .colonadr_normalize(c(
    "\u30dd\u30ea\u30fc\u30d7",
    "polyp",
    "serrated",
    "\u92f8\u6b6f\u72b6",
    "\u5074\u65b9\u767a\u80b2\u578b",
    "\u904e\u5f62\u6210\u6027\u75c5\u5909",
    "laterally spreading tumor",
    "laterally spreading lesion"
  ))

  adenoma_substring_terms <- .colonadr_normalize(c(
    "\u817a\u816b",
    "adenoma"
  ))

  polyp_exact_terms <- .colonadr_nfkc(c(
    "HP", "Tsp", "Ts", "Tp", "Ua", "Uc", "Ip", "Isp", "Is",
    "SSA/P", "Mixed", "TSA", "IIa", "IIb", "IIc", "III", "LST",
    "SSL", "SSA", "SSP", "SSA-P", "SSLD", "SSL-D",
    "CMSEP", "LST-G", "LST-NG", "LST-G-H", "LST-G-HM",
    "LST-G-M", "LST-G-NM", "LST-NG-F", "LST-NG-FE", "LST-NG-PD"
  ))

  polyp_label_terms <- .colonadr_normalize("SSL with dysplasia")

  paris_pattern <- paste0(
    "^0-(Ip|Isp|Is|II[abc]|III)",
    "(\\s*\\+\\s*(0-)?(Ip|Isp|Is|II[abc]|III|[abc]))*$"
  )

  japanese_polyp_treated_terms <- c(
    "polypectomy\u5f8c",
    as.vector(outer(
      c("\u30dd\u30ea\u30fc\u30d7", "\u817a\u816b", "polyp", "adenoma"),
      c("\u5207\u9664\u5f8c", "\u6458\u9664\u5f8c", "\u6458\u51fa\u5f8c", "\u6cbb\u7642\u5f8c", "\u5207\u9664\u7622\u75d5"),
      paste0
    ))
  )

  english_polyp_treated_terms <- c(
    "after polypectomy",
    "post-polypectomy",
    "post polypectomy",
    "postpolypectomy",
    "status post polypectomy",
    "s/p polypectomy",
    "polypectomy scar",
    "polypectomy site scar",
    "history of polypectomy",
    "previous polypectomy",
    "prior polypectomy",
    "after polyp removal",
    "post-polyp removal",
    "post polyp removal",
    "polyp removal scar",
    "history of polyp",
    "history of polyps",
    "previous polyp",
    "prior polyp",
    "after adenoma resection",
    "post-adenoma resection",
    "post adenoma resection",
    "adenoma resection scar",
    "history of adenoma",
    "history of adenomas",
    "previous adenoma",
    "prior adenoma"
  )

  polyp_treated_terms <- .colonadr_normalize(c(
    japanese_polyp_treated_terms,
    english_polyp_treated_terms
  ))

  recurrent_adenoma_terms <- .colonadr_normalize(c(
    "\u907a\u6b8b\u817a\u816b", "\u518d\u767a\u817a\u816b", "residual adenoma", "recurrent adenoma"
  ))

  recurrent_adenoma_treated_terms <- .colonadr_normalize(c(
    as.vector(outer(
      c("\u907a\u6b8b\u817a\u816b", "\u518d\u767a\u817a\u816b"),
      c("\u5207\u9664\u5f8c", "\u6458\u9664\u5f8c", "\u6458\u51fa\u5f8c", "\u6cbb\u7642\u5f8c", "\u5207\u9664\u7622\u75d5"),
      paste0
    )),
    as.vector(outer(
      c("history of ", "previous ", "prior ", "after ", "post-", "post "),
      c("residual adenoma", "recurrent adenoma"),
      paste0
    )),
    as.vector(outer(
      c("residual adenoma", "recurrent adenoma"),
      c(" resection scar", " removal scar"),
      paste0
    ))
  ))

  polyp_negative_terms <- .colonadr_normalize(c(
    "no polyp",
    "no polyps",
    "without polyp",
    "without polyps",
    "negative for polyp",
    "negative for polyps",
    "no evidence of polyp",
    "no evidence of polyps",
    "\u30dd\u30ea\u30fc\u30d7\u306a\u3057",
    "\u30dd\u30ea\u30fc\u30d7\u8a8d\u3081\u305a",
    "free of polyps",
    "polyp-free"
  ))

  adenoma_negative_terms <- .colonadr_normalize(c(
    "no adenoma",
    "no adenomas",
    "without adenoma",
    "without adenomas",
    "negative for adenoma",
    "negative for adenomas",
    "no evidence of adenoma",
    "no evidence of adenomas",
    "\u817a\u816b\u306a\u3057",
    "\u817a\u816b\u3092\u8a8d\u3081\u306a\u3044",
    "\u817a\u816b\u9670\u6027",
    "\u817a\u816b\\s*\\(-\\)",
    "absence of adenoma",
    "no residual adenoma",
    "no recurrent adenoma"
  ))

  nonadenomatous_terms <- .colonadr_normalize(c(
    "\u975e\u817a\u816b\u6027", "\u817a\u816b\u3067\u306f\u306a\u3044", "nonadenomatous", "non-adenomatous"
  ))

  is_active_adenoma <- function(findings) {
    is_recurrent <-
      .colonadr_contains_any(findings, recurrent_adenoma_terms) &
      !.colonadr_contains_any(findings, recurrent_adenoma_treated_terms)

    .colonadr_contains_any(findings, adenoma_substring_terms) &
      (!.colonadr_contains_any(findings, polyp_treated_terms) | is_recurrent) &
      !.colonadr_contains_any(findings, recurrent_adenoma_treated_terms) &
      !.colonadr_contains_any(findings, adenoma_negative_terms) &
      !.colonadr_contains_any(findings, nonadenomatous_terms) &
      !.colonadr_contains_any(findings, polyp_negative_terms)
  }

  is_active_polyp <- function(findings, case_sensitive_findings) {
    ((.colonadr_contains_any(findings, polyp_substring_terms) |
        .colonadr_is_exact(case_sensitive_findings, polyp_exact_terms) |
        .colonadr_is_exact(findings, polyp_label_terms) |
        grepl(paris_pattern, case_sensitive_findings)) &
       !.colonadr_contains_any(findings, polyp_treated_terms) &
       !.colonadr_contains_any(findings, recurrent_adenoma_treated_terms) &
       !.colonadr_contains_any(findings, polyp_negative_terms) &
       !.colonadr_contains_any(findings, adenoma_negative_terms)) |
      is_active_adenoma(findings)
  }

  polyp_counts <- vapply(
    seq_along(findings_by_exam),
    function(index) as.integer(sum(is_active_polyp(
      findings_by_exam[[index]],
      case_sensitive_findings_by_exam[[index]]
    ))),
    integer(1)
  )

  adenoma_counts <- vapply(
    findings_by_exam,
    function(findings) as.integer(sum(is_active_adenoma(findings))),
    integer(1)
  )

  result <- list(
    APC = sum(adenoma_counts) / n_eligible,
    ADR = sum(adenoma_counts > 0L) / n_eligible,
    PPC = sum(polyp_counts) / n_eligible,
    PDR = sum(polyp_counts > 0L) / n_eligible
  )

  print(paste0(
    "APC:", format(result$APC, digits = 5),
    ", ADR:", format(result$ADR, digits = 5),
    ", PPC:", format(result$PPC, digits = 5),
    ", PDR:", format(result$PDR, digits = 5)
  ))

  result
}
