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
#' numerators of all indicators.
#' Missing and blank labels remain in the denominators as negative examinations.
#' `ADRloose` includes adenoma findings that are suspected or also match a
#' polyp term in the same finding. `ADRstrict` requires at least one adenoma
#' finding without either feature. `APCstrict` and `APCloose` count matching
#' findings per eligible examination using the corresponding ADR criteria.
#' `PPCstrict` and `PDRstrict` exclude suspected polyp or adenoma findings.
#' `PPCloose` and `PDRloose` include suspected findings and accept an exact
#' polyp label after removing leading or trailing suspicion wording.
#' `PPC` and `PDR` retain the original matching rules for compatibility.
#' Explicit absence of recurrence or residual disease takes precedence over
#' the inclusion of residual or recurrent adenomas.
#'
#' @param x A character vector. Each element represents one colonoscopy.
#'   Multiple structured findings are separated by `|`.
#' @param preview_n Maximum number of findings per preview group, in input
#'   order. Use 0 (the default) to return only the indicators.
#' @return A list containing `APC`, `ADR`, `ADRstrict`, `ADRloose`, `PPC`,
#'   `PDR`, `APCstrict`, `APCloose`, `PPCstrict`, `PPCloose`,
#'   `PDRstrict`, and `PDRloose`.
#'   For compatibility, `APC` equals `APCloose` and `ADR` equals `ADRloose`.
#'   If `preview_n` is positive, `preview` contains four tables
#'   with original input row, within-row finding index, and finding text.
#'   The adenoma and polyp loose-only groups may overlap.
#'
#' @examples
#' x <- c(
#'   "Tubular adenoma|Hyperplastic polyp",
#'   "Suspected adenoma",
#'   "No remarkable findings",
#'   "Post-polypectomy scar",
#'   "FAP",
#'   "Sessile serrated lesion"
#' )
#' apcadr(x)
#'
#' @export
apcadr <- function(x, preview_n = 0L) {
  if (length(preview_n) != 1L || !is.numeric(preview_n) ||
      is.complex(preview_n) || !is.finite(preview_n) ||
      preview_n < 0 || preview_n != floor(preview_n)) {
    stop("preview_n must be a non-negative whole number.", call. = FALSE)
  }
  preview_open <- preview_n > 0L
  if (preview_open) {
    empty <- data.frame(exam_row = integer(), finding_index = integer(),
                        finding = character(), stringsAsFactors = FALSE)
    preview <- list(adenoma_strict = empty, adenoma_loose_only = empty,
                    polyp_strict_without_adenoma = empty, polyp_loose_only = empty)
  }

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
  n_eligible <- sum(eligible)
  if (preview_open) exam_rows <- which(eligible)

  if (n_eligible == 0L) {
    result <- list(
      APC = NA_real_, ADR = NA_real_, ADRstrict = NA_real_, ADRloose = NA_real_,
      PPC = NA_real_, PDR = NA_real_,
      APCstrict = NA_real_, APCloose = NA_real_,
      PPCstrict = NA_real_, PPCloose = NA_real_,
      PDRstrict = NA_real_, PDRloose = NA_real_
    )
    warning(
      "No eligible colonoscopies remained after excluding polyposis records.",
      call. = FALSE
    )
    if (preview_n > 0L) result$preview <- preview
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
    "\u904e\u5f62\u6210",
    "laterally spreading tumor",
    "laterally spreading lesion"
  ))

  adenoma_substring_terms <- .colonadr_normalize(c(
    "\u817a\u816b",
    "adenoma"
  ))

  polyp_exact_terms <- .colonadr_nfkc(c(
    "HP", "Tsp", "Ts", "Tp", "Ua", "Uc", "Ip", "Isp", "Is",
    "SSA/P", "Mixed", "TSA", "IIa", "IIb", "IIc", "III",
    "SSL", "SSA", "SSP", "SSA-P", "SSAP", "SuSA", "SSLD", "SSL-D", "CMSEP"
  ))

  polyp_case_sensitive_substring <- "LST"
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

  japanese_absence <- paste0(
    "\\s*[:=]?\\s*(?:\u306f|\u304c|\u3092)?\\s*",
    "(?:\u306a\u3057|\u7121\u3057|\u306a\u3044|\u8a8d\u3081(?:\u3089\u308c)?(?:\u305a|\u306a\u3044|\u307e\u305b\u3093)|",
    "(?:\u307f|\u898b)\u3089\u308c(?:\u305a|\u306a\u3044|\u307e\u305b\u3093)|\u7591\u308f(?:\u308c)?(?:\u305a|\u306a\u3044|\u307e\u305b\u3093)|",
    "\u7591\u3044(?:\u306f)?(?:\u306a\u3057|\u7121\u3057|\u306a\u3044)|",
    "\u9670\u6027|\\(\\s*-\\s*\\))"
  )
  japanese_finding_detail <- "(?:\u6027\u75c5\u5909|\u75c5\u5909|(?:\u306e|\u3092\u7591\u3046|\u3092\u793a\u5506\u3059\u308b)?\u6240\u898b)?"

  polyp_negative_terms <- .colonadr_normalize(c(
    "no polyp",
    "no polyps",
    "without polyp",
    "without polyps",
    "negative for polyp",
    "negative for polyps",
    "no evidence of polyp",
    "no evidence of polyps",
    "free of polyps",
    "polyp-free",
    paste0("(?:\u30dd\u30ea\u30fc\u30d7|\u904e\u5f62\u6210|lst(?:-[a-z]+)*)",
           japanese_finding_detail, japanese_absence)
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
    "absence of adenoma",
    "no residual adenoma",
    "no recurrent adenoma",
    paste0("\u817a\u816b", japanese_finding_detail, japanese_absence)
  ))

  nonadenomatous_terms <- .colonadr_normalize(c(
    "\u975e\u817a\u816b\u6027", "\u975e\u816b\u760d\u6027",
    "nonadenomatous", "non-adenomatous", "non-neoplastic",
    "nonneoplastic", "non neoplastic"
  ))

  japanese_recurrence_negative_pattern <- paste0(
    "(?:\u518d\u767a|\u907a\u6b8b|\u6b8b\u5b58)", japanese_finding_detail, japanese_absence
  )

  english_lesion_modifier <- paste0(
    "(?:(?:residual|recurrent|remaining|suspected|possible|probable|visible|hyperplastic)",
    "(?:\\s*(?:and/or|and|or|/)\\s*|\\s+))*"
  )
  english_lesion <- paste0(
    "(?:", english_lesion_modifier,
    "(?:adenoma(?:s|ta)?|adenomatous(?:\\s+(?:polyp|lesion)s?)?|polyps?|",
    "lst(?:-[a-z]+)*)|recurrences?|regrowth|residual\\s+(?:disease|tissue|lesions?))"
  )
  english_negative_pattern <- paste0(
    "\\b(?:(?:no|without)(?:\\s+(?:endoscopic\\s+)?",
    "(?:(?:evidence|signs?)\\s+of|findings?\\s+(?:suggestive\\s+)?of))?|",
    "negative\\s+for|(?:absence|free)\\s+of)\\s+(?:local\\s+)?",
    english_lesion, "\\b|",
    "\\b", english_lesion, "\\b(?:\\s+(?:recurrence|regrowth))?",
    "\\s*[:=-]?\\s*(?:(?:is|was|are|were)\\s+)?",
    "(?:not\\s+(?:seen|detected|identified|observed|found|present|suspected)\\b|",
    "(?:absent|none)\\b|negative\\b(?!\\s+(?:resection\\s+)?margins?\\b)|",
    "\\(\\s*-\\s*\\))"
  )
  negative_pattern <- paste(
    c(polyp_negative_terms, adenoma_negative_terms,
      japanese_recurrence_negative_pattern, english_negative_pattern),
    collapse = "|"
  )

  suspected_pattern <- paste0(
    "\u7591(?:\u3044)?|(?:\u5426\u5b9a|\u9664\u5916)\u3067\u304d(?:\u306a[\u3044\u304f]|\u305a)|\\?|",
    "\\b(?:suspect(?:ed)?|suspicious for|possibl[ey]|probabl[ey]|",
    "suggestive of|favou?r(?:ed|s)?|",
    "(?:cannot|can not|can['\u2019]t) (?:exclude|rule out)|rule out|r/o)\\b"
  )

  suspected_affix <- paste0(
    "(?:\\(\\s*(?:", suspected_pattern, ")\\s*\\)|",
    "(?:", suspected_pattern, "))"
  )
  suspected_edges <- paste0(
    "^(?:", suspected_affix, "\\s*:?\\s*)+|",
    "(?:\\s*:?\\s*", suspected_affix, ")+$"
  )

  polyp_code_matches <- function(findings, case_sensitive_findings) {
    .colonadr_is_exact(case_sensitive_findings, polyp_exact_terms) |
      .colonadr_is_exact(findings, polyp_label_terms) |
      grepl(paris_pattern, case_sensitive_findings)
  }

  classify_findings <- function(findings, case_sensitive_findings) {
    treated <- .colonadr_contains_any(findings, polyp_treated_terms)
    recurrent <- .colonadr_contains_any(findings, recurrent_adenoma_terms)
    treated_recurrence <-
      .colonadr_contains_any(findings, recurrent_adenoma_treated_terms)
    absent <- grepl(negative_pattern, findings, perl = TRUE)
    suspected <- grepl(suspected_pattern, findings, perl = TRUE)
    polyp_allowed <- !treated & !treated_recurrence & !absent

    polyp <- (.colonadr_contains_any(findings, polyp_substring_terms) |
                grepl(polyp_case_sensitive_substring, case_sensitive_findings,
                      fixed = TRUE) |
                polyp_code_matches(findings, case_sensitive_findings)) &
      polyp_allowed

    adenoma_loose <- .colonadr_contains_any(findings, adenoma_substring_terms) &
      (!treated | recurrent) & !treated_recurrence & !absent &
      !.colonadr_contains_any(findings, nonadenomatous_terms)
    adenoma_strict <- adenoma_loose & !polyp & !suspected

    polyp_legacy <- polyp | adenoma_loose
    polyp_strict <- polyp_legacy & !suspected
    polyp_loose <- polyp_legacy
    recheck <- suspected & !polyp_legacy & polyp_allowed
    if (any(recheck)) {
      # Strip only leading or trailing suspicion wording; keep code case.
      label <- trimws(gsub(suspected_edges, "", case_sensitive_findings[recheck],
                           ignore.case = TRUE, perl = TRUE))
      polyp_loose[recheck] <- polyp_code_matches(.colonadr_normalize(label), label)
    }

    list(adenoma_loose = adenoma_loose, adenoma_strict = adenoma_strict,
         polyp = polyp_legacy, polyp_strict = polyp_strict, polyp_loose = polyp_loose)
  }

  counts <- vapply(
    seq_along(findings_by_exam),
    function(index) {
      matches <- classify_findings(findings_by_exam[[index]],
                                  case_sensitive_findings_by_exam[[index]])
      if (preview_open) {
        groups <- list(
          adenoma_strict = matches$adenoma_strict,
          adenoma_loose_only = matches$adenoma_loose & !matches$adenoma_strict,
          polyp_strict_without_adenoma = matches$polyp_strict & !matches$adenoma_loose,
          polyp_loose_only = matches$polyp_loose & !matches$polyp_strict
        )
        exam_row <- exam_rows[[index]]
        original <- NULL
        for (group in names(groups)) {
          remaining <- preview_n - nrow(preview[[group]])
          if (remaining == 0L) next
          positions <- which(groups[[group]])
          if (!length(positions)) next
          positions <- positions[seq_len(min(length(positions), remaining))]
          if (is.null(original)) {
            original <- strsplit(x[[exam_row]], "[|\uff5c]", perl = TRUE)[[1L]]
          }
          preview[[group]] <<- rbind(preview[[group]], data.frame(
            exam_row = rep.int(exam_row, length(positions)),
            finding_index = positions,
            finding = original[positions],
            stringsAsFactors = FALSE
          ))
        }
        preview_open <<- any(vapply(preview, nrow, integer(1L)) < preview_n)
      }
      vapply(matches, sum, integer(1L))
    },
    c(adenoma_loose = 0L, adenoma_strict = 0L, polyp = 0L,
      polyp_strict = 0L, polyp_loose = 0L)
  )

  apc_loose <- sum(counts["adenoma_loose", ]) / n_eligible
  adr_loose <- sum(counts["adenoma_loose", ] > 0L) / n_eligible
  result <- list(
    APC = apc_loose,
    ADR = adr_loose,
    ADRstrict = sum(counts["adenoma_strict", ] > 0L) / n_eligible,
    ADRloose = adr_loose,
    PPC = sum(counts["polyp", ]) / n_eligible,
    PDR = sum(counts["polyp", ] > 0L) / n_eligible,
    APCstrict = sum(counts["adenoma_strict", ]) / n_eligible,
    APCloose = apc_loose,
    PPCstrict = sum(counts["polyp_strict", ]) / n_eligible,
    PPCloose = sum(counts["polyp_loose", ]) / n_eligible,
    PDRstrict = sum(counts["polyp_strict", ] > 0L) / n_eligible,
    PDRloose = sum(counts["polyp_loose", ] > 0L) / n_eligible
  )
  if (preview_n > 0L) result$preview <- preview

  print(paste0(
    "APCstrict:", format(result$APCstrict, digits = 5),
    ", APCloose:", format(result$APCloose, digits = 5),
    ", ADRstrict:", format(result$ADRstrict, digits = 5),
    ", ADRloose:", format(result$ADRloose, digits = 5),
    ", PPCstrict:", format(result$PPCstrict, digits = 5),
    ", PPCloose:", format(result$PPCloose, digits = 5),
    ", PDRstrict:", format(result$PDRstrict, digits = 5),
    ", PDRloose:", format(result$PDRloose, digits = 5)
  ))

  result
}
