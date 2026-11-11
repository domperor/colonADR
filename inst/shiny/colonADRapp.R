options(shiny.maxRequestSize = 30 * 1024^2)
library(shiny)
library(bslib)

.colonadr_nfkc <- function(x) {
  x <- stringi::stri_trans_nfkc(x)
  x <- gsub("[‐‑‒–—―−]", "-", x, perl = TRUE)
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
    "家族性大腸腺腫症",
    "familial adenomatous polyposis",
    "FAP",
    "ポリポーシス",
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
    "ポリープ",
    "polyp",
    "serrated",
    "鋸歯状",
    "側方発育型",
    "過形成",
    "laterally spreading tumor",
    "laterally spreading lesion"
  ))

  adenoma_substring_terms <- .colonadr_normalize(c(
    "腺腫",
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
    "polypectomy後",
    as.vector(outer(
      c("ポリープ", "腺腫", "polyp", "adenoma"),
      c("切除後", "摘除後", "摘出後", "治療後", "切除瘢痕"),
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
    "遺残腺腫", "再発腺腫", "residual adenoma", "recurrent adenoma"
  ))

  recurrent_adenoma_treated_terms <- .colonadr_normalize(c(
    as.vector(outer(
      c("遺残腺腫", "再発腺腫"),
      c("切除後", "摘除後", "摘出後", "治療後", "切除瘢痕"),
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
    "\\s*[:=]?\\s*(?:は|が|を)?\\s*",
    "(?:なし|無し|ない|認め(?:られ)?(?:ず|ない|ません)|",
    "(?:み|見)られ(?:ず|ない|ません)|疑わ(?:れ)?(?:ず|ない|ません)|",
    "疑い(?:は)?(?:なし|無し|ない)|",
    "陰性|\\(\\s*-\\s*\\))"
  )
  japanese_finding_detail <- "(?:性病変|病変|(?:の|を疑う|を示唆する)?所見)?"

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
    paste0("(?:ポリープ|過形成|lst(?:-[a-z]+)*)",
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
    paste0("腺腫", japanese_finding_detail, japanese_absence)
  ))

  nonadenomatous_terms <- .colonadr_normalize(c(
    "非腺腫性", "非腫瘍性",
    "nonadenomatous", "non-adenomatous", "non-neoplastic",
    "nonneoplastic", "non neoplastic"
  ))

  japanese_recurrence_negative_pattern <- paste0(
    "(?:再発|遺残|残存)", japanese_finding_detail, japanese_absence
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
    "疑(?:い)?|(?:否定|除外)でき(?:な[いく]|ず)|\\?|",
    "\\b(?:suspect(?:ed)?|suspicious for|possibl[ey]|probabl[ey]|",
    "suggestive of|favou?r(?:ed|s)?|",
    "(?:cannot|can not|can['’]t) (?:exclude|rule out)|rule out|r/o)\\b"
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
            original <- strsplit(x[[exam_row]], "[|｜]", perl = TRUE)[[1L]]
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

ui <- page_sidebar(
  title = "colonADR: Colonoscopy quality indicators",
  sidebar = sidebar(
    fileInput(
      "file1",
      "Step 1: Choose a CSV or TSV file",
      multiple = FALSE,
      accept = c(".csv", ".tsv", "text/csv", "text/tab-separated-values")
    ),
    checkboxInput("header", "First row contains column names", TRUE),
    radioButtons(
      "sep",
      "Field separator",
      choices = c(Comma = ",", Tab = "\t"),
      selected = ","
    ),
    radioButtons(
      "disp",
      "Data preview",
      choices = c("First 6 rows" = "head", "All rows" = "all"),
      selected = "head"
    ),
    uiOutput("colname1"),
    actionButton("submit", "Calculate")
  ),
  h4("Results"),
  div(
    style = "overflow-x: auto;",
    tableOutput("APCADR"),
    tableOutput("PPCPDR")
  ),
  uiOutput("finding_examples"),
  verbatimTextOutput("abbr"),
  h4("Data preview"),
  tableOutput("dataframe")
)

server <- function(input, output, session) {
  data_file <- reactive({
    req(input$file1)

    df <- tryCatch(
      read.csv(
        input$file1$datapath[[1L]],
        header = input$header,
        sep = input$sep,
        fileEncoding = "UTF-8-BOM",
        blank.lines.skip = FALSE,
        colClasses = "character"
      ),
      error = function(e) {
        validate(need(FALSE, paste(
          "Could not read the file. Check the separator and UTF-8 encoding.",
          conditionMessage(e)
        )))
      }
    )

    validate(
      need(ncol(df) > 0L, "The file does not contain any columns."),
      need(nrow(df) > 0L, "The file does not contain any examination records.")
    )
    df
  })

  output$colname1 <- renderUI({
    selectInput(
      "rowname",
      "Step 2: Choose the diagnosis column",
      choices = colnames(data_file())
    )
  })

  submitted_findings <- reactiveVal(NULL)

  observeEvent(
    list(input$file1, input$header, input$sep, input$rowname),
    {
      submitted_findings(NULL)
    }
  )

  observeEvent(input$submit, {
    df <- data_file()
    req(input$rowname, input$rowname %in% colnames(df))
    submitted_findings(df[[input$rowname]])
  })

  summary_result <- reactive({
    x <- submitted_findings()
    req(!is.null(x))
    apcadr(x, preview_n = 10L)
  })

  output$APCADR <- renderTable({
    result <- summary_result()
    as.data.frame(result[c("APCstrict", "APCloose", "ADRstrict", "ADRloose")])
  }, digits = 2, rownames = FALSE)

  output$PPCPDR <- renderTable({
    result <- summary_result()
    as.data.frame(result[c("PPCstrict", "PPCloose", "PDRstrict", "PDRloose")])
  }, digits = 2, rownames = FALSE)

  output$finding_examples <- renderUI({
    req(summary_result())
    tagList(
      h4("Finding examples"),
      p("Up to 10 findings per group, in input order. Data row numbers exclude the header."),
      p("A finding can appear in both loose-only tabs."),
      tabsetPanel(
        tabPanel("Adenoma strict", tableOutput("preview_adenoma_strict")),
        tabPanel("Adenoma loose only", tableOutput("preview_adenoma_loose_only")),
        tabPanel("Polyp strict, adenoma loose negative",
                 tableOutput("preview_polyp_strict_without_adenoma")),
        tabPanel("Polyp loose only", tableOutput("preview_polyp_loose_only"))
      )
    )
  })

  preview_table <- function(group) {
    renderTable({
      findings <- summary_result()$preview[[group]]
      validate(need(nrow(findings) > 0L, "No matching findings."))
      setNames(findings, c("Data row", "Finding number", "Finding"))
    }, digits = 0, rownames = FALSE)
  }

  output$preview_adenoma_strict <- preview_table("adenoma_strict")
  output$preview_adenoma_loose_only <- preview_table("adenoma_loose_only")
  output$preview_polyp_strict_without_adenoma <- preview_table("polyp_strict_without_adenoma")
  output$preview_polyp_loose_only <- preview_table("polyp_loose_only")

  output$abbr <- renderText({
    paste(
      "APCstrict: adenoma findings per eligible colonoscopy, using ADRstrict criteria",
      "APCloose: adenoma findings per eligible colonoscopy, using ADRloose criteria",
      "ADRstrict: proportion of examinations with an unambiguous adenoma label",
      "ADRloose: proportion with an unambiguous, suspected, or mixed adenoma/polyp label",
      "PPCstrict: polyp findings per eligible colonoscopy, excluding suspected findings",
      "PPCloose: polyp findings per eligible colonoscopy, including suspected findings",
      "PDRstrict: proportion of examinations with at least one strict polyp finding",
      "PDRloose: proportion of examinations with at least one loose polyp finding",
      "",
      "Japanese and English structured diagnosis labels are supported.",
      "Use one row per colonoscopy and separate individual findings with |.",
      "e.g. Tubular adenoma|Suspected adenoma: APCstrict = 1 and APCloose = 2 for one examination.",
      "",
      "File size limit: 30 MB. File encoding: UTF-8.",
      "2026-11-11 v0.0.5  © Mitsuaki Oura (@domperor)",
      sep = "\n"
    )
  })

  output$dataframe <- renderTable({
    df <- data_file()
    if (input$disp == "head") {
      head(df)
    } else {
      df
    }
  })
}

shinyApp(ui, server)
