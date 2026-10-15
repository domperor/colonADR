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
apcadr <- function(x) {
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
    "ポリープ",
    "polyp",
    "serrated",
    "鋸歯状",
    "側方発育型",
    "過形成性病変",
    "laterally spreading tumor",
    "laterally spreading lesion"
  ))

  adenoma_substring_terms <- .colonadr_normalize(c(
    "腺腫",
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

  polyp_negative_terms <- .colonadr_normalize(c(
    "no polyp",
    "no polyps",
    "without polyp",
    "without polyps",
    "negative for polyp",
    "negative for polyps",
    "no evidence of polyp",
    "no evidence of polyps",
    "ポリープなし",
    "ポリープ認めず",
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
    "腺腫なし",
    "腺腫を認めない",
    "腺腫陰性",
    "腺腫\\s*\\(-\\)",
    "absence of adenoma",
    "no residual adenoma",
    "no recurrent adenoma"
  ))

  nonadenomatous_terms <- .colonadr_normalize(c(
    "非腺腫性", "腺腫ではない", "nonadenomatous", "non-adenomatous"
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

ui <- page_sidebar(
  title = "colonADR: APC, ADR, PPC and PDR",
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
  tableOutput("APCADR"),
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

  output$APCADR <- renderTable({
    x <- submitted_findings()
    req(!is.null(x))

    x[is.na(x)] <- ""

    as.data.frame(apcadr(x))
  }, digits = 2, rownames = FALSE)

  output$abbr <- renderText({
    paste(
      "APC: adenomas per colonoscopy",
      "ADR: adenoma detection rate",
      "PPC: polyps per colonoscopy",
      "PDR: polyp detection rate",
      "",
      "Japanese and English structured diagnosis labels are supported.",
      "Use one row per colonoscopy and separate individual findings with |.",
      "Example: Tubular adenoma|Hyperplastic polyp",
      "",
      "File size limit: 30 MB. File encoding: UTF-8.",
      "2026-10-15 v0.0.4  © Mitsuaki Oura (@domperor)",
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
