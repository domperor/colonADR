options(shiny.maxRequestSize = 30 * 1024^2) #30MB
library(shiny)
library(bslib)

ui <- page_sidebar(

  # App title ----
  title = "APC, ADR, PPC, PDR: calculation",

  # Sidebar panel for inputs ----
  sidebar = sidebar(

    # Input: Select a file ----
    fileInput(
      "file1",
      "Step1: Choose CSV File",
      multiple = TRUE,
      accept = c(
        "text/csv",
        "text/comma-separated-values,text/plain",
        ".csv"
      )
    ),

    # Input: Checkbox if file has header ----
    checkboxInput("header", "Header", TRUE),

    # Input: Select separator ----
    radioButtons(
      "sep",
      "Separator",
      choices = c(
        Comma = ",",
        Tab = "\t"
      ),
      selected = ","
    ),

    # Input: Select number of rows to display ----
    radioButtons(
      "disp",
      "Display",
      choices = c(
        Head = "head",
        All = "all"
      ),
      selected = "head"
    ),

    htmlOutput("colname1"),
    actionButton("submit", "SUBMIT")
  ),

  # Output: Data file ----
  textOutput("reshead"),
  tableOutput("APCADR"),
  verbatimTextOutput("abbr"),
  tableOutput("dataframe")
)

# Define server logic to read selected file ----
server <- function(input, output, session) {

  #Output - colname
  output$colname1 = renderUI({

    req(input$file1)

    df <- read.csv(
      input$file1$datapath,
      header = input$header,
      sep = input$sep
    )

    selectInput("rowname", "Step 2: Choose Row Name", colnames(df))
  })

  #Output - result header
  output$reshead <- renderText({"Result: "})

  #Output - main
  observeEvent(input$submit,{
  output$APCADR <- renderTable({
    req(input$file1)

    df <- read.csv(
      input$file1$datapath,
      header = input$header,
      sep = input$sep
    )

    x<-df[,input$rowname]

    split_values <- unlist(strsplit(x, "\\|"))

    polyposis_terms <- c(
      "家族性大腸腺腫症", "FAP", "ＦＡＰ", "ポリポーシス", "peutz-jeghers",
      "Peutz-Jeghers",
      "polyposis", "Polyposis", "cronkhite-canada", "Cronkhite-Canada", "lynch",
      "Lynch", "cowden", "Cowden", "gardner", "Gardner", "turcot", "Turcot",
      "HNPCC", "ｐｅｕｔｚ", "Ｐｅｕｔｚ", "ｐｏｌｙｐｏｓｉｓ",
      "Ｐｏｌｙｐｏｓｉｓ", "ｃｒｏｎｋｈｉｔｅ", "Ｃｒｏｎｋｈｉｔｅ",
      "ｌｙｎｃｈ", "Ｌｙｎｃｈ", "ｃｏｗｄｅｎ", "Ｃｏｗｄｅｎ", "ＨＮＰＣＣ",
      "ｇａｒｄｎｅｒ", "Ｇａｒｄｎｅｒ", "ｔｕｒｃｏｔ", "Ｔｕｒｃｏｔ"
    )

    polyp_terms <- c("ポリープ", "腺腫", "polyp", "serrated", "adenoma",
                     "Polyp", "Serrated", "Adenoma", "ｐｏｌｙｐ",
                     "ｓｅｒｒａｔｅｄ", "ａｄｅｎｏｍａ", "Ｐｏｌｙｐ",
                     "Ｓｅｒｒａｔｅｄ", "Ａｄｅｎｏｍａ",
                     "鋸歯状", "HP", "ＨＰ", "Tsp", "Ｔｓｐ", "Ts", "Ｔｓ",
                     "Tp", "Ｔｐ", "Ua", "Ｕａ", "Uc", "Ｕｃ", "Ip", "Ｉｐ",
                     "Isp", "Ｉｓｐ", "Is", "Ｉｓ", "SSA/P", "ＳＳＡ／Ｐ",
                     "Mixed", "Ｍｉｘｅｄ", "TSA", "ＴＳＡ",
                     "IIa", "ＩＩａ", "IIb", "ＩＩｂ", "IIc", "ＩＩｃ",
                     "III", "ＩＩＩ", "LST", "ＬＳＴ", "側方発育型")

    polyp_treated_terms <- c(
      "polypectomy後", "Polypectomy後",
      "ｐｏｌｙｐｅｃｔｏｍｙ後", "Ｐｏｌｙｐｅｃｔｏｍｙ後",
      "ポリープ切除後", "ポリープ摘除後", "ポリープ摘出後", "ポリープ治療後",
      "腺腫切除後", "腺腫摘除後", "腺腫摘出後", "腺腫治療後",
      "polyp切除後", "polyp摘除後", "polyp摘出後", "polyp治療後",
      "Polyp切除後", "Polyp摘除後", "Polyp摘出後", "Polyp治療後",
      "ｐｏｌｙｐ切除後", "ｐｏｌｙｐ摘除後", "ｐｏｌｙｐ摘出後", "ｐｏｌｙｐ治療後",
      "Ｐｏｌｙｐ切除後", "Ｐｏｌｙｐ摘除後", "Ｐｏｌｙｐ摘出後", "Ｐｏｌｙｐ治療後",
      "adenoma切除後", "adenoma摘除後", "adenoma摘出後", "adenoma治療後",
      "Adenoma切除後", "Adenoma摘除後", "Adenoma摘出後", "Adenoma治療後",
      "ａｄｅｎｏｍａ切除後", "ａｄｅｎｏｍａ摘除後", "ａｄｅｎｏｍａ摘出後", "ａｄｅｎｏｍａ治療後",
      "Ａｄｅｎｏｍａ切除後", "Ａｄｅｎｏｍａ摘除後", "Ａｄｅｎｏｍａ摘出後", "Ａｄｅｎｏｍａ治療後"
    )

    adenoma_treated_terms <- c(
      "腺腫切除後", "腺腫摘除後", "腺腫摘出後", "腺腫治療後",
      "adenoma切除後", "adenoma摘除後", "adenoma摘出後", "adenoma治療後",
      "Adenoma切除後", "Adenoma摘除後", "Adenoma摘出後", "Adenoma治療後",
      "ａｄｅｎｏｍａ切除後", "ａｄｅｎｏｍａ摘除後", "ａｄｅｎｏｍａ摘出後", "ａｄｅｎｏｍａ治療後",
      "Ａｄｅｎｏｍａ切除後", "Ａｄｅｎｏｍａ摘除後", "Ａｄｅｎｏｍａ摘出後", "Ａｄｅｎｏｍａ治療後"
    )

    polyp_pattern <- paste(c(polyp_terms, polyposis_terms), collapse = "|")
    polyposis_pattern <- paste(polyposis_terms, collapse = "|")
    polyp_treated_pattern <- paste(polyp_treated_terms, collapse = "|")
    adenoma_treated_pattern <- paste(adenoma_treated_terms, collapse = "|")

    num_polyp <- sum(grepl(polyp_pattern, split_values))  - sum(grepl(polyp_treated_pattern, split_values))
    num_polyppositive <- sum(grepl(polyp_pattern, x)) - sum(grepl(polyp_treated_pattern, x))
    num_adenoma <- sum(grepl("腺腫|adenoma|Adenoma|ａｄｅｎｏｍａ|Ａｄｅｎｏｍａ", split_values)) - sum(grepl("家族性大腸腺腫症|FAP|ＦＡＰ", split_values)) - sum(grepl(adenoma_treated_pattern, split_values))
    num_adenomapositive <- sum(grepl("腺腫|adenoma|Adenoma|ａｄｅｎｏｍａ|Ａｄｅｎｏｍａ", x)) - sum(grepl("家族性大腸腺腫症|FAP|ＦＡＰ", x)) - sum(grepl(adenoma_treated_pattern, x))
    num_polyposis <- sum(grepl(polyposis_pattern, x))

    ppc <- num_polyp / length(x)
    pdr <- num_polyppositive / length(x)
    apc <- num_adenoma / (length(x) - num_polyposis)
    adr <- num_adenomapositive / (length(x) - num_polyposis)

    result <- list(
      APC = apc, ADR = adr, PPC = ppc, PDR = pdr
    )

    return(result)

  })})

  #Output - abbreviation
  output$abbr<-renderText("APC: adenoma per colonoscopy\nADR: adenoma detection rate\nPPC: polyp per colonoscopy\nPDR: polyp detection rate\n\n現在日本語のみの対応です。一内視鏡中の所見は「|」で区切ってください。\nFile size max: 30MB, Encoding: UTF-8 でお願いします。\n大きいファイルは解析に1分弱要します。\n 2026-06-11 v.0.0.3  ©︎ Mitsuaki Oura (@domperor) ")

  #Output - dataframe
  output$dataframe <- renderTable({

    req(input$file1)

    df <- read.csv(
      input$file1$datapath,
      header = input$header,
      sep = input$sep,
      quote = input$quote
    )

    if (input$disp == "head") {
      return(head(df))
    } else {
      return(df)
    }
  })

}

# Create Shiny app ----
shinyApp(ui, server)
