options(shiny.maxRequestSize = 30 * 1024^2) #30MB
library(shiny)
library(bslib)

ui <- page_sidebar(

  # App title ----
  title = "APC, ADR, PPC, PDR, ACN DR, ACN per colonoscopy: calculation",

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
#  textOutput("test"),
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

  #Output - testcode
  #observeEvent(input$submit,{output$test<-renderText({df()[,input$rowname]})})

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

    polyp_pattern <- paste(c(polyp_terms, polyposis_terms), collapse = "|")
    polyposis_pattern <- paste(polyposis_terms, collapse = "|")

    num_polyp <- sum(grepl(polyp_pattern, split_values)) - sum(grepl("polypectomy後|Polypectomy後|ｐｏｌｙｐｅｃｔｏｍｙ後|Ｐｏｌｙｐｅｃｔｏｍｙ後", split_values))
    num_polyppositive <- sum(grepl(polyp_pattern, x)) - sum(grepl("polypectomy後|Polypectomy後|ｐｏｌｙｐｅｃｔｏｍｙ後|Ｐｏｌｙｐｅｃｔｏｍｙ後", x))
    num_adenoma <- sum(grepl("腺腫|adenoma|Adenoma|ａｄｅｎｏｍａ|Ａｄｅｎｏｍａ", split_values)) - sum(grepl("家族性大腸腺腫症|FAP|ＦＡＰ", split_values))
    num_adenomapositive <- sum(grepl("腺腫|adenoma|Adenoma|ａｄｅｎｏｍａ|Ａｄｅｎｏｍａ", x)) - sum(grepl("家族性大腸腺腫症|FAP|ＦＡＰ", x))
    num_polyposis <- sum(grepl(polyposis_pattern, x))

    ppc <- num_polyp / length(x)
    pdr <- num_polyppositive / length(x)
    apc <- num_adenoma / (length(x) - num_polyposis)
    adr <- num_adenomapositive / (length(x) - num_polyposis)

#confidence interval 表示，準備中
    #割合はいいが，ppc，apcのCI表示はカウントの実装をまるきり直す必要があり手間あり
#    pdr_lower_ci <- pdr - sqrt(pdr*(1-pdr)/length(x))*qt(0.975,length(x)-1)
#    pdr_upper_ci <- pdr + sqrt(pdr*(1-pdr)/length(x))*qt(0.975,length(x)-1)
#    adr_lower_ci <- adr - sqrt(adr*(1-adr)/(length(x) - num_polyposis))*qt(0.975,(length(x) - num_polyposis)-1)
#    adr_upper_ci <- adr + sqrt(adr*(1-adr)/(length(x) - num_polyposis))*qt(0.975,(length(x) - num_polyposis)-1)

    advcancerpositive <- grepl("進行", x)
    nonrelapsed_cancer_yes <- grepl("再発[^|]{0,15}(無|無し|なし|無い|ない|認めず|みとめず|ません|無さそう|なさそう|指摘できず|指摘し得ず|指摘しえず|見当たらず|みあたらず)", advcancerpositive)
    after_crt_residual_yes <- grepl("(RT後|Chemotherapy後|放射線療法後|化学療法後|ＲＴ後|Ｃｈｅｍｏｔｈｅｒａｐｙ後)[^|]{0,15}(PR|PD|SD|ＰＲ|ＰＤ|ＳＤ)",x)
    after_ope_residual_yes <- grepl("(回盲部切除後|局所切除後|結腸切除後|HAR後|LAR後|大腸全摘後|Hartmann手術後|Miles|ＨＡＲ後|ＬＡＲ後|Ｈａｒｔｍａｎｎ手術後|Ｍｉｌｅｓ)[^|]{0,15}(再発：あり|再発あり|再発:あり|再発：有り|再発有り|再発:有り)",x)

    advcancer <- grepl("進行", split_values)
    nonrelapsed_cancer <- grepl("再発.{0,15}(無|無し|なし|無い|ない|認めず|みとめず|ません|無さそう|なさそう|指摘できず|指摘し得ず|指摘しえず|見当たらず|みあたらず)", advcancer)
    after_crt_residual <- grepl("(RT後|Chemotherapy後|放射線療法後|化学療法後|ＲＴ後|Ｃｈｅｍｏｔｈｅｒａｐｙ後).{0,15}(PR|PD|SD|ＰＲ|ＰＤ|ＳＤ)",split_values)
    after_ope_residual <- grepl("(回盲部切除後|局所切除後|結腸切除後|HAR後|LAR後|大腸全摘後|Hartmann手術後|Miles|ＨＡＲ後|ＬＡＲ後|Ｈａｒｔｍａｎｎ手術後|Ｍｉｌｅｓ).{0,15}(再発：あり|再発あり|再発:あり|再発：有り|再発有り|再発:有り)",split_values)

    acndr <- (sum(advcancerpositive|after_crt_residual_yes|after_ope_residual_yes) - sum(nonrelapsed_cancer_yes))/(length(x) - num_polyposis)
    acnpc <- (sum(advcancer|after_crt_residual|after_ope_residual) - sum(nonrelapsed_cancer))/(length(x) - num_polyposis)

    result <- list(
      APC = apc, ADR = adr, PPC = ppc, PDR = pdr, ACNDR = acndr, ACNPC = acnpc
    )

    return(result)

  })})

  #Output - abbreviation
  output$abbr<-renderText("APC: adenoma per colonoscopy\nADR: adenoma detection rate\nPPC: polyp per colonoscopy\nPDR: polyp detection rate\nACN DR: advanced colorectal neoplasia detection rate\nACN per colonoscopy: advanced colorectal neoplasia per colonoscopy\n\n現在日本語のみの対応です。一内視鏡中の所見は「|」で区切ってください。\nFile size max: 30MB, Encoding: UTF-8 でお願いします。\n大きいファイルは解析に1分弱要します。\n 2025-12-19 v.0.0.1  ©︎ Mitsuaki Oura (@domperor) ")

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
