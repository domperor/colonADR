# code with non-ascii char for reference, v0.0.2

apcadr <- function(x) {
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

  print(paste0("APC:", format(apc, digits = 5),
               ", ADR:", format(adr, digits = 5),
               ", PPC:", format(ppc, digits = 5),
               ", PDR:", format(pdr, digits = 5),
               ", ACN DR:", format(acndr, digits = 5),
               ", ACN per colonoscopy:", format(acnpc, digits = 5)))

  return(result)
}
