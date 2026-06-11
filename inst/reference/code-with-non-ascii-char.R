# code with non-ascii char for reference, v0.0.3

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
                   "III", "ＩＩＩ", "LST", "ＬＳＴ", "側方発育型","SSL", "ＳＳＬ")
  
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

  print(paste0("APC:", format(apc, digits = 5),
               ", ADR:", format(adr, digits = 5),
               ", PPC:", format(ppc, digits = 5),
               ", PDR:", format(pdr, digits = 5)))

  return(result)
}
