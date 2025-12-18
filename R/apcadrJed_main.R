#' apcadr
#'
#' @param x Row of a dataset.
#'
#' @examples
#' data<-(c("腺腫|腺腫|憩室", "憩室|痔核", "特記所見なし", "家族性大腸腺腫症", "進行大腸癌|ポリープ", "polyp|adenoma"))
#' x<-apcadr(data)
#'
#' # prints "APC:0.6, ADR:0.4, PPC:1, PDR:0.66667, ACN DR:0.2, ACN per colonoscopy:0.2"
#' # Then you can access each value with x$APC, x$ADR, x$PPC, x$PDR, x$ACNDR and x$ACNPC.
#'
#' @note
#' This function calculates adenoma per colonoscopy (APC), adenoma detection rate (ADR), polyp per colonoscopy (PPC), polyp detection rate (PDR), advanced colorectal neoplasm detection rate (ACN DR), and advanced colorectal neoplasm per colonoscopy (ACN per colonoscopy) from JED-style database.
#' You can find sample database in "tests/testthat/sample_CF_data.csv".
#'
#' @export
#'
apcadr <- function(x) {
  split_values <- unlist(strsplit(x, "\\|"))

  polyposis_terms <- c(
    "\u5bb6\u65cf\u6027\u5927\u8178\u817a\u816b\u75c7", "FAP", "\uff26\uff21\uff30", "\u30dd\u30ea\u30dd\u30fc\u30b7\u30b9", "peutz-jeghers",
    "Peutz-Jeghers",
    "polyposis", "Polyposis", "cronkhite-canada", "Cronkhite-Canada", "lynch",
    "Lynch", "cowden", "Cowden", "gardner", "Gardner", "turcot", "Turcot",
    "HNPCC", "\uff50\uff45\uff55\uff54\uff5a", "\uff30\uff45\uff55\uff54\uff5a", "\uff50\uff4f\uff4c\uff59\uff50\uff4f\uff53\uff49\uff53",
    "\uff30\uff4f\uff4c\uff59\uff50\uff4f\uff53\uff49\uff53", "\uff43\uff52\uff4f\uff6e\uff6b\uff68\uff69\uff74\uff65", "\uff23\uff52\uff4f\uff6e\uff6b\uff68\uff69\uff74\uff65",
    "\uff4c\uff79\uff6e\uff63\uff68", "\uff2c\uff79\uff6e\uff63\uff68", "\uff43\uff4f\uff77\uff64\uff65\uff6e", "\uff23\uff4f\uff77\uff64\uff65\uff6e", "\uff28\uff2e\uff30\uff23\uff23",
    "\uff47\uff41\uff52\uff44\uff6e\uff65\uff72", "\uff27\uff41\uff52\uff44\uff6e\uff65\uff72", "\uff54\uff55\uff52\uff63\uff6f\uff74", "\uff34\uff55\uff52\uff63\uff6f\uff74"
  )

  polyp_terms <- c(
    "\u30dd\u30ea\u30fc\u30d7", "\u817a\u816b", "polyp", "serrated", "adenoma",
    "Polyp", "Serrated", "Adenoma", "\uff50\uff4f\uff4c\uff59\uff50",
    "\uff53\uff45\uff52\uff52\uff41\uff54\uff45\uff44", "\uff41\uff44\uff45\uff4e\uff4f\uff4d\uff41", "\uff30\uff4f\uff4c\uff59\uff50",
    "\uff33\uff45\uff52\uff52\uff41\uff54\uff45\uff44", "\uff21\uff44\uff45\uff4e\uff4f\uff4d\uff41",
    "\u92f8\u6b6f\u72b6", "HP", "\uff28\uff30", "Tsp", "\uff34\uff53\uff50", "Ts", "\uff34\uff53",
    "Tp", "\uff34\uff50", "Ua", "\uff35\uff41", "Uc", "\uff35\uff43", "Ip", "\uff29\uff50",
    "Isp", "\uff29\uff53\uff50", "Is", "\uff29\uff53", "SSA/P", "\uff33\uff33\uff21\uff0f\uff30",
    "Mixed", "\uff2d\uff49\uff58\uff45\uff44", "TSA", "\uff34\uff33\uff21",
    "IIa", "\uff29\uff29\uff41", "IIb", "\uff29\uff29\uff42", "IIc", "\uff29\uff29\uff43",
    "III", "\uff29\uff29\uff29", "LST", "\uff2c\uff33\uff34", "\u5074\u65b9\u767a\u80b2\u578b"
  )

  polyp_pattern <- paste(c(polyp_terms, polyposis_terms), collapse = "|")
  polyposis_pattern <- paste(polyposis_terms, collapse = "|")

  num_polyp <- sum(grepl(polyp_pattern, split_values)) - sum(grepl("polypectomy\u5f8c|Polypectomy\u5f8c|\uff50\uff4f\uff4c\uff59\uff50\uff45\uff43\uff54\uff4f\uff4d\uff59\u5f8c|\uff30\uff4f\uff4c\uff59\uff50\uff45\uff43\uff54\uff4f\uff4d\uff59\u5f8c", split_values))
  num_polyppositive <- sum(grepl(polyp_pattern, x)) - sum(grepl("polypectomy\u5f8c|Polypectomy\u5f8c|\uff50\uff4f\uff4c\uff59\uff50\uff45\uff43\uff54\uff4f\uff4d\uff59\u5f8c|\uff30\uff4f\uff4c\uff59\uff50\uff45\uff43\uff54\uff4f\uff4d\uff59\u5f8c", x))
  num_adenoma <- sum(grepl("\u817a\u816b|adenoma|Adenoma|\uff41\uff44\uff45\uff4e\uff4f\uff4d\uff41|\uff21\uff44\uff45\uff4e\uff4f\uff4d\uff41", split_values)) - sum(grepl("\u5bb6\u65cf\u6027\u5927\u8178\u817a\u816b\u75c7|FAP|\uff26\uff21\uff30", split_values))
  num_adenomapositive <- sum(grepl("\u817a\u816b|adenoma|Adenoma|\uff41\uff44\uff45\uff4e\uff4f\uff4d\uff41|\uff21\uff44\uff45\uff4e\uff4f\uff4d\uff41", x)) - sum(grepl("\u5bb6\u65cf\u6027\u5927\u8178\u817a\u816b\u75c7|FAP|\uff26\uff21\uff30", x))
  num_polyposis <- sum(grepl(polyposis_pattern, x))

  ppc <- num_polyp / length(x)
  pdr <- num_polyppositive / length(x)
  apc <- num_adenoma / (length(x) - num_polyposis)
  adr <- num_adenomapositive / (length(x) - num_polyposis)

  advcancerpositive <- grepl("\u9032\u884c", x)
  nonrelapsed_cancer_yes <- grepl("\u518d\u767a[^|]{0,15}(\u7121|\u7121\u3057|\u306a\u3057|\u7121\u3044|\u306a\u3044|\u8a8d\u3081\u305a|\u307f\u3068\u3081\u305a|\u307e\u305b\u3093|\u7121\u3055\u305d\u3046|\u306a\u3055\u305d\u3046|\u6307\u6458\u3067\u304d\u305a|\u6307\u6458\u3057\u5f97\u305a|\u6307\u6458\u3057\u3048\u305a|\u898b\u5f53\u305f\u3089\u305a|\u307f\u3042\u305f\u3089\u305a)", advcancerpositive)
  after_crt_residual_yes <- grepl("(RT\u5f8c|Chemotherapy\u5f8c|\u653e\u5c04\u7dda\u7642\u6cd5\u5f8c|\u5316\u5b66\u7642\u6cd5\u5f8c|\uff32\uff34\u5f8c|\uff23\uff48\uff45\uff4d\uff4f\uff54\uff48\uff45\uff52\uff41\uff50\uff59\u5f8c)[^|]{0,15}(PR|PD|SD|\uff30\uff32|\uff30\uff24|\uff33\uff24)", x)
  after_ope_residual_yes <- grepl("(\u56de\u76f2\u90e8\u5207\u9664\u5f8c|\u5c40\u6240\u5207\u9664\u5f8c|\u7d50\u8178\u5207\u9664\u5f8c|HAR\u5f8c|LAR\u5f8c|\u5927\u8178\u5168\u6458\u5f8c|Hartmann\u624b\u8853\u5f8c|Miles|\uff28\uff21\uff32\u5f8c|\uff2c\uff21\uff32\u5f8c|\uff28\uff41\uff52\uff54\uff4d\uff41\uff4e\uff4e\u624b\u8853\u5f8c|\uff2d\uff49\uff4c\uff45\uff53)[^|]{0,15}(\u518d\u767a\uff1a\u3042\u308a|\u518d\u767a\u3042\u308a|\u518d\u767a:\u3042\u308a|\u518d\u767a\uff1a\u6709\u308a|\u518d\u767a\u6709\u308a|\u518d\u767a:\u6709\u308a)", x)

  advcancer <- grepl("\u9032\u884c", split_values)
  nonrelapsed_cancer <- grepl("\u518d\u767a.{0,15}(\u7121|\u7121\u3057|\u306a\u3057|\u7121\u3044|\u306a\u3044|\u8a8d\u3081\u305a|\u307f\u3068\u3081\u305a|\u307e\u305b\u3093|\u7121\u3055\u305d\u3046|\u306a\u3055\u305d\u3046|\u6307\u6458\u3067\u304d\u305a|\u6307\u6458\u3057\u5f97\u305a|\u6307\u6458\u3057\u3048\u305a|\u898b\u5f53\u305f\u3089\u305a|\u307f\u3042\u305f\u3089\u305a)", advcancer)
  after_crt_residual <- grepl("(RT\u5f8c|Chemotherapy\u5f8c|\u653e\u5c04\u7dda\u7642\u6cd5\u5f8c|\u5316\u5b66\u7642\u6cd5\u5f8c|\uff32\uff34\u5f8c|\uff23\uff48\uff45\uff4d\uff4f\uff54\uff48\uff45\uff52\uff41\uff50\uff59\u5f8c).{0,15}(PR|PD|SD|\uff30\uff32|\uff30\uff24|\uff33\uff24)", split_values)
  after_ope_residual <- grepl("(\u56de\u76f2\u90e8\u5207\u9664\u5f8c|\u5c40\u6240\u5207\u9664\u5f8c|\u7d50\u8178\u5207\u9664\u5f8c|HAR\u5f8c|LAR\u5f8c|\u5927\u8178\u5168\u6458\u5f8c|Hartmann\u624b\u8853\u5f8c|Miles|\uff28\uff21\uff32\u5f8c|\uff2c\uff21\uff32\u5f8c|\uff28\uff41\uff52\uff54\uff4d\uff41\uff4e\uff4e\u624b\u8853\u5f8c|\uff2d\uff49\uff4c\uff45\uff53).{0,15}(\u518d\u767a\uff1a\u3042\u308a|\u518d\u767a\u3042\u308a|\u518d\u767a:\u3042\u308a|\u518d\u767a\uff1a\u6709\u308a|\u518d\u767a\u6709\u308a|\u518d\u767a:\u6709\u308a)", split_values)

  acndr <- (sum(advcancerpositive | after_crt_residual_yes | after_ope_residual_yes) - sum(nonrelapsed_cancer_yes)) / (length(x) - num_polyposis)
  acnpc <- (sum(advcancer | after_crt_residual | after_ope_residual) - sum(nonrelapsed_cancer)) / (length(x) - num_polyposis)

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
