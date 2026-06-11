#' apcadr
#'
#' @param x Row of a dataset.
#'
#' @examples
#' data<-(c("腺腫|腺腫|憩室", "憩室|痔核", "特記所見なし", "家族性大腸腺腫症", "進行大腸癌|ポリープ", "polyp|adenoma"))
#' x<-apcadr(data)
#'
#' # prints "APC:0.6, ADR:0.4, PPC:1, PDR:0.66667
#' # Then you can access each value with x$APC, x$ADR, x$PPC and x$PDR.
#'
#' @note
#' This function calculates adenoma per colonoscopy (APC), adenoma detection rate (ADR), polyp per colonoscopy (PPC) and polyp detection rate (PDR) from JED-style database.
#' You can find sample database in "tests/testthat/sample_CF_data.csv".
#'
#' @export
#'
# code with non-ascii char for reference, v0.0.3

apcadr <- function(x) {
  split_values <- unlist(strsplit(x, "\\|"))
  
  polyposis_terms <- c(
    "\u5bb6\u65cf\u6027\u5927\u8178\u817a\u816b\u75c7", "FAP", "\uff26\uff21\uff30", "\u30dd\u30ea\u30dd\u30fc\u30b7\u30b9", "peutz-jeghers",
    "Peutz-Jeghers",
    "polyposis", "Polyposis", "cronkhite-canada", "Cronkhite-Canada", "lynch",
    "Lynch", "cowden", "Cowden", "gardner", "Gardner", "turcot", "Turcot",
    "HNPCC", "\uff50\uff45\uff55\uff54\uff5a", "\uff30\uff45\uff55\uff54\uff5a", "\uff50\uff4f\uff4c\uff59\uff50\uff4f\uff53\uff49\uff53",
    "\uff30\uff4f\uff4c\uff59\uff50\uff4f\uff53\uff49\uff53", "\uff43\uff52\uff4f\uff4e\uff4b\uff48\uff49\uff54\uff45", "\uff23\uff52\uff4f\uff4e\uff4b\uff48\uff49\uff54\uff45",
    "\uff4c\uff59\uff4e\uff43\uff48", "\uff2c\uff59\uff4e\uff43\uff48", "\uff43\uff4f\uff57\uff44\uff45\uff4e", "\uff23\uff4f\uff57\uff44\uff45\uff4e", "\uff28\uff2e\uff30\uff23\uff23",
    "\uff47\uff41\uff52\uff44\uff4e\uff45\uff52", "\uff27\uff41\uff52\uff44\uff4e\uff45\uff52", "\uff54\uff55\uff52\uff43\uff4f\uff54", "\uff34\uff55\uff52\uff43\uff4f\uff54"
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
    "III", "\uff29\uff29\uff29", "LST", "\uff2c\uff33\uff34", "\u5074\u65b9\u767a\u80b2\u578b", "SSL", "\uff33\uff33\uff2c"
  )
  
  polyp_treated_terms <- c(
    "polypectomy\u5f8c", "Polypectomy\u5f8c",
    "\uff50\uff4f\uff4c\uff59\uff50\uff45\uff43\uff54\uff4f\uff4d\uff59\u5f8c", "\uff30\uff4f\uff4c\uff59\uff50\uff45\uff43\uff54\uff4f\uff4d\uff59\u5f8c",
    "\u30dd\u30ea\u30fc\u30d7\u5207\u9664\u5f8c", "\u30dd\u30ea\u30fc\u30d7\u6458\u9664\u5f8c", "\u30dd\u30ea\u30fc\u30d7\u6458\u51fa\u5f8c", "\u30dd\u30ea\u30fc\u30d7\u6cbb\u7642\u5f8c",
    "\u817a\u816b\u5207\u9664\u5f8c", "\u817a\u816b\u6458\u9664\u5f8c", "\u817a\u816b\u6458\u51fa\u5f8c", "\u817a\u816b\u6cbb\u7642\u5f8c",
    "polyp\u5207\u9664\u5f8c", "polyp\u6458\u9664\u5f8c", "polyp\u6458\u51fa\u5f8c", "polyp\u6cbb\u7642\u5f8c",
    "Polyp\u5207\u9664\u5f8c", "Polyp\u6458\u9664\u5f8c", "Polyp\u6458\u51fa\u5f8c", "Polyp\u6cbb\u7642\u5f8c",
    "\uff50\uff4f\uff4c\uff59\uff50\u5207\u9664\u5f8c", "\uff50\uff4f\uff4c\uff59\uff50\u6458\u9664\u5f8c", "\uff50\uff4f\uff4c\uff59\uff50\u6458\u51fa\u5f8c", "\uff50\uff4f\uff4c\uff59\uff50\u6cbb\u7642\u5f8c",
    "\uff30\uff4f\uff4c\uff59\uff50\u5207\u9664\u5f8c", "\uff30\uff4f\uff4c\uff59\uff50\u6458\u9664\u5f8c", "\uff30\uff4f\uff4c\uff59\uff50\u6458\u51fa\u5f8c", "\uff30\uff4f\uff4c\uff59\uff50\u6cbb\u7642\u5f8c",
    "adenoma\u5207\u9664\u5f8c", "adenoma\u6458\u9664\u5f8c", "adenoma\u6458\u51fa\u5f8c", "adenoma\u6cbb\u7642\u5f8c",
    "Adenoma\u5207\u9664\u5f8c", "Adenoma\u6458\u9664\u5f8c", "Adenoma\u6458\u51fa\u5f8c", "Adenoma\u6cbb\u7642\u5f8c",
    "\uff41\uff44\uff45\uff4e\uff4f\uff4d\uff41\u5207\u9664\u5f8c", "\uff41\uff44\uff45\uff4e\uff4f\uff4d\uff41\u6458\u9664\u5f8c", "\uff41\uff44\uff45\uff4e\uff4f\uff4d\uff41\u6458\u51fa\u5f8c", "\uff41\uff44\uff45\uff4e\uff4f\uff4d\uff41\u6cbb\u7642\u5f8c",
    "\uff21\uff44\uff45\uff4e\uff4f\uff4d\uff41\u5207\u9664\u5f8c", "\uff21\uff44\uff45\uff4e\uff4f\uff4d\uff41\u6458\u9664\u5f8c", "\uff21\uff44\uff45\uff4e\uff4f\uff4d\uff41\u6458\u51fa\u5f8c", "\uff21\uff44\uff45\uff4e\uff4f\uff4d\uff41\u6cbb\u7642\u5f8c"
  )
  
  adenoma_treated_terms <- c(
    "\u817a\u816b\u5207\u9664\u5f8c", "\u817a\u816b\u6458\u9664\u5f8c", "\u817a\u816b\u6458\u51fa\u5f8c", "\u817a\u816b\u6cbb\u7642\u5f8c",
    "adenoma\u5207\u9664\u5f8c", "adenoma\u6458\u9664\u5f8c", "adenoma\u6458\u51fa\u5f8c", "adenoma\u6cbb\u7642\u5f8c",
    "Adenoma\u5207\u9664\u5f8c", "Adenoma\u6458\u9664\u5f8c", "Adenoma\u6458\u51fa\u5f8c", "Adenoma\u6cbb\u7642\u5f8c",
    "\uff41\uff44\uff45\uff4e\uff4f\uff4d\uff41\u5207\u9664\u5f8c", "\uff41\uff44\uff45\uff4e\uff4f\uff4d\uff41\u6458\u9664\u5f8c", "\uff41\uff44\uff45\uff4e\uff4f\uff4d\uff41\u6458\u51fa\u5f8c", "\uff41\uff44\uff45\uff4e\uff4f\uff4d\uff41\u6cbb\u7642\u5f8c",
    "\uff21\uff44\uff45\uff4e\uff4f\uff4d\uff41\u5207\u9664\u5f8c", "\uff21\uff44\uff45\uff4e\uff4f\uff4d\uff41\u6458\u9664\u5f8c", "\uff21\uff44\uff45\uff4e\uff4f\uff4d\uff41\u6458\u51fa\u5f8c", "\uff21\uff44\uff45\uff4e\uff4f\uff4d\uff41\u6cbb\u7642\u5f8c"
  )
  
  polyp_pattern <- paste(c(polyp_terms, polyposis_terms), collapse = "|")
  polyposis_pattern <- paste(polyposis_terms, collapse = "|")
  polyp_treated_pattern <- paste(polyp_treated_terms, collapse = "|")
  adenoma_treated_pattern <- paste(adenoma_treated_terms, collapse = "|")
  
  num_polyp <- sum(grepl(polyp_pattern, split_values)) -
    sum(grepl(polyp_treated_pattern, split_values))
  
  num_polyppositive <- sum(grepl(polyp_pattern, x)) -
    sum(grepl(polyp_treated_pattern, x))
  
  num_adenoma <- sum(grepl("\u817a\u816b|adenoma|Adenoma|\uff41\uff44\uff45\uff4e\uff4f\uff4d\uff41|\uff21\uff44\uff45\uff4e\uff4f\uff4d\uff41", split_values)) -
    sum(grepl("\u5bb6\u65cf\u6027\u5927\u8178\u817a\u816b\u75c7|FAP|\uff26\uff21\uff30", split_values)) -
    sum(grepl(adenoma_treated_pattern, split_values))
  
  num_adenomapositive <- sum(grepl("\u817a\u816b|adenoma|Adenoma|\uff41\uff44\uff45\uff4e\uff4f\uff4d\uff41|\uff21\uff44\uff45\uff4e\uff4f\uff4d\uff41", x)) -
    sum(grepl("\u5bb6\u65cf\u6027\u5927\u8178\u817a\u816b\u75c7|FAP|\uff26\uff21\uff30", x)) -
    sum(grepl(adenoma_treated_pattern, x))
  
  num_polyposis <- sum(grepl(polyposis_pattern, x))
  
  ppc <- num_polyp / length(x)
  pdr <- num_polyppositive / length(x)
  apc <- num_adenoma / (length(x) - num_polyposis)
  adr <- num_adenomapositive / (length(x) - num_polyposis)
  
  result <- list(
    APC = apc,
    ADR = adr,
    PPC = ppc,
    PDR = pdr
  )
  
  print(
    paste0(
      "APC:", format(apc, digits = 5),
      ", ADR:", format(adr, digits = 5),
      ", PPC:", format(ppc, digits = 5),
      ", PDR:", format(pdr, digits = 5)
    )
  )
  
  return(result)
}