#' Adjust p-value
#' @importFrom stats p.adjust
#' @param p.value p-value to adjust
#' @param type parametric of non-parametric
#' @param adjust.method p-values adjusted using several methods
adjust.p.value <- function(p, type = c("parametric","non-parametric"), adjust.method = c("holm", "bonferroni", "BH")){
  if((adjust.method == "holm")||(adjust.method == "Holm")){
    p.adjust <- p.adjust(p, method = "holm", n = length(p))
    resultMessage <- "P value adjustment method: holm"
  }
  else if((adjust.method == "bonferroni")||(adjust.method == "Bonferroni")||(adjust.method == "Bonf")||(adjust.method == "bonf")){
    p.adjust <- p.adjust(p, method = "bonferroni", n = length(p))
    resultMessage <- "P value adjustment method: bonferroni"
  }
  else if(adjust.method == "BH"){
    p.adjust <- p.adjust(p, method = "BH", n = length(p))
    resultMessage <- "P value adjustment method: Benjamini & Hochberg"
  }
  else{
    # エラー処理
    warning(sprintf("adjust.method = \"%s\" is invalid!", adjust.method))
    return(NA)
  }
  p.adjust <- data.frame(p.adjust)
  print(p.adjust)
  cat(sprintf("\n%s\n", resultMessage))
  return(p.adjust)
}
