#' Adjust p-value
#' @importFrom stats p.adjust
#' @param p.value p-value to adjust
#' @param type parametric of non-parametric
#' @param adjust.method p-values adjusted using several methods
adjust.p.value <- function(p.value, type = c("parametric","non-parametric"), adjust.method = c("holm", "bonferroni", "BH")){
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
    warning(sprintf("adjust.method = \"%s\" is invalid!", adjust.method))
    return(NA)
  }
  p.adjust <- data.frame(p.adjust)
  print(p.adjust)
  cat(sprintf("\n%s\n", resultMessage))
  return(invisible(p.adjust))
}

#' Calculate p-value
#' @importFrom lawstat brunner.munzel.test
#' @importFrom stats t.test
#' @importFrom exactRankTests wilcox.exact
#' @param x numeric vector of data values.
#' @param paired a logical indicating whether you want a paired test.
calculate.p.value <- function(x, paired = c(T, F), type = c("parametric","non-parametric")){

  xTitle <- colnames(data)

  size <- dim(data)
  chooseNum <- choose(size[2],2)

  p <- numeric(chooseNum)  # p値保存用
  combination <- numeric(chooseNum)  # 2標本検定の組み合わせ

  jStart <- 1
  count <- 1

  for(i in 1:size[2]){
    for( j in jStart:size[2]){
      if(i != j){

        # 検定手法選択
        if(method == "parametric"){
          if(paired == T){
            res <- t.test(data[[i]], data[[j]], paired = T)
          }else{
            res <- t.test(data[[i]], data[[j]], paired = F, var.equal=F)
          }

        }
        else if(method == "non-parametric"){
          if(paired == T){
            res <- wilcox.exact(data[[i]], data[[j]], alternative="t",paired=T)
          }else{
            res <-brunner.munzel.test(data[[i]], data[[j]])
          }
        }
        else{
          # エラー処理
          warning(sprintf("method = \"%s\" is invalid!", method))
          return(NA)
        }

        # p値を取り出し
        p[count] <- res$p.value
        combination[count] <- sprintf("%s-%s",xTitle[i],xTitle[j])
        count <- count + 1
      }
    }
    jStart <- jStart + 1
  }

  if(method == "parametric"){
    if(paired == T){
      resultMessage <- "Pairwise comparisons using paired t tests"
    }else{
      resultMessage <- "Pairwise comparisons using welch t tests"
    }
  }
  cat("\n-------------------------------------------------------\n")
  cat(sprintf("%s\n", resultMessage))
  cat("-------------------------------------------------------\n\n")

}

