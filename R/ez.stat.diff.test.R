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
#' @param data data frame
#' @param paired a logical indicating whether you want a paired test.
#' @param type parametric of non-parametric
calculate.p.value <- function(data, paired = FALSE, type = c("parametric","non-parametric")){

  xTitle <- colnames(x)

  size <- dim(data)
  chooseNum <- choose(size[2],2)

  p.value <- numeric(chooseNum)  # p値保存用
  combination <- numeric(chooseNum)  # 2標本検定の組み合わせ

  jStart <- 1
  count <- 1

  for(i in 1:size[2]){
    for( j in jStart:size[2]){
      if(i != j){

        # 検定手法選択
        if(method == "parametric"){
          if(paired){
            res <- t.test(data[[i]], data[[j]], paired = T)
          }else{
            res <- t.test(data[[i]], data[[j]], paired = F, var.equal=F)
          }

        }
        else if(method == "non-parametric"){
          if(paired){
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
        p.value[count] <- res$p.value
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
  else if(method == "non-parametric"){
    if(paired == T){
      resultMessage <- "Pairwise comparisons using Wilcoxon signed rank tests"
    }else{
      resultMessage <- "Pairwise comparisons using Brunner-Munzel tests"
    }
  }
  cat("\n-------------------------------------------------------\n")
  cat(sprintf("%s\n", resultMessage))
  cat("-------------------------------------------------------\n\n")

  return(invisible(list(p.value, combination)))
}

#' Plot graph
#' @param data data frame
#' @param type parametric of non-parametric
plot.graph <- function(data, type = c("parametric","non-parametric")){
  if(method == "parametric"){
    meanValue <- apply(data,2,mean, na.rm=TRUE)
    sdValue <- apply(data, 2, sd, na.rm=TRUE)
    yRoof=round(max(meanValue+sdValue)*1.2, 1)
    bar <- barplot(meanValue, ylim=c(0,yRoof), names.arg = xTitle)
    arrows(bar, meanValue-sdValue, bar, meanValue+sdValue, angle=90, length=0.1)
    arrows(bar, meanValue+sdValue, bar, meanValue-sdValue, angle=90, length=0.1)
    axis(side=1, bar, labels=F)
  }
  else if(method == "non-parametric"){
    boxplot(data, range = 0)
  }
}

#' Conduct statistical difference test
#' @param data data frame
#' @param paired a logical indicating whether you want a paired test.
#' @param type parametric of non-parametric.
#' @param adjust.method p-values adjusted using several methods.
#' @param plota a logical indicating whether you want a plot graph.
ez.stat.diff.test <- function(data,
                              type = c("parametric", "non-parametric"),
                              paired = FALSE,
                              adjust.method = c("holm", "bonferroni", "BH"),
                              plot = TRUE){
  options(scipen=10)

  p <- calculate.p.value(data, paired, type)

  if(plot){
    plot.graph(data, type)
  }

  result <- adjust.p.value(p[[1]], type, adjust.method)
  rownames(result) <- p[[2]]
  return(invisible(result));
}
