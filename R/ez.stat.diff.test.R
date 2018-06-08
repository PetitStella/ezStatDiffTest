#' Adjust p-value for multiple comparisons
#'
#' @description Given a set of p-values, returns adjusted p-values.
#'
#' @importFrom stats p.adjust
#'
#' @param p.value numeric vector of p-values.
#' @param adjust.method method for adjusting p-values.
#'
adjust.p.value <- function(p.value, adjust.method = c("holm", "bonferroni", "BH")){
  if((adjust.method == "holm")||(adjust.method == "Holm")){
    p.adjust <- stats::p.adjust(p.value, method = "holm", n = length(p.value))
    resultMessage <- "P value adjustment method: holm"
  }
  else if((adjust.method == "bonferroni")||(adjust.method == "Bonferroni")||(adjust.method == "Bonf")||(adjust.method == "bonf")){
    p.adjust <- stats::p.adjust(p.value, method = "bonferroni", n = length(p.value))
    resultMessage <- "P value adjustment method: bonferroni"
  }
  else if(adjust.method == "BH"){
    p.adjust <- stats::p.adjust(p.value, method = "BH", n = length(p.value))
    resultMessage <- "P value adjustment method: Benjamini & Hochberg"
  }
  else{
    warning(sprintf("adjust.method = \"%s\" is invalid!", adjust.method))
    return(NA)
  }
  p.adjust <- data.frame(p.adjust)
  return(invisible(list(p.adjust,resultMessage)))
}


#' Calculate p-value
#'
#' @description Perform two sample statistical test for each combination.
#'
#' @importFrom lawstat brunner.munzel.test
#' @importFrom stats t.test
#' @importFrom exactRankTests wilcox.exact
#'
#' @param data data frame
#' @param paired a logical indicating whether you want a paired test.
#' @param type select test type whether parametric or non-parametric.
#'
calculate.p.value <- function(data, paired = FALSE, type = c("parametric","non-parametric")){

  xTitle <- colnames(data)

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
        if(type == "parametric"){
          if(paired){
            res <- stats::t.test(data[[i]], data[[j]], paired = T)
          }else{
            res <- stats::t.test(data[[i]], data[[j]], paired = F, var.equal=F)
          }

        }
        else if(type == "non-parametric"){
          if(paired){
            res <- exactRankTests::wilcox.exact(data[[i]], data[[j]], alternative="t",paired=T)
          }else{
            res <-lawstat::brunner.munzel.test(data[[i]], data[[j]])
          }
        }
        else{
          # エラー処理
          stop(paste(sprintf("method = \"%s\" is invalid!", type)))
        }

        # p値を取り出し
        p.value[count] <- res$p.value
        combination[count] <- sprintf("%s - %s",xTitle[i],xTitle[j])
        count <- count + 1
      }
    }
    jStart <- jStart + 1
  }

  if(type == "parametric"){
    if(paired){
      resultMessage <- "Pairwise comparisons using paired t tests"
    }else{
      resultMessage <- "Pairwise comparisons using welch t tests"
    }
  }
  else if(type == "non-parametric"){
    if(paired == TRUE){
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
#'
#' @description  Plot bar graphs (parametric case) or box plots (non-parametric case).
#'
#' @import ggplot2
#' @importFrom graphics plot
#' @importFrom stats sd
#' @importFrom tidyr gather
#'
#' @param data response vector.
#' @param type select test type whether parametric or non-parametric.
#' @param group.name a name for the group (default: "group")
#' @param value.name a name for the value (default: "value")
#'
plot.graph <- function(data, type = c("parametric","non-parametric"), group.name = "group", value.name = "value"){

    ggplot()+theme_set(theme_classic(base_size = 16))

  xTitle <- colnames(data)
  if(type == "parametric"){
    df <- data.frame(
      group = colnames(data),
      mean = apply(data, 2, mean),
      sd = apply(data, 2, sd)
    )
    df$group <- factor(df$group, levels=colnames(data))
    g <- ggplot(df, aes(x = group, y = mean, fill = group))
    g <- g + geom_bar(stat = "identity")
    g <- g + geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd, width = 0.1))
    g <- g + labs(x = group.name, y = value.name, fill=group.name)
    plot(g)
  }
  else if(type == "non-parametric"){
    df <- gather(data, key = "group")
    df$group <- factor(df$group, levels=colnames(data))
    g <- ggplot(df, aes(y=value, x = group, fill = group))
    g <- g + stat_boxplot(geom = "errorbar", width=0.3)
    g <- g+geom_boxplot()
    g <- g + labs(x = group.name, y = value.name, fill = group.name)
    plot(g)
  }
}


#' Conduct statistical difference tests
#'
#' @description Calculate two sample or pairwise comparisons with corrections.
#'
#' @param data response vector.
#' @param paired a logical indicating whether you want a paired test.
#' @param type select test type whether parametric or non-parametric.
#' @param adjust.method method for adjusting p-values.
#' @param plot a logical indicating whether you want a plot graph.
#' @param group.name a name for the group (default: "group")
#' @param value.name a name for the value (default: "value")
#'
#'
#' @examples {
#' A <- c(1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2)
#' B <- c(3, 3, 4, 3, 1, 2, 3, 1, 1, 5, 4)
#' data <- data.frame(A,B)
#'
#' ez.stat.diff.test(data, "parametric", paired=TRUE, adjust.method = "holm")
#' }
#' @export
#'
ez.stat.diff.test <- function(data,
                              type = c("parametric", "non-parametric"),
                              paired = FALSE,
                              adjust.method = c("holm", "bonferroni", "BH"),
                              plot = TRUE,
                              group.name = "group",
                              value.name = "value"){
  options(scipen=10)
  options(error=NULL)

  if(!is.data.frame(data)){
    stop(paste("argument \"data\" must be data.frame"))
  }

  p <- calculate.p.value(data, paired, type)

  if(plot){
    plot.graph(data, type, group.name, value.name)
  }


  result <- adjust.p.value(p[[1]], adjust.method)

  rownames(result[[1]]) <- p[[2]]

  print(result[[1]])
  cat(sprintf("\n%s\n", result[[2]]))

  return(invisible(result[[1]]));
}


#' Check probability density
#'
#' @description Compute and plot probability density.
#'
#' @import ggplot2
#' @importFrom stats density
#'
#' @param data response vector.
#' @param group.name a name for the group (default: "group")
#' @param value.name a name for the value (default: "value")
#'
#' @examples {
#' A <- c(1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2)
#' B <- c(3, 3, 4, 3, 1, 2, 3, 1, 1, 5, 4)
#' data <- data.frame(A,B)
#'
#' ez.check.density(data)
#' }
#'
#' @export
#'
ez.check.density <- function(data,
                             group.name = "group",
                             value.name = "value"){
  df <- gather(data, key = "group")
  df$group <- factor(df$group, levels=colnames(data))

  dens <- density(df$value)

  g <- ggplot(df, aes(x=value))
  g <- g + geom_density(aes(fill=group),size=0.5,alpha=0.4)
  g <- g + xlim(range(dens$x))
  g <- g + labs(x = value.name, fill = group.name)
  plot(g)
}

