# 
#   Simulate data with varying levels of sparseness to illustrate ROC and 
#   Precision-recall behavior.
#

setwd("~/Work/mireg-blogs-not-on-git/auc-pr")

library(ggplot2)
library(scales)
library(xtable)

# Latex code; skip
# \begin{table}[ht]
# \centering
# \begin{tabular}{r|c|c|}
# \multicolumn{1}{c}{} & \multicolumn{1}{c}{$p<\theta$} & \multicolumn{1}{c}{$p\geq\theta$} \\ \cline{2-3}
# Y=0 & True Neg. & False Pos. \\ \cline{2-3}
# Y=1 & False Neg. & True Pos. \\ \cline{2-3}
# \end{tabular}
# \end{table}
# 
# \begin{table}[ht]
# \centering
# \begin{tabular}{r|c|c|}
# \multicolumn{1}{c}{} & \multicolumn{1}{c}{$p<\theta$} & \multicolumn{1}{c}{$p\geq\theta$} \\ \cline{2-3}
# Y=0 & 10,000 & 1,000 \\ \cline{2-3}
# Y=1 & 100 & 100 \\ \cline{2-3}
# \end{tabular}
# \end{table}



# Simulate binary data and predictions given wanted class imbalanc --------
#
#   This is accomplished by mapping a vector of initial predictions to an 
#   outcome y so that P(p[positive outcome] > p[]
# 
#   The predictions for a given imbalance level are comparable accross specified
#   AUC, i.e. they have the same distribution, just the mapping to outcomes is 
#   different. 
#
#   Probalistic, so different runs will give different results, especially for 
#   very imbalanced data. 
#
sim_model <- function(pos_neg, a, n, shuffle=100, plot=FALSE) {
  # Outcome
  y <- rbinom(n, 1, pos_neg)
  
  # Create predictions:
  # Raw predictions are logistic distribution shifted so that `a` fraction 
  # of density is above 0
  p <- plogis(rlogis(n, qlogis(pos_neg, lower.tail=TRUE), 1))
  
  # We will compare all positives to a sample of negatives, and reassign
  # probabilities with p = desired AUC (a)
  # 
  # (Assumes n+ < n- (positive outcomes is minority), won't work otherwise)
  idx_pos <- which(y==1)
  
  # Not sure how to explain this part, result of trial and error
  a <- qbeta(a, shape1=0.58, shape2=0.58)
  
  # Initialize results; for checking 
  res <- data.frame(n=vector("integer", 20), auc_roc=vector("numeric", 20))
  
  # Since n+ < n-, we need several iterations to shuffle probabilities around
  for (n in 1:shuffle) {
    
    # Sample p_{y=0} for pair comparison
    idx_neg <- sample(which(y==0), sum(y))
    prob <- runif(length(idx_pos))
    
    # For each +/- pair, flip with probability `a`
    for (i in 1:length(idx_pos)) {
      if (p[idx_pos[i]] < p[idx_neg[i]] & prob[i] < a) {
        temp <- p[idx_pos[i]]
        p[idx_pos[i]] <- p[idx_neg[i]]
        p[idx_neg[i]] <- temp
      } 
      if (p[idx_pos[i]] > p[idx_neg[i]] & prob[i] > a) {
        temp <- p[idx_pos[i]]
        p[idx_pos[i]] <- p[idx_neg[i]]
        p[idx_neg[i]] <- temp
      }
    }
    res[n, 1] <- n
    res[n, 2] <- auc_roc(y, p)
  }
  
  if (plot) {
    print(qplot(data=res, x=n, y=auc_roc, ylim=c(0.5, 1)))
  }
  
  mod <- data.frame(y, p)
  return(mod)
}



# Functions for AUC-ROC and AUC-PR ----------------------------------------


library(ROCR)
library(caTools)

#' Area under the ROC curve
#'
#' improt ROCR
auc_roc <- function(obs, pred) {
  pred <- prediction(pred, obs)
  auc  <- performance(pred, "auc")@y.values[[1]]
  return(auc)
}

#' Area under Precision-recall curve
#'
#' import ROCR
#' import caTools
auc_pr <- function(obs, pred) {
  xx.df <- prediction(pred, obs)
  perf  <- performance(xx.df, "prec", "rec")
  xy    <- data.frame(recall=perf@x.values[[1]], precision=perf@y.values[[1]])
  
  # take out division by 0 for lowest threshold
  xy <- subset(xy, !is.nan(xy$precision))
  
  res   <- trapz(xy$recall, xy$precision)
  res
}


# # Beta beta;
# foo <- data.frame(a=seq(0.5, 1, by=0.01), ar=vector("numeric", 51))
# for (a in seq(0.5, 1, by=0.01)) {
#   res <- sim_model(0.4, a, 1000, shuffle=50)
#   foo[foo$a==a, 2] <- mean(res[25:50, 2])
# }
# qplot(foo$a, foo$ar) + 
#   stat_function(fun=pbeta, args=list(shape1=0.57, shape2=0.57))



# Run simulation ----------------------------------------------------------

set.seed(1235)

bal <- c(0.4, 0.1, 0.01)
auc_want <- c(0.8, 0.9, 0.95)

n_sims = 5000

for (b in seq_along(bal)) {
  for (a in seq_along(auc_want)) {
    cat(paste0("Balance ", bal[b], "  AUC ", auc_want[a], "\n"))
    sim_name <- paste0("s", 10*b + a)
    assign(sim_name, sim_model(pos_neg=bal[b], a=auc_want[a], n=n_sims, shuffle=50))
  }
}



# Example ROC curve and data ----------------------------------------------
#
#   We'll just use the first simulation from above, `s11`, loop through all
#   possible treshold values (which are equivalent to the unique p values),
#   and record the ROC space x-y coordinates associated with it 
#   (i.e. true and false positive rates for each treshold)
#

theta <- rev(sort(c(unique(s11$p), 0)))

roc_ex <- matrix(NA, nrow=length(theta), ncol=4, dimnames=list(NULL, c("theta", "tpr", "fpr", "prec")))
for (t in seq_along(theta)) {
  # Confusion matrix
  # with(s11, table(y, factor(p>theta[t], levels=c(FALSE, TRUE))))
  p_bin <- as.numeric(s11$p > theta[t])
  roc_ex[t, ] <- c(
    theta[t],
    sum(s11$y==1 & p_bin==1) / sum(s11$y==1),
    sum(s11$y==0 & p_bin==1) / sum(s11$y==0),
    sum(s11$y==1 & p_bin==1) / sum(p_bin==1)
  )
}

print(xtable(head(roc_ex), digits=5), type="html", include.rownames=FALSE)

# ROC curve
p <- ggplot(as.data.frame(roc_ex)) + 
  geom_line(aes(x=fpr, y=tpr), show_guide=TRUE, alpha=0.7) +
  geom_abline(slope=1, color="gray", alpha=0.5) +
  scale_x_continuous(expand = c(0.01, 0.01)) + 
  scale_y_continuous(expand = c(0.01, 0.01)) + 
  labs(x="FPR", y="TPR") +
  theme_bw()

ggsave(filename="graphics/roc-example.png", plot=p, width=2.2, height=2, units="in",
       dpi=400, scale=1.5)

# Instead of FPR, plot precision to make a precision-recall curve
p <- ggplot(as.data.frame(roc_ex)) + 
  geom_line(aes(x=tpr, y=prec), alpha=0.7) +
  scale_x_continuous(expand = c(0.01, 0.01)) + 
  scale_y_continuous(expand = c(0.01, 0.01)) + 
  labs(x="Recall (TPR)", y="Precision") +
  theme_bw()

ggsave(filename="graphics/pr-example.png", plot=p, width=2.2, height=2, units="in",
       dpi=400, scale=1.5)



# Plot results ------------------------------------------------------------


# For a vector of observed and predicted, creates x-y coordinates for a ROC
# or PR curve.
rocdf <- function(pred, obs, data=NULL, type=NULL) {
  # plot_type is "roc" or "rp"
  if (!is.null(data)) {
    pred <- eval(substitute(pred), envir=data)
    obs  <- eval(substitute(obs), envir=data)
  }
  
  rocr_xy <- switch(type, roc=c("tpr", "fpr"), pr=c("prec", "rec"))
  rocr_df <- prediction(pred, obs)
  rocr_pr <- performance(rocr_df, rocr_xy[1], rocr_xy[2])
  xy <- data.frame(rocr_pr@x.values[[1]], rocr_pr@y.values[[1]])
  colnames(xy) <- switch(type, roc=c("tpr", "fpr"), pr=c("rec", "prec"))
  return(xy)
}

# Combine xy coords for simulation ROC curves
roc_xy <- data.frame(NULL)
for (sim in ls()[grep("s[0-9]", ls())]) {
  sim_res <- get(sim)
  xy <- cbind(data.frame(
    balance = bal[as.numeric(substr(sim, 2, 2))],
    auc_roc = auc_want[as.numeric(substr(sim, 3, 3))]),
    rocdf(p, y, sim_res, type="roc")
  )
  roc_xy <- rbind(roc_xy, xy)
}
rm(sim_res, xy)

roc_xy$balance <- factor(roc_xy$balance, levels=rev(sort(bal)))
roc_xy$auc_roc <- factor(roc_xy$auc_roc, levels=rev(sort(auc_want)))

# ROC curves
p1 <- ggplot(data=roc_xy, aes(x=tpr, y=fpr, color=factor(auc_roc))) +
  facet_wrap(~ balance) +
  scale_x_continuous(expand = c(0.01, 0.01)) + 
  scale_y_continuous(expand = c(0.01, 0.01)) + 
  geom_line(show_guide=TRUE, alpha=0.7) +
  geom_abline(slope=1, color="gray", alpha=0.5) +
  labs(x="FPR", y="TPR") + 
  scale_color_discrete(name="Approx.\nAUC") +
  theme_bw()

p1

ggsave(filename="graphics/roc.png", plot=p1, width=6.4, height=2, units="in",
       dpi=400, scale=1.5)


# Combine xy coords for simulation PR curves
pr_xy <- data.frame(NULL)
for (sim in ls()[grep("s[0-9]", ls())]) {
  sim_res <- get(sim)
  xy <- cbind(data.frame(
    balance = bal[as.numeric(substr(sim, 2, 2))],
    auc_roc = auc_want[as.numeric(substr(sim, 3, 3))]),
    rocdf(p, y, sim_res, type="pr")
  )
  pr_xy <- rbind(pr_xy, xy)
}
rm(sim_res, xy)

pr_xy$balance <- factor(pr_xy$balance, levels=rev(sort(bal)))
pr_xy$auc_roc <- factor(pr_xy$auc_roc, levels=rev(sort(auc_want)))

# Precision-recall curves
p2 <- ggplot(data=pr_xy, aes(x=rec, y=prec, col=auc_roc)) + 
  facet_wrap(~ balance) +
  geom_line(show_guide=TRUE, alpha=0.7) + 
  scale_x_continuous(expand = c(0.01, 0.01)) + 
  scale_y_continuous(expand = c(0.01, 0.01)) + 
  labs(x="Recall", y="Precision") +
  scale_color_discrete(name="Approx.\nAUC") +
  theme_bw()
p2

ggsave(filename="graphics/rpc.png", plot=p2, width=6.4, height=2, units="in",
       dpi=400, scale=1.5)


# Separation plots for all 9 models

library(separationplot)

png("graphics/all-seps.png", width=1800, height=1800)
par(mfrow=c(3, 3))
for (b in 1:3) {
  for (a in 1:3) {
    dat <- get(paste0("s", a, b))
    p_title <- paste0()
    separationplot(dat[, "p"], dat[, "y"], newplot=FALSE, heading=paste0(a, b))
  }
}
dev.off()
