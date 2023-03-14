#' @title lnRR_Q10
#' @description Calculates the log Q10 response ratio.  Note that temperature 2 is placed in the numerator and temperature 1 is in the denominator
#' @param t1  Lowest of the two treatment temperatures
#' @param t2  Highest of the two treatment tempertures
#' @param r1  Mean physiological rate for temperature 1
#' @param r2  Mean physiolgical rate for temperature 2
#' @param sd1 Standard deviation for physiological rates at temperature 1
#' @param sd2 Standard deviation for physiological rates at temperature 2
#' @param n1  Sample size at temperature 1
#' @param n2  Sample size at temperature 2
#' @param name Character string for column name
#' @example
#' lnRR_Q10(20, 30, 10, 5, 1, 1, 30, 30)
#' lnRR_Q10(20, 30, 10, 5, 1, 1, 30, 30, name = "acclim")
#' @export

lnRR_Q10 <- function(t1, t2, r1, r2, sd1, sd2, n1, n2, name ="acute"){
        
    lnRR_Q10 <- (10 / (t2 - t1))   * log(r2 / r1)
  V_lnRR_Q10 <- (10 / (t2 - t1))^2 * ((sd1^2 / (n1*r1^2)) + (sd2^2 / (n2*r2^2)))
  
              dat <- data.frame(lnRR_Q10, V_lnRR_Q10)
    colnames(dat) <- c(paste0("lnRR_Q10", "_", name),  
                       paste0("V_lnRR_Q10", "_", name))
  return(dat)
}

#' @title lnVR_Q10
#' @description Calculates the log Q10 variance ratio.  Note that temperature 2 is placed in the numerator and temperature 1 is in the denominator
#' @param t1  Lowest of the two treatment temperatures
#' @param t2  Highest of the two treatment tempertures
#' @param r1  Mean physiological rate for temperature 1
#' @param r2  Mean physiolgical rate for temperature 2
#' @param sd1 Standard deviation for physiological rates at temperature 1
#' @param sd2 Standard deviation for physiological rates at temperature 2
#' @param n1  Sample size at temperature 1
#' @param n2  Sample size at temperature 2
#' @param name Character string for column name
#' @example
#' lnVR_Q10(20, 30, 10, 5, 1, 1, 30, 30)
#' @export

lnVR_Q10 <- function(t1, t2, r1, r2, sd1, sd2, n1, n2, name = "acute"){
  
    lnVR_Q10 <- (10 / (t2 - t1))   * log(sd2 / sd1)
  V_lnVR_Q10 <- (10 / (t2 - t1))^2 * ((1 / (2*(n1 - 1)) ) + (1 / (2*(n2 - 1))))
  
            dat <- data.frame(lnVR_Q10, V_lnVR_Q10)
  colnames(dat) <- c(paste0("lnVR_Q10", "_", name),  
                     paste0("V_lnVR_Q10", "_", name))
  return(dat)
}


#' @title lnCVR_Q10
#' @description Calculates the log Q10 coefficient of variance ratio. Note that temperature 2 is placed in the numerator and temperature 1 is in the denominator
#' @param t1  Lowest of the two treatment temperatures
#' @param t2  Highest of the two treatment tempertures
#' @param r1  Mean physiological rate for temperature 1
#' @param r2  Mean physiolgical rate for temperature 2
#' @param sd1 Standard deviation for physiological rates at temperature 1
#' @param sd2 Standard deviation for physiological rates at temperature 2
#' @param n1  Sample size at temperature 1
#' @param n2  Sample size at temperature 2
#' @param name Character string for column name
#' @example
#' lnCVR_Q10(20, 30, 10, 5, 1, 1, 30, 30)
#' @export

lnCVR_Q10 <- function(t1, t2, r1, r2, sd1, sd2, n1, n2, name = "acute"){
  
    lnCVR_Q10 <- (10 / (t2 - t1))   * log( (sd2 / r2) / (sd1 / r1) )
  V_lnCVR_Q10 <- (10 / (t2 - t1))^2 * ((1 / (2*(n1 - 1)) ) + (1 / (2*(n2 - 1))) +
                                         (sd1^2 / (n1*r1^2)) + (sd2^2 / (n2*r2^2)))
  
            dat <- data.frame(lnCVR_Q10, V_lnCVR_Q10)
  colnames(dat) <- c(paste0("lnCVR_Q10", "_", name),  
                     paste0("V_lnCVR_Q10", "_", name))
  return(dat)
}


#' @title se_to_sd
#' @description Convert standard error to standard deviations
#' @param se Standard error for physiological rates at temperature 1
#' @param n  Sample size
#' @example
#' se_to_sd(10, 8)
#' @export

se_to_sd <- function(se, n){
  sd <- se*sqrt(n)
  return(sd)
}


## pMCMC Function
# Calculates the, p-value or pMCMC value for a posterior distribution
# x The vector for the posterior distribution. Note that this will test the null hypothesis that the parameter of interest is significantly different from 0. 
pmcmc <- function(x){
  2*(1 - max(table(x<0) / length(x)))
}


#Tree checks function
tree_checks <- function(data, tree, dataCol, type = c("checks", "prune")){
  type = match.arg(type)
  # How many unique species exist in data and tree
  Numbers <- matrix(nrow = 2, ncol = 1)
  Numbers[1,1] <- length(unique(data[,dataCol])) 
  Numbers[2,1] <- length(tree$tip.label) 
  rownames(Numbers)<- c("Species in data:", "Species in tree:")
  # Missing species or species not spelt correct      
  species_list1= setdiff(sort(tree$tip.label), sort(unique(data[,dataCol])))
  species_list2= setdiff(sort(unique(data[,dataCol])), sort(tree$tip.label) )
  if(type == "checks"){
    return(list(SpeciesNumbers = data.frame(Numbers), 
                Species_InTree_But_NotData=species_list1, 
                Species_InData_But_NotTree=species_list2))
  }
  if(type == "prune"){
    if(length(species_list2) >=1) stop("Sorry, you can only prune a tree when you have no taxa existing in the data that are not in the tree")
    return(ape::drop.tip(tree, species_list1))
  }
}








#' Title: calculate Q10 and S10 and their sampling variance
#'
#' @param data 
#' @param temp_1 
#' @param temp_2 
#' @param r1.1 
#' @param r1.2 
#' @param r2.1 
#' @param r2.2 
#' @param r1.1._se 
#' @param r1.2._se 
#' @param r2.1._se 
#' @param r2.2._se 
#' @param r1.1._N 
#' @param r1.2._N 
#' @param r2.1._N 
#' @param r2.2._N 
#'
#' @return
#' @export
#'
#' @examples
Q10S10 <- function(data, temp_1, temp_2, 
                   r1.1, r1.2, r2.1, r2.2, 
                   r1.1_se, r1.2_se, r2.1_se, r2.2_se,
                   r1.1_N, r1.2_N, r2.1_N, r2.2_N) {
  
  temp_1 <- data[[deparse(substitute(temp_1))]]
  temp_2 <- data[[ deparse(substitute(temp_2))]]
  r1.1 <- data[[deparse(substitute(r1.1))]]
  r1.2 <- data[[deparse(substitute(r1.2))]]
  r2.1 <- data[[deparse(substitute(r2.1))]]
  r2.2 <- data[[deparse(substitute(r2.2))]]
  r1.1_se <- data[[ deparse(substitute(r1.1_se))]]
  r1.2_se <- data[[deparse(substitute(r1.2_se))]]
  r2.1_se <- data[[deparse(substitute(r2.1_se))]]
  r2.2_se <- data[[deparse(substitute(r2.2_se))]]
  r1.1_N <- data[[deparse(substitute(r1.1_N))]]
  r1.2_N <- data[[ deparse(substitute(r1.2_N))]]
  r2.1_N <- data[[deparse(substitute(r2.1_N))]]
  r2.2_N <- data[[deparse(substitute(r2.2_N))]]
  
  # cover se to sd
  r1.1_sd <- r1.1_se*sqrt(r1.1_N)
  r1.2_sd <- r1.2_se*sqrt(r1.2_N)
  r2.1_sd <- r2.1_se*sqrt(r2.1_N)
  r2.2_sd <- r2.2_se*sqrt(r2.2_N)
  
  # lnRR equivalent
  # acute thermal sensitivity (cold) - Q10ac
  lnQ10acRR <- (10 / (temp_2 - temp_1)) * log(r1.2 / r1.1)
  V_lnQ10acRR <- (10 / (temp_2 - temp_1))^2 * ((r1.1_sd^2 / (r1.1_N*r1.1^2)) + (r1.2_sd^2 / (r1.2_N*r1.2^2)))
  
  # acute thermal sensitivity (warm) - Q10aw
  lnQ10awRR <- (10 / (temp_2 - temp_1)) * log(r2.2 / r2.1)
  V_lnQ10awRR <- (10 / (temp_2 - temp_1))^2 * ((r2.1_sd^2 / (r2.1_N*r2.1^2)) + (r2.2_sd^2 / (r2.2_N*r2.2^2)))  
  
  # post-acclimation thermal sensitivity - Q10pa
  lnQ10paRR <- (10 / (temp_2 - temp_1)) * log(r2.2 / r1.1)
  V_lnQ10paRR <- (10 / (temp_2 - temp_1))^2 * ((r1.1_sd^2 / (r1.1_N*r1.1^2)) + (r2.2_sd^2 / (r2.2_N*r2.2^2)))  
  
  # lnVR equivalent
  # acute thermal idiosyncrasy (cold) - S10ac
  lnSQ10ac <- (10 / (temp_2 - temp_1)) * log(r1.2_sd / r1.1_sd)
  V_lnSQ10ac <- (10 / (temp_2 - temp_1))^2 * ((1 / (r1.1_N - 1) ) + (1 / (r1.2_N - 1)))
  
  # acute thermal idiosyncrasy (warm) - S10aw
  lnSQ10aw <- (10 / (temp_2 - temp_1)) * log(r2.2_sd / r2.1_sd)
  V_lnSQ10aw <- (10 / (temp_2 - temp_1))^2 * ((1 / (r2.1_N - 1) ) + (1 / (r2.2_N - 1)))
  
  # post-acclimation thermal idiosyncrasy - S10pa
  lnSQ10pa <- (10 / (temp_2 - temp_1)) * log(r2.2_sd / r1.1_sd)
  V_lnSQ10pa <- (10 / (temp_2 - temp_1))^2 * ((1 / (r1.1_N - 1) ) + (1 / (r2.2_N - 1)))
  
  # lnCVR equivalent
  # acute thermal SQ (cold) - SQ10ac
  lnCVQ10ac <- (10 / (temp_2 - temp_1)) * log((r1.2_sd / r1.2) / (r1.1_sd / r1.1))
  V_lnCVQ10ac <- (10 / (temp_2 - temp_1))^2 * ( ((r1.1_sd^2) / (r1.1_N*(r1.1^2)) ) + 
                                                  ( (r1.2_sd)^2 / (r1.2_N *(r1.2^2)) ) + 
                                                  (1 / (2 * (r1.1_N - 1))) + 
                                                  (1 / (2 * (r1.2_N - 1))) )
  
  # acute SQ (warm) - SQ10aw
  lnCVQ10aw <- (10 / (temp_2 - temp_1)) * log((r2.2_sd / r2.2) / (r2.1_sd / r2.1))
  V_lnCVQ10aw <- (10 / (temp_2 - temp_1))^2 * ((1 / (r2.1_N - 1) ) + 
                                                (1 / (r2.2_N - 1)) + (r2.1_sd^2 / (r2.1_N*r2.1^2)) + (r2.2_sd^2 / (r2.2_N*r2.2^2)))
  
  # post-acclimation SQ - SQ10pa
  lnCVQ10pa <- (10 / (temp_2 - temp_1)) * log((r2.2_sd / r2.2) / (r1.1_sd / r1.1))
  V_lnCVQ10pa <- (10 / (temp_2 - temp_1))^2 * ((1 / (r1.1_N - 1) ) + 
                                                (1 / (r2.2_N - 1)) + (r1.1_sd^2 / (r1.1_N*r1.1^2)) + (r2.2_sd^2 / (r2.2_N*r2.2^2)))
  
  
  # putting all together in a tibble and combine
  lnQ10S10 <- tibble(lnQ10acRR = lnQ10acRR, V_lnQ10acRR = V_lnQ10acRR, 
                     lnQ10awRR = lnQ10awRR, V_lnQ10awRR = V_lnQ10awRR, 
                     lnQ10paRR = lnQ10paRR, V_lnQ10paRR = V_lnQ10paRR, 
                     
                     lnSQ10ac = lnSQ10ac, V_lnSQ10ac = V_lnSQ10ac,
                     lnSQ10aw = lnSQ10aw, V_lnSQ10aw = V_lnSQ10aw,
                     lnSQ10pa = lnSQ10pa, V_lnSQ10pa = V_lnSQ10pa,

                     lnCVQ10ac = lnCVQ10ac, V_lnCVQ10ac = V_lnCVQ10ac,
                     lnCVQ10aw = lnCVQ10aw, V_lnCVQ10aw = V_lnCVQ10aw,
                     lnCVQ10pa = lnCVQ10pa, V_lnCVQ10pa = V_lnCVQ10pa)
  data <- bind_cols(data, lnQ10S10)
  return(data)
}

#

#' Title: Functions for multi-level I2
#'
#' @param model 
#' @param method 
#'
#' @return
#' @export
#'
#' @examples
I2 <- function(model, method = c("Wolfgang", "Shinichi")){
  warning("Make sure you have the observation (effec size) level random effect\n")
  ## evaluate choices
  method <- match.arg(method)
  
  # Wolfgang's method
  if(method == "Wolfgang"){
    W <- solve(model$V) 
    X <- model.matrix(model)
    P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
    I2_total <- sum(model$sigma2) / (sum(model$sigma2) + (model$k - model$p) / sum(diag(P)))
    I2_each  <- model$sigma2 / (sum(model$sigma2) + (model$k - model$p) / sum(diag(P)))
    names(I2_each) = paste0("I2_", model$s.names)
    
    # putting all together
    I2s <- c(I2_total = I2_total, I2_each)
    
    # or my way
  } else {
    # sigma2_v = typical sampling error variance
    sigma2_v <- sum(1/model$vi) * (model$k-1) / (sum(1/model$vi)^2 - sum((1/model$vi)^2)) 
    I2_total <- sum(model$sigma2) / (sum(model$sigma2) + sigma2_v) #s^2_t = total variance
    I2_each  <- model$sigma2 / (sum(model$sigma2) + sigma2_v)
    names(I2_each) = paste0("I2_", model$s.names)
    
    # putting all together
    I2s <- c(I2_total = I2_total, I2_each)
  }
  return(I2s)
}


#' Title: Functions for multi-level R2
#'
#' @param model 
#'
#' @return
#' @export
#'
#' @examples
R2 <- function(model){
  warning("Make sure you have the observation (effec size) level random effect as the last in the formula\n")
  
  # fixed effect variance
  fix <- var(as.numeric(as.vector(model$b) %*% t(as.matrix(model$X))))
  
  # marginal
  R2m <- fix / (fix + sum(model$sigma2))
  R2
  #Rm <- round(100*R2m, 3)
  
  # conditional
  R2c <- (fix + sum(model$sigma2) - model$sigma2[length(model$sigma2)]) / 
    (fix + sum(model$sigma2))
  
  R2s <- c(R2_marginal = R2m, R2_coditional = R2c)
  
  return(R2s)
}

#' Title: the function to get estimates
#'
#' @param model: rma.mv object 
#' @param mod: the name of a moderator 
get_est <- function (model, mod = " ") {
  
  name <- as.factor(str_replace(row.names(model$beta), mod, ""))
  estimate <- as.numeric(model$beta)
  lowerCL <- model$ci.lb
  upperCL <- model$ci.ub 
  
  table <- tibble(name = name, estimate = estimate, lowerCL = lowerCL, upperCL = upperCL)
  return(table)
}



# making forest plots

#' Title: s_plot: making forest plots for different categories
#'
#' @param res 
#' @param title 
#' @param N: whether we put sample size (N) on the figure
s_plot <- function(res, title = "", N = FALSE) {
  
  if(N == TRUE) {
    p <- ggplot(res, aes(x = name, y = estimate, colour = name)) +
      geom_pointrange(aes(ymin = lowerCL, ymax = upperCL), size = 1.5) +
      geom_hline(yintercept = 0, lty = 2, lwd = 1, alpha = 0.5) +
      geom_text(aes(y = -0.2, label = str_c("n = ", n)), size = 5, hjust = "left", nudge_x = 0.2) +
      labs(x = "", y = "Effect size", title = title) +
      coord_flip()
  } else {
    p <- ggplot(res, aes(x = name, y = estimate, colour = name)) +
      geom_pointrange(aes(ymin = lowerCL, ymax = upperCL), size = 1.5) +
      geom_hline(yintercept = 0, lty = 2, lwd = 1, alpha = 0.5) +
      labs(x = "", y = "Effect size", title = title) +
      coord_flip()
  }
  # ggplot theme
  p + theme(#panel.background = element_rect(fill = "white"),
    axis.line.x = element_line(size = 2, colour = "black"),
    axis.ticks.x = element_line(size = 2),
    axis.ticks.length = unit(0.3,"cm"),
    axis.text.x = element_text(size = 10),
    axis.ticks.y = element_line(size = 0, colour = "white"),
    axis.text.y = element_text(size = 15, face = "plain", colour = "black"),
    title = element_text(size = 15, face = "italic"),
    axis.title.x = element_text(size = 15, face = "plain"),
    legend.position="none")
}

# making forest plots of all

f_plot <- function(res, title = "", place = "positive") {
  
  if(place == "positive") {
    p <- ggplot(res, aes(x = lab, y = estimate, colour = name)) +
      geom_pointrange(aes(ymin = lowerCL, ymax = upperCL), size = 1.5) +
      geom_hline(yintercept = 0, lty = 2, lwd = 1, alpha = 0.5) +
      geom_text(aes(y = -0.2, label = str_c("n = ", n)), size = 5, hjust = "left", nudge_x = 0.2) +
      #coord_cartesian(ylim = c(-0.2, 1)) + # this is not working
      labs(x = "", y = "effect size", title = title) +
      coord_flip()
  } else {
    p <- ggplot(res, aes(x = lab, y = estimate, colour = name)) +
      geom_pointrange(aes(ymin = lowerCL, ymax = upperCL), size = 1.5) +
      geom_hline(yintercept = 0, lty = 2, lwd = 1, alpha = 0.5) +
      geom_text(aes(y = 0.05, label = str_c("n = ", n)), size = 5, hjust = "right", nudge_x = 0.2) +
      labs(x = "", y = "Effect size", title = title) +
      coord_flip()
  }
  # ggplot theme
  p + theme(#panel.background = element_rect(fill = "white"),
    axis.line.x = element_line(size = 2, colour = "black"),
    axis.ticks.x = element_line(size = 2),
    axis.ticks.length = unit(0.3,"cm"),
    axis.text.x = element_text(size = 10),
    axis.ticks.y = element_line(size = 0, colour = "white"),
    axis.text.y = element_text(size = 15, face = "plain", colour = "black"),
    title = element_text(size = 15, face = "italic"),
    axis.title.x = element_text(size = 15, face = "plain"),
    legend.position="none")
}

## Calculation a correction factor to apply to AIC to calculate AICc from metafor model objects
AICc_correction <- function(model){
  k = length(coef(model)) + length(model$sigma2) + length(model$gamma2) + length(model$tau2)
  cor = (2*k^2 + 2*k) / model$k - k - 1
  cor
}

#' @title p_value
#' @description Checks p-value and assigned to catagories unless non-significant then gives actual p
#' @param x the value to check against
#' 
p_value <- function(x){
  if(x <= 0.0001) {tmp = "< 0.0001"}
  if(x <= 0.001 & x >= 0.0001) {tmp ="< 0.001"}
  if(x <= 0.01 & x >= 0.001) {tmp ="< 0.01"}
  if(x >= 0.01) {tmp = round(x, digits =2)}
  return(tmp)
}

#' @title text
#' @description Converts posterior distribution into a text statement for plotting
#' @param post the posterior distribution as a vector
#' 
 text <- function(post){
      paste0("B = ", round(mean(post), 2), ", 95% CI: ", round(quantile(post, c(0.025, 0.975)[1]), 2), " to ", round(quantile(post, c(0.025, 0.975)[2]), 2))
  }
