function (items, vectors, x_name, a_name, b_name, b = 300, se.calc = c("sd", 
                                                                       "quantile")) 
{
  se.calc <- match.arg(se.calc)
  # repeated assignment of variable name to se.calc = c("sd", "quantile")
  
  x_words <- items$Word[items$Condition == x_name]
  # assigns words with corresponding conditions 
  
  x_vecs <- vectors[x_words, ]
  # obtain vectors from the filtered words, which was acquired using cbn_get_item_vectors
  
  repl <- matrix(NA, nrow = length(x_words), ncol = b)
  # create a variable with a matrix with its the number of rows correspond with the number
  # of x_words. This will be used later to store cosine differences between a and b vectors.
  
  a_v <- vectors[items$Condition == a_name, ]
  b_v <- vectors[items$Condition == b_name, ]
  # obtaining vectors for Conditions (i.e., FemaleAttributes, MaleAttributes)
  
  orig_a <- cbn_cosine(colMeans(a_v), x_vecs)
  orig_b <- cbn_cosine(colMeans(b_v), x_vecs)
  # According to the script author, cbn_cosine function "is taken directly from the lsa
  # pacakge but adjusted to operate rowwise.
  # In other words, we are calculating cosine relationship for studied words
  # (i.e., vetor distances for career and gender words)
  
  orig <- orig_a - orig_b
  # The cosine differences between conditions (i.e., FemaleAttributes - MaleAttributes)
  
  
  for (i in 1:b) {
    a_repl <- a_v[sample(1:nrow(a_v), replace = TRUE), ]
    b_repl <- b_v[sample(1:nrow(b_v), replace = TRUE), ]
    # this is randomly sampling a score from the corresponding rows. Sampled score does not
    # get chosen again because replacement = TRUE. There is a good explanation of sampling replacements here https://web.ma.utexas.edu/users/parker/sampling/repl.htm
    
    sims_a <- cbn_cosine(colMeans(a_repl), x_vecs)
    sims_b <- cbn_cosine(colMeans(b_repl), x_vecs)
    
    
    repl[, i] <- sims_a - sims_b
    # This is how you know sim a words are longer or lengthier thn the sim b words
    # But, there is no code here that reports/ identifies the lenthier word
    # So, we may want to create a separate function to achieve this
    
  }
  if (se.calc == "sd") {
    se <- apply(repl, 1, sd)
    tmp <- matrix(c(orig - 2 * se, orig + 2 * se), ncol = 2)
  }
  else {
    tmp <- t(apply(repl, 1, quantile, probs = c(0.025, 0.975)))
  }
  names(tmp) <- c("lwr", "upr")
  df <- data.frame(x = x_words, diff = orig, tmp)
  if (se.calc == "quantile") 
    df$median <- apply(repl, 1, median)
  colnames(df)[1:4] <- c(x_name, "diff", "lwr", "upr")
  df <- df[order(df$diff), ]
  rownames(df) <- NULL
  df
}
