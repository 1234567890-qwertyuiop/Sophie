weat_perm_apple <- function (items, vectors, x_name, y_name, a_name, b_name, b = 1000)
  # First, compare the function's structure with the function Caliskan team's example:
  # weat_perm(custom, mat, x_name = "Flowers", y_name = "Insects", a_name = "Pleasant", b_name = "Unpleasant", 1000)
  # Do you see the pattern? Functions elements are applied by the order listed in that function,
  # so, this works too:
  # weat_perm(custom, mat, "Flowers", "Insects", "Pleasant","Unpleasant", 1000)
  # So, we are going to use a shorter version from now on.
{
  x_words <- items$Word[items$Condition == x_name]
  y_words <- items$Word[items$Condition == y_name]
  a_words <- items$Word[items$Condition == a_name]
  b_words <- items$Word[items$Condition == b_name]
  # This is saying, among the items$Word, look for a items$Condition that matches whatever
  # that is next to the equal sign, then store it as the specified name to the left side of
  # the arrow

  pre_cos <- cbn_cosine(vectors)
  # cosine is used to determine the distance between words

  S_xab   <- apply(pre_cos[x_words, a_words], 1, mean) - apply(pre_cos[x_words, b_words], 1, mean)
  # The author applied the mean function to an array of pre_cosine in each row for x-a pairs
  # minus mean of an array of pre_cosine in each row for x-b pairs
  S_yab   <- apply(pre_cos[y_words, a_words], 1, mean) - apply(pre_cos[y_words, b_words], 1, mean)
  # Like above, mean of y-a pairs minus mean of x-b pairs

  S_xy_denom <- apply(pre_cos[c(x_words, y_words), a_words], 1, mean) -
                apply(pre_cos[c(x_words, y_words), b_words], 1, mean)
  # In S_xab and S_yab, x and y were processed differently because we wanted to look at the
  # differences between sum of all x_words and y_words
  # But, xy are grouped together because we need to use it to obtain the standard deviation
  # of all scores

  S_xyab <- sum(S_xab) - sum(S_yab)
  #       sum of the differences of x-a means and x-b means
  # minus sum of the differences of y-a means and y-b means

  effect <- (mean(S_xab) - mean(S_yab))/sd(S_xy_denom)
  # This is the effect size. The equation can be found in the original paper

  lx <- length(x_words)
  ly <- length(y_words)
  # It looks like we are getting to the p-value

  # Below, we have a permutation method. remember that the default b = 1000. This means that
  # this function will apply 1000 times by default. For this particular analysis, we are
  # going to insert a comment using the sharp symbol '#'
  reps <- rep(NA, b)
  for (i in 1:b) {
    shuf <- sample(c(x_words, y_words))
    # the sample function randomly selects a pair of x_words and y_words

    repl_x_words <- shuf[1:lx]
    # then one random sample is selected among all x_words (lx)

    repl_y_words <- shuf[(lx + 1):(lx + ly)]
    # for y_words, randomly sample from (length of x_words + 1) to (length of x_words + length of y_words)

    S_xab <- apply(pre_cos[repl_x_words, a_words], 1, mean) - apply(pre_cos[repl_x_words, b_words], 1, mean)
    S_yab <- apply(pre_cos[repl_y_words, a_words], 1, mean) - apply(pre_cos[repl_y_words, b_words], 1, mean)
    # Remember how this works?

    reps[i] <- sum(S_xab) - sum(S_yab)
    # reps' object of i is subtraction of those two

  }

  p_val <- sum(reps > S_xyab)/length(reps) # force decimal points
  # the number of instances when reps were greater than the S_xyab over reps
  # is the proability of this occuring when we flip the coin again (That's an analogy).

  df <- data.frame(S_xyab = S_xyab, d = effect, p_value = p_val)
  rownames(df) <- NULL
  df
}
# So much math.