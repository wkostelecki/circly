sample_data = function(NROW = 5, NCOL = 5){
  if (NROW > 26 || NCOL > 26){
    stop('Number of segments per hemisphere must be less than 26')
  }
  if (NROW < 1 || NCOL < 1){
    stop('Number of segments per hemisphere must be greater than 0')
  }
  data = data.frame(FROM_AMOUNT = 1:(NROW * NCOL),
                    TO_AMOUNT = (NROW * NCOL):1,
                    FROM_LABEL = rep(letters[1:NCOL], each = NROW),
                    TO_LABEL = rep(LETTERS[1:NROW], NCOL))
}