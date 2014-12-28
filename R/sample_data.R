sample_data = function(NROW = 5, NCOL = 5){
  if (NROW > 26 || NCOL > 26){
    stop('Number of segments per hemisphere must be less than 26')
  }
  if (NROW < 1 || NCOL < 1){
    stop('Number of segments per hemisphere must be greater than 0')
  }
  data = data.frame(FROM = rep(letters[1:NCOL], each = NROW),
                    TO = rep(LETTERS[1:NROW], NCOL),
                    OUT = 1:(NROW * NCOL),
                    IN = (NROW * NCOL):1)
}