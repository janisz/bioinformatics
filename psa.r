psa <- function(A, B) {

  string_to_vector <- function(x) {
      len <- nchar(x)
      return(substring(x, 1:len, 1:len))
  }

  move_cost <- function() {
      x <- pointer[1]
      y <- pointer[2]
      vertical_gap_penalty <- if (semi_global_mode && pointer[1] == (length(a))) 1 else -1
      vertical_move <- psa_array[x,y - 1] +  if (y > 0) vertical_gap_penalty else -Inf
      horizontal_gap_penalty <- if (semi_global_mode && y == (length(b))) 1 else -1
      horizontalMove <- psa_array[x - 1,y] + if (x > 0) horizontal_gap_penalty else -Inf
      if (x > 1 && y > 1 && a[x - 1] == b[y - 1]) {
        diagonalScore <- 1
        diagonalMove <- psa_array[x - 1,y - 1] + diagonalScore
      }  else {
        diagonalScore <- -Inf
        diagonalMove <- diagonalScore
      }
      moves_cost <- c(vertical_move, horizontalMove, diagonalMove)
      move_types <- rbind(c(0, -1),   # vertical
                          c(-1, 0),   # horizontal
                          c(-1, -1))  # diagonal
      return(c(max(moves_cost), move_types[which.max(moves_cost),]))
  }

    a <-  string_to_vector(A)
    b <- string_to_vector(B)
    a_len <- length(a)
    b_len <- length(b)
    semi_global_mode <-  min(a_len, b_len)/max(a_len, b_len) < 0.6

    psa_array <- array(0, dim=c(a_len+1, b_len+1))
    psa_array[1,] <- if (!semi_global_mode) 0:-b_len
    psa_array[,1] <- if (!semi_global_mode) 0:-a_len

    for (i in 1:a_len+1) {
        for (j in 1:b_len+1) {
            pointer <- c(i, j)
            psa_array[pointer[1], pointer[2]] <- move_cost()[1]
        }
    }

    pointer <- c(a_len+1,b_len+1)
    optimal_move_cost <- psa_array[pointer[1], pointer[2]]
    alignment_path <- c()
    while ((pointer[1] > 2) || (pointer[2] > 2)) {
        best_move <- move_cost()
        move <- best_move[2:3]
        alignment_path <- rbind(alignment_path, move)
        pointer <- pointer + move
    }
    return(tail(c(psa_array),1))
}

psa("ACACT", "ACT")
psa("ATGCTAAGCATA", "TACGATTCGTAT")
psa("ATGCTAAGCATA", "TTAGCTAAGCATA")
