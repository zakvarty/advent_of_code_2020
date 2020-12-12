get_NN_neighbour <- function(current_layout, row, col, n_rows, n_cols){
  R <- row - 1 ;  C <- col
  in_layout <- between(R, 1, n_rows - 1) & between(C, 1, n_cols - 1)

  while(in_layout){
    if(current_layout[R,C] != ".") return(current_layout[R,C])
    R <- R - 1 ; C <- C
    in_layout <- between(R, 1, n_rows - 1) & between(C, 1, n_cols - 1)
  }
  return(".")
}

get_SS_neighbour <- function(current_layout, row, col, n_rows, n_cols){
  R <- row + 1 ;  C <- col
  in_layout <- between(R, 1, n_rows - 1) & between(C, 1, n_cols - 1)

  while(in_layout){
    if(current_layout[R,C] != ".") return(current_layout[R,C])
    R <- R + 1 ; C <- C
    in_layout <- between(R, 1, n_rows - 1) & between(C, 1, n_cols - 1)
  }
  return(".")
}

get_EE_neighbour <- function(current_layout, row, col, n_rows, n_cols){
  R <- row  ;  C <- col + 1
  in_layout <- between(R, 1, n_rows - 1) & between(C, 1, n_cols - 1)

  while(in_layout){
    if(current_layout[R,C] != ".") return(current_layout[R,C])
    R <- R  ; C <- C + 1
    in_layout <- between(R, 1, n_rows - 1) & between(C, 1, n_cols - 1)
  }
  return(".")
}

get_WW_neighbour <- function(current_layout, row, col, n_rows, n_cols){
  R <- row  ;  C <- col - 1
  in_layout <- between(R, 1, n_rows - 1) & between(C, 1, n_cols - 1)

  while(in_layout){
    if(current_layout[R,C] != ".") return(current_layout[R,C])
    R <- R  ; C <- C - 1
    in_layout <- between(R, 1, n_rows - 1) & between(C, 1, n_cols - 1)
  }
  return(".")
}

get_NE_neighbour <- function(current_layout, row, col, n_rows, n_cols){
  R <- row - 1  ;  C <- col + 1
  in_layout <- between(R, 1, n_rows - 1) & between(C, 1, n_cols - 1)

  while(in_layout){
    if(current_layout[R,C] != ".") return(current_layout[R,C])
    R <- R - 1  ; C <- C + 1
    in_layout <- between(R, 1, n_rows - 1) & between(C, 1, n_cols - 1)
  }
  return(".")
}

get_SE_neighbour <- function(current_layout, row, col, n_rows, n_cols){
  R <- row + 1  ;  C <- col + 1
  in_layout <- between(R, 1, n_rows - 1) & between(C, 1, n_cols - 1)

  while(in_layout){
    if(current_layout[R,C] != ".") return(current_layout[R,C])
    R <- R + 1  ; C <- C + 1
    in_layout <- between(R, 1, n_rows - 1) & between(C, 1, n_cols - 1)
  }
  return(".")
}

get_SW_neighbour <- function(current_layout, row, col, n_rows, n_cols){
  R <- row + 1  ;  C <- col - 1
  in_layout <- between(R, 1, n_rows - 1) & between(C, 1, n_cols - 1)

  while(in_layout){
    if(current_layout[R,C] != ".") return(current_layout[R,C])
    R <- R + 1  ; C <- C - 1
    in_layout <- between(R, 1, n_rows - 1) & between(C, 1, n_cols - 1)
  }
  return(".")
}

get_NW_neighbour <- function(current_layout, row, col, n_rows, n_cols){
  R <- row - 1  ;  C <- col - 1
  in_layout <- between(R, 1, n_rows - 1) & between(C, 1, n_cols - 1)

  while(in_layout){
    if(current_layout[R,C] != ".") return(current_layout[R,C])
    R <- R - 1  ; C <- C - 1
    in_layout <- between(R, 1, n_rows - 1) & between(C, 1, n_cols - 1)
  }
  return(".")
}
