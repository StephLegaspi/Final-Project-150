
RowNames <- function(size){
  rows = c()
  for(i in 1:(size+1)){
    rows <- c(rows, i)
  }
  return(rows);
}

ColNames <- function(size){
  cols = c()
  for(i in 1:(size+2)){
    if(i==size+2){  col_i = "RHS"}
    else{ col_i = paste("x", i, sep = "")}
    cols <- c(cols, col_i)
  }
  return(cols);
}

GetPowers <- function(size){
  powers = c()
  for(i in 0:(2*size)){
    powers <- c(powers, i)
  }
  
  x_raised = c()
  for(i in 1:(2*size+1)){
    x_raised = c(x_raised, sum(x^powers[i]))
  }
  return(x_raised)
}

GetRHS <- function(size){
  powers = c()
  rhs_list = c()
  for(i in 1:(size+1)){
    powers <- c(powers, i-1)
  }
  
  for(r in 1:(size+1)){
    raised = x^powers[r]
    rhs = sum(raised * y)
    rhs_list <- c(rhs_list, rhs)
  }
  return(rhs_list)
}


CreateAugCoeff <- function(m, x, y, size){
  x_raised = GetPowers(size)
  summation_y = sum(y)
  p = 0
  for(row in 1:(size+1)){
    for(col in 1:(size+2)){
      if(col == size+2){
        rhs = GetRHS(size)
        #rhs = x_raised[p+1] * summation_y
        m[row, col] = rhs[p+1]
      }else{
        m[row, col] = x_raised[col+p]
      }
    }
    p = p + 1
  }
  return(m)
}

GetVars <- function(size){
  v = ColNames(size)
  v = v[-length(v)]
  #print(v)
  return(v)
}

AugCoeffMatrix <- function(x, y, size){
  len_x = length(x)
  len_y = length(y)
  
  if(len_x != len_y){
    return(NA)
  }else{
    m = matrix(data=0, nrow=size+1, ncol=size+2)
    matrix_result = CreateAugCoeff(m, x, y, size)
    result = list(variables = GetVars(size), augcoeffmatrix = matrix_result)
    return(result)
  }
}

