this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
source("aug_coeff.r")

GetUpperTriangular <- function(a, n){
  for(i in 1:(n-1)){
    p = FindPivotRow(i, a, n)
    if(a[p$piv_index, i] == 0){
      return(FALSE)
    }
    
    temp = a[i, ]
    a[i, ] = p$piv_row
    a[p$piv_index, ] = temp
    for(j in (i+1):n){
      PE = a[i, i]
      MULT = a[j, i]/PE
      NR = MULT * a[i, ]
      a[j,] = a[j,] - NR
    }
  }
  return(a)
}

GetCoeffs <- function(a, n){
  b = c()
  column = n+1
  for(row in 1:n){
    b <- c(b, a[row, column])
  }
  return(b)
}

BackwardSubstitution <- function(a, n){
  x = c()
  b = GetCoeffs(a, n)
  x[n] = a[n, n+1] / a[n, n]
  
  for(i in (n-1):1){
    sum_ans = sum(a[i, (i+1):n] * x[(i+1):n])
    x[i] = (b[i] - sum_ans) / a[i, i]
  }
  return(x)
}

GaussianElimination <- function(aug_coeff_matrix, n){ 
  a= GetUpperTriangular(aug_coeff_matrix, n)
  if(a[1] == FALSE){
    print(NA)
  }else{
    x = BackwardSubstitution(a, n)
  }
  result = list(solutionSet = x, variables = vars, matrix = a)
  return(result)
}

FindPivotRowIndex <- function(pivot, column, a, n){
  for(i in column:n){
    if(pivot == abs(a[i, column])){
      pivot_row_index = i
      return(pivot_row_index)
    }
  }
}

FindPivotRow <- function(i, a, n){
  temp = a[i:n,i]
  pivot = max(abs(a[i:n,i]))
  pivot_row_index = FindPivotRowIndex(pivot, i, a, n)
  pivot_row = a[pivot_row_index, ]
  p = list(piv_index = pivot_row_index, piv_row = pivot_row)
  return(p)
}

GetIdentityMatrix <- function(a, n){
  for(i in 1:n){
    if(i != n){
      p = FindPivotRow(i, a, n)
      #print(p)
      if(a[p$piv_index, i] == 0){
        return(FALSE)
      }
      
      temp = a[i, ]
      a[i, ] = p$piv_row
      a[p$piv_index, ] = temp
      
    }
    a[i, ] = a[i, ] / a[i, i]
    
    for(j in 1:n){
      if(i == j){
        next
      }
      NR = a[j, i] * a[i, ]
      a[j, ] = a[j, ] - NR
    }
  }
  return(a)
}

GaussJordanElimination <- function(a, n, vars){
  a = GetIdentityMatrix(a, n)
  #print(a)
  if(a[1] == FALSE){
    print(NA)
  }else{
    x = GetCoeffs(a, n)
  }
  result = list(solutionSet = x, variables = vars, matrix = a)
  return(result)
}



