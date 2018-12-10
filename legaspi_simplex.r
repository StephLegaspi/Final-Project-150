
GetPivotColumn <- function(last_row, col_tableu){
  for(j in 1:(col_tableu-1)){
    if(last_row[j] < 0 ){
      max_col = j
      break
    }
  }
  
  for(i in 1:(col_tableu-1)){
    if(last_row[i] < 0){
      if(abs(last_row[i]) > abs(last_row[max_col])){ 
        max_col = i 
      }
    }
  }
  return(max_col)
}

CheckNegative <- function(last_row, col_tableu){
  for(i in 1:(col_tableu-1)){
    if(last_row[i] < 0) { return(TRUE) }
  }
  return(FALSE)
}

GetTestRatio <- function(pivot_col, RHS, row_tableu){
  pivot_col <- pivot_col[-row_tableu]
  RHS <- RHS[-row_tableu]
  test_ratio = RHS / pivot_col
  #print(test_ratio)
  
  for(j in 1:(row_tableu-1)){
    if(test_ratio[j] > 0){
      smallest_pos_index = j
      break
    }
  }
  
  for(i in 1:(row_tableu-1)){
    if(test_ratio[i] > 0 && test_ratio[i] < test_ratio[smallest_pos_index]){
      smallest_pos_index = i
    }
  }
  
  return(smallest_pos_index)
}

ComputeTemp <- function(val_to_zero, pivot_row){
  temp = val_to_zero * pivot_row
  return(temp)
}

ComputeNewRow <- function(temp, old_row){
  new_row = old_row -temp
  return(new_row)
}

InitValues <- function(col_names, col){
  values <- list()
  
  for(i in 1:(col-1)){
    values[col_names[i]] = 0
  }
  
  return(values)
}


getRHS_given <- function(init_matrix, row, col){
  RHS <- c()
  
  for(i in 1:3){
    RHS <- c(RHS, init_matrix[i, 1])
  }
  
  for(j in 2:6){
    RHS <- c(RHS, init_matrix[row, j])
  }
  print(RHS)
  return(RHS)
}

preprocessMatrix <- function(init_matrix){
  init_tableu = matrix(data=0, nrow = 9, ncol = 25)
  r = nrow(init_tableu)
  c = ncol(init_tableu)
  r2 = nrow(init_matrix)
  c2 = ncol(init_matrix)
  dimnames(init_tableu) = list(
    c("s1", "s2", "s3", "s4","s5","s6","s7","s8", "Z"),
    c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "Z", "RHS")
  )
  
  col_counter = 1
  for(row in 1:3){
    for(col in 1:5){
      init_tableu[row, paste("s", row, sep = "")] = 1
      init_tableu[row, col_counter] = 1
      col_counter = col_counter+1
    }
  }
  
  col_counter = 1
  old_counter = col_counter
  for(row in 4:8){
    for(col in 1:3){
      init_tableu[row, paste("s", row, sep = "")] = 1
      init_tableu[row, col_counter] = -1
      col_counter = col_counter+5
    }
    col_counter = old_counter+1
    old_counter = col_counter
  }
  
  col_counter = 2
  row_counter = 1
  for(col in 1:15){
    init_tableu[r, col] = init_matrix[row_counter, col_counter]
    col_counter = col_counter + 1
    if(col_counter > 6){
      col_counter = 2
      row_counter = row_counter+1
    }
  }
  init_tableu[r, "Z"] = 1
  
  #print(init_matrix)
  RHS_g = getRHS_given(init_matrix, r2, c2)
  #print(RHS)
  for(row in 1:(r-1)){
    if(row > 3){ 
      init_tableu[row, c] = RHS_g[row]*-1 
    }else{
      init_tableu[row, c] = RHS_g[row]
    }
    #print(init_tableu[row, c])
  }
  return(init_tableu)
}

CheckNegative2 <- function(last_col, row){
  
  for(i in 1:(row-1)){
    #print(last_col[i])
    if(last_col[i] < 0) {return(TRUE)}
  }
  return(FALSE)
}

GetPivotRow_one <- function(last_col, row){
  for(j in 1:(row-1)){
    if(last_col[j] < 0 ){
      max_row = j
      break
    }
  }
  
  for(i in 1:(row-1)){
    if(last_col[i] < 0){
      if(abs(last_col[i]) > abs(last_col[max_row])){ 
        max_row = i 
      }
    }
  }
  #print(last_col[max_row])
  return(max_row)
}

GetPivotColumn_one <- function(pivot_row, col, RHS){
  
  for(j in 1:col){
    if(pivot_row[j] < 0){
      max_ratio_index = j
      break
    }
  }
  
  for(i in 1:col){
    if(pivot_row[i] < 0){
      max_ratio = pivot_row[max_ratio_index] / RHS
      temp_ratio = pivot_row[i] / RHS
      if(temp_ratio > max_ratio){
        max_ratio_index = i
      }
    }
  }
  
  return(max_ratio_index)
}

PhaseOne <- function(tableu){
  list_phase_one = list()
  
  r = nrow(tableu)
  c = ncol(tableu)
  last_col = tableu[,c]
  
  #print(tableu)
  #print(last_col)
  if_neg = CheckNegative2(last_col, r)
  while (if_neg == TRUE) {
    #for(ctr in 1:13){
    pivot_row_index = GetPivotRow_one(last_col, r)
    pivot_row = tableu[pivot_row_index,]
    #print(pivot_row)
    
    RHS_pivot_row = pivot_row[c]
    pivot_col_index = GetPivotColumn_one(pivot_row, c-1, RHS_pivot_row)
    pivot_col = tableu[,pivot_col_index]
    
    pivot_element = tableu[pivot_row_index, pivot_col_index]
    pivot_row = tableu[pivot_row_index, ] / pivot_element
    tableu[pivot_row_index,] = pivot_row
    
    #print(pivot_row_index)
    #print(pivot_col_index)
    for(i in 1:r){
      if(i != pivot_row_index){
        val_to_zero = tableu[i, pivot_col_index]
        #print(val_to_zero)
        temp = ComputeTemp(val_to_zero, pivot_row)
        #print(temp)
        new_row = ComputeNewRow(temp, tableu[i, ])
        tableu[i, ] = new_row
      }
    }
    last_col = tableu[, c]
    if_neg = CheckNegative2(last_col, r)
    
    list_phase_one[[length(list_phase_one)+1]] <- tableu
    #print(tableu)
  }
  res = list(tableu=tableu, list_phase_one=list_phase_one)
  return(res)
}

CheckSol <- function(col_tableu, row){
  ctr = 0
  for(i in 1:row){
    if(col_tableu[i] == 1){ctr = ctr+1}
  }
  if(ctr > 1){ return(FALSE)}
  else{ return(TRUE) }
}

GetValues <- function(tableu, row_tableu, col_tableu){
  col_names = colnames(tableu)
  values = InitValues(col_names, col_tableu)
  
  for(col in 1:col_tableu){  
    if(CheckSol(tableu[,col], row_tableu)==TRUE){
      for(row in 1:row_tableu){
        if(tableu[row, col] == 1){
          values[col_names[col]] = (tableu[row, col_tableu])
        }    
      }
    }
  }
  #print(values)
  return(values)
}

PhaseTwo <- function(tableu){
  list_phase_two = list()
  
  row_tableu = nrow(tableu)
  col_tableu = ncol(tableu)
  last_row = tableu[row_tableu, ]
  
  neg = CheckNegative(last_row, col_tableu)
  while(neg == TRUE){
    pivot_col_index = GetPivotColumn(last_row, col_tableu)
    pivot_col = tableu[,pivot_col_index]
    #print(pivot_col_index)
    RHS = tableu[, col_tableu]
    
    pivot_row_index = GetTestRatio(pivot_col, RHS, row_tableu)
    #print(pivot_row_index)
    pivot_element = tableu[pivot_row_index, pivot_col_index]
    pivot_row = tableu[pivot_row_index, ] / pivot_element
    tableu[pivot_row_index,] = pivot_row
    #print(tableu[pivot_row_index,])
    
    for(i in 1:row_tableu){
      if(i != pivot_row_index){
        val_to_zero = tableu[i, pivot_col_index]
        temp = ComputeTemp(val_to_zero, pivot_row)
        new_row = ComputeNewRow(temp, tableu[i, ])
        tableu[i, ] = new_row
      }
    }
    last_row = tableu[row_tableu, ]
    neg = CheckNegative(last_row, col_tableu)
    
    list_phase_two[[length(list_phase_two)+1]] <- tableu
    #print(tableu)
  }
  values = GetValues(tableu,row_tableu, col_tableu)
  res = list(tableu=tableu, values=values, list_phase_two=list_phase_two)
}

SimplexMethod <- function(df2){
  proj_tableu = preprocessMatrix(df2)
  phase_one = PhaseOne(proj_tableu)
  phase_two = PhaseTwo(phase_one$tableu)
  
  #print(phase_one$list_phase_one)
  #print(phase_two$tableu)
  #print(phase_two$list_phase_two)
  return(list(phase_one=phase_one, phase_two=phase_two))
}

df2 = matrix(c(310, 10, 8, 6, 5, 4,
               260, 6, 5, 4, 3, 6,
               280, 3, 4, 5, 5, 9,
               0, 0, 0, 0, 0, 0,
               NA, 180, 80, 200, 160, 220
),
nrow = 5,
ncol = 6,
dimnames = list(c("Denver", "Phoenix", "Dallas", "Shipping", "Demands" ), c("Supply", "California", "Utah", "New Mexico", "Illinois", "New York")),
byrow = TRUE)


rezz = SimplexMethod(df2)
print(rezz)
#proj_tableu = preprocessMatrix(df2)
#print(proj_tableu)
#phase_one = PhaseOne(proj_tableu)
#print(phase_one)






