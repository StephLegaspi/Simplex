
GetPivotColumn <- function(last_row, col_tableu){
  for(j in 1:col_tableu){
    if(last_row[j] < 0 ){
      max_col = j
      break
    }
  }
  
  for(i in 1:col_tableu){
    if(last_row[i] < 0){
      if(abs(last_row[i]) > abs(last_row[max_col])){ 
        max_col = i 
      }
    }
  }
  return(max_col)
}

CheckNegative <- function(last_row, col_tableu){
  for(i in 1:col_tableu){
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

GetValues <- function(tableu, row_tableu, col_tableu){
  col_names = colnames(tableu)
  values = InitValues(col_names, col_tableu)
  
  for(row in 1:row_tableu){
    for(col in 1:col_tableu){
      if(tableu[row, col] == 1){
        values[col_names[col]] = (tableu[row, col_tableu])
      }    
    }
  }  
  return(values)
}

SimplexMethod <- function(tableu){
  row_tableu = nrow(tableu)
  col_tableu = ncol(tableu)
  last_row = tableu[row_tableu, ]
  
  neg = CheckNegative(last_row, col_tableu)
  while(neg == TRUE){
    pivot_col_index = GetPivotColumn(last_row, col_tableu)
    pivot_col = tableu[,pivot_col_index]
    #print(pivot_col)
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
    print(tableu)
  }
  values = GetValues(tableu,row_tableu, col_tableu)
  print(values)
  
}


init_tableu = matrix(
  c(7, 11, 1, 0, 0, 0, 0, 77,
    10, 8, 0, 1, 0, 0, 0, 80,
    1, 0, 0, 0, 1, 0, 0, 9,
    0, 1, 0, 0, 0, 1, 0, 6, 
    -150, -175, 0, 0, 0, 0, 1, 0),
  nrow = 5,
  ncol = 8,
  byrow = TRUE
)

dimnames(init_tableu) = list(
  c("s1", "s2", "s3", "s4", "Z"),
  c("r", "p", "s1", "s2", "s3", "s4", "Z", "RHS")
)

print(init_tableu)
SimplexMethod(init_tableu)







