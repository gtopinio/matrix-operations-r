#' This is an R script that contains the implementation of different operations on matrices
#' @author Mark Genesis C. Topinio
#' @created_date 2021-10-01 14:18

# This function below checks if a matrix is a square matrix or not. 
# It returns TRUE if the matrix mat passed as parameter is a square matrix, FALSE otherwise
SquareMatrix = function(mat){
  if(nrow(mat)==ncol(mat)){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}

#This function below accepts three parameters mat, i, and j, and returns the minor of matrix with respect to i and j.
MatrixMinor = function(mat,i,j){
  if(SquareMatrix(mat)==FALSE){
    error = "Not a Square Matrix!"
    return(error)
  }
  else if(i>nrow(mat) || j>ncol(mat) || i<0 || j<0){
    error = "Invalid Inputs!"
    return(error)
  }
  else{
    minor = mat[-i,-j]
    cat("Minor i =",i,"and j =",j,"\n")
    return(minor)
  }
}

#This function below accepts three parameters mat, i, and j, and returns the cofactor of matrix with respect to i and j.
MatrixCofactor = function(mat,i,j){
  if(SquareMatrix(mat)==FALSE){
    error = "Not a Square Matrix!"
    return(error)
  }
  else if(i>nrow(mat) || j>ncol(mat) || i<0 || j<0){
    error = "Invalid Inputs!"
    return(error)
  }
  else{
    minor = mat[-i,-j]
    if(!is.null(dim(minor))){
      cofactor = ((-1)^(i+j)) * (det(minor))
      return(cofactor)
    }
    else{
      cofactor = ((-1)^(i+j)) * (minor[1])
      return(cofactor)
    }
  }
}

#This function below returns the adjoint of the matrix matrix if matrix is a square matrix; returns NA if mat is not a square matrix.
MatrixAdjoint = function(mat){
  row = nrow(mat)
  col = ncol(mat)
  if(SquareMatrix(mat)==FALSE){
    print("NA")
  }else{
    cofactors = c()
    count = 1
    
    for(i in 1:row){
      for(j in 1:col){
        cofactors[count] = MatrixCofactor(mat,i,j)
        count = count + 1
      }
    }
    adjoint = matrix(cofactors, row, col)
    return(adjoint)
  }
}

# This function below returns the inverse of the matrix mat if mat is a square matrix; returns NA if mat is not a square matrix.
MatrixInverse = function(mat){
  row = nrow(mat)
  col = ncol(mat)
  if(SquareMatrix(mat)==FALSE){
    print("NA")
  }
  else if(det(mat)==0){
    print("The Matrix is singular! The Matrix has no inverse since the determinant is Zero!")
  }else{
  multiplier = 1/(det(mat))

  adjMat = MatrixAdjoint(mat)
  inverse = multiplier*adjMat
  return(inverse)
  }
}

# This function below helps create a matrix for the user. It can also be passed to the formal parameters of the other functions since it
# returns a matrix according to the user.
InputMatrix = function(){
  valid = TRUE
  
  while(valid){
    row = readline(prompt="Enter the row size for the matrix: ")
    row = as.integer(row)
    col = readline(prompt="Enter the column size for the matrix: ")
    col = as.integer(col)
    if(row<=0 || col<=0){
      print("Please enter positive integers for row/column.")
    }
    else{
      valid = FALSE
    }
  }
  
  toggle = readline(prompt="Input by Row?[T/F]: ")
  toggle = toupper(toggle)
  toggle = as.logical(toggle)
  
  vector = c()
  k=1;
  if(toggle==TRUE){
  for(i in 1:row){
    for(j in 1:col){
      cat("For row",i,"and column",j)
      input = readline(prompt="Enter a number: ")
      input = as.numeric(input)
      vector[k] = input
      k = k+1
    }
  }
  }
  else{
  for(i in 1:col){
    for(j in 1:row){
      cat("For row",j,"and column",i)
      input = readline(prompt="Enter a number: ")
      input = as.numeric(input)
      vector[k] = input
      k = k+1
    }
  }
  }
  mat = matrix(vector,row,col, toggle)
  return(mat)
}
