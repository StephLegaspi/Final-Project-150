this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
source("legaspi_augCoeff.r")
source("legaspi_ex4.r")

x = c(50, 50, 50, 70, 70, 70, 80, 80, 80, 90, 90, 90, 100, 100, 100)
y = c(3.3, 2.8, 2.9, 2.3, 2.6, 2.1, 2.5, 2.9, 2.4, 3.0, 3.1, 2.8, 3.3, 3.5, 3.0)

#res = GaussJordanElimination(aug_coeff_matrix)

PolynomialRegression <- function(x, y, degree){
  result1 = AugCoeffMatrix(x, y, degree)
  aug_coeff_matrix = result1$augcoeffmatrix
  vars = result1$variables
  
  res = GaussJordanElimination(aug_coeff_matrix, degree+1, vars)
  #print(res$variables)
  
  for(i in 1:length(res$variables)){
    term = paste(res$solutionSet[i], "x", sep = " * ")
    term = paste(term, (i-1), sep = " ^ ")
    if(i == 1){
      polynomial = term
    }else{
      polynomial = paste(polynomial, term, sep = " + ")  
    }
  }
  poly_res = list(poly = polynomial, sol_set = res$solutionSet)
  return(poly_res)
}

#polynomial <- PolynomialRegression(x, y, 3)

#func = paste('f <- function(', args, ') { return(' , polynomial$poly , ')}', sep='')
#args <- "x"
#result = list(coefficients =  polynomial$sol_set, func = func, f = eval(parse(text = paste('f <- function(', args, ') { return(' , polynomial$poly , ')}', sep=''))))

#print(result$func)
#print(result$f(100))


