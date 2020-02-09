sdDiff <- function(x, method='MAD'){
  n = length(x)
  if(method == "MAD"){
    return(mad(diff(x)/sqrt(2)))	
  }
  if(method == "HALL"){
    wei <- c(0.1942, 0.2809, 0.3832, -0.8582)
    mat <- wei %*% t(x)
    mat[2, -n] = mat[2, -1]
    mat[3, -c(n-1, n)] = mat[3, -c(1, 2)]
    mat[4, -c(n-2, n-1, n)] = mat[4, -c(1, 2, 3)]   
    return(sqrt(sum(apply(mat[, -c(n-2, n-1, n)], 2, sum)^2) / (n-3)))
  }
}