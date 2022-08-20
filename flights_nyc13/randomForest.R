outlier_detect <- function(variable, n){
 std <- sd(variable)
 for (val in variable) {
  if(val> n*std){
    val <- NA
  } 
 }return(variable)
}