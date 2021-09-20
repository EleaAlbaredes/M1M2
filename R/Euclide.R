## GPL-3 License
## Copyright (c) 2021 Elea Albaredes

#' Computing Euclidean Distance
#'
#' @description Euclidean Distance with R functions
#' @param v a vector of numeric data
#' @param w a vector of numeric data
#' @return the Euclidean distance between v and w



Euclide_R <- function(v,w){
  return(sqrt(sum((v-w)^2)))   #racine carrée de la somme des carrés
}


#' Computing Euclidean Distance with a for loop
#'
#' @description Computing Euclidean Distance with a for loop
#' @param v a vector of numeric data
#' @param w a vector of numeric data
#' @return the Euclidean distance between v and w


Euclide_R_for<- function(v,w){
  n<-length(v)
  tmp<- 0
  for(i in 1:n){
    tmp<- tmp + (v[i]-w[i])^2
  }
  return(sqrt(tmp))
}




