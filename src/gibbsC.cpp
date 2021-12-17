#include <Rcpp.h>
using namespace Rcpp;

//' @title A Gibbs sampler using Rcpp
//' @description A Gibbs sampler using Rcpp
//' @param N the number of samples
//' @param t the number of between-sample random numbers
//' @return a random sample of size \code{n}
//' @examples
//' \dontrun{
//' rnC <- gibbsC(100,5)
//' }
//' @export
// [[Rcpp::export]]
NumericMatrix gibbsC(int N, int t) {
  NumericMatrix mat(N, 2);
  double x = 1, y = 0.5;
  for(int i = 0; i < N; i++) {
    for(int j = 0; j < t; j++) {
      x = rbinom(1, 2, y)[0];
      y = rbeta(1, x + 1,  2-x+1)[0];
    }
    mat(i, 0) = x;
    mat(i, 1) = y;
  }
  return(mat);
}

