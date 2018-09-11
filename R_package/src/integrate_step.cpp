#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector integrateStepFunc(NumericVector x, NumericVector bounds, NumericVector values) {
  int nx=x.size()-1;
  int ix=0,ib=0;
  double xcur;
  NumericVector integ(nx);
  //enlève les intervalles à gauche
  for(;bounds[ib+1]<x[ix];ib++) {};ib++;
  for(;ix<nx;ix++) {
    xcur=x[ix];
    double intd=0;
    for(;bounds[ib]<=x[ix+1];ib++) {
      intd += values[ib-1]*(bounds[ib]-xcur);
      xcur=bounds[ib];
    }
    intd += values[ib-1]*(x[ix+1]-xcur);
    xcur=x[ix+1];
    integ[ix]=intd;
  }
  return integ;
}
