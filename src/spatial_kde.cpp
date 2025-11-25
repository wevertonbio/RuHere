#include <Rcpp.h>
#include <cmath>
#include <string>

// Usamos Rcpp::NumericMatrix e Rcpp::NumericVector para a interface com R.
using namespace Rcpp;

// Definindo M_PI
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

// --- Funções Kernel (Mesmas que antes) ---

double uniformKernel(const double& d, const double& bw, const bool& scaled){
  if (scaled) {
    double k = 2./(M_PI*bw);
    return k * (0.5/bw);
  } else {
    return 1.;
  }
}

double quarticKernel(const double& d, const double& bw, const bool& scaled){
  double u = d/bw;
  double decay = 1. - u*u;
  if (scaled) {
    double k = 116./(5.*M_PI*std::pow(bw, 2));
    return k*(15./16.)*std::pow(decay, 2);
  } else {
    return std::pow(decay, 2);
  }
}

double triweightKernel(const double& d, const double& bw, const bool& scaled){
  double u = d/bw;
  double decay = 1. - u*u;
  if (scaled) {
    double k = 128./(35.*M_PI*std::pow(bw, 2));
    return k * (35./32.)*std::pow(decay, 3);
  } else {
    return std::pow(decay, 3);
  }
}

double epanechnikovKernel(const double& d, const double& bw, const bool& scaled){
  double u = d/bw;
  double decay = 1. - u*u;
  if (scaled) {
    double k = 8./(3.*M_PI*std::pow(bw, 2));
    return k*(3./4.)*decay;
  } else {
    return decay;
  }
}

double triangularKernel(const double& d, const double& bw, const bool& scaled, const double& decay){
  double u = d/bw;
  if (scaled) {
    if (decay >= 0){
      double k = 3./((1.+2.*decay)*M_PI*std::pow(bw, 2));
      return k*(1.-(1.-decay)*u);
    } else {
      return (1.-(1.-decay)*u);
    }
  } else {
    return (1.-(1.-decay)*u);
  }
}

double kde_element(const double& d, const double& bw, const std::string& kernel, const bool& scaled, const double& decay){
  if (d <= bw) {
    if (kernel == "uniform") {
      return uniformKernel(d, bw, scaled);
    } else if(kernel == "quartic") {
      return quarticKernel(d, bw, scaled);
    } else if(kernel == "triweight") {
      return triweightKernel(d, bw, scaled);
    } else if(kernel == "epanechnikov") {
      return epanechnikovKernel(d, bw, scaled);
    } else if(kernel == "triangular") {
      return triangularKernel(d, bw, scaled, decay);
    }
    else{
      return uniformKernel(d, bw, scaled);
    }
  } else {
    return 0;
  }
}

const double distance(const NumericVector& a, const NumericVector& b) {
  double dist_sq = 0;
  for (int i=0; i < a.size(); i++){
    dist_sq += std::pow(a[i] - b[i], 2);
  }
  return std::sqrt(dist_sq);
}


// --- Função Principal de Estimação de KDE (Sem Progress Bar) ---

// [[Rcpp::export]]
NumericVector kde_rcpp(NumericMatrix fishnet,
                       NumericMatrix points,
                       double bw,
                       std::string kernel,
                       bool scaled,
                       double decay,
                       NumericVector weights) {

  NumericVector out(fishnet.nrow());

  // Loop Principal: Iterar sobre cada PIXEL
  for (int i = 0; i < fishnet.nrow(); i++) {

    // A única checagem que permanece é a interrupção do usuário (boa prática)
    Rcpp::checkUserInterrupt();

    double result = 0;
    NumericVector fishnet_row = fishnet.row(i);
    double w = 1.0;

    // Loop Secundário: Iterar sobre cada PONTO
    for (int j = 0; j < points.nrow(); j++) {

      NumericVector points_row = points.row(j);

      if (weights.size() == points.nrow()) {
        w = weights[j];
      } else {
        w = 1.0;
      }

      double dist = distance(fishnet_row, points_row);

      // Acumula o valor do kernel
      result += (w * kde_element(dist,
                                 bw,
                                 kernel,
                                 scaled,
                                 decay));
    }

    out[i] = result;
  }

  return out;
}
