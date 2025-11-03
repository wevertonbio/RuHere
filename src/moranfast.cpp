#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
List moranfast(arma::vec x, arma::mat weight, bool scaled = false, bool na_rm = false, std::string alternative = "two.sided") {

  // 1. Verificações iniciais de dimensão
  if (weight.n_rows != weight.n_cols) {
    stop("'weight' must be a square matrix");
  }
  if (weight.n_rows != x.n_elem) {
    stop("'weight' must have as many rows as observations in 'x'");
  }

  // 2. Tratamento de NA
  // Verifica se há valores não finitos (NA, NaN, Inf) em x
  arma::uvec bad_indices = arma::find_nonfinite(x);
  bool any_nas = bad_indices.n_elem > 0;

  if (any_nas) {
    if (na_rm) {
      // Encontra os índices "bons" (finitos)
      arma::uvec good_indices = arma::find_finite(x);
      // Subconjunto de x e da matriz de pesos
      x = x.elem(good_indices);
      weight = weight.submat(good_indices, good_indices);
    } else {
      warning("'x' has missing values: maybe you wanted to set na.rm = TRUE?");
      return List::create(Named("observed") = NA_REAL,
                          Named("expected") = -1.0 / (x.n_elem - 1.0), // Mantém o esperado original mesmo com NA? O R original faz isso.
                          Named("sd") = NA_REAL,
                          Named("p.value") = NA_REAL);
    }
  }

  double n = (double)x.n_elem;
  double ei = -1.0 / (n - 1.0);

  // 3. Normalização das linhas (Row-normalization)
  // Equivalente a ROWSUM <- rowSums(weight); ROWSUM[ROWSUM == 0] <- 1; weight <- weight/ROWSUM
  arma::vec row_sums = arma::sum(weight, 1); // Soma das linhas
  for(unsigned int i = 0; i < row_sums.n_elem; ++i) {
    if (row_sums[i] == 0) row_sums[i] = 1.0;
  }
  weight.each_col() /= row_sums; // Divisão de cada coluna pelo vetor de somas (broadcasting)

  // 4. Cálculos principais
  double s = arma::accu(weight);
  double m = arma::mean(x);
  arma::vec y = x - m;

  // Otimização: em vez de 'sum(weight * y %o% y)' que cria uma matriz gigante N x N,
  // usamos álgebra matricial: y.t() * weight * y.
  // O resultado é um escalar (1x1 matrix), então usamos as_scalar.
  double cv = arma::as_scalar(y.t() * weight * y);

  double v = arma::accu(arma::square(y));
  double obs = (n / s) * (cv / v);

  // 5. Scaling (se solicitado)
  if (scaled) {
    // i.max <- (n/s) * (sd(rowSums(weight) * y)/sqrt(v/(n - 1)))
    // Nota: rowSums(weight) após a normalização acima deve ser quase tudo 1.
    // Recalculamos para garantir precisão idêntica ao R caso haja linhas zeradas.
    arma::vec rsw = arma::sum(weight, 1);
    double sd_num = arma::stddev(rsw % y); // % é multiplicação elemento-a-elemento
    double i_max = (n / s) * (sd_num / std::sqrt(v / (n - 1.0)));
    obs = obs / i_max;
  }

  // 6. Cálculos da Variância/SD para o p-valor
  // S1 <- 0.5 * sum((weight + t(weight))^2)
  double S1 = 0.5 * arma::accu(arma::square(weight + weight.t()));

  // S2 <- sum((apply(weight, 1, sum) + apply(weight, 2, sum))^2)
  // apply(weight, 1, sum) -> arma::sum(weight, 1) (linhas)
  // apply(weight, 2, sum) -> arma::sum(weight, 0) (colunas - retorna rowvec, precisa transpor)
  double S2 = arma::accu(arma::square(arma::sum(weight, 1) + arma::sum(weight, 0).t()));

  double s_sq = s * s;

  // k <- (sum(y^4)/n)/(v/n)^2
  double k = (arma::accu(arma::pow(y, 4)) / n) / std::pow(v / n, 2);

  // Fórmula complexa do SDI
  double sdi_sq = (n * ((n * n - 3 * n + 3) * S1 - n * S2 + 3 * s_sq) -
                   k * (n * (n - 1) * S1 - 2 * n * S2 + 6 * s_sq)) /
                     ((n - 1) * (n - 2) * (n - 3) * s_sq) - 1.0 / ((n - 1) * (n - 1));
  double sdi = std::sqrt(sdi_sq);

  // 7. Cálculo do P-valor
  // Usa R::pnorm para acessar a função pnorm do R
  double pv = R::pnorm(obs, ei, sdi, true, false); // lower.tail=TRUE, log.p=FALSE

  if (alternative == "two.sided") {
    if (obs <= ei) {
      pv = 2.0 * pv;
    } else {
      pv = 2.0 * (1.0 - pv);
    }
  } else if (alternative == "greater") {
    pv = 1.0 - pv;
  }
  // se "less", pv já está correto (lower.tail=TRUE)

  // Retorno da lista similar ao ape::Moran.I
  return List::create(Named("observed") = obs,
                      Named("expected") = ei,
                      Named("sd") = sdi,
                      Named("p.value") = pv);
}

