#include <Rcpp.h>
#include <map>
#include <string>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector get_rarefaction_components(CharacterVector x) {
  // Count number of species
  std::map<std::string, int> counts;

  for(int i = 0; i < x.size(); ++i) {
    counts[as<std::string>(x[i])]++;
  }

  double n = x.size();
  double s_obs = counts.size();
  double f1 = 0;
  double f2 = 0;

  for (auto const& [especie, freq] : counts) {
    if (freq == 1) f1++;
    if (freq == 2) f2++;
  }

  return NumericVector::create(
    _["n"] = n,
    _["s_obs"] = s_obs,
    _["f1"] = f1,
    _["f2"] = f2
  );
}
