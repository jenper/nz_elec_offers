#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
std::vector<std::string> rcpp_time(std::vector<double> TP) {
  int hours, minutes;
  std::vector<std::string> time(TP.size());
  for(size_t i = 0; i != TP.size(); i++) {
    TP[i] = (TP[i]-1)*30;
    hours = std::floor(TP[i] / 60);
    minutes = fmod(TP[i],60);
    time[i] = std::to_string(hours) + ":" + std::to_string(minutes) + ":00";
  }
  return time;
}