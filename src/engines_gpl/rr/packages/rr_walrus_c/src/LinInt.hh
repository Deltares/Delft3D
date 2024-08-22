#ifndef LinIntHH
#define LinIntHH

#include "LinInt.hh"
using namespace std;

#include <vector>

class approxfun {
public:
  approxfun(const vector<double> &x, const vector<double> &y) {
    this->x = x;
    this->y = y;
    this->Nxy = x.size();
    if (y.size() < Nxy) {
      Nxy = y.size();
    }
  }
  double operator()(double x) {
    if (x <= this->x[0]) {
      return this->y[0];
    }
    if (x >= this->x[Nxy - 1]) {
      return this->y[Nxy - 1];
    }
    unsigned int i = 0;
    while (this->x[i] < x) {
      i++;
    }
    double f = (this->x[i] - x) / (this->x[i] - this->x[i - 1]);
    return f * this->y[i - 1] + (1 - f) * this->y[i];
  }

private:
  unsigned int Nxy;
  std::vector<double> x;
  std::vector<double> y;
};

#endif
