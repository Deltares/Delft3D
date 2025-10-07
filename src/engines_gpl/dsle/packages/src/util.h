#ifndef ZSF_UTIL_H
#define ZSF_UTIL_H

#include "zsf.h"
#include <math.h>

int is_close(double a, double b, double rtol, double atol);
double sal_psu_2_density(double sal_psu, double temperature);
double sal_2_density(double sal_kgm3, double temperature, double rtol, double atol);

int is_close(double a, double b, double rtol, double atol) {
  double max_abs = fmax(fabs(a), fabs(b));
  if (fabs(a - b) <= fmax(rtol * max_abs, atol))
    return 1;
  else
    return 0;
}

double sal_psu_2_density(double sal_psu, double temperature) {
  // Calculates the density of sea water using the UNESCO 1981 algorithm.
  double a = (8.24493E-1 - 4.0899E-3 * temperature + 7.6438E-5 * pow(temperature, 2.0) -
              8.2467E-7 * pow(temperature, 3.0) + 5.3875E-9 * pow(temperature, 4.0));
  double b = -5.72466E-3 + 1.0227E-4 * temperature - 1.6546E-6 * pow(temperature, 2.0);
  double c = 4.8314E-4;

  double rho_ref = (999.842594 + 6.793952E-2 * temperature - 9.095290E-3 * pow(temperature, 2.0) +
                    1.001685E-4 * pow(temperature, 3.0) - 1.120083E-6 * pow(temperature, 4.0) +
                    6.536332E-9 * pow(temperature, 5.0));

  return rho_ref + a * sal_psu + b * pow(sal_psu, 1.5) + c * pow(sal_psu, 2.0);
}

double sal_2_density(double sal_kgm3, double temperature, double rtol, double atol) {
  /*
    Calculates the density of sea water using the UNESCO 1981 algorith, but
    using salinity in kg/m3 as input.

    It defers to the reference implementation (with salinity in psu), and
    loops until absolute or relative convergence tolerance (on the density)
    has been reached.

    Typically only a handful (1-10) of iterations are needed to reach any
    reasonably desired absolute tolerance. An upper bound of 100 iterations is
    used to catch any case where the algorithm does not converge.
    */

  double sal_psu = sal_kgm3;
  double rho = 1000.0;

  for (int i = 0; i < 100; i++) {
    double rho_new = sal_psu_2_density(sal_psu, temperature);
    sal_psu = sal_kgm3 / rho_new * 1000.0;

    if (is_close(rho_new, rho, rtol, atol))
      return rho_new;

    rho = rho_new;
  }
  return ZSF_NAN;
}
#endif
