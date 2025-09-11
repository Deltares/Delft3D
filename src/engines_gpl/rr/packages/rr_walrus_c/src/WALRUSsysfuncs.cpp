//----- AGPL ---------------------------------------------------------------------
//                                                                               
//  Copyright (C)  Stichting Deltares, 2011-2025.                                
//                                                                               
//  This program is free software: you can redistribute it and/or modify         
//  it under the terms of the GNU Affero General Public License as               
//  published by the Free Software Foundation version 3.                         
//                                                                               
//  This program is distributed in the hope that it will be useful,              
//  but WITHOUT ANY WARRANTY; without even the implied warranty of               
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
//  GNU Affero General Public License for more details.                          
//                                                                               
//  You should have received a copy of the GNU Affero General Public License     
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
//                                                                               
//  contact: delft3d.support@deltares.nl                                         
//  Stichting Deltares                                                           
//  P.O. Box 177                                                                 
//  2600 MH Delft, The Netherlands                                               
//                                                                               
//  All indications and logos of, and references to, "Delft3D" and "Deltares"    
//  are registered trademarks of Stichting Deltares, and remain the property of  
//  Stichting Deltares. All rights reserved.                                     
//                                                                               
//-------------------------------------------------------------------------------

#include "LinInt.hh"
#include "WALRUS.hh"
#include <cmath>

//#ifdef _WIN32
#include<algorithm>
#define max(a,b) std::max(a,b)
#define min(a,b) std::min(a,b)
//#endif


using namespace std;

/*! @file
 * @brief Implementation of the system functions
 */

double WALRUS::W_dV(double dV) {
  if (W_dV_analytical) {
    return cos(max(min(dV, cW), 0.0) * 3.1415926535897 / cW) / 2 + 0.5;
  } else {
    return (*(W_dV_table))(dV);
  }
}
void WALRUS::set_W_dV_bytable(const vector<double> &dV,
                              const vector<double> &W) {
  W_dV_analytical = false;
  W_dV_table = new approxfun(dV, W);
}

double WALRUS::dVeq_dG(double dG) {
  if (dVeq_dG_analytical) {
    if (dG > psi_ae) {
      double dummy = pow(dG / psi_ae, -1.0 / b);
      return (dG - psi_ae / (1 - b) - dG * dummy + dG / (1 - b) * dummy) *
             theta_s;
    } else if (dG < 0) {
      return dG;
    } else {
      return 0;
    }
  } else {
    return (*(dVeq_dG_table))(dG);
  }
}

void WALRUS::set_dVeq_dG_bytable(const vector<double> &dG,
                                 const vector<double> &dVeq) {
  dVeq_dG_analytical = false;
  dVeq_dG_table = new approxfun(dG, dVeq);
}

double WALRUS::beta_dV(double dV) {
  if (beta_dV_analytical) {
    double temp = exp(-zeta1 * (zeta2 - dV));
    return (1 - temp) / (1 + temp) / 2 + 0.5;
  } else {
    return (*(beta_dV_table))(dV);
  }
}

void WALRUS::set_beta_dV_bytable(const vector<double> &dV,
                                 const vector<double> &beta) {
  beta_dV_analytical = false;
  beta_dV_table = new approxfun(dV, beta);
}

double WALRUS::Q_hS(double hS) {
  double hSm = get_hSmin(_time);
  if (hS < hSm) {
    return 0.0;
  } else {
    if (Q_hS_analytical) {
        double nominator = cD-hSm;
        if (nominator <= 0) {
           nominator = MinFactor;
        }
      if (hS < cD) {
        double hfrac = (hS - hSm) / (cD - hSm);
        return cS * pow(hfrac, expS);
      } else {
        double hfrac = (hS - cD) / (cD - hSm);
        return cS + cS * pow(hfrac, expS);
      }
    } else {
// GP 14 july 2020
//      return (*(Q_hS_table))(hS);
        double hSuse = hS - hSm;
        return (*(Q_hS_table))(hSuse);
// end GP 14 july 2020

    }
  }
}

double WALRUS::hS_Q(double Q) {
  if (Q_hS_analytical) {
    double hSm = get_hSmin(_time);
    double hfrac = pow((Q / cS), 1 / expS);
    return (hSm + hfrac * (cD - hSm));
  } else {
// GP 14 july 2020
  //   return (*(hS_Q_table))(Q);
      double hSuse= (*(hS_Q_table))(Q);
      double hSm = get_hSmin(_time);
      return hSuse+hSm;
// end GP 14 july 2020
  }
}

void WALRUS::set_Q_hS_bytable(const vector<double> &hS,
                              const vector<double> &Q) {
  Q_hS_analytical = false;
  Q_hS_table = new approxfun(hS, Q);
  hS_Q_table = new approxfun(Q, hS);
}
