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

#include "WALRUS.hh"
#include "uniroot.cpp"

class dGfinderclass {
public:
  void setproblem(WALRUS *w, double rhs) {
    this->w = w;
    this->rhs = rhs;
  }
  double operator()(double dG) {
  return w->fGS_dG(dG) - rhs;
  }

  // private:
  WALRUS *w;
  double rhs;
} dGfinder;

void WALRUS::init(double starttime, double Q0, double hS0, double dG0,
                   double dV0, double hQ0, double Gfrac) {
  // starttime and Q0 **MUST** be given, the others may be NANS, last one
  // has default value

  _time = starttime;

  if (std::isnan(hS0)) {
    // hS0 not given, derive it from Q0
    _hS = hS_Q(Q0);
  } else {
    _hS = hS0;
  }

  //
  // hS0 and Q0 do have values , now the others
  //
  if (std::isnan(dG0)) {
    //  dG0 is not given
    if (std::isnan(hQ0)) {
      //  dG0 and hQ0 are not given
      double Q0used = Q0 * Gfrac;
      while (((cD - _hS) * cD / cG) < Q0used) {
        Q0used /= 2.0;
      }
      dGfinder.setproblem(this, Q0used);
      _dG = findzero(0, cD - _hS, 1e-4, &dGfinder);
    } else {
      //  dG0 is not given, hQ0 is given
      _hQ = hQ0;
      double rhs = Q0 - _hQ / cQ;
      if (rhs < 0)
        rhs = 0;
      dGfinder.setproblem(this, rhs);
      _dG = findzero(0, cD - _hS, 1e-4, &dGfinder);
    }
  } else {
    //  dG0 is given
    _dG = dG0;
    if (std::isnan(hQ0)) {
      //  dG0 is given and hQ0 is not given
      if ((cD - _dG) < _hS) {
        // Groundwater below surface water level:
        // all from the quickflow
        _hQ = Q0 * cQ;
      } else {
        // Grdounwater above surface water level
        double hQtry = (Q0 - (cD - _dG - _hS) * (cD - _dG) / cG) * cQ;
        if (hQtry > 0) {
          _hQ = hQtry;
        } else {
          _hQ = 0;
        }
      }
    } else {
      //  dG0 is  given and  hQ0 is given
      _hQ = hQ0;
    }
  }
  if (std::isnan(dV0)) {
    // dV0 is not given
    _dV = dVeq_dG(_dG);
  } else {
    // dV0 is given
    _dV = dV0;
  }
}
void WALRUS::init_by_all(double starttime, double Q0, double hS0, double dG0,
                         double dV0, double hQ0, double Gfrac)
{
        init(starttime,Q0,hS0,dG0,dV0,hQ0,Gfrac);
}
// the simpliest initialization
void WALRUS::init_by_Q(double Q0)
{
        init(0.0, Q0, NAN, NAN, NAN, NAN, 1.0);
}
void WALRUS::init(double Q0) {
        init(0.0, Q0, NAN, NAN, NAN, NAN, 1.0);
}

void WALRUS::init_by_tQ(double starttime, double Q0) {
        init(starttime, Q0, NAN, NAN, NAN, NAN, 1.0);
}
void WALRUS::init(double starttime, double Q0) {
  init(starttime, Q0, NAN, NAN, NAN, NAN, 1.0);
}

void WALRUS::init_by_Qstates(double Q0, double hS0, double dG0, double dV0, double hQ0)
{
          init(0.0, Q0, hS0, dG0, dV0, hQ0, 1.0);
}
void WALRUS::init(double Q0, double hS0, double dG0, double dV0, double hQ0) {
  init(0.0, Q0, hS0, dG0, dV0, hQ0, 1.0);
}

void WALRUS::init_by_tQstates(double starttime, double Q0, double hS0, double dG0, double dV0, double hQ0)
{
          init(starttime, Q0, hS0, dG0, dV0, hQ0, 1.0);
}
void WALRUS::init(double starttime, double Q0, double hS0, double dG0,
                   double dV0, double hQ0) {
  init(starttime, Q0, hS0, dG0, dV0, hQ0, 1.0);
}
