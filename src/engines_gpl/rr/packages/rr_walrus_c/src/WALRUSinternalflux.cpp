//----- AGPL ---------------------------------------------------------------------
//                                                                               
//  Copyright (C)  Stichting Deltares, 2011-2024.                                
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
#include <fstream>

//#ifdef WIN32
#include<algorithm>
#define max(a,b) std::max(a,b)
#define min(a,b) std::min(a,b)
//#endif

/*! @file
 * @brief implements the try and check of a time step
*/

double WALRUS::fGS_dG(double dG) {
  return (cD - _dG - _hS) * max(cD - dG, _hS) / cG;
}



void WALRUS::calctryvalues(double tend) {
  double cumP = get_cum(fc_P, _time, tend);
  double cumETpot = get_cum(fc_ETpot, _time, tend);
  P_try = cumP;
  ETpot_try = cumETpot;
  fXG_try = get_cum(fc_XG, _time, tend);
  fXS_try = get_cum(fc_XS, _time, tend);
  fXSact_try = fXS_try;
  _W = W_dV(_dV);
  _dVeq = dVeq_dG(_dG);
  PQ_try = cumP * _W * aG;
  PV_try = cumP * (1 - _W) * aG;
  PS_try = cumP * aS;
  ETV_try = cumETpot * beta_dV(_dV) * aG;
  if (_hS > hSzeroforET) {
    ETS_try = cumETpot * aS;
  } else {
    ETS_try = 0;
  }
  double dt = (tend - _time) / 3600; // dt is in hours now
  ETact_try = ETV_try + ETS_try;
  fQS_try = _hQ / cQ * dt;
  fGS_try = fGS_dG(_dG) * dt;
  Q_try = Q_hS(_hS) * dt;
  // now states, save the changes for later control
  dV_try = _dV + (-fXG_try - PV_try + ETV_try + fGS_try) / aG;
  hQ_try = _hQ + (PQ_try - fQS_try) / aG;
 // update March2018, update May 2021
  if (hQ_try < min_h ) {
        fQS_try = fQS_try + (hQ_try - min_h) * aG;
        hQ_try = min_h ;
  }
  // end update
  hS_try = _hS + (fXSact_try + PS_try - ETS_try + fGS_try + fQS_try - Q_try) / aS;
 // update March2018
  if (hS_try < 0 && fXSact_try < 0) {
    fXSact_try = fXSact_try - hS_try * aS;
    hS_try = 0;
  }
  // end update

  dG_try = _dG + (_dV - _dVeq) / cV * dt;


  // special case: large scale ponding and flooding
  if ((dV_try < 0) || (hS_try > cD)) {
    if ((dV_try < 0) && (hS_try <= cD)) {
      // ponding and no flooding
      // hS_try = _hS - dV_try * aG / aS;  GP 6may2021
      hS_try = hS_try - dV_try * aG / aS;
      // all ponds to surface water
      dV_try = 0;
      // soil completely saturated
      // addition GP for stability/quick convergence
      if (hS_try > cD) {
      // flooding and no ponding
      dV_try = dV_try + (-hS_try + cD) * aS / aG;
      hS_try = cD;
      }
    }
    else if ((dV_try >= 0) && (hS_try > cD)) {
      // flooding and no ponding
      // dV_try = _dV + (-hS_try + cD) * aS / aG;  GP 6may2021 ok
      dV_try = dV_try + (-hS_try + cD) * aS / aG;
      // flood goes all in soil
      hS_try = cD;
      // bankfull
    }
    else if ((dV_try < 0) && (hS_try > cD)) {
      // ponding and flooding
      // dV_try = _dV * aG - (hS_try - cD) * aS;   GP 6may2021
      dV_try = dV_try - (hS_try - cD) * aS / aG ;
      // compute total excess water
      // hS_try = cD - dV_try;
      hS_try = cD;
    }
    if (dV_try < 0) {
      dG_try = dV_try;
      // groundwater to pond level
    }
  }
}

double WALRUS::pred_time_tries_OK(double tend) {
  if (tend < _time + min_deltime) {
    return tend;
  }
  //return tend;
  double tc = tend;
  // check op < ipv <=
  if (hS_try < min_h) {
    double f = (_hS - min_h) / (_hS - hS_try);
    double thS = f * (tend) + (1 - f) * _time;
    if (thS < tc) {
      tc = thS;
    }
    hS_try = min_h; // avoid negative heights
  }
  // check op < ipv <=
  if (hQ_try < min_h) {
    double f = (_hQ - min_h) / (_hQ - hQ_try);
    double thQ = f * (tend) + (1 - f) * _time;
    if (thQ < tc) {
      tc = thQ;
    }
    hQ_try = min_h; // avoid negative heights
  }
  double P_step = get_cum(fc_P, _time, tend);
  if (P_step > max_Pstep) {
    double f = max_Pstep / P_step;
    double tP = f * (tend) + (1 - f) * _time;
    if (tP < tc) {
      tc = tP;
    }
  }
  double ETpot_step = get_cum(fc_ETpot, _time, tend);
  if (ETpot_step > max_Pstep) {
    double f = max_Pstep / ETpot_step;
    double tETpot = f * (tend) + (1 - f) * _time;
    if (tETpot < tc) {
      tc = tETpot;
    }
  }
  double fXG_step = get_cum(fc_XG, _time, tend);
  if (fXG_step > max_Pstep) {
    double f = max_Pstep / fXG_step;
    double tfXG = f * (tend) + (1 - f) * _time;
    if (tfXG < tc) {
      tc = tfXG;
    }
  }
  double fXS_step = get_cum(fc_XS, _time, tend);
  if (fXS_step > max_Pstep) {
    double f = max_Pstep / fXS_step;
    double tfXS = f * (tend) + (1 - f) * _time;
    if (tfXS < tc) {
      tc = tfXS;
    }
  }
  double abs_hS_change_step = std::fabs(hS_try - _hS);
  if (abs_hS_change_step > max_h_change) {
    double f = max_h_change / abs_hS_change_step;
    double thS_change = f * (tend) + (1 - f) * _time;
    if (thS_change < tc) {
      tc = thS_change;
    }
  }
  double abs_hQ_change_step = std::fabs(hQ_try - _hQ);
  if (abs_hQ_change_step > max_h_change) {
    double f = max_h_change / abs_hQ_change_step;
    double thQ_change = f * (tend) + (1 - f) * _time;
    if (thQ_change < tc) {
      tc = thQ_change;
    }
  }
  double abs_dG_change_step = std::fabs(dG_try - _dG);
  if (abs_dG_change_step > max_h_change) {
    double f = max_h_change / abs_dG_change_step;
    double tdG_change = f * (tend) + (1 - f) * _time;
    if (tdG_change < tc) {
      tc = tdG_change;
    }
  }
  if (tc <= _time) {
     tc = _time + min_deltime;
  }
  if (tc > tend) {
     tc = tend;
  }

  return tc;
}
