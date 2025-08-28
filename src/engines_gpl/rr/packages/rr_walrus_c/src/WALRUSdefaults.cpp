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

/*! @file
 * @brief sets the defaults of the WALRUS class
*/

WALRUS::WALRUS() {

  cW = 200;
  cV = 4;
  cG = 5.0e6;
  cQ = 10;
  cS = 4;
  expS = 1.5;
  cD = 1500;
  aS = 0.1;
  aG = 1 - aS;
  // psi_ae = 90;
  // b = 4.38;
  // theta_s = 0.41;
  area = 1.0;
  min_deltime = 60;
  max_h_change = 10;
  min_h = 0.001;
  max_Pstep = 10;
  max_substeps = 288;
  W_dV_analytical = true;
  dVeq_dG_analytical = true;
  beta_dV_analytical = true;
  Q_hS_analytical = true;
  P = 0;
  Pfromseries = false;
  ETpot = 0;
  ETpotfromseries = false;
  fXG = 0;
  fXGfromseries = false;
  fXS = 0;
  fXSfromseries = false;
  hSmin = 0;
  hSminfromseries = false;
  _time = 0;
  _lastdeltime = 0;
  _dV = 0;
  _hQ = 0;
  _dG = 0;
  _hS = 0;
  cur_fXG_c = 0;
  cur_fXS_c = 0;
  cur_PQ_c = 0;
  cur_PV_c = 0;
  cur_PS_c = 0;
  cur_ETV_c = 0;
  cur_ETS_c = 0;
  cur_ETact_c = 0;
  cur_fXSact_c = 0;
  cur_fQS_c = 0;
  cur_fGS_c = 0;
  cur_Q_c = 0;
  this->set_st(loamy_sand);
}

const double WALRUS::zeta1 = 0.02;
const double WALRUS::zeta2 = 400;
const double WALRUS::hSzeroforET = 1;
const double WALRUS::MinFactor = 0.001;
