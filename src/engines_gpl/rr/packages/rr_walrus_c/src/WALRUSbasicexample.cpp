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

 #include <fstream>
#include <iostream>

#include "WALRUS.hh"

using namespace std;

int main(void) {

  WALRUS Wvalid;

  // read forcing input
  ifstream inFile("validinput.dat", std::ifstream::in);
  vector<double> forcing_time;
  vector<double> forcing_P;
  vector<double> forcing_ETpot;
  double last_time, last_P, last_ETpot, dummy;
  bool donereading = false;
  while (!donereading) {
    inFile >> last_time >> last_P >> last_ETpot >> dummy;
    donereading = inFile.eof();
    if (!donereading) {
      forcing_time.push_back(last_time);
      forcing_P.push_back(last_P);
      forcing_ETpot.push_back(last_ETpot);
    }
  }
  double t_begin = forcing_time[0];
  double t_end = forcing_time[forcing_time.size() - 1];
  // set forcing
  Wvalid.set_seq_C(fc_P, forcing_time, forcing_P);
  Wvalid.set_seq_C(fc_ETpot, forcing_time, forcing_ETpot);
  // set parameters
  Wvalid.set(par_cW, 200);
  Wvalid.set(par_cV, 4);
  Wvalid.set(par_cG, 5e6);
  Wvalid.set(par_cQ, 10);
  Wvalid.set(par_cS, 4);
  Wvalid.set(par_cD, 1500);
  Wvalid.set(par_aS, 0.01);
  Wvalid.set_st(loamy_sand);

  // iniitialize model
  Wvalid.init(0.0044, NAN, 1250, NAN, NAN);
  Wvalid.set(cur_time, t_begin);
  // calculate and save data
  Wvalid.dosteps(t_end, 3600, "WALRUSresult.csv", true, false, false);

  return 0;
}
