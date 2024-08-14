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

#include "WalrusInterface.hh" 

using namespace std;

int main(void) {

   // read forcing input
   ifstream inFile("D:\\carniato\\LUCA\\ENGINES\\WALRUS\\WALRUSC_1.0\\x64\\Debug\\validinput.dat", std::ifstream::in);
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
   int err = -1;

   // add 0 instance
   err = addWalrusInstance();

   // set parameters
   if (err == 0) 
   {
      err = set_seq_C(0, fc_P, &forcing_time[0], forcing_time.size(), &forcing_P[0], forcing_P.size());
   }
   if (err == 0)
   {
      err = set(0, par_cW, 200);
   }
   if (err == 0)
   {
      err = set(0, par_cV, 4);
   }
   if (err == 0)
   {
      err = set(0,par_cG, 5e6);
   }
   if (err == 0)
   {
      err = set(0, par_cQ, 10);
   }
   if (err == 0)
   {
      err = set(0, par_cS, 4);
   }
   if (err == 0)
   {
      err = set(0, par_cD, 1500);
   }
   if (err == 0)
   {
      err = set(0, par_aS, 0.01);
   }
   if (err == 0) 
   {
      err = set_st(0,loamy_sand);
   }


   // iniitialize model
   if (err == 0) 
   {
      err = init(0, 0.0044, NAN, 1250, NAN, NAN);
   }
   if (err == 0) 
   {
      err = set(0, cur_time, t_begin);
   }

   // do one step
   if (err == 0)
   {
      err = dostep(0, 3600);  
   }

   // get data
   double val = -1.0;
   if (err == 0)
   {
      err = get(0, par_cW, &val);
   }
   if (err == 0)
   {
      err = get(0, par_cV, &val);
   }
   if (err == 0)
   {
      err = get(0, par_cG, &val);
   }
   if (err == 0)
   {
      err = get(0, par_cQ, &val);
   }

   return err;
}
