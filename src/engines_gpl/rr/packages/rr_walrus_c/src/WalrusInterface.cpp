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

 /* Calling convention, stdcall in windows, cdecl in the rest of the world */
#include "WalrusInterface.hh"


/* control functions. These return an error code. */
int ADDWALRUSINSTANCE()
{
   try
   {
      walrusInstances.push_back(WALRUS());
      return 0;
   }
   catch (...)
   {
      return -1;
   }
};


int ADDWALRUSINSTANCES(int *nWalrusInstances)
{
    int nInstances = *nWalrusInstances;
    try
   {
      for (int i = 0; i < nInstances; ++i)
      {
         walrusInstances.push_back(WALRUS());
      }
      return 0;
   }
   catch (...)
   {
      return -1;
   }
};


int WALRUSSETSEQC(int * walrusInstanceIndex, int * varEnum,
   void * times, int* timesLenght, void * fc_vec, int * fc_vecLenght, double * timestepsize, int * startIndex)
{

   try
   {
      int walrusInstance = *walrusInstanceIndex;
      if (*startIndex == 1)
      {
         walrusInstance = walrusInstance - 1;
      }
      double* start = (double*)times;
      double* end = start + *timesLenght;
      std::vector<double> times(start, end);

      start = (double*)fc_vec;
      end = start + *fc_vecLenght;
      std::vector<double> fc_vec(start, end);

      walrusInstances[walrusInstance].set_seq_C(*varEnum, times, fc_vec, *timestepsize);
      return 0;
   }
   catch (...)
   {
      return -1;
   }
};

int WALRUSSET(int* walrusInstanceIndex, int* varEnum, double * val, int * startIndex)
{
   try
   {
      int walrusInstance = *walrusInstanceIndex;
      if (*startIndex == 1)
      {
         walrusInstance = walrusInstance - 1;
      }
      walrusInstances[walrusInstance].set(*varEnum, *val);
      return 0;
   }
   catch (...)
   {
      return -1;
   }
};

int WALRUSGET(int* walrusInstanceIndex, int* varEnum, void * val, int * startIndex)
{
   try
   {
      int walrusInstance = *walrusInstanceIndex;
      if (*startIndex == 1)
      {
         walrusInstance = walrusInstance - 1;
      }
      *(double*)val = walrusInstances[walrusInstance].get(*varEnum);
      return 0;
   }
   catch (...)
   {
      return -1;
   }
};


int WALRUSSETST(int* walrusInstanceIndex, int* soil, int * startIndex)
{
   try
   {
      int walrusInstance = *walrusInstanceIndex;
      if (*startIndex == 1)
      {
         walrusInstance = walrusInstance - 1;
      }
      walrusInstances[walrusInstance].set_st(*soil);
      return 0;
   }
   catch (...)
   {
      return -1;
   }
};



int WALRUSINIT(int* walrusInstanceIndex, double* StartTime, double* Q0, double * hS0,
   double* dG0, double * dV0, double* hQ0, double* Gfrac, int * startIndex)
{
   try
   {
      int walrusInstance = *walrusInstanceIndex;
      if (*startIndex == 1)
      {
         walrusInstance = walrusInstance - 1;
      }
      walrusInstances[walrusInstance].init(*StartTime, *Q0, *hS0, *dG0, *dV0, *hQ0, *Gfrac);
      return 0;
   }
   catch (...)
   {
      return -1;
   }
};


int WALRUSDOSTEP(int* walrusInstanceIndex, double* deltime, int * startIndex)
{
   try
   {
      int walrusInstance = *walrusInstanceIndex;
      if (*startIndex == 1)
      {
         walrusInstance = walrusInstance - 1;
      }
      walrusInstances[walrusInstance].dostep(*deltime);
      return 0;
   }
   catch (...)
   {
      return -1;
   }
};


/*  void set_W_dV_bytable(const vector<double> &dV, const vector<double> &W); */
int WALRUSSETWDVBYTABLE(int * walrusInstanceIndex,
   void * times, int* timesLenght, void * fc_vec, int * fc_vecLenght, int * startIndex)
{

   try
   {
      int walrusInstance = *walrusInstanceIndex;
      if (*startIndex == 1)
      {
         walrusInstance = walrusInstance - 1;
      }
      double* start = (double*)times;
      double* end = start + *timesLenght;
      std::vector<double> times(start, end);

      start = (double*)fc_vec;
      end = start + *fc_vecLenght;
      std::vector<double> fc_vec(start, end);

      walrusInstances[walrusInstance].set_W_dV_bytable(times, fc_vec);
      return 0;
   }
   catch (...)
   {
      return -1;
   }
};


/*  void set_dVeq_dG_bytable(const vector<double> &dG, */
/*                           const vector<double> &dVeq, */
int WALRUSSETDVEQDGBYTABLE(int * walrusInstanceIndex,
   void * times, int* timesLenght, void * fc_vec, int * fc_vecLenght, int * startIndex)
{

   try
   {
      int walrusInstance = *walrusInstanceIndex;
      if (*startIndex == 1)
      {
         walrusInstance = walrusInstance - 1;
      }
      double* start = (double*)times;
      double* end = start + *timesLenght;
      std::vector<double> times(start, end);

      start = (double*)fc_vec;
      end = start + *fc_vecLenght;
      std::vector<double> fc_vec(start, end);

      walrusInstances[walrusInstance].set_dVeq_dG_bytable(times, fc_vec);
      return 0;
   }
   catch (...)
   {
      return -1;
   }
};


/*  void set_beta_dV_bytable(const vector<double> &dV, */
/*                           const vector<double> &beta */
int WALRUSSETBETADVBYTABLE(int * walrusInstanceIndex,
   void * times, int* timesLenght, void * fc_vec, int * fc_vecLenght, int * startIndex)
{

   try
   {
      int walrusInstance = *walrusInstanceIndex;
      if (*startIndex == 1)
      {
         walrusInstance = walrusInstance - 1;
      }
      double* start = (double*)times;
      double* end = start + *timesLenght;
      std::vector<double> times(start, end);

      start = (double*)fc_vec;
      end = start + *fc_vecLenght;
      std::vector<double> fc_vec(start, end);

      walrusInstances[walrusInstance].set_beta_dV_bytable(times, fc_vec);
      return 0;
   }
   catch (...)
   {
      return -1;
   }
};


/*  void set_Q_hS_bytable(const vector<double> &hS, const vector<double> &Q); */
int WALRUSSETQHSBYTABLE(int * walrusInstanceIndex,
   void * times, int* timesLenght, void * fc_vec, int * fc_vecLenght, int * startIndex)
{

   try
   {
      int walrusInstance = *walrusInstanceIndex;
      if (*startIndex == 1)
      {
         walrusInstance = walrusInstance - 1;
      }
      double* start = (double*)times;
      double* end = start + *timesLenght;
      std::vector<double> times(start, end);

      start = (double*)fc_vec;
      end = start + *fc_vecLenght;
      std::vector<double> fc_vec(start, end);

      walrusInstances[walrusInstance].set_Q_hS_bytable(times, fc_vec);
      return 0;
   }
   catch (...)
   {
      return -1;
   }
};


