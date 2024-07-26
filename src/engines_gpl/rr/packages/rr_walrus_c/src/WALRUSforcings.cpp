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

#include "LinInt.hh"
#include "WALRUS.hh"
#include <cstring>

/*! @file
 * @brief implements the forcing in-output
 */
void WALRUS::set_Ibyname(const char* name, double intensity) {
        if(strcmp(name,"fc_P")==0) {
                set_I(fc_P, intensity);
        } else if(strcmp(name,"fc_ETpot")==0) {
                set_I(fc_ETpot, intensity);
        } else if(strcmp(name,"fc_XS")==0) {
                set_I(fc_XS, intensity);
        } else if(strcmp(name,"fc_XG")==0) {
                set_I(fc_XG, intensity);
        } else {
               // reporterror("unkown forcing name")
                ;
        }
}

 void WALRUS::set_I(unsigned int idnum, double intensity) {
     set_I((WALRUS_FORCING)idnum, intensity);
 }

void WALRUS::set_I(enum WALRUS_FORCING fc, double intensity) {
  switch (fc) {
  case fc_P:
    P = intensity;
    Pfromseries = false;
    break;
  case fc_ETpot:
    ETpot = intensity;
    ETpotfromseries = false;
    break;
  case fc_XS:
    fXS = intensity;
    fXSfromseries = false;
    break;
  case fc_XG:
    fXG = intensity;
    fXGfromseries = false;
  }
}

void WALRUS::set_seq_Cbyname(const char* name, const vector<double> times,
                             const vector<double> fc_vec) {
        double timestepsize = 3600. ;
        if(strcmp(name,"fc_P")==0) {
                set_seq_C(fc_P, times, fc_vec, timestepsize);
        } else if(strcmp(name,"fc_ETpot")==0) {
                set_seq_C(fc_ETpot, times, fc_vec, timestepsize);
        } else if(strcmp(name,"fc_XS")==0) {
                set_seq_C(fc_XS, times, fc_vec, timestepsize);
        } else if(strcmp(name,"fc_XG")==0) {
                set_seq_C(fc_XG, times, fc_vec, timestepsize);
        } else {
                // reporterror("unkown forcing name")
                ;
        }
}

void WALRUS::set_seq_C(unsigned int idnum, const vector<double> times,
                        const vector<double> fc_vec, double timestepsize) {
     set_seq_C((WALRUS_FORCING)idnum, times, fc_vec, timestepsize);
}

void WALRUS::set_seq_C(enum WALRUS_FORCING fc, const vector<double> times,
                  const vector<double> fcvec, double timestepsize) {
  // first number corresponds to period between times[1] and times[0]
  unsigned int N = times.size();
  vector<double> ctimes = times;
  if (N == 1)
  {
     ctimes.push_back(ctimes[N - 1] + timestepsize);
  }
  else
  {
     ctimes.push_back(2 * ctimes[N - 1] - ctimes[N - 2]);
  }
  vector<double> cfcvec = fcvec;
  cfcvec.insert(cfcvec.begin(), 0);
  for (unsigned int i = 1; i < N + 1; i++) {
    cfcvec[i] += cfcvec[i - 1];
  }
  switch (fc) {
  case fc_P:
    cumPfun = new approxfun(ctimes, cfcvec);
    Pfromseries = true;
    break;
  case fc_ETpot:
    cumETpotfun = new approxfun(ctimes, cfcvec);
    ETpotfromseries = true;
    break;
  case fc_XS:
    cumfXSfun = new approxfun(ctimes, cfcvec);
    fXSfromseries = true;
    break;
  case fc_XG:
    cumfXGfun = new approxfun(ctimes, cfcvec);
    fXGfromseries = true;
    break;
  }
}

void WALRUS::set_seq_Ibyname(const char* name, double tstart, double timestep,
                             const vector<double> fc_vec) {
        if(strcmp(name,"fc_P")==0) {
                set_seq_I(fc_P, tstart, timestep, fc_vec);
        } else if(strcmp(name,"fc_ETpot")==0) {
                set_seq_I(fc_ETpot, tstart, timestep, fc_vec);
        } else if(strcmp(name,"fc_XS")==0) {
                set_seq_I(fc_XS, tstart, timestep, fc_vec);
        } else if(strcmp(name,"fc_XG")==0) {
                set_seq_I(fc_XG, tstart, timestep, fc_vec);
        } else {
                // reporterror("unkown forcing name")
                ;
        }
}
void WALRUS::set_seq_I(unsigned int idnum, double tstart, double timestep,
                        const vector<double> fc_vec) {
     set_seq_I((WALRUS_FORCING)idnum, tstart, timestep, fc_vec);
}

void WALRUS::set_seq_I(enum WALRUS_FORCING fc, double tstart, double timestep,
                  const vector<double> fcvec) {
  vector<double> times;
  vector<double> fccum;
  times.push_back(tstart);
  fccum.push_back(fcvec[0] / 3600); // time in s, fc in mm/h
  for (unsigned int i = 1; i < fcvec.size(); i++) {
    times.push_back(times[i - 1] + timestep);
    fccum.push_back(fccum[i - 1] + fcvec[i] / 3600);
  }
  switch (fc) {
  case fc_P:
    cumPfun = new approxfun(times, fccum);
    Pfromseries = true;
    break;
  case fc_ETpot:
    cumETpotfun = new approxfun(times, fccum);
    ETpotfromseries = true;
    break;
  case fc_XS:
    cumfXSfun = new approxfun(times, fccum);
    fXSfromseries = true;
    break;
  case fc_XG:
    cumfXGfun = new approxfun(times, fccum);
    fXGfromseries = true;
    break;
  }
}

double WALRUS::get_cumbyname(const char* name, double from_time, double to_time) {
        if(strcmp(name,"fc_P")==0) {
                return get_cum(fc_P, from_time, to_time);
        } else if(strcmp(name,"fc_ETpot")==0) {
                return get_cum(fc_ETpot, from_time, to_time);
        } else if(strcmp(name,"fc_XS")==0) {
                return get_cum(fc_XS, from_time, to_time);
        } else if(strcmp(name,"fc_XG")==0) {
                return get_cum(fc_XG, from_time, to_time);
        } else {
                // reporterror("unkown forcing name")
                ;
                return 0.0; // just to avaoid warnings
        }
}

double WALRUS::get_cum(unsigned int idnum, double from_time, double to_time) {
     return get_cum((WALRUS_FORCING)idnum, from_time, to_time);
}

double WALRUS::get_cum(enum WALRUS_FORCING fc, double from_time,
                        double to_time) {
  switch (fc) {
  case fc_P:
    if (Pfromseries) {
            return (*(cumPfun))(to_time) - (*(cumPfun))(from_time);
    } else {
            return (P) * (to_time - from_time);
    }
    break;
  case fc_ETpot:
    if (ETpotfromseries) {
            return (*(cumETpotfun))(to_time) - (*(cumETpotfun))(from_time);
    } else {
            return (ETpot) * (to_time - from_time);
    }
    break;
  case fc_XS:
    if (fXSfromseries) {
            return (*(cumfXSfun))(to_time) - (*(cumfXSfun))(from_time);
    } else {
            return (fXS) * (to_time - from_time);
    }
    break;
  case fc_XG:
    if (fXGfromseries) {
            return (*(cumfXGfun))(to_time) - (*(cumfXGfun))(from_time);
    } else {
            return (fXG) * (to_time - from_time);
    }
    break;
  }
  return 0.0;
}

// level forcings

void WALRUS::set_hSmin(double value) {
  this->hSmin = value;
  this->hSminfromseries = false;
}
void WALRUS::set_hSminseries(vector<double> times, vector<double> hSmin) {
  this->hSminfun = new approxfun(times, hSmin);
  this->hSminfromseries = true;
}
double WALRUS::get_hSmin(double attime) {
  if (this->hSminfromseries) {
    return (*(this->hSminfun))(attime);
  } else {
    return this->hSmin;
  }
}
