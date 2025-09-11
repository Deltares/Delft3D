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
#include <cmath>
#include <cstring>

/*! @file
 * @brief Implementation of the set/get parameters and states
*/

void WALRUS::set(unsigned int idnum, double newvalue) {
  if (idnum >= par_cW && idnum < par_num_PARS) {
      set((WALRUS_PAR)idnum, newvalue);
  }
  else if (idnum >= sand && idnum < par_num_SOILS) {
      set((WALRUS_SOIL)idnum, newvalue);
  }
  else if (idnum >= par_min_deltime && idnum < par_num_NUMS) {
      set((WALRUS_NUM)idnum, newvalue);
  }
  else if (idnum >= cur_time && idnum < par_num_STATES) {
      set((WALRUS_STATE)idnum, newvalue);
  }
  else if (idnum == 96) {
      set_hSmin(newvalue);
  }
  else {
     // TODO ERROR
  ;
  }
};

void WALRUS::setbyname(const char *name, double newvalue) {
        if(strcmp(name,"par_cW")==0) {
                set(par_cW,newvalue);
        } else if (strcmp(name,"par_cV")==0) {
                set(par_cV,newvalue);
        } else if (strcmp(name,"par_cG")==0) {
                set(par_cG,newvalue);
        } else if (strcmp(name,"par_cQ")==0) {
                set(par_cQ,newvalue);
        } else if (strcmp(name,"par_cS")==0) {
                set(par_cS,newvalue);
        } else if (strcmp(name,"par_cD")==0) {
                set(par_cD,newvalue);
        } else if (strcmp(name,"par_psi_ae")==0) {
                set(par_psi_ae,newvalue);
        } else if (strcmp(name,"par_b")==0) {
                set(par_b,newvalue);
        } else if (strcmp(name,"par_theta_s")==0) {
                set(par_theta_s,newvalue);
        } else if (strcmp(name,"par_aS")==0) {
                set(par_aS,newvalue);
        } else if (strcmp(name,"par_area")==0) {
                set(par_area,newvalue);
        } else if (strcmp(name,"par_cexpS")==0) {
                set(par_cexpS,newvalue);
        } else if (strcmp(name,"par_min_deltime")==0) {
                set(par_min_deltime,newvalue);
        } else if (strcmp(name,"par_max_h_change")==0) {
                set(par_max_h_change,newvalue);
        } else if (strcmp(name,"par_min_h")==0) {
                set(par_min_h,newvalue);
        } else if (strcmp(name,"par_max_Pstep")==0) {
                set(par_max_Pstep,newvalue);
        } else if (strcmp(name,"par_max_substeps")==0) {
                set(par_max_substeps,newvalue);
        } else if (strcmp(name,"cur_time")==0) {
                set(cur_time,newvalue);
        } else if (strcmp(name,"cur_dV")==0) {
                set(cur_dV,newvalue);
        } else if (strcmp(name,"cur_dG")==0) {
                set(cur_dG,newvalue);
        } else if (strcmp(name,"cur_hQ")==0) {
                set(cur_hQ,newvalue);
        } else if (strcmp(name,"cur_hS")==0) {
                set(cur_hS,newvalue);
        } else {
          // reporterror("unknown field to set")
          ;
        }
};

double WALRUS::get(unsigned int idnum) {
      if (idnum >= par_cW && idnum < par_num_PARS) {
        return get((WALRUS_PAR)idnum);
      }
      else if (idnum >= sand && idnum < par_num_SOILS) {
        return get((WALRUS_SOIL)idnum);
      }
      else if (idnum >= par_min_deltime && idnum < par_num_NUMS) {
        return get((WALRUS_NUM)idnum);
      }
      else if (idnum >= cur_time && idnum < par_num_STATES) {
        return get((WALRUS_STATE)idnum);
      }
      else if (idnum >= cur_W && idnum < par_num_DEPS) {
        return get((WALRUS_DEPS)idnum);
      }
      else if (idnum >= last_deltime && idnum < par_num_USEDFLUXES) {
        return get((WALRUS_USEDFLUX)idnum);
      }
      else {
        return DBL_MIN;
      }
};

double WALRUS::getbyname(const char* name) {
        if(strcmp(name,"par_cW")==0) {
                return get(par_cW);
        } else if (strcmp(name,"par_cV")==0) {
                return get(par_cV);
        } else if (strcmp(name,"par_cG")==0) {
                return get(par_cG);
        } else if (strcmp(name,"par_cQ")==0) {
                return get(par_cQ);
        } else if (strcmp(name,"par_cS")==0) {
                return get(par_cS);
        } else if (strcmp(name,"par_cD")==0) {
                return get(par_cD);
        } else if (strcmp(name,"par_psi_ae")==0) {
                return get(par_psi_ae);
        } else if (strcmp(name,"par_b")==0) {
                return get(par_b);
        } else if (strcmp(name,"par_theta_s")==0) {
                return get(par_theta_s);
        } else if (strcmp(name,"par_aS")==0) {
                return get(par_aS);
        } else if (strcmp(name,"par_area")==0) {
                return get(par_area);
        } else if (strcmp(name,"par_cexpS")==0) {
                return get(par_cexpS);
        } else if (strcmp(name,"par_min_deltime")==0) {
                return get(par_min_deltime);
        } else if (strcmp(name,"par_max_h_change")==0) {
                return get(par_max_h_change);
        } else if (strcmp(name,"par_min_h")==0) {
                return get(par_min_h);
        } else if (strcmp(name,"par_max_Pstep")==0) {
                return get(par_max_Pstep);
        } else if (strcmp(name,"par_max_substeps")==0) {
                return get(par_max_substeps);
        } else if (strcmp(name,"cur_time")==0) {
                return get(cur_time);
        } else if (strcmp(name,"cur_dV")==0) {
                return get(cur_dV);
        } else if (strcmp(name,"cur_dG")==0) {
                return get(cur_dG);
        } else if (strcmp(name,"cur_hQ")==0) {
                return get(cur_hQ);
        } else if (strcmp(name,"cur_hS")==0) {
                return get(cur_hS);
        } else {
          // reporterror("unknown field to get")
          ;
          return 0;
        }
};

void WALRUS::set(enum WALRUS_PAR par, double newvalue) {
  switch (par) {
  case par_cW:
    if(newvalue>=0)
    {
            cW = newvalue;
    } else {
          // reporterror("values for par_cW should be positive")
          ;
    }
    break;
  case par_cV:
    cV = newvalue;
    break;
  case par_cG:
    if(newvalue>0) {
            cG = newvalue;
    } else {
          // reporterror("values for par_cG should be strictly positive")
          ;
    }
    break;
  case par_cQ:
    if(newvalue>0) {
            cQ = newvalue;
    } else {
          // reporterror("values for par_cQ should be strictly positive")
          ;
    }
    break;
  case par_cS:
    if(newvalue>0) {
            cS = newvalue;
    } else {
          // reporterror("values for par_cS should be strictly positive")
          ;
    }
    break;
  case par_cexpS:
    if(newvalue>0) {
            expS = newvalue;
    } else {
          // reporterror("values for par_cexpS should be strictly positive")
          ;
    }
    break;
  case par_cD:
    if(newvalue>=0) {
            cD = newvalue;
    } else {
          // reporterror("values for par_cD should be positive")
          ;
    }
    break;
  case par_psi_ae:
    if(newvalue>=0) {
            psi_ae = newvalue;
    } else {
          // reporterror("values for par_psi_ae should be positive")
          ;
    }
    st = custom;
    break;
  case par_b:
    if(newvalue>=0) {
            b = newvalue;
    } else {
          // reporterror("values for par_b should be positive")
          ;
    }
    st = custom;
    break;
  case par_theta_s:
    if(newvalue>=0) {
            theta_s = newvalue;
    } else {
          // reporterror("values for par_theta_s should be positive")
          ;
    }
    st = custom;
    break;
  case par_aS:
    if((newvalue > MinFactor ) & (newvalue < (1-MinFactor))) {
            aS = newvalue;
    } else {
          // reporterror("values for par_aS strictly positive and strictly less than 1")
          ;
    }
    aG = 1 - aS;
    break;
  case par_area:
    if(newvalue >= 0)
    {
            area = newvalue;
    } else {
           // reporterror("values for par_area should be positive")
           ;
    }
    break;
  }
};

double WALRUS::get(enum WALRUS_PAR par) {
  switch (par) {
  case par_cW:
    return cW;
  case par_cV:
    return cV;
  case par_cG:
    return cG;
  case par_cQ:
    return cQ;
  case par_cS:
    return cS;
  case par_cexpS:
    return expS;
  case par_cD:
    return cD;
  case par_psi_ae:
    return psi_ae;
  case par_b:
    return b;
  case par_theta_s:
    return theta_s;
  case par_aS:
    return aS;
  case par_area:
    return area;
  default: // just to avaoid warnings
    return 0.0;
  }
}

void WALRUS::set_st(unsigned int idnum) {
  switch (idnum) {
  case 21:
    set_st(sand);
    break;
  case 22:
    set_st(loamy_sand);
    break;
  case 23:
    set_st(sandy_loam);
    break;
  case 24:
    set_st(silt_loam);
    break;
  case 25:
    set_st(loam);
    break;
  case 26:
    set_st(sandy_clay_loam);
    break;
  case 27:
    set_st(silt_clay_loam);
    break;
  case 28:
    set_st(clay_loam);
    break;
  case 29:
    set_st(sandy_clay);
    break;
  case 30:
    set_st(silty_clay);
    break;
  case 31:
    set_st(clay);
    break;
  case 32:
    set_st(cal_H);
    break;
  case 33:
    set_st(cal_C);
    break;
  }
}

void WALRUS::set_stbyname(const char* name) {
        if(strcmp(name,"sand")==0) {
                set_st(sand);
        } else if(strcmp(name,"loamy_sand")==0) {
                set_st(loamy_sand);
        } else if(strcmp(name,"sandy_loam")==0) {
                set_st(sandy_loam);
        } else if(strcmp(name,"silt_loam")==0) {
                set_st(silt_loam);
        } else if(strcmp(name,"loam")==0) {
                set_st(loam);
        } else if(strcmp(name,"sandy_clay_loam")==0) {
                set_st(sandy_clay_loam);
        } else if(strcmp(name,"silt_clay_loam")==0) {
                set_st(silt_clay_loam);
        } else if(strcmp(name,"clay_loam")==0) {
                set_st(clay_loam);
        } else if(strcmp(name,"sandy_clay")==0) {
                set_st(sandy_clay);
        } else if(strcmp(name,"silty_clay")==0) {
                set_st(silty_clay);
        } else if(strcmp(name,"clay")==0) {
                set_st(clay);
        } else if(strcmp(name,"cal_H")==0) {
                set_st(cal_H);
        } else if(strcmp(name,"cal_C")==0) {
                set_st(cal_C);
        } else {
          // reporterror("unknown soil type")
          ;
        }
}

void WALRUS::set_st(enum WALRUS_SOIL soil) {
  st = soil;
  switch (soil) {
  case sand:
    b = 4.05;
    psi_ae = 121;
    theta_s = 0.395;
    break;
  case loamy_sand:
    b = 4.38;
    psi_ae = 90;
    theta_s = 0.410;
    break;
  case sandy_loam:
    b = 4.9;
    psi_ae = 218;
    theta_s = 0.435;
    break;
  case silt_loam:
    b = 5.3;
    psi_ae = 786;
    theta_s = 0.485;
    break;
  case loam:
    b = 5.39;
    psi_ae = 478;
    theta_s = 0.451;
    break;
  case sandy_clay_loam:
    b = 7.12;
    psi_ae = 299;
    theta_s = 0.42;
    break;
  case silt_clay_loam:
    b = 7.75;
    psi_ae = 356;
    theta_s = 0.477;
    break;
  case clay_loam:
    b = 8.52;
    psi_ae = 630;
    theta_s = 0.476;
    break;
  case sandy_clay:
    b = 10.4;
    psi_ae = 153;
    theta_s = 0.426;
    break;
  case silty_clay:
    b = 10.4;
    psi_ae = 490;
    theta_s = 0.492;
    break;
  case clay:
    b = 11.4;
    psi_ae = 405;
    theta_s = 0.482;
    break;
  case cal_H:
    b = 2.63;
    psi_ae = 90;
    theta_s = 0.418;
    break;
  case cal_C:
    b = 16.77;
    psi_ae = 9;
    theta_s = 0.639;
    break;
  case custom:
    break;
  }
}

std::string WALRUS::get_st() {
  switch (st) {
  case sand:
    return "sand";
  case loamy_sand:
    return "loamy sand";
  case sandy_loam:
    return "sandy loam";
  case silt_loam:
    return "silt loam";
  case loam:
    return "loam";
  case sandy_clay_loam:
    return "sandy clay loam";
  case silt_clay_loam:
    return "silt clay loam";
  case clay_loam:
    return "clay loam";
  case sandy_clay:
    return "sandy clay";
  case silty_clay:
    return "silty clay";
  case clay:
    return "clay";
  case cal_H:
    return "cal H";
  case cal_C:
    return "cal C";
  default:
    return "custom";
  }
}

// set get numerical parameters

void WALRUS::set(enum WALRUS_NUM par, double newvalue) {
  double fnv = std::abs(newvalue);
  if(fnv<MinFactor) {
          fnv = MinFactor;
  }
  switch (par) {
  case par_min_deltime:
    min_deltime = fnv;
    break;
  case par_max_h_change:
    max_h_change = fnv;
    break;
  case par_max_Pstep:
    max_Pstep = fnv;
    break;
  case par_max_substeps:
    max_substeps = fnv;
    break;
  case par_min_h:
    min_h = fnv;
    break;
  }
}

double WALRUS::get(enum WALRUS_NUM par) {
  switch (par) {
  case par_min_deltime:
    return min_deltime;
  case par_max_h_change:
    return max_h_change;
  case par_max_Pstep:
    return max_Pstep;
  case par_max_substeps:
    return max_substeps;
  case par_min_h:
    return min_h;
  default:
    return 0.0; // to avoid warnings
  }
}

// set get states

void WALRUS::set(enum WALRUS_STATE s, double value) {
  switch (s) {
  case cur_time:
    _time = value;
    break;
  case cur_dV:
    if(value >= 0) {
            _dV = value;
    } else {
            // reporterror("value for state dV must be positive")
            ;
    }
    break;
  case cur_hQ:
    if(value >= 0) {
            _hQ = value;
    } else {
            // reporterror("value for state hQ must be positive")
            ;
    }
    break;
  case cur_hS:
    if(value >= 0) {
            _hS = value;
    } else {
            // reporterror("value for state hS must be positive")
            ;
    }
    break;
  case cur_dG:
    if(value >= 0) {
            _dG = value;
    } else {
            // reporterror("value for state dG must be positive")
            ;
    }
  }
}

double WALRUS::get(enum WALRUS_STATE s) {
  switch (s) {
  case cur_time:
    return _time;
  case cur_dV:
    return _dV;
  case cur_hQ:
    return _hQ;
  case cur_hS:
    return _hS;
  case cur_dG:
    return _dG;
  default:
    return 0.0;
  }
}

double WALRUS::get(enum WALRUS_DEPS d) {
  switch (d) {
  case cur_W:
    return W_dV(_dV);
  case cur_beta:
    return beta_dV(_dV);
  case cur_dVeq:
    return dVeq_dG(_dG);
  default:
    return 0.0;
  }
}

double WALRUS::get(enum WALRUS_USEDFLUX f) {
  switch (f) {
  case last_deltime:
    return _lastdeltime;
  case last_fXG:
    return cur_fXG_c;
  case last_fXS:
    return cur_fXS_c;
  case last_PQ:
    return cur_PQ_c;
  case last_PV:
    return cur_PV_c;
  case last_PS:
    return cur_PS_c;
  case last_ETV:
    return cur_ETV_c;
  case last_ETS:
    return cur_ETS_c;
  case last_ETact:
    return cur_ETact_c;
  case last_fXSact:
    return cur_fXSact_c;
  case last_fQS:
    return cur_fQS_c;
  case last_fGS:
    return cur_fGS_c;
  case last_Q:
    return cur_Q_c;
  case last_Qdischarge: {
    return cur_Q_c / _lastdeltime * area / 3.6;
// added GP
  case last_P:
    return cur_P_c;
    break;
  case last_ETPot:
    return cur_ETpot_c;
    break;
  };
  default:
    return 0.0;
  }
}
