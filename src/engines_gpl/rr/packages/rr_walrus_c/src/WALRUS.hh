#ifndef WALRUSHH
#define WALRUSHH
#include "LinInt.hh"
#include <cmath>
#include <float.h>
#include <fstream>
#include <vector>
using namespace std;

#include <string>

/*! @file
 * @brief the basic WALRUS class
 */

/**
 * @enum  WALRUS_PAR
 * @brief the list of physical WALRUS parameters
 */
enum WALRUS_PAR {
  par_cW = 1, /**<  wetness index parameter, controls the divider (mm)*/
  par_cV = 2, /**<  vadose zone relaxation time, controls the connection
               saturated-unsaturated*/
  par_cG = 3, /**<  groundwater reservoir constant, controls the outflow of
               groundwater to open water (mm h)*/
  par_cQ =
      4, /**<  quickflow reservoir constant, controls the outflow of quickflow
          to open water*/
  par_cS = 5,      /**<  bankfulle discharge of open water (mm/h) */
  par_cD = 6,      /**<  channel depth, depth bottom open water below surface */
  par_psi_ae = 7,  /**< air entry value unsaturated zone */
  par_b = 8,       /**< pore size distribution unsaturated zone */
  par_theta_s = 9, /**< saturated soil moisture of unsaturated zone */
  par_aS = 10,  /**< surface water fraction, 1-par_aS = ground water fraction */
  par_area = 11, /**< area (in km^2) of catchment */
  par_cexpS = 12,  /**<  power of hS in the Q-hS relation (-) */
  par_num_PARS
};

/**
 * @enum  WALRUS_SOIL
 * @brief the list of predefined soils in WALRUS_SOIL
 */

enum WALRUS_SOIL {
  sand = 21,
  loamy_sand = 22,
  sandy_loam = 23,
  silt_loam = 24,
  loam = 25,
  sandy_clay_loam = 26,
  silt_clay_loam = 27,
  clay_loam = 28,
  sandy_clay = 29,
  silty_clay = 30,
  clay = 31,
  cal_H = 32,
  cal_C = 33,
  custom = 34,
  par_num_SOILS
};

/**
 * @enum  WALRUS_NUM
 * @brief the list of numerical parameters in WALRUS
 */

enum WALRUS_NUM {
  par_min_deltime = 41,  /**< minimal length of time step (s) */
  par_max_h_change = 42, /**< changes in level larger then this will lead to
                       substeps (mm)*/
  par_min_h =
      43, /**< levels smaller than this will be considered numerically to
 be 0 (mm) */
  par_max_Pstep = 44, /**< P amounts larger then this will lead to substeps (mm)
                  */
  par_max_substeps = 45, /**< maximum number of substeps [-]*/
  par_num_NUMS
};

/**
 * @enum  WALRUS_FORCING
 * @brief the list external forcings in WALRUS
 */

enum WALRUS_FORCING {
  fc_P = 50,     /**< rainfall */
  fc_ETpot = 51, /**< potential evaporation*/
  fc_XS = 52,    /**< external in\out to open water*/
  fc_XG = 53,    /**< external in\out to groundwater*/
  par_num_FORCINGS
};

/**
 * @enum  WALRUS_STATE
 * @brief the list of state of a WALRUS model
 */
enum WALRUS_STATE {
  cur_time = 60, /**< the current time of the model (s) */
  cur_dV = 61,   /**<  the current storage deficit */
  cur_dG = 62,   /**< the current groundwater depth (mm) */
  cur_hQ = 63,   /**< current level in quickflow reservoir (mm) */
  cur_hS = 64,   /**< current surface water level (mm) */
  par_num_STATES
};

/**
 * @enum  WALRUS_DEPS
 * @brief the list of state dependent variables of a WALRUS model
 */
enum WALRUS_DEPS {
  cur_W = 71,    /**< the current wetness index */
  cur_beta = 72, /**<  the current evap redux factor */
  cur_dVeq = 73, /**< the current equilibrium storage deficit (mm) */
  par_num_DEPS
};

/**
 * @enum  WALRUS_USEDFLUX
 * @brief the list of fluxes cumulative over the last time step
 */
enum WALRUS_USEDFLUX {
  last_deltime = 80, /**< last time step used for the fluxes below */
  last_fXG = 81,     /**< cumulative external to-from groundwater (mm) */
  last_fXS = 82,     /**< cumulative external to-from open water (mm) */
  last_PQ = 83,    /**< cumulative precipitation in quickflow reservoir (mm) */
  last_PV = 84,    /**< cumulative precipitation in vadose zone (mm) */
  last_PS = 85,    /**< cumulative precipitation in open water (mm) */
  last_ETV = 86,   /**< cumulative actual transpiration (mm) */
  last_ETS = 87,   /**< cumulative open water evaporation (mm) */
  last_ETact = 88, /**< cumulative actual evapotranspiration (mm) */
  last_fQS = 89,   /**< cumulative quickflow (mm) */
  last_fGS = 90,   /**< cumulative baseflow (mm) */
  last_Q = 91,     /**< cumulative open water outflow (mm) */
  last_Qdischarge = 92, /**< cumulative open water outflow (m^3/s) */
  last_fXSact = 93, /**< cumulative actual external to-from open water (mm) */
  // added GP
  last_P = 94,     /**< cumulative precipitation (mm) */
  last_ETPot = 95, /**< cumulative ETPot (mm) */
  par_num_USEDFLUXES
};

/**  The WALRUS class
 *  contains all the data and methods of a WALRUS model
 */
class WALRUS {
  friend class dGfinderclass;

public:
  WALRUS();
  // setting and getting physical parameters by name
  // physical parameters

  // if enumerates not available
  void set(unsigned int idnum, double newvalue);
  double get(unsigned int idnum);
  // for Rcpp interface
  void setbyname(const char* name, double newvalue);
  double  getbyname(const char* name);
  /**
   *  @brief set a parameter of the WALRUS class
   *  @par the parameter of the WALRUS_PAR type
   *  @newvalue the new value for the parameter
   *  @sa WALRUS::get
   *  @sa enum WALRUS_PAR
   */
  void set(enum WALRUS_PAR par, double newvalue);

  /**
   *  @brief get the value of a parameter of the WALRUS class
   *  @par the parameter of the WALRUS_PAR type
   *  @result the value of the parameter
   *  @sa WALRUS::set
   *  @sa enum WALRUS_PAR
   */
  double get(enum WALRUS_PAR par);
  void set_st(unsigned int idnum);
  // for Rcpp interface
  void set_stbyname(const char* name);
  /**
   *  @brief sets the unsaturated parameters by
   *  predefined soil types
   *  @par the soil from the enum WALRUS_SOIL type
   *  @newvalue the new value for the parameter
   */
  void set_st(enum WALRUS_SOIL soil);
  /**
   *  @brief gets the name of the soil type
   *  (see WALRUS_SOIL)
   *  @result the name as string
   */
  std::string get_st();
  /**
   *  @brief set a numerical parameter of the WALRUS class
   *  @par the parameter of the WALRUS_NUM type
   *  @newvalue the new value for the parameter
   *  @sa WALRUS::get
   *  @sa enum WALRUS_NUM
   */
  void set(enum WALRUS_NUM par, double newvalue);

  /**
   *  @brief get the value of a numerical parameter of the WALRUS class
   *  @par the parameter of the WALRUS_NUM type
   *  @result the value of the parameter
   *  @sa WALRUS::set
   *  @sa enum WALRUS_NUM
   */
  double get(enum WALRUS_NUM par);

  // system functions
  /**
   *   @brief  the function that gives the wetness index W as function
   *   of dV \n
   *   if the analytical form is chosen (defuault) this depends on
   *   the parameter cW
   *   @param  dV: deficit volume in mm
   *   @result W value (dimensionless)
   *   @sa set_W_dV_bytable
   */
  double W_dV(double dV);
  /**
   *   @brief replaces the default analytically defined
   *   relation between the wetness index W and the deficit dV
   *   by a linearly interpolating in a table
   *   provided by the user
   *   @param dV,W : vector<double>tors defining the values
   *   between which will be interpolated
   *   @sa W_dV
   */
  void set_W_dV_bytable(const vector<double> &dV, const vector<double> &W);
  /**
   *   @brief  the function that gives the equilibrium storage deficit
   *   as function of dG \n
   *   if the analytical form is chosen (defuault) this depends on
   *   the soil physical parameters
   *   @param  dG groundwater depth in mm
   *   @result dVeq deficit in mm
   *   @sa set_dVeq_dG_bytable
   */
  double dVeq_dG(double dG);
  void set_dVeq_dG_bytable(const vector<double> &dG,
                           const vector<double> &dVeq);
  double beta_dV(double dV);
  void set_beta_dV_bytable(const vector<double> &dV,
                           const vector<double> &beta);
  double Q_hS(double hS);
  void set_Q_hS_bytable(const vector<double> &hS, const vector<double> &Q);
  //
  // flux forcings
  //
  void set_Ibyname(const char* name, double intensity);
  void set_I(unsigned int idnum, double intensity);
  /**
   *   @brief  sets the intensity of the forcing \n
   *   this constant intensity is used for all times until a new
   *   value has been set
   *   @param  fc: the forcing as in WALRUS_FORCING
   *   @param  intensity: the intensity in mm/h
   */
  void set_I(enum WALRUS_FORCING fc, double intensity);
  /**
   *   @brief  sets  an external forcing by a series of
   *   accumulated  values \n
   *   (linearly interpolated) values of this series are used until
   *   a new rainfall value is set
   *   @param  times: an increasing series of times in sec
   *   @param fcvector<double>: corresponding accumlated fc values in mm
   */
  void set_seq_Cbyname(const char* name, const vector<double> times,
                 const vector<double> fc_vec);
  void set_seq_C(unsigned int idnum, const vector<double> times,
                 const vector<double> fc_vec, double timestepsize);
  void set_seq_C(enum WALRUS_FORCING fc, const vector<double> times,
                 const vector<double> fc_vec, double timestepsize);
  /**
   *   @brief  sets an external forcing flux by series of
   *   regularly measured intensities
   *   (linearly interpolated) values of this series are used until
   *   a new rainfall value is set
   *   @param  tstart: starting time (s)
   *   @param timestep: constant time between two intensities (s)
   *   @param fcvector<double>: rainfall intensities in mm/h
   */
   void set_seq_Ibyname(const char* name, double tstart, double timestep,
                  const vector<double> fc_vec);
  void set_seq_I(unsigned int idnum, double tstart, double timestep,
                 const vector<double> fc_vec);
  void set_seq_I(enum WALRUS_FORCING fc, double tstart, double timestep,
                 const vector<double> fc_vec);
  /**
   *   @brief  gets the total external flxu between two times
   *   @param  from_time: starting time in sec
   *   @param  to_time: ending time in sec
   *   @return  total external flux  in mm during this interval
   */
  double get_cumbyname(const char* name, double from_time, double to_time);
  double get_cum(unsigned int idnum, double from_time, double to_time);
  double get_cum(enum WALRUS_FORCING fc, double from_time, double to_time);

  // level forcings
  void set_hSmin(double value);
  void set_hSminseries(vector<double> times, vector<double> hSmin);
  double get_hSmin(double attime);

  // set get states
  /**
   *   @brief  sets  a state in the WALRUS model
   *   @param  s: a state of the enum WALRUS_STATE
   *   @param value: the new value for the set_beta_dV_bytable
   */
  void set(enum WALRUS_STATE s, double value);
  /**
   *   @brief  gets  a state in the WALRUS model
   *   @param  s: a state of the enum WALRUS_STATE
   *   @return the current value of that state
   */
  double get(enum WALRUS_STATE s);
  /**
   *   @brief  gets  a state in the WALRUS model
   *   @param  d: a state dependent variable  of the enum WALRUS_DEPS
   *   @return the current value of that dependent variable
   */

  double get(enum WALRUS_DEPS d);

  /**
   *   @brief  gets internal fluxes cumulative over last time step
   *   @param  d: a state dependent variable  of the enum WALRUS_USEDFLUX
   *   @return the current value of that dependent variable
   */
  double get(enum WALRUS_USEDFLUX f);

  /**
   *   @brief initializes states starting from a given open water flux
   *   avoiding by that (long) warm up period
   *   intial time is set to zero
   *   @param  Q0 the open water flux (mm/day)
   */
  void init_by_Q(double Q0);
  void init(double Q0);
  /**
   *   @brief initializes states starting from a given open water flux
   *   avoiding by that (long) warm up period
   *   @param attime the initial time set in the model
   *   @param  Q0 the open water flux (mm/day)
   */
  void init_by_tQ(double attime, double Q0);
  void init(double attime, double Q0);
  /**
   *   @brief initializes states
   *   avoiding by that (long) warm up period
   *   @param  Q0 the open water flux (mm/day)
   *   @param hS0 initial open water level (mm), NAN if not available
   *   @param dG0 initial groundwater depth (mm), NAN if not available
   *   @param dV0 initial soil moisture deficit (mm), NAN if not available
   *   @param hQ0 initial level fast reservoir (mm), NAN if not available
   */
  void init_by_Qstates(double Q0, double hS0, double dG0, double dV0, double hQ0);
  void init(double Q0, double hS0, double dG0, double dV0, double hQ0);
  /**
   *   @brief initializes states
   *   avoiding by that (long) warm up period
   *   @param attime the initial time set in the model
   *   @param  Q0 the open water flux (mm/day)
   *   @param hS0 initial open water level (mm), NAN if not available
   *   @param dG0 initial groundwater depth (mm), NAN if not available
   *   @param dV0 initial soil moisture deficit (mm), NAN if not available
   *   @param hQ0 initial level fast reservoir (mm), NAN if not available
   */
  void init_by_tQstates(double attime, double Q0, double hS0, double dG0,
    double dV0, double hQ0);
  void init(double attime, double Q0, double hS0, double dG0, double dV0,
            double hQ0);
  /**
   *   @brief initializes states
   *   avoiding by that (long) warm up period
   *   @param attime the initial time set in the model
   *   @param  Q0 the open water flux (mm/day)
   *   @param hS0 initial open water level (mm), NAN if not available
   *   @param dG0 initial groundwater depth (mm), NAN if not available
   *   @param dV0 initial soil moisture deficit (mm), NAN if not available
   *   @param hQ0 initial level fast reservoir (mm), NAN if not available
   *   @param Gfrac: initial ratio slow/total outlow (default 1)
   */
  void init_by_all(double attime, double Q0, double hS0, double dG0,
    double dV0, double hQ0,double Gfrac);
  void init(double attime, double Q0, double hS0, double dG0, double dV0,
            double hQ0, double Gfrac);
  /**
   *   @brief does a time step \n
   *   Performs one stime step given by the argment.
   *   If considered needed performs smaller substeps
   *   @param deltime: the step to make (s)
   *   @sa dosteps
   */
  void dostep(double deltime);

  /**
   *   @brief does a number of time steps \n
   *   Performs several time steps given by the argments.
   *   If considered needed performs smaller substeps
   *   @param endtime stepping stops whent this time is reached (s)
   *   @param deltime the step to make (s)
   *   @param csvfile filename in which results are stored in csv format
   *   @param store_states boolean, if true (default) states are stored (see
   * WALRUS_STATE)
   *   @param store_deps  boolean, if true (default) states dependents are
   * stored (see WALRUS_DEPS)
   *   @param store_usedfluxes boolean, if true (default) calculated fluxes  are
   * stored (see WALRUS_USEDFLUX)
   */
  void dosteps(double endtime, double deltime, const char *csvfilename,
               bool store_states, bool store_deps, bool store_usedfluxes);

private:
  // parameters
  double cW;
  double cV;
  double cG;
  double cQ;
  double cS;
  double expS;
  double cD;
  double aS;
  double aG;
  enum WALRUS_SOIL st;
  double psi_ae;
  double b;
  double theta_s;
  double area;
  // pure numerical parameters
  double min_deltime;
  double min_h;
  double max_Pstep;
  double max_h_change;
  double max_substeps;
  // system functions
  bool W_dV_analytical;
  approxfun *W_dV_table;
  bool dVeq_dG_analytical;
  approxfun *dVeq_dG_table;
  bool beta_dV_analytical;
  approxfun *beta_dV_table;
  static const double zeta1;
  static const double zeta2;
  static const double MinFactor;
  bool Q_hS_analytical;
  approxfun *Q_hS_table;
  approxfun *hS_Q_table;
  // forcing fluxes
  double P;
  bool Pfromseries;
  approxfun *cumPfun;
  double ETpot;
  bool ETpotfromseries;
  approxfun *cumETpotfun;
  double fXG;
  bool fXGfromseries;
  approxfun *cumfXGfun;
  double fXS;
  bool fXSfromseries;
  approxfun *cumfXSfun;
  // forcing level
  double hSmin;
  static const double hSzeroforET;
  bool hSminfromseries;
  approxfun *hSminfun;
  // an internal flux function, defined as it is used on more
  // then one place
  // and the general update of all
  double fGS_dG(double dG);
  // underscores to avoid conflicts with argument names
  double _dV;
  double _hQ;
  double _hS;
  double _dG;
  double _time;
  double _lastdeltime;
  double _W;
  double _dVeq;
  // fluxes of last step cumulative
  double cur_fXG_c;
  double cur_fXS_c;
  double cur_PQ_c;
  double cur_PV_c;
  double cur_PS_c;
  double cur_ETV_c;
  double cur_ETS_c;
  double cur_ETact_c;
  double cur_fXSact_c;
  double cur_fQS_c;
  double cur_fGS_c;
  double cur_Q_c;
  // added GP
  double cur_P_c;
  double cur_ETpot_c;
  // fluxes of during substeps
  double hS_Q(double Q);
  double fXG_try;
  double fXS_try;
  double PQ_try;
  double PV_try;
  double PS_try;
  double ETV_try;
  double ETS_try;
  double ETact_try;
  double fXSact_try;
  double fQS_try;
  double fGS_try;
  double Q_try;
  // added GP
  double P_try;
  double ETpot_try;
  // end addition GP
  // initialisation, nan can be an option

  // intermediate values in dostep
  void calctryvalues(double tend);
  double pred_time_tries_OK(double tend);

  double dV_try;
  double hQ_try;
  double hS_try;
  double dG_try;
};

#endif
