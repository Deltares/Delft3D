// Copyright (C) 2012 Deltares
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License version 2 as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

/**
 * @file
 * @brief rtcRuntimeConfigSettings.h
 * @author 
 * @version 1.0
 * @date 2012
 */

#ifndef RTCRUNTIMECONFIGSETTINGS_H
#define RTCRUNTIMECONFIGSETTINGS_H

#include <string>
#include <vector>

#include "boost/filesystem.hpp"

#include "RTCToolsDLLDefs.h"
#include "rtcToolsEnums.h"

using namespace std;

/**
  * @brief Stores all NEW settings in rtcRuntimeConfig.xml
  * In addition, this also defines some settings that may be passed as commandline
  * arguments when running in standalone mode.
  * 
  * schemaLocation is the only settings that may be passed via commandline exclusively;
  * mode, optimizerMode loggingLevel and useLogFlushing can be specified in rtcRuntimeConfig.xml,
  * but may be overridden on commandline.
  * The remaining settings are exclusive to rtcRuntimeConfig.xml
  *        
  */

class RTCTOOLS_DLL_API rtcRuntimeConfigSettings
{
public:
	struct GAMS { 
		string algorithm;
	};

	struct IPOPT {
		// output 
		int print_level;
		string print_user_options;
		string print_options_documentation;
		string print_timing_statistics;
		int file_print_level;
		// termination 
		double tol;
		int max_iter;
		double max_cpu_time;
		double dual_inf_tol;
		double constr_viol_tol;
		double compl_inf_tol;
		double acceptable_tol;
		int acceptable_iter;
		double acceptable_constr_viol_tol;
		double acceptable_dual_inf_tol;
		double acceptable_compl_inf_tol;
		double acceptable_obj_change_tol;
		double diverging_iterates_tol;
		bool treat_unsuccess_as_error;
		// nlpScaling 
		double obj_scaling_factor;
		string nlp_scaling_method;
		double nlp_scaling_max_gradient;
		double nlp_scaling_min_value;
		// nlp
		double bound_relax_factor;
		string honor_original_bounds;
		string check_derivatives_for_naninf;
		string fixed_variable_treatment;
		string jac_c_constant;
		string jac_d_constant;
		// linear solver
		string linear_solver;
		string linear_system_scaling;
		string linear_scaling_on_demand;
		int min_refinement_steps;
		int max_refinement_steps;

		// MA27 Linear Solver
		bool ma27options;
		double ma27_pivtol;
		double ma27_pivtolmax;
		double ma27_liw_init_factor;
		double ma27_la_init_factor;
		double ma27_meminc_factor;

		// MA57 Linear Solver
		bool ma57options;
		double ma57_pivtol;
		double ma57_pivtolmax;
		double ma57_pre_alloc;
		int ma57_pivot_order;
		string ma57_automatic_scaling;
		int ma57_block_size;
		int ma57_node_amalgamation;
		int ma57_small_pivot_flag;
	

		// MA77 Linear Solver
		bool ma77options;
		int ma77_print_level;
		int ma77_buffer_lpage;
		int ma77_buffer_npage;
		int ma77_file_size;
		int ma77_maxstore;
		int ma77_nemin;
		string ma77_order;
		double ma77_small;
		double ma77_static;
		double ma77_u;
		double ma77_umax;

		// MA86 Linear Solver
		bool ma86options;
		int ma86_print_level;
		int ma86_nemin;
		string ma86_order;
		string ma86_scaling;
		double ma86_small;
		double ma86_static;
		double ma86_u;
		double ma86_umax;

		// MA97 Linear Solver
		bool ma97options;
		int ma97_print_level;
		int ma97_nemin;
		string ma97_order;
		string ma97_scaling;
		string ma97_scaling1;
		string ma97_scaling2;
		string ma97_scaling3;
		double ma97_small;
		string ma97_solve_blas3;
		string ma97_switch1;
		string ma97_switch2;
		string ma97_switch3;
		double ma97_u;
		double ma97_umax;

		// MUMPS Linear Solver
		bool mumpsoptions;
		double mumps_pivtol;
		double mumps_pivtolmax;
		int mumps_mem_percent;
		int mumps_permuting_scaling;
		int mumps_pivot_order;
		int mumps_scaling;

		// Pardiso Linear Solver
		bool pardisooptions;
		string pardiso_matching_strategy;
		int pardiso_max_iterative_refinement_steps;
		int pardiso_msglvl;
		string pardiso_order;

		// hessian perturbation
		double min_hessian_perturbation;
		double max_hessian_perturbation;
		double first_hessian_perturbation;
		double perturb_inc_fact_first;
		double perturb_inc_fact;
		double perturb_dec_fact;
        double jacobian_regularization_value;

		// restoration phase
		string expect_infeasible_problem;
		double expect_infeasible_problem_ctol;
		double expect_infeasible_problem_ytol;
		string start_with_resto;
		double soft_resto_pderror_reduction_factor;
		double required_infeasibility_reduction;
        double bound_mult_reset_threshold;
		double constr_mult_reset_threshold;
		string evaluate_orig_obj_at_resto_trial;

		// multiplier updates
		string alpha_for_y;
		double alpha_for_y_tol;
		string recalc_y;
		double recalc_y_feas_tol;

		// line search
		int max_soc;
		int watchdog_shortened_iter_trigger;
		int watchdog_trial_iter_max;
		string accept_every_trial_step;

		// initialization
		double bound_frac;
		double bound_push;
		double slack_bound_frac;
		double slack_bound_push;
		double bound_mult_init_val;
		double constr_mult_init_max;
		string bound_mult_init_method;

		// warm start
		string warm_start_init_point;
		double warm_start_bound_frac;
		double warm_start_bound_push;
		double warm_start_slack_bound_frac;
		double warm_start_slack_bound_push;
		double warm_start_mult_bound_push;
		double warm_start_mult_init_max;

		// barrier parameter
		string mehrotra_algorithm;
		string mu_strategy;
		string mu_oracle;
		int quality_function_max_section_steps;
		string fixed_mu_oracle;
		string adaptive_mu_globalization;
		double mu_init;
		double mu_max_fact;
		double mu_max;
		double mu_min;
		double mu_target;
		double barrier_tol_factor;
		double mu_linear_decrease_factor;
		double mu_superlinear_decrease_power;


		// quasi newton
		int limited_memory_max_history;
		int limited_memory_max_skipping;

		// derivative checker
		string derivative_test;
		double derivative_test_perturbation;
		double derivative_test_tol;
		string derivative_test_print_all;
		string jacobian_approximation;
		double findiff_perturbation;
	};

	struct SA {
		double ftoll;
		double dels;
		int max_iter;
		double max_cpu_time;
	};

	struct parameterFile {
		string filename;
		fileTypeEnum type;
		prefixEnum prefix;
	};

	struct modeInfoContainer {
		runModeEnum mode;
		simPeriodEnum period;
		optimizerEnum optimizer;
		GAMS optimizerGAMS;
		IPOPT optimizerIPOPT;
		SA optimizerSA;
		bool executeObjectiveFunction;
		bool executeConstraints;
	};

    rtcRuntimeConfigSettings() {

		parameterConfigFile = vector<parameterFile>();
		parameterFile pf;
		pf.filename = "rtcParameterConfig.xml";
		pf.type = TREEVECTOR;
		pf.prefix = PREFIX_NONE;
		parameterConfigFile.push_back(pf);

        dataConfigFile = "rtcDataConfig.xml";
        objectiveConfigFile = "rtcObjectiveConfig.xml";
		postprocessingConfigFile = "rtcPostprocessingConfig.xml";
        scenarioTreeConfigFile = "rtcScenarioTreeConfig.xml";
        toolsConfigFile = "rtcToolsConfig.xml";
        schemaDir = "./";
		workDir = "./";
            
        // specifically declare them undefined (since we need to trigger on this later on):
        // when not defined on commandline, take their values from rtcRuntimeConfig.xml
        modeInfo = vector<modeInfoContainer>();  
		limitedMemory = false;

		constraintViolationAsError = false;
    }

    boost::filesystem::path schemaDir;
	boost::filesystem::path workDir;

    // files
	vector<parameterFile> parameterConfigFile;
    string dataConfigFile;
    string objectiveConfigFile;
	string postprocessingConfigFile;
    string scenarioTreeConfigFile;
    string toolsConfigFile;

    // period
    //   userDefined
    long long startDate;
    long long endDate;
    long long timeStep;
    int numberEnsembles;
		
    //   PIIinput
    char* piInputFile;
    // also uses numberEnsembles

    // mode
	vector<modeInfoContainer> modeInfo;
	bool limitedMemory;
    //   optimization (for now: only 1 optimizer which should be IPOPT, should be + in the future)
    optimizerExecutionModeEnum optimizerExecutionMode;
    //   closed loop (also assume that optimizer is one instance of IPOPT for now)
    int recedingHorizon;
    int forecastHorizon;

	// parallelization
	int nThread;
	bool parallelEnsembleSim;
	bool parallelEnsembleCon;
	bool parallelInternalSim;
	bool parallelInternalCon;

	bool constraintViolationAsError;
	int constraintViolationLevel;
	double constraintViolationTol;

	bool outputObjectiveFunction;
	bool outputObjectiveFunctionGradient;
	bool outputConstraint;
	bool outputJacobian;
};

#endif //RTCRUNTIMECONFIGSETTINGS_H
