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
 * @brief rtcToolsRuntime.h
 * @author Camiel van Breugelen, Dirk Schwanenberg
 * @version 1.0
 * @date 2012
 */


#ifndef RTCTOOLSRUNTIME_H
#define RTCTOOLSRUNTIME_H

#define HAVE_CONFIG_H

#include "rtcRuntimeConfigSettings.h"
#include "rtcToolsOptimizer.h"

#include "RTCToolsDLLDefs.h"
#include "timeseries/timeSeriesModel.h"
#include "rtcToolsSimulator.h"
#include "schematization/parameterInterface.h"
#include "schematization/schematisation.h"
#include "dataBinding/pi_timeseries.hxx"
#include "dataBinding/rtcRuntimeConfig.hxx"
#include "utilities/utils.h"
#include "fbc_version.h"

#include <string>
#include <iostream>
#include <exception>
#include <ctime>

using namespace rtctools::timeseries;
using namespace rtctools::schematization;
using namespace std;

namespace rtctools
{

struct versionInfo {
	bool isProprietary;
	string version;
	string revision;
    versionInfo();
    versionInfo(bool isProprietary, string version, string revision);
};

/**
  * @brief This is the main class of RTC Tools.
  */
class RTCTOOLS_DLL_API rtcToolsRuntime
{
	private:
		bool watchdogInitialization;
		int iStart;
		int iEnd;
		int iForecastTime;
		bool eval_g_new;

		/**
		 * @brief time series model with 3D matrix of
		 * input and output time series, dimensions are representing
		 * ensembles, time steps, and series
		 */
		timeSeriesModel *tsModel;

		/**
		 * @brief interface with external parameters (accessible for model calibration)
		 */
		parameterInterface *parInt;

		/**
		 * @brief model schematisation
		 */
		schematisation *schema;

		/**
		 * @brief array with simulator objects for each ensemble
		 */
		rtcToolsSimulator *rtcSim;

		// period
		// number of milliseconds after January 1, 1970
		long long t1, t2;
		long long T0;
		long long dt;
		int nEnsemble;

		// frequency in seconds for writing state files (-1.0, no state file written)
		long long startExportStateFile, endExportStateFile;
		double stateFileExportTimeStep = -1.0;

      // last time when the state file was written
      double lastTimeStateExported   = -1.0;

		vector<int> ensembleMap;

		/* OPENMI-specific functions */

		rtcRuntimeConfigSettings runtimeSettings;

		vector<double> getPVec();
		static void assertFile(string filename);
		long long getTime(fews::DateTimeComplexType::DateType date, fews::DateTimeComplexType::TimeType time);
		long long getTimeStep(fews::TimeStepComplexType::UnitType unit);

        std::vector<string> additionalTimeSerieNames;
        std::vector<int> additionalTimeseries;
    
        std::vector<string>& getCSVExtraColumnNames();
        std::vector<int>& getCSVExtraValues();

	public:

		static int main(int argc, const char * const argv[], versionInfo info = versionInfo());

		/**
		 * @brief Constructor
		 *
 		 * @param schemaDir Optional definition of directory with XSD schemas
 		 * @param workDir Optional definition of working directory 
 		 * @param isProprietary Flag for printing the open source or proprietary header
		 */
        rtcToolsRuntime(const char schemaDir[] = 0, const char workDir[] = 0, versionInfo info = versionInfo());

		/**
		 * @brief Destructor
		 */
		~rtcToolsRuntime(void);

		/**
		 * @brief Function for parsing the rtcRuntimeConfig.xml
		 */
		void parseRuntimeConfigFile();

		/**
		 * @brief Function for parsing all files the rtcRuntimeConfig.xml refers to
		 */
        void parseInputFiles();

		rtcRuntimeConfigSettings::modeInfoContainer getModeInfo(fews::RtcRuntimeConfigComplexType::ModeType m);

		/**
		 * @brief Sets externalized parameters
		 *
 		 * @param n Number of external inputs
		 * @param id Array with ids of external parameters
		 * @param inputArray Array with values of external parameters
		 */
		void setParameters(int n, string* id, double* inputArray);


		/**
		 * @brief Gets the number of simulation time steps
		 **/
		int getNTimeStep() { return tsModel->getTimeSeriesTensor()->getNTimeStep(); };


		/**
		 * @brief Gets the number of time series
		 **/
		int getNSeries() { return tsModel->getTimeSeriesTensor()->getNSeries(); };

		/**
		 * @brief Gets the runtime settings
		 */
		rtcRuntimeConfigSettings *getRuntimeSettings() { return &runtimeSettings; };


		/**
		 * @brief Performs a complete simulation
		 */
		double simulate(void);


		/**
		 * @brief Performs a simulation with a single time step
		 *
		 * @param i Time step index.
		 */
		void simulate(int i);


   		/**
		 * @brief Performs a complete simulation under consideration of the external input x
		 *
		 * @param n Number of external inputs
		 * @param x Array with external input data.
		 */
		double simulate(int n, const double *x, double*** JInc3DArray = (double***)0);


        /**
         * @brief Opens the output files before the computation starts
         */
        void openOutput();


        /**
         * @brief Writes the output for the current (or final) timestep
		 *
		 * @param timeStep Time step
		 * @param isFinalTimeStep Flag for the final time step for closing the files
         */
        void writeOutput(int timeStep, bool isFinalTimeStep);


		/**
		 * @brief Saves data, frees memory etc.
		 */
		void finish(int timeStep = 0);


		/**
		 * @brief Saves data, frees memory etc. for call from OpenMI
		 *
		 * @param timeStep time step number that was performed by last OpenMI-perform-timestep
		 */
		void finishFromOpenMI(int timeStep);


		/**
		 * @brief Gets time series model
		 */
		timeSeriesModel* getTimeSeriesModel() { return this->tsModel; };


		/**
		 * @brief Gets a time series matrix from the time series model
		 * 
		 * @param nTimeStep Number of time steps
		 * @param nSeries Number of time series
		 * @param tsm One-dimensional array for storing the time series matrix according to Matlab conventions
		 */
		void getTimeSeriesMatrix(int nTimeStep, int nSeries, double *tsm);


		/**
		 * @brief Puts an external time series matrix into the time series model
		 * 
		 * @param nTimeStep Number of time steps
		 * @param nSeries Number of time series
		 * @param tsm One-dimensional array with the time series matrix according to Matlab conventions
		 */
		void setTimeSeriesMatrix(int nTimeStep, int nSeries, double *tsm);


		/**
		 * @brief Info if the diagnostics writer is in debug mode
		 */
		bool showDebug();
	
		int getIStart() { return iStart; }
		int getIEnd() { return iEnd; }


		/**
		 * @brief Top-level call to perform algorithm according to settings
		 */
        bool execute(int timeStep = 0);


		/**
		 * @brief Top-level call from OpenMI to perform algorithm according to settings
		 */
		bool executeFromOpenMI(int timeStep = 0); 

      /**
      * @brief Writes the export file at a specific time index
      */
      void writeStateExport(int tIndex, bool isSnapshot = false);

      /**
      * @brief Gets the frequency for writing state files
      */
      double getStateFileExportTimeStep() const;

      /**
      * @brief Gets the start time for exporting state files
      */
      double getStartExportStateFile() const;
      
      /**
      * @brief Gets the end time for exporting state files
      */
      double getEndExportStateFile() const;

      /**
      * @brief Gets lastTimeStateExported
      */
      double getLastTimeStateExported();

      /**
      * @brief Sets lastTimeStateExported
      */
      void setLastTimeStateExported(const double value);
};

}
#endif //RTCTOOLSRUNTIME_H
