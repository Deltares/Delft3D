// Copyright (C) 2026 Deltares
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
 * @brief xxx
 * @author Dirk Schwanenberg
 * @version 1.0
 * @date 2010
 */

#include "rtcToolsSimulator.h"
#include "piDiagInterface.h"

const string RTCTOOLSSIMULATOR_CODE = "RTCTools.runtime.rtcToolsSimulator";

using namespace rtctools;


rtcToolsSimulator::rtcToolsSimulator(
	int iEnsemble,
	timeSeriesMatrixInterface* tsMatrix,
	schematisation *schema,
	double p,
	rtcRuntimeConfigSettings* runtimeSettings) : iEnsemble(iEnsemble), schema(schema),
	    tsMatrix(tsMatrix), p(p), runtimeSettings(runtimeSettings)
{ }


double rtcToolsSimulator::simulate(int iStart, int iEnd, double** JInc2DArray)
{
	try {
		// calls simulation stepwise
		for (int i=iStart; i<=iEnd; i++) {
			simulate(i);
		}
	} catch (exception &e) {
		piDiagInterface::addLine(1, "double rtcToolsSimulator::simulate(int iStart, int iEnd) - error during simulation - " + string(e.what()), RTCTOOLSSIMULATOR_CODE);
		throw;
	} catch (...) {
		piDiagInterface::addLine(1, "double rtcToolsSimulator::simulate(int iStart, int iEnd) - unknown error during simulation", RTCTOOLSSIMULATOR_CODE);
		throw;
	}

	// computes objective function value
	double J = 0.0;

	return J;
}

void rtcToolsSimulator::simulate(int iStep)
{
	if ((iStep<1) || (iStep>=this->tsMatrix->getNTimeStep()))
		throw runtime_error("void rtcToolsSimulator::simulate(int iStep) - time step is not available in time series matrix");

	// t, dt
	long long t = tsMatrix->getTime(iStep);
	double dt = ((double)tsMatrix->getDT(iStep))/1000.0;

    // get vectors with old and new states
    double *stateOld = tsMatrix->getState(iStep-1);
    double *stateNew = tsMatrix->getState(iStep);

	// rules statetransfer
	int numberOfRules = schema->getNRule();
    rule **hydraulicRules = schema->getRules();
	try {
		for (int i=0; i<numberOfRules; i++) {
			hydraulicRules[i]->stateTransfer(stateOld, stateNew, t, dt);
		}
	} catch (exception &e) {
		piDiagInterface::addLine(1, "void rtcToolsSimulator::simulate(int iStep) - error during rule state transfer execution - " + string(e.what()), RTCTOOLSSIMULATOR_CODE);
		throw;
	} catch (...) {
		piDiagInterface::addLine(1, "void rtcToolsSimulator::simulate(int iStep) - unknown error during rule state transfer execution", RTCTOOLSSIMULATOR_CODE);
		throw;
	}

    // triggers
    int nTrigger = schema->getNTrigger();
    trigger **tr = schema->getTriggers();
    try {
        for (int i = 0; i < nTrigger; i++) {
            // this deactivates all referenced rules in the triggers
            // note a rules is not deactivated if NOT referenced in any trigger
            tr[i]->deactivate();
        }
        for (int i = 0; i < nTrigger; i++)
        {

            tr[i]->solve(stateOld, stateNew, t, dt);
        }
    }
    catch (exception &e) {
        piDiagInterface::addLine(1, "void rtcToolsSimulator::simulate(int iStep) - error during trigger execution - " + string(e.what()), RTCTOOLSSIMULATOR_CODE);
        throw;
    }
    catch (...) {
        piDiagInterface::addLine(1, "void rtcToolsSimulator::simulate(int iStep) - unknown error during trigger execution", RTCTOOLSSIMULATOR_CODE);
        throw;
    }

    std::map<int, string> activeOutputsWithRuleName;
    char currentTime[50];
    utils::time2datetimestring(tsMatrix->getTimes()[iStep], currentTime);
    try 
    {
        for (int i = 0; i < numberOfRules; i++) 
        {
            if (hydraulicRules[i]->isActive())
            {
                const auto idOutput = hydraulicRules[i]->getIYOut();
                if (activeOutputsWithRuleName.count(idOutput))
                {
                    const auto & idMaps = schema->getTsTensor()->getScalarIDMap();
                    const auto idMap = std::find_if(idMaps.cbegin(), idMaps.cend(), 
                        [idOutput](const std::pair<string, int>& element) { return element.second == idOutput; });

                    throw std::runtime_error(string(currentTime) + " " + hydraulicRules[i]->getName() + " is going to enable "
                        + idMap->first + " when it is already enabled in " + activeOutputsWithRuleName[idOutput]);
                }

                // ignore unitDelay rule that does not have an assigned control point
                if (idOutput != -1)
                {
                    activeOutputsWithRuleName[idOutput] = hydraulicRules[i]->getName();
                }

                hydraulicRules[i]->solve(stateOld, stateNew, t, dt);
            }
        }
    }
    catch (exception &e) 
    {
        piDiagInterface::addLine(1, "void rtcToolsSimulator::simulate(int iStep) - error during rule execution - " + string(e.what()), RTCTOOLSSIMULATOR_CODE);
        throw;
    }
    catch (...) 
    {
        piDiagInterface::addLine(1, "void rtcToolsSimulator::simulate(int iStep) - unknown error during rule execution", RTCTOOLSSIMULATOR_CODE);
        throw;
    }

	// components
	int nComponent = schema->getNComponent();
    component **co = schema->getComponents();
	try 
    {
		for (int i=0; i<nComponent; i++) 
        {
			co[i]->solve(stateOld, stateNew, t, dt);
		}
	} catch (exception &e) {
		piDiagInterface::addLine(1, "void rtcToolsSimulator::simulate(int iStep) - error during component execution - " + string(e.what()), RTCTOOLSSIMULATOR_CODE);
		throw;
	} 
    catch (...) 
    {
		piDiagInterface::addLine(1, "void rtcToolsSimulator::simulate(int iStep) - unknown error during component execution", RTCTOOLSSIMULATOR_CODE);
		throw;
	}

	// increments the temporary states in limited memory option except for the last one
	// we should also increment the state in fulle memory option
	if ( iStep<tsMatrix->getNTimeStep()-1) {
		tsMatrix->incrementTimeStep();
	}
}

void rtcToolsSimulator::evaluateGradient(int iStart, int iEnd)
{
    int nComponent = schema->getNComponent();
    component **co = schema->getComponents();

	int numberOfRules = schema->getNRule();
    rule **hydraulicRules = schema->getRules();

	int nTrigger = schema->getNTrigger();
    trigger **tr = schema->getTriggers();

    // reverse simulation for lambda and gradient
    for (int iStep=iEnd-1; iStep>=iStart-1; iStep--) {

        // t, dt
        long long t = tsMatrix->getTime(iStep);
        double dt = ((double)tsMatrix->getDT(iStep))/1000.0;

        // get vectors with old and new states
        double *stateOld = tsMatrix->getState(iStep);
        double *stateNew = tsMatrix->getState(iStep+1);
		double *dStateOld = ((timeSeriesMatrix*)tsMatrix)->getStateObj(iStep);
        double *dStateNew = ((timeSeriesMatrix*)tsMatrix)->getStateObj(iStep+1);

        // reverse component loop
        for (int i=nComponent-1; i>=0; i--) {
            co[i]->solveDer(stateOld, stateNew, t, dt, dStateOld, dStateNew);
        }

		// triggers in foward order to reproduce rule activation
		for (int i=0; i<nTrigger; i++) {
			tr[i]->deactivate();
		}
		for (int i=0; i<nTrigger; i++) {
			tr[i]->solve(stateOld, stateNew, t, dt);
		}

		// reverse rule loop
		for (int i=numberOfRules-1; i>=0; i--) {
			if (hydraulicRules[i]->isActive()) {
				hydraulicRules[i]->solveDer(stateOld, stateNew, t, dt, dStateOld, dStateNew);
			}
		}
    }
}
