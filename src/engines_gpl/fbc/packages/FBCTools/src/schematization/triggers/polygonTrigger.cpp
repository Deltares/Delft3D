// Copyright (C) 2010 Deltares
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

#include <schematization/triggers/polygonTrigger.h>

#include <limits>

using namespace rtctools::schematization::triggers;

polygonTrigger::polygonTrigger(string id, string name,
    int iX1In, int iX2In, int iYOut, vector<polygon> polygons, 
    double yDefaultValue, int iTimeTrueOut, int iTimeFalseOut) 
    : trigger(id, name, iYOut, iTimeTrueOut, iTimeFalseOut), iX1In(iX1In), iX2In(iX2In), 
    polygons(polygons), yDefaultValue(yDefaultValue)
{ }

void polygonTrigger::solve(double *stateOld, double *stateNew, long long t, double dt)
{
    double x1 = stateOld[iX1In];
    double x2 = stateOld[iX2In];

    // evaluate new trigger status
    stateNew[iYOut] = yDefaultValue;
    if (x1==x1 && x2==x2) 
    {
        for (int i=0; i<(int)polygons.size(); i++) 
        {
            if (polygons[i].contains(x1, x2)) 
            {
                stateNew[iYOut] = polygons[i].getValue();
                break;
            }  
        }
    }

    // evaluate sub-triggers
    evaluateSubtriggers(stateOld, stateNew, t, dt);
}
