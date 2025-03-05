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

#include <schematization/rules/deadBandValueRule.h>
#include <stdexcept>

using namespace rtctools::schematization::rules;

deadBandValueRule::deadBandValueRule(string id, string name, double threshold, int yIn)
    : rule(id, name)
{
    this->threshold = threshold;
    this->yIn = yIn;
}

deadBandValueRule::~deadBandValueRule(void)
{
}

void deadBandValueRule::solve(double *stateOld, double *stateNew, long long t, double dt)
{
    double yOld = stateOld[yIn];
    double yNew = stateNew[yIn];

    if ((yOld==yOld) & (yNew==yNew)) {
        if (yNew>yOld+threshold) yNew = yOld+threshold;
        if (yNew<yOld-threshold) yNew = yOld-threshold;
    }

    stateNew[yIn] = yNew;
}

void deadBandValueRule::solveDer(double *stateOld, double *stateNew, long long t, double dt, double *dStateOld, double *dStateNew)
{
    throw runtime_error("void deadBandValueRule::solveDer(double *stateOld, double *stateNew, long long t, double dt, double *dStateOld, double *dStateNew) not implemented");
}
