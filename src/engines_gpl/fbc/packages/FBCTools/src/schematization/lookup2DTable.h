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


#pragma once
#ifndef LOOKUP2DTABLE_H
#define LOOKUP2DTABLE_H

#include "components/component.h"
#include "rules/rule.h"
#include "utilities/lookup2DTableConverter.h"

using namespace rtctools::schematization::components;
using namespace rtctools::schematization::rules;
using namespace rtctools::utilities;

namespace rtctools
{
namespace schematization
{

class lookup2DTable : public component, public rule
{
private:
	lookup2DTableConverter *converter;
	int iXIn;
	int iYIn;
	int iZIn;
	int iZOut;

public:
	lookup2DTable(
		string id, string name,
		lookup2DTableConverter *converter,
		int iXIn, int iYIn, int iZIn, int iZOut);
	~lookup2DTable(void) {};

	void solve(double *stateOld, double *stateNew, long long t, double dt);
	void solveDer(double *stateOld, double *stateNew, long long t, double dt, double *dStateOld, double *dStateNew);
};

} // end namespace schematization
} // end namespace rtctools

#endif //LOOKUP2DTABLE_H
