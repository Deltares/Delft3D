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

#include <schematization/triggers/ruleReferenceTrigger.h>

using namespace rtctools::schematization::triggers;

ruleReferenceTrigger::ruleReferenceTrigger(string id, string name, rule *r) : trigger(id, name, -1, -1, -1)
{
	// reuse the first true component for reference to rule
	this->nTrueComponent = 1;
	this->trueComponent = new element*[1];
	this->trueComponent[0] = r;
}

ruleReferenceTrigger::~ruleReferenceTrigger(void)
{
}

void ruleReferenceTrigger::solve(double *stateOld, double *stateNew, long long t, double dt)
{
	// activate rule, if trigger is called
	this->trueComponent[0]->activate();
}
