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

#ifndef EXPRESSION_H
#define EXPRESSION_H

#include <vector>
#include "schematization/triggers/trigger.h"
#include "schematization/rules/rule.h"
#include "schematization/components/component.h"

using namespace rtctools::schematization::components;
using namespace rtctools::schematization::rules;
using namespace rtctools::schematization::triggers;

namespace rtctools
{
namespace schematization
{

/**
  * @brief Mathematical expression
  *
  * The class combines two input values (either constant or from a time series)
  * and a mathematical operator (+,-,*,/,min,max).
  */
class expression : public trigger, public rule, public component
{
public:
	/**
	  * @brief Mathematical operator
	  */
	enum OPERATOR{PLUS, MINUS, MULTIPLY, DIVIDE, MIN, MAX, POWER};

	/**
	  * @brief Struct of input variable, either value or time series reference
      */
	struct INPUT {
		/**
		  * @brief Constant value
		  */
		double val;
		/**
		  * @brief Vector with time series indices
		  */
		vector<int> iVal;
	};

private:
	/**
	  * @brief Array length of input vector
	  */
	int nVec;
	/**
	  * @brief First input
	  */
	INPUT x1In;
	/**
	  * @brief Mathematical operator
	  */
	OPERATOR op;
	/**
	  * @brief Second input
	  */
	INPUT x2In;
	/**
	  * @brief Vector with output indices
	  */
	vector<int> iYOut;

    /**
    * @brief the number of messages 
    */
    int diagnosticMessagesCounter = 0;

    /**
    * @brief maximum number of messages
    */
    int maxNumDiagnostic = 3;

public:
	/**
	  * @brief Constructor
	  *
	  * @param id		Identifyer
	  * @param name		Name
	  * @param nVec		Array length of input and output vectors
	  * @param x1		First input indices
	  * @param op		Mathematical operator
	  * @param x2		Second input indices
	  * @param iYOut	Output indices
	  */
	expression(string id, string name, 
		        int nVec, INPUT x1, OPERATOR op, INPUT x2, vector<int> iYOut);
	/**
	  * @brief Destructor
	  */
	~expression(void) {};

	void solve(double *stateOld, double *stateNew, long long t, double dt);
    void solveDer(double *stateOld, double *stateNew, long long t, double dt, double *objOld, double *objNew);
};

} // end namespace schematization
} // end namespace rtctools

#endif /* EXPRESSION_H */
