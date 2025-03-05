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

#include <schematization/expression.h>

#include <limits>
#include <piDiagInterface.h>

using namespace rtctools::schematization;


expression::expression(
	string id,
	string name,
	int nVec,
	INPUT x1In,
	OPERATOR op,
	INPUT x2In,
	vector<int> iYOut) : component(id, name), rule(id, name), trigger(id, name, -1, -1, -1)
{
	this->nVec = nVec;
	this->x1In = x1In;
	this->op = op;
	this->x2In = x2In;
	this->iYOut = iYOut;
}

void expression::solve(double *stateOld, double *stateNew, long long t, double dt)
{
	for (int i=0; i<nVec; i++) {
	
		double x1, x2;
		if (x1In.iVal[i]>-1) x1 = stateOld[x1In.iVal[i]]; else x1 = x1In.val;
		if (x2In.iVal[i]>-1) x2 = stateOld[x2In.iVal[i]]; else x2 = x2In.val;

		double yNew = numeric_limits<double>::quiet_NaN();

		if (op==PLUS) 
        {
			yNew = x1+x2;
		} 
	    else if (op==MINUS) 
        {
			yNew = x1-x2;
		} 
	    else if (op==MULTIPLY)
        {
			yNew = x1*x2;
		} 
	    else if (op==DIVIDE) 
        {
			yNew = x1/x2;
		} 
	    else if (op==MIN) 
        {
			if (x1<x2) 
                yNew = x1; 
	        else 
                yNew = x2;
		} 
	    else if (op==MAX) 
        {
			if (x1<x2) 
                yNew = x2; 
	        else 
                yNew = x1;
		}
        else if (op == POWER)
        {
            auto eps = numeric_limits<double>::epsilon();
            const bool isInvalid = x1 < 0.0 || abs(x1) <= eps && abs(x2) <= eps;

            if(isInvalid)
            {
                if (diagnosticMessagesCounter < maxNumDiagnostic)
                {
                    diagnosticMessagesCounter++;
                    piDiagInterface::addLine(1, "expression::solve - warning invalid operands: base x1 " + std::to_string(x1) + " exponent x2 " + std::to_string(x2));
                }
            }
            else
            {
                yNew = pow(x1, x2);
            }
            
        }

		stateNew[iYOut[i]] = yNew;
	}
}

void expression::solveDer(double *stateOld, double *stateNew, long long t, double dt, double *dStateOld, double *dStateNew)
{
	for (int i=0; i<nVec; i++) 
    {

		double x1, x2;
		if (x1In.iVal[i]>-1) x1 = stateOld[x1In.iVal[i]]; else x1 = x1In.val;
		if (x2In.iVal[i]>-1) x2 = stateOld[x2In.iVal[i]]; else x2 = x2In.val;
		double dYNew = dStateNew[iYOut[i]];
		double dX1, dX2;
		if (x1In.iVal[i]>-1) dX1 = dStateOld[x1In.iVal[i]]; else dX1 = 0.0;
		if (x2In.iVal[i]>-1) dX2 = dStateOld[x2In.iVal[i]]; else dX2 = 0.0;

		if (op==PLUS) 
        {
			// yNew = x1+x2;
			dX1 += dYNew;
			dX2 += dYNew;
		} 
	    else if (op==MINUS) 
        {
			// yNew = x1-x2;
			dX1 += dYNew;
			dX2 += -dYNew;
		} 
	    else if (op==MULTIPLY) 
        {
			// yNew = x1*x2;
			dX1 += dYNew * x2;
			dX2 += dYNew * x1;
		} 
	    else if (op==DIVIDE)
        {
			// yNew = x1/x2;
			dX1 += dYNew / x2;
			dX2 += dYNew * (-1)*x1/(x2*x2);
		} 
	    else if (op==MIN)
        {
			// if (x1<x2) yNew = x1; else yNew = x2;
			if (x1<x2) 
                dX1 += dYNew; 
	        else 
                dX2 += dYNew;
		} 
	    else if (op==MAX) 
        {
			// if (x1<x2) yNew = x2; else yNew = x1;
			if (x1<x2) 
                dX2 += dYNew; 
		    else 
                dX1 += dYNew;
		}
        else if (op == POWER)
        {
            auto eps = numeric_limits<double>::epsilon();

            const bool isInvalid = x1 < 0.0 ||
                abs(x1) <= eps && abs(x2) <= eps ||
                abs(x1) <= eps && abs(x2 - 1.0) <= eps;

            if (x1 < 0.0)
            {
                if (diagnosticMessagesCounter < maxNumDiagnostic)
                {
                    piDiagInterface::addLine(1, "expression::solveDer - warning invalid operands: base x1 " + std::to_string(x1));
                    diagnosticMessagesCounter++;
                }
            }
            else if (abs(x1) <= eps && abs(x2) <= eps)
            {
                if (diagnosticMessagesCounter < maxNumDiagnostic)
                {
                    piDiagInterface::addLine(1, "expression::solveDer - warning invalid operands: base x1 " + std::to_string(x1) + " exponent x2 " + std::to_string(x2));
                    diagnosticMessagesCounter++;
                }
            }
            else if (abs(x1) <= eps && abs(x2 - 1.0) <= eps)
            {
                if (diagnosticMessagesCounter < maxNumDiagnostic)
                {
                    piDiagInterface::addLine(1, "expression::solveDer - warning invalid operands: base x1 " + std::to_string(x1) + " exponent x2 - 1.0 " + std::to_string(x2 - 1.0));
                    diagnosticMessagesCounter++;
                }
            }
            else if (!isInvalid)
            {
                dX1 += dYNew * x2 * pow(x1, x2 - 1.0);
                dX2 += dYNew * pow(x1, x2) * log(x1);
            }
        }

		if (x1In.iVal[i]>-1) dStateOld[x1In.iVal[i]] = dX1;
		if (x2In.iVal[i]>-1) dStateOld[x2In.iVal[i]] = dX2;
	}
}
