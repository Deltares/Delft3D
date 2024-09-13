//---- GPL ---------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2016.
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation version 3.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
// contact: delft3d.support@deltares.nl
// Stichting Deltares
// P.O. Box 177
// 2600 MH Delft, The Netherlands
//
// All indications and logos of, and references to, "Delft3D" and "Deltares"
// are registered trademarks of Stichting Deltares, and remain the property of
// Stichting Deltares. All rights reserved.
//
//------------------------------------------------------------------------------
// $Id: gaws_wang_eq.h 878 2011-10-07 12:58:46Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/mapper/gaws_wang_eq.h $
//------------------------------------------------------------------------------
//  Class for Wang Equations
//
//  Stef.Hummel@Deltares.NL
//  30 may 11
//------------------------------------------------------------------------------


#pragma once


#include "context-gawsside.h"       // Delft3d-Flow context object


//
// Block size for new equations
//
#define NEW_EQ_BLOCK    500
#define MAX_DEP     5

//
// Structure for a line of Wang Equations
//
// the equation at the end of each column/row that represents a subdomain
// interface, is connected to the equation corresponding to the begin
// of a column/row in the next subdomain.

typedef struct STR_EquationLine {
    FlowScalar *A;          // Ref to A-term of equation at end of line
    FlowScalar *B;          // Ref to B-  "   "     "     "     "
    FlowScalar *C;          // Ref to C-  "   "     "     "     "
    FlowScalar *D;          // Ref to D-  "   "     "     "     "
    FlowScalar *Dnext;      // Ref to D-  "   " next equation
    bool        endOfRow;   // boolean: end of a multidomainRow
    FlowScalar *E;          // Ref to A-term of equation at begin of line
    FlowScalar *F;          // Ref to B-  "   "     "     "     "
    FlowScalar *G;          // Ref to C-  "   "     "     "     "
    FlowScalar *H;          // Ref to D-  "   "     "     "     "
    FlowScalar *Hprev;      // Ref to D-  "   " previous equation

} EquationLine;


//
// WangEquations Class
//

class WangEquations
{
    public:

    ////////////////////////
    //
    // PUBLIC FUNCTIONS
    //

    WangEquations(void);

    void AddEquation(
        bool            l2r,        // Left->right or bottom-top line?
        int             curM,       // M-index of equation
        int             curN,       // N-index of equation
        int             coupM,      // M-index of next or previous
                                    // equation (is a coupling point)
        int             coupN,      // N-index of next or previous
                                    // equation (is a coupling point)
        D3dFlowContextGawsSide *cntxt,      // Pointer to context containing eq.
        bool            BeginOfLine // Flag for begin/end of column/row
        );

    void PreEliminate(void);

    void TridiagSolve(void);

    void CopyTridiagSol(void);

    virtual ~WangEquations(void);

    protected:

    /////////////////////
    //
    // PROTECTED FUNCTIONS
    //

    void AddEquationSpace(void);

    /////////////////////
    //
    // PROTECTED DATA
    //

    int     maxEq;      // current max.#eq. that can be stored
    int     numEq;      // current #eq. that are be stored

    EquationLine  * equations;  // set of equations

};
