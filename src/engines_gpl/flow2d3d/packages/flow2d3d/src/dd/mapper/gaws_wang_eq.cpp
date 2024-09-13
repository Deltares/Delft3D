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
// $Id: gaws_wang_eq.cpp 878 2011-10-07 12:58:46Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/mapper/gaws_wang_eq.cpp $
//------------------------------------------------------------------------------
// Class: WangEquations
// Store/Solve Equations for ADI-Wang Solver
//
//  Stef.Hummel@deltares.nl
//  Erik.deGoede@deltares.nl
//  1 jun 11
//-------------------------------------------------------------------------------


#include "flow2d3d.h"


#define LOG_EQUATIONS   0
#define LOG_ADRESS  0
#define LOG_SOLVE   0
#define LOG_ELIMINATE   0

#if(LOG_EQUATIONS)
#define ON_LOG_EQUATIONS(code)   code
#else
#define ON_LOG_EQUATIONS(code)
#endif

#if(LOG_ADRESS)
#define ON_LOG_ADRESS(code)    code
#else
#define ON_LOG_ADRESS(code)
#endif

#if(LOG_SOLVE)
#define ON_LOG_SOLVE(code)   code
#else
#define ON_LOG_SOLVE(code)
#endif

#if(LOG_ELIMINATE)
#define ON_LOG_ELIMINATE(code)   code
#else
#define ON_LOG_ELIMINATE(code)
#endif




//////////////////////////////////////////////////////////////////////
//
// WangEquations
//
//


//////////////////////
// Public functions
//


WangEquations::WangEquations(void)
{
    //
    // Add spaces
    //

    this->maxEq     = NEW_EQ_BLOCK;
    this->numEq     = 0;
    this->equations = ( EquationLine * ) malloc(
            this->maxEq * sizeof(EquationLine) );
}


void WangEquations::AddEquation(
    bool            l2r,        // equation in L2R or B2T line?
    int             curM,       // M-index of equation
    int             curN,       // N-index of equation
    int             coupM,      // M-index of next or previous
                                // equation (is a coupling point)
    int             coupN,      // N-index of next or previous
                                // equation (is a coupling point)
    D3dFlowContextGawsSide        * cntxt,      // Pointer to context containing eq.
    bool            BeginOfLine // Flag for begin/end of column or row
    )
{
    int  numEqP;             // numEq -1

    if ( ! BeginOfLine )
    {
        //
        // Build equation at END of column/row
        //

        //
        // Check for enough space
        //
        if ( this->numEq  >= this->maxEq )
        {
            AddEquationSpace();
        }

        this->equations [ numEq ] . A = cntxt->Get_Adress(l2r, curM, curN, GawsTerm_A);
        this->equations [ numEq ] . B = cntxt->Get_Adress(l2r, curM, curN, GawsTerm_B);
        this->equations [ numEq ] . C = cntxt->Get_Adress(l2r, curM, curN, GawsTerm_C);
        this->equations [ numEq ] . D = cntxt->Get_Adress(l2r, curM, curN, GawsTerm_D);
        this->equations [ numEq ] . Dnext =
                cntxt->Get_Adress(l2r, coupM, coupN, GawsTerm_D);

        this->equations [ numEq ] . endOfRow = true;

        ON_LOG_EQUATIONS(
            // printf("ADD-EQ-END(%3d): %6d(%3d,%3d) %10.4f %10.4f %10.4f %10.4f\n",
            FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "ADD-EQ-END(%3d): %6d(%3d,%3d) %10.4f %10.4f %10.4f %10.4f",
                numEq,
                cntxt->GetContextId(),
                curM, curN,
                *(this->equations[numEq].A),
                *(this->equations[numEq].B),
                *(this->equations[numEq].C),
                *(this->equations[numEq].D) );
        )

        numEq++;
    }

    else

    {

        //
        // Build equation at BEGIN of column/row
        //

        numEqP = numEq - 1;
        this->equations [ numEqP] . E = cntxt->Get_Adress(l2r, curM, curN, GawsTerm_A);
        this->equations [ numEqP] . F = cntxt->Get_Adress(l2r, curM, curN, GawsTerm_B);
        this->equations [ numEqP] . G = cntxt->Get_Adress(l2r, curM, curN, GawsTerm_C);
        this->equations [ numEqP] . H = cntxt->Get_Adress(l2r, curM, curN, GawsTerm_D);
        this->equations [ numEqP] . Hprev =
                cntxt->Get_Adress(l2r, coupM, coupN, GawsTerm_D);

        this->equations [ numEqP] . endOfRow    = false;

        ON_LOG_EQUATIONS(
            // printf("ADD-EQ-BEGIN(%3d): %6d(%3d,%3d) %10.4f %10.4f %10.4f %10.4f\n",
            FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "ADD-EQ-BEGIN(%3d): %6d(%3d,%3d) %10.4f %10.4f %10.4f %10.4f",
                numEqP,
                cntxt->GetContextId(),
                curM, curN,
                *(this->equations[numEqP].E),
                *(this->equations[numEqP].F),
                *(this->equations[numEqP].G),
                *(this->equations[numEqP].H) );
        )

    }
}


void WangEquations::PreEliminate(
)
{
    int             n;      // Loop index of number of equations
    FlowScalar          Factor;     // auxilary variable
    FlowScalar        * B;      // Pointer to diagonal B
    FlowScalar        * C;      // Pointer to diagonal C
    FlowScalar        * D;      // Pointer to diagonal D
    FlowScalar        * E;      // Pointer to diagonal E
    FlowScalar        * F;      // Pointer to diagonal F
    FlowScalar        * G;      // Pointer to diagonal G
    FlowScalar        * H;      // Pointer to diagonal H


    // printf("start pre_eliminate\n");
    // fflush(stdout);

#if LOG_ELIMINATE
    FlowScalar        * A;      // Pointer to diagonal A voor print

    for ( n = 0 ; n < this->numEq ; n++ )
    {
        A = this->equations[n].A;
        B = this->equations[n].B;
        C = this->equations[n].C;
        D = this->equations[n].D;

    printf("       stelsel: (%6d) %11.4f %10.4f %10.4f %10.4f\n", n,
                       *A,*B,*C,*D);
#if LOG_ADRESS
        printf("       address: (%6d) %x %x %x %x\n", n, A,B,C,D);
#endif
        fflush(stdout);

        if ( ! this->equations[n].endOfRow )
        {
            E = this->equations[n].E;
            F = this->equations[n].F;
            G = this->equations[n].G;
            H = this->equations[n].H;

            printf("andere stelsel: (%6d) %11.4f %10.4f %10.4f %10.4f\n",
                     n, *E,*F,*G,*H);
#if LOG_ADRESS
        printf("andere address: (%6d) %x %x %x %x\n", n, E,F,G,H);
#endif
        fflush(stdout);
        }
    }
#endif

    for ( n = 0 ; n < this->numEq-1 ; n++ )
    {

        if ( ! this->equations[n].endOfRow )
        {
            B = this->equations[n].B;
            C = this->equations[n].C;
            D = this->equations[n].D;
            E = this->equations[n].E;
            F = this->equations[n].F;
            G = this->equations[n].G;
            H = this->equations[n].H;

            Factor = *C / *F;
            *B = *B - Factor * (*E);
            *C = -Factor * (*G);
            *D = *D - Factor * (*H);
#if LOG_ELIMINATE
            A = this->equations[n].A; // used in print statement
            printf("eind pre: %6d %10.4f %10.4f %10.4f %10.4f\n", n, *A,*B,*C,*D );
            fflush(stdout);
#endif
        }

    }

}


void WangEquations::TridiagSolve(
)
{
    int             n;      // Loop index of number of equations
    FlowScalar          Factor;     // auxilary variable
    FlowScalar        * A;      // Pointer to diagonal A[i]
    FlowScalar        * B;      // Pointer to diagonal B[i]
    FlowScalar        * Bmin;       // Pointer to diagonal B[i-1]
    FlowScalar        * Bplus;      // Pointer to diagonal B[i+1]
    FlowScalar        * C;      // Pointer to diagonal C[i]
    FlowScalar        * Cmin;       // Pointer to diagonal C[i-1]
    FlowScalar        * Dmin;       // Pointer to diagonal D[i-1]
    FlowScalar        * D;      // Pointer to diagonal D[i]
    FlowScalar        * D2;     // Pointer to diagonal D[i]
    FlowScalar        * Dplus;      // Pointer to diagonal D[i+1]
    FlowScalar        * E;      // Pointer to diagonal E[i]
    FlowScalar        * F;      // Pointer to diagonal F[i]
    FlowScalar        * G;      // Pointer to diagonal G[i]
    FlowScalar        * H;      // Pointer to diagonal H[i]


    // Forward elimination

#if LOG_SOLVE
    printf("start tri_diagsolve\n");
    fflush(stdout);
#endif

    for ( n = 1 ; n < this->numEq ; n++ )
    {
        A = this->equations[n].A;
        B = this->equations[n].B;
        Bmin = this->equations[n-1].B;
        Cmin = this->equations[n-1].C;
        Dmin = this->equations[n-1].D;
        D = this->equations[n].D;
        C = this->equations[n].C;   // voor print

        Factor = *A / *Bmin;
        *A = 0.;
        *B = *B - Factor * (*Cmin);
        *D = *D - Factor * (*Dmin);
    }

    // Backward elimination

    for ( n = this->numEq-2 ; n >= 0 ; n-- )
    {
        Bplus = this->equations[n+1].B;
        C = this->equations[n].C;
        D = this->equations[n].D;
        Dplus = this->equations[n+1].D;

        Factor = *C / *Bplus;
        *C = 0.;
        *D = *D - Factor * (*Dplus);
    }

    for ( n = 0 ; n < this->numEq ; n++ )
    {
        B = this->equations[n].B;
        D = this->equations[n].D;

        *D = *D / *B;
        *B = 1.;

#if LOG_SOLVE
        FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "solution: (%6d)) %10.5e %10.5e %10.5e %10.5e", n, *A,*B,*C,*D );
#endif
    }

    // Compute solution other subdomain

    for ( n = 0 ; n < this->numEq-1 ; n++ )
    {
        if ( ! this->equations[n].endOfRow )
        {
            D = this->equations[n].D;
            D2 =this->equations[n+1].D;
            E = this->equations[n].E;
            F = this->equations[n].F;
            G = this->equations[n].G;
            H = this->equations[n].H;

            *H = ( *H - (*E) * (*D) - (*G) * ( *D2) ) / *F;
            *E = 0.;
            *F = 1.;
            *G = 0.;

#if LOG_SOLVE
            FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "solution other domain: (%6d) %10.5e", n,*H );
#endif
        }
    }


#if 0 // TODO: Remove (and remove required administration, check with EdG).
    //
    // Copy the coarse line equations to its dependent fine lines
    // Remark: Has been removed, because it doesn't work.
    //         The required administration however, still exists.
    //
    for ( n = 0 ; n < this->numEq ; n++ )
    {
        int d;              // dependency countres

        int numDep = this->equations[n].numABCDdeps;

        for ( d = 0  ; d < numDep ; d++ )
        {
            *(this->equations[n].Adeps[d]) =  0.;
            *(this->equations[n].Bdeps[d]) =  1.;
            *(this->equations[n].Cdeps[d]) =  0.;
            *(this->equations[n].Ddeps[d]) =  *(this->equations[n].D);
            printf("SetRef ABCD (%d) (%10.5f)\n", d, *(this->equations[n].D));
        }

        numDep = this->equations[n].numEFGHdeps;

        for ( d = 0  ; d < numDep ; d++ )
        {
            *(this->equations[n].Edeps[d]) =  0.;
            *(this->equations[n].Fdeps[d]) =  1.;
            *(this->equations[n].Gdeps[d]) =  0.;
            *(this->equations[n].Hdeps[d]) =  *(this->equations[n].H);
            printf("SetRef EFGH (%d) (%10.5f)\n", d, *(this->equations[n].H));
        }
    }
#endif

}


void WangEquations::CopyTridiagSol(
)
{
    int             n;      // Loop index of number of equations
    FlowScalar        * D;      // Pointer to diagonal D[i]
    FlowScalar        * Dnext;      // Pointer to diagonal D[i]
    FlowScalar        * H;      // Pointer to diagonal H[i]
    FlowScalar        * Hprev;      // Pointer to diagonal H[i]


    // Copy solution to coupling points at other domains

    for ( n = 0 ; n < this->numEq ; n++ )
    {
        if ( ! this->equations[n].endOfRow )
        {
            D = this->equations[n].D;
            H = this->equations[n].H;
            Hprev = this->equations[n].Hprev;

            *Hprev = *D;

#if LOG_SOLVE
            // printf("at end other domain: (%6d) %10.4f\n", n,*Hprev );
            // printf("dx left and right : %10.4fd %10.4f\n",
            // DxDyLast,DxDyFirst );
            FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "solution couple pt. left dom.: %10.5e %10.5e",*D,*H );
#endif
        }
    }


    for ( n = 0 ; n < this->numEq ; n++ )
    {
        if ( ! this->equations[n].endOfRow )
        {
            H = this->equations[n].H;
            D = this->equations[n].D;
            Dnext = this->equations[n].Dnext;

            *Dnext = *H;

#if LOG_SOLVE
            // printf("at begin other domain: (%6d) %10.4f\n", n,*Hprev );
            // printf("dx left and right : %10.4fd %10.4f\n",
            // DxDyLast,DxDyFirst );
            FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "solution couple pt. right dom.: %10.5e %10.5e",*D,*H);
#endif
        }
    }
}


WangEquations::~WangEquations(void)
{
    //
    // Clean up
    //

    if ( this->equations != NULL )
    {
        free(this->equations);
    }
}


//////////////////////
// Protected functions
//


void WangEquations::AddEquationSpace(void)
{
    EquationLine      * newEquations;

    this->maxEq += NEW_EQ_BLOCK;

    newEquations = ( EquationLine * ) realloc(this->equations,
            this->maxEq*sizeof(EquationLine) );

    if ( newEquations == NULL )
    {
                printf("ABORT: Unable to realloc in WangEquations::AddEquationSpace\n");
                throw new Exception (true, "Unable to realloc in WangEquations::AddEquationSpace\n");
        }

    this->equations = newEquations;

}
