//---- GPL ---------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2024.
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
// $Id: maploops.h 878 2011-10-07 12:58:46Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/mapper/maploops.h $
//------------------------------------------------------------------------------
//  Mapper for DELFT3D-FLOW Domain Decomposition
//  Loop Macro's for 1:n coupling (N = EVEN OR ODD); In november 2002
//  implementation has been changed for 1-to-even coupling
//
//  Stef.Hummel@Deltares.NL
//  Erik.deGoede@deltares.nl
//  30 may 11
//-------------------------------------------------------------------------------


#pragma once


#include "map_debug.h"



//
// This source contains macros that are used for the coupling
// of the Normal velocity, the Tangential velocity, and concentrations.
//
// The assume the existence and correct setting of the following var.s,
// which are indeed available in each mapper (see 3df_mapper.h):
//
// CONTEXT INDICES and REFINEMENT FACTORS:
//
// int ctx             current context (0 or 1)
// int oCtx            other context (1 - ctx, i.e. 1 or 0)
//
// int Ref[NR_CNTXTS]  Refinement factor R for each context:
//                     R=1: context not refined, so it is eather a 1:1
//                          coupling, or the context is the coarse one.
//                     R>1: context is refined with factor R compared
//                          to the other context.
//
// START / END CELLS of MAPPER LOOPS, and DIRECTION FLAGS
//
// Start/End cells in M and N direction for the current context
// (the 'target') for each 'equation-type' (for U, V, and constituents,
// which are stored in Zeta-points), and the related Start/End cells
// in M and N direction for the other context (the source):
//
// int mStart    / nStart    / mEnd    / nEnd    [NR_CNTXTS][NR_EQ]
// int mOthStart / nOthStart / mOthEnd / nOthEnd [NR_CNTXTS][NR_EQ]
//
// int l2r            Boolean flag indicating it is a mapper from left
//                    to right.
// int b2t            Boolean flag indicating it is a mapper from bottom
//                    to top.
//
// LOOP COUNTERS
//
// int  m,n,k         loop counters current Cntxt
// int  oM,oN,oK      loop counters other Cntxt
// int  l             loop counter constituents
//
//int   fM,fN,fK      loop counters for refined (hor. / vert.) loops
//int   nHorRef       number of M or N points in a horizontally refined loop
//
//
//
//
// Overview of coupling Constituents points (i.e. Zeta points) and
// Normal velocity points.
//
//
//        o . . ! . . O   14  |        |  6    O . . . . ! . . . . o
//                    :                        :
//        --    x    ---  14  X                :
//                    :         \              :
//        o . . ! . . O   13  |  \             :
//                    :           \            :
//        --    x    ---  13  X -----  X  6  -----       x       ---
//                    :           /            :
//        o . . ! . . O   12  |  /             :
//                    :         /              :
//        --    x    ---  12  X                :
//                    :                        :
//        o . . ! . . O   11  |        |  5    O . . . . ! . . . . o
//                    :                        :
//        --    x    ---  11  X                :
//                    :         \              :
//        o . . ! . . O   10  |  \             :
//                    :           \            :
//        --    x    ---  10  X -----  X  5  -----       x       ---
//                    :           /            :
//        o . . ! . . O   9   |  /             :
//                    :         /              :
//        --    x    ---  9   X                :
//                    :                        :
//        o . . ! . . O   8   |        |  4    O . . . . ! . . . . o
//                    :                        :
//        --    x    ---  8   X                :
//                    :         \              :
//        o . . ! . . O   7   |  \             :
//                    :           \            :
//        --    x    ---  7   X -----  X  4  -----       x       ---
//                    :           /            :
//        o . . ! . . O   6   |  /             :
//                    :         /              :
//        --    x    ---  6   X                :
//                    :                        :
//        o . . ! . . O   5   |        |  3    O . . . . ! . . . . o
//                    :                        :
//        --    x    ---  5   X                :
//                    :         \              :
//        o . . ! . . O   4   |  \             :
//                    :           \            :
//        --    x    ---  4   X -----  X  3  -----       x       ---
//                    :           /            :
//        o . . ! . . O   3   |  /             :
//                    :         /              :
//        --    x    ---  3   X                :
//                    :                        :
//        o . . ! . . O   2   |        |  2    O . . . . ! . . . . o
//
//
//
//
// Overview of coupling Tangential velocity points.
//
//
//        o . . ! . . O   14  |        |  6    O . . . . ! . . . . o
//                    :                        :
//        --    x    ---  14  X                :
//                    :                        :
//        o . . ! . . O   13  |                :
//                    :         \              :
//        --    x    ---  13  X  \     X  6  -----       x       ---
//                    :           \            :
//        o . . ! . . O   12  |    \           :
//                    :         \   \          :
//        --    x    ---  12  X  \   \         :
//                    :           \   \        :
//        o . . ! . . O   11  | ------ |  5    O . . . . ! . . . . o
//                    :           /            :
//        --    x    ---  11  X  /             :
//                    :         /              :
//        o . . ! . . O   10  |                :
//                    :                        :
//        --    x    ---  10  X        X  5  -----       x       ---
//                    :                        :
//        o . . ! . . O   9   |                :
//                    :         \              :
//        --    x    ---  9   X  \             :
//                    :           \            :
//        o . . ! . . O   8   | ------ |  4    O . . . . ! . . . . o
//                    :           /            :
//        --    x    ---  8   X  /             :
//                    :         /              :
//        o . . ! . . O   7   |                :
//                    :                        :
//        --    x    ---  7   X        X  4  -----       x       ---
//                    :                        :
//        o . . ! . . O   6   |                :
//                    :         \              :
//        --    x    ---  6   X  \             :
//                    :           \            :
//        o . . ! . . O   5   | ------ |  3    O . . . . ! . . . . o
//                    :           /   /        :
//        --    x    ---  5   X  /   /         :
//                    :         /   /          :
//        o . . ! . . O   4   |    /           :
//                    :           /            :
//        --    x    ---  4   X  /     X  3  -----       x       ---
//                    :         /              :
//        o . . ! . . O   3   |                :
//                    :                        :
//        --    x    ---  3   X                :
//                    :                        :
//        o . . ! . . O   2   |        |  2    O . . . . ! . . . . o
//
//
//
//
//
// Loop-Debug Macro's:
//
// In the mapper-loops, FMapLog-statements are inserted before the
// increments take place. To activate these statements, set do-loop-debug
// to 1. De de-activate the statements, set do-loop-debug to 0.
//
//


#define DO_LOOP_DEBUG 0

#if DO_LOOP_DEBUG

#define  DBCELLS(txt,e,oc,om,on,cc,cm,cn)  FMapLog(txt,e,oc,om,on,cc,cm,cn),
#define  DBLAYER(txt,oc,ok,cc,ck)          FMapLog(txt,txt,oc,ok,cc,ck),
#define  DBREFINE(txt,cc,cm,cn)            FMapLog(txt,cc,cm,cn),

#else

#define  DBCELLS(txt,e,oc,om,on,cc,cm,cn)
#define  DBLAYER(txt,oc,ok,cc,ck)
#define  DBREFINE(txt,cc,cm,cn)

#endif


//
//
// HELP MACRO's for horizontal loop macro's
//
//
// The macro 'start-offset' determines the offset from m/n-Start
// for the actual start of the m/n-loop.
//
// If 'type' (which is filled in by the actual loop macro's as 'l2r' or
// 'b2t') is True, we loop in tangential direction (if it's False, we
// loop in normal direction, so the loop length is 1, and the offset 0).
// When looping in tangential direction, the actual start of the loop
// depends on the refinement of the context, and on the type of equation
// currently handled.
//
// If the refinement factor for the current context is 1 (i.e. no refinement
// for this context), the offset is 0 (the loop starts at its real m/n-Start).
// If the refinement factor for the current context is greater 1 (i.e. it is
// the refined context), the offset depends on handling the Tangential
// equation or not (see example in the 'picture' above for a 1:3 refinement).
// This eq-type condition is contained in 'tang', which is filled in by the
// actual loop macro's as 'eq==Eq_V' for the N-direction (l2r) loops or as
// 'eq==Eq_U' for the M-direction (b2t) loops.
//
//
// The macro 'other-index' determines the loop-counter in the other context,
// that's related to the loop counter in the current context. If we're
// looping in normal direction, simply increment the counter (the loop
// will terminate then). Otherwise the algorithm is:
//
// If (the current context is the fine one)
//     Find the index of the coarse neighbor cell:
//     For Normal velocity and Zeta points this is the current index devided
//     by the refinement factor (and of coarse shifted with the m/n-Start
//     in the other context minus the m/n-Start in the current context).
//     For the Tangential velocity the fine index first has to be shifted
//     with half of the refinement factor, because of the staggering.
//     For the upper last fine cell, the result may go above the real last
//     coarse cell, so it is truncated to this cell by means of the min-macro.
// Else if (the other context is the fine one)
//     Find the index of the find neighbor cell that's in the centre of the
//     current coarse cell:
//     For Normal velocity and Zeta points this is the current index multiplied
//     with the refinement factor (and of coarse shifted with the m/n-Start
//     in the other context minus the m/n-Start in the current context).
//     For the Tangential velocity this computed findex has to be shifted
//     with half of the refinement factor, because of the staggering.
// Else    -- i.e. no refinement at all --
//     Find the index of the neighbor cell:
//     Simply increment the counter in the other context.
// Endif
//


//
// In November 2002 the macro's have been changed for a 1-to-even coupling.
//
//
//           9                6    (grid enclosure)
//           8                5
//           7 **
//           6                4
//           5 **
//           4                3
//           3 **
//           2                2    (grid enclosure)
//
//  This is an example of an 1-to-2 refinement.
//  The grid cells with "**" indicate the central grid cell in the fine domain.
//  Thus, both for the u- and the v-velocity the same copling is used: e.g.
//  cells 5 and 6 in the fine domain are coupled to cell 4 in the coarse domain.
//  For the v-velocity this yields a skew (asymmetric) coupling.
//  For the u-velocity thecoupling is symmetric (comparable to 1-to-odd).
//
//  The 1-to-even coupling has been implemented by adjusting the
//  MAP_REFINED_LOOP (including START_OFFSET and OTHER_INDEX)
//  Furthermore, a few changes have been made in the WANG administration:
//  see gaws.C and wang_eq.C. The "numerical" Mapper routines
//  (e.g. 3df_uvz.C) have not been changed.
//
// The 1-to-even implementation is such that the 1-to-odd coupling
// has not been changed.
//
//

#define  START_OFFSET(type,cntxt,tang)                                    \
       ( type ? ( ((tang)) ? Ref[cntxt]-1 : (Ref[cntxt]-1)/2 )            \
              : 0                                                         \
       )

#define  OTHER_INDEX(type,ind,oInd,start,oStart,oEnd,e,tang)              \
                                                                          \
( type ? ( (Ref[ctx]>1) ? B_MIN(  oStart[ctx][e]                          \
                  + (ind - start[ctx][e]                  \
                     - ( ((tang)) ? (Ref[ctx]-1)/2    \
                              : 0                 \
                       )                              \
                    ) / Ref[ctx]                          \
                   ,                                          \
                  ( ((tang)) ? oEnd[ctx][eq]-1 : oEnd[ctx][eq] )  \
                   )                                          \
               : (Ref[oCtx]>1) ? oStart[ctx][e]                   \
                         + (ind-start[ctx][e])*Ref[oCtx]  \
                         + ( ((tang)) ? Ref[oCtx]-1       \
                              : (Ref[oCtx]-1)/2   \
                           )                              \
                       : oInd + 1                         \
         )                                                                \
 : oInd + 1                                                               \
)


//
//
// HORIZONTAL LOOP MACRO's
//
//
// The macro 'map-cells-loop' is used for Build and Adjust.
// It loops over all cells in the current context, starting at the m/n-start
// in that context, and in the other context at m/n-start with an offset
// depending on the refinement factor and the type of equation, as described
// above. The related fine centre cell is computed by the 'other-index' macro.
//
// The macro 'map-coarse-cells-loop' is used for CheckConvergence.
// It loops over all cells in a coarse context, but in the fine context
// it only loops over the cells that are an 'opposite centre' to a coarse
// cell in the other context. This related coarse cell is computed by the
// 'other-index' macro.
//



#define  MAP_CELLS_LOOP(ctx,eq)                                             \
                                                                            \
for (                                                                       \
                                                                            \
 m =     mStart[ctx][eq],                                                   \
                                                                            \
oM =     mOthStart[ctx][eq] + START_OFFSET(b2t,oCtx,eq==Eq_U);              \
                                                                            \
                                                                            \
 m <= mEnd[ctx][eq] ;                                                       \
                                                                            \
                                                                            \
 m ++ ,                                                                     \
                                                                            \
oM  = OTHER_INDEX(b2t,m,oM,mStart,mOthStart,mOthEnd,eq,eq==Eq_U)            \
                                                                            \
)                                                                           \
                                                                            \
for (                                                                       \
                                                                            \
 n =     nStart[ctx][eq],                                                   \
                                                                            \
oN =     nOthStart[ctx][eq] + START_OFFSET(l2r,oCtx,eq==Eq_V);              \
                                                                            \
                                                                            \
 n <= nEnd[ctx][eq] ;                                                       \
                                                                            \
                                                                            \
 DBCELLS("\t\tCellLoop(%d) did C%d(%3d,%3d) -> C%d(%3d,%3d)\n",             \
                                       eq, oCtx, oM, oN, ctx, m, n )        \
                                                                            \
 n ++ ,                                                                     \
                                                                            \
oN  = OTHER_INDEX(l2r,n,oN,nStart,nOthStart,nOthEnd,eq,eq==Eq_V)            \
                                                                            \
)



#define  MAP_COARSE_CELLS_LOOP(ctx,eq)                                      \
                                                                            \
for (                                                                       \
                                                                            \
 m = mStart   [ctx][eq] + START_OFFSET(b2t,ctx, eq==Eq_U),                  \
                                                                            \
oM = mOthStart[ctx][eq] + START_OFFSET(b2t,oCtx,eq==Eq_U);                  \
                                                                            \
                                                                            \
 m <= mEnd[ctx][eq] ;                                                       \
                                                                            \
                                                                            \
 m += b2t ? Ref[ctx] : 1 ,                                                  \
                                                                            \
oM  = OTHER_INDEX(b2t,m,oM,mStart,mOthStart,mOthEnd,eq,eq==Eq_U)            \
                                                                            \
)                                                                           \
                                                                            \
for (                                                                       \
                                                                            \
 n = nStart   [ctx][eq] + START_OFFSET(l2r,ctx, eq==Eq_V),                  \
                                                                            \
oN = nOthStart[ctx][eq] + START_OFFSET(l2r,oCtx,eq==Eq_V);                  \
                                                                            \
                                                                            \
 n <= nEnd[ctx][eq] ;                                                       \
                                                                            \
                                                                            \
 DBCELLS("\t\tCrseLoop(%d) did C%d(%3d,%3d) -> C%d(%3d,%3d)\n",             \
                                       eq, oCtx, oM, oN, ctx, m, n )        \
                                                                            \
 n += l2r ? Ref[ctx] : 1 ,                                                  \
                                                                            \
oN  = OTHER_INDEX(l2r,n,oN,nStart,nOthStart,nOthEnd,eq,eq==Eq_V)            \
)

//
//
// VERTICAL LOOP MACRO's
//
//
// Loop over K-layers
// - all layers
// - in a fine context only the layers that are 'opposite' to a coarse layer
//   (used for checking convergence)
//

#define MAP_LAYERS_LOOP(ctx)                            \
    for ( k = 1 , oK = 1;                           \
            k <= C[ctx]->kMax ;                     \
            DBLAYER("\t\t\tLayer did C%d(%2d) -> C%d(%2d)\n",   \
                             oCtx, oK, ctx, k)      \
            k++, oK = (1+(k-1)*C[oCtx]->kMax/C[ctx]->kMax) )

#define MAP_COARSE_LAYERS_LOOP(ctx)                     \
    for ( k = 1 , oK = 1;                           \
            k <= C[ctx]->kMax ;                     \
            DBLAYER("\t\t\tCrsLayer did C%d(%2d) -> C%d(%2d)\n",\
                             oCtx, oK, ctx, k)      \
            k+=B_MAX(1,(C[ctx]->kMax/C[oCtx]->kMax) ),      \
            oK = (1+(k-1)*C[oCtx]->kMax/C[ctx]->kMax) )

#define MAP_CELLS_AND_LAYERS_LOOP(ctx,eq)               \
    MAP_CELLS_LOOP(ctx,eq)                      \
    MAP_LAYERS_LOOP(ctx)

#define MAP_COARSE_CELLS_AND_COARSE_LAYERS_LOOP(ctx,eq)         \
    MAP_COARSE_CELLS_LOOP(ctx,eq)                   \
    MAP_COARSE_LAYERS_LOOP(ctx)

#define MAP_REFINED_LOOP(ctx,m,n)                   \
for (nHorRef = 0 , fM = b2t ? (m) - (Ref[ctx]-1)/2 : (m) ;      \
    fM <= (b2t ? (m) + Ref[ctx]/2 : (m)) ; fM++)            \
        for (fN = l2r ? (n) - (Ref[ctx]-1)/2 : (n) ;        \
            fN <= (l2r ? (n) + Ref[ctx]/2 : (n)) ;          \
         DBREFINE("\t\t\t\tRefLoop did C%d(%3d,%3d)\n",                 \
                                           ctx, fM, fN)                 \
            fN++, nHorRef ++)

//
//
// VERTICAL INTERPOLATION MACRO's
//
//

#define V_AVERAGE3D(ctx,m,n,k,var,result)               \
    totThick  = 0.0;                            \
    evalValue = 0.0;                            \
    for (fK = k - C[ctx]->refDown ; fK <= k + C[ctx]->refUp ; fK++) \
    {                                   \
    layThick   = I1D(C[ctx]->thick, fK);                \
    totThick  += layThick;                      \
    evalValue += cI3D(ctx,m,n,fK,var) * layThick;           \
        ON_DEBUG (DBLEV4,                                              \
        FMapLog((char*)"\t\t\tVar C%d(k%2d-%d)(%2d,%2d):%12.6e\n",  \
             ctx, k, fK, m, n, cI3D(ctx,m,n,fK,var));   \
        )                               \
    }                                   \
    result = evalValue / totThick ;


#define V_SUM3D(ctx,m,n,k,var,result)                   \
    result = 0.0;                           \
    for (fK = k - C[ctx]->refDown ; fK <= k + C[ctx]->refUp ; fK++) \
    {                                   \
    result += cI3D(ctx,m,n,fK,var);                 \
        ON_DEBUG (DBLEV4,                                              \
        FMapLog((char*)"\t\t\tVar C%d(k%2d-%d)(%2d,%2d):%12.6e\n",  \
             ctx, k, fK, m, n, cI3D(ctx,m,n,fK,var));   \
        )                               \
    }                                   \


#define V_AVERAGE4D(ctx,m,n,k,l,var,result)             \
    totThick  = 0.0;                            \
    evalValue = 0.0;                            \
    for (fK = k - C[ctx]->refDown ; fK <= k + C[ctx]->refUp ; fK++) \
    {                                   \
    layThick   = I1D(C[ctx]->thick, fK);                \
    totThick  += layThick;                      \
    evalValue += cI4D(ctx,m,n,fK,l,var) * layThick;         \
        ON_DEBUG (DBLEV4,                                              \
        FMapLog((char*)"\t\t\tVar %d C%d(k%2d-%d)(%2d,%2d):%12.6e\n",   \
             l, ctx, k, fK, m, n, cI4D(ctx,m,n,fK,l,var));  \
        )                               \
    }                                   \
    result = evalValue / totThick ;

