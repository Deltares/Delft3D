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
// $Id: gaws.h 878 2011-10-07 12:58:46Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/mapper/gaws.h $
//------------------------------------------------------------------------------
//  Class for Global ADI Wang Solver
//
//  Stef.Hummel@Deltares.NL
//  30 may 11
//-------------------------------------------------------------------------------


#pragma once


#include "context-gawsside.h"   // GAWS Delft3d-Flow context object
#include "gaws_wang_eq.h"       // Wang-Equations
#include "iterator.h"


#define MAX_GAWS_MAP    100     // Max #mappers in a Gaws object


//
// Structures for Gaws administration information
//

typedef struct {          // Gaws-info on a D3d flow process:
    Iterator * gawsBarrier;     //   Current gaws barrier iterator
    Iterator * flowIterator;    //   D3d flow process iterator
    MemType    memType;         //   Data exchange type for this process
} GawsProcRelation;


typedef struct {                            // Gaws-info on a mapper:
    Iterator * mapperIterator;              //   Mapper's hydra executive object-Id
    Iterator * flowIterator[NR_CNTXTS];     //   Mapper's neighbor d3dflow processes
    char       configStr[_POSIX_PATH_MAX];  //   Configuration file for this mapper
} GawsMapInfo;


typedef struct STR_MapperCells {
    bool        leftToRight;// boolean L->R or Top->Bottom
    GawsMapInfo mapInfo;    // Gaws info on mapper
    int         mCurrBegin; // M-coord of begin cell in current domain
    int         mCurrEnd;   // M-coord of end     "   "    "        "
    int         nCurrBegin; // N-coord of begin   "   "    "        "
    int         nCurrEnd;   // N-coord of end     "   "    "        "
    int         mNbrBegin;  // M-coord of begin cell in neighbor domain
    int         mNbrEnd;    // M-coord of end     "   "    "        "
    int         nNbrBegin;  // N-coord of begin   "   "    "        "
    int         nNbrEnd;    // N-coord of end     "   "    "        "
    int         curRefine;  // Refinement factor C_0
    int         nbrRefine;  // Refinement factor C_1
} MapperCells;


//
// Enumeration for X- or Y-direction of type of Wang equations
//
typedef enum {
    Wang_XDir     = 1,
    Wang_YDir     = 0,
    NUM_WANG_DIRS = 2
} WangDir;


/*
 *
 * EqPoint:     M/N-point in a subdomain (context)
 *              that contains an equation
 * EqSeries:    Series of equation points
 *              on the same line (may be refined) line.
 *
 */


#define EQ_PNT_BLOCK_SIZE  10    // Size of growth of #points in an eq.serie
#define EQ_SER_BLOCK_SIZE 200    // Size of growth of #eq.series


//
// Enumeration for type of equation points when building
// equation line administration
//
typedef enum {
    Pnt_Regular,
    Pnt_StartRefine,
    Pnt_EndRefine,
    Pnt_EndLine,
    MAX_PNT_TYPE
    } PntType;


//
// Admin for an equation point in equation line administration.
//
typedef struct {
    D3dFlowContextGawsSide *curCntxt;   // context where point is in
    D3dFlowContextGawsSide *nbrCntxt;   // neighbor context
    int                     cell;       // cell nr. in current context
    int                     line;       // line nr. in current context
    int                     nbrCell;    // cell nr. in neighbor context
    int                     nbrLine;    // line nr. in neighbor context
    int                     numDependents;// #refined lines for <line>
    PntType                 type;       // type of equation point
} EqPoint;


//
// Admin for an equation series (equation line).
//
typedef struct {
    EqPoint *pnt;                        // involved points
    int      nPnt;                       // #involved points
    bool     leftToRight;                // left to right? (or bottom to top)
    int      level;                      // refinement level
    bool     hasDependents;              // depending refined lines?
} EqSeries;


//
// Gaws Class
//

class Gaws
{
    public:

    ////////////////////////
    //
    // PUBLIC FUNCTIONS
    //

    Gaws(void);

    virtual int Setup(
        Iterator      *     gaws,            // GAWS iterator
        char          *     aConfigStr,      // configuration info
        int                 aNumprocs,       // #processes
        GawsProcRelation    procRelations[]  // info on the processes
        );

    virtual void DoSolving(void);

    virtual void GawsLog(
        char          * format,         // 'fprintf-format' for log
        ...                 // arguments of log message
        );

    virtual ~Gaws(void);

    protected:

    /////////////////////
    //
    // PROTECTED DATA
    //

    Iterator  * gawsIterator;               // Gaws Iterator
    char      * config;                     // config string

    FILE      * logFile;                    // file handle to log-file
                                            // for this gaws object
    int         numMappers;                 // #mappers
    GawsMapInfo mapInfo[MAX_GAWS_MAP];      // gaws info for mappers
    MapperCells mapCells[MAX_GAWS_MAP];     // cell info of all mappers

    int                         nProc;      // #processes
    D3dFlowContextGawsSide   ** contexts;   // context object for each proc

    WangEquations **wangEquations_X;        // wang eqs in x-dir. levels
    WangEquations **wangEquations_Y;        // wang eqs in y-dir. levels

    EqSeries   *eqSeries;                   // equation line administration
    int         nSeries;                    // #equation lines in administration
    int         maxRefineLevel[NUM_WANG_DIRS];// maximum refine level (XDir/YDir)


    /////////////////////
    //
    // PROTECTED FUNCTIONS
    //

    virtual void InitializeMapperInfo(void);

    virtual int FillMapperCells(
        MapperCells       * cells,  // cell-info to fill
        GawsMapInfo     mapper      // gaws info for a mapper
        );

    virtual int FindNeighborPoint(
        bool    leftToRight,    // boolean L->R or Top->Bottom
        D3dFlowContextGawsSide
              * curCntxt,       // context of current domain
        int     curM,           // M-index in current  domain
        int     curN,           // N-index  "     "      "
        int   * nbrM,           // M-index in neighbor domain
        int   * nbrN,           // N-index  "     "      "
        D3dFlowContextGawsSide
             ** nbrCntxt        // context of current domain
        );

    virtual int CreateWangEquations(
        WangEquations * Wang_Eq_XorY,   // pointer Wang Eqs context
        WangDir         wangDir,        // X- or Y-direction
        int             level           // Refinement level
        );

    virtual int CreateWangAdministration(
        WangDir         wangDir         // X- or Y-direction
        );

    virtual int ContinueInNextContext(
        int         eqSerie,    // current eq. series
        int         curM,       // M-index in this next domain
        int         curN,       // N-index  "  "    "      "
        D3dFlowContextGawsSide
                  * curCntxt    // context of current domain
        );

    virtual int CheckDependents(
        D3dFlowContextGawsSide
              * curCntxt,       // context of current domain
        D3dFlowContextGawsSide
              * nbrCntxt,       // context of neighbor domain
        bool    leftToRight,    // boolean L->R or Top->Bottom
        int     myLine,         // line in current domain
        int   * centreLine      // centre line in current
                                //      or in neighbor domain
        );

    virtual int GetCentreLine(
        int     map,            // mapper index
        int     myLine,         // line in current domain
        int     ctx             // context index (0/1)
        );

    virtual int NewEquationSerie(
        bool        leftToRight,// boolean L->R or Top->Bottom
        int         level       // intial ref. level (0 / -1)
        );

    virtual void AddEqPoint(
        int         eqSerie,    // current eq.series
        D3dFlowContextGawsSide
                  * curCntxt,   // context of current domain
        int         curCell,    // cell in current domain
        int         curLine,    // line in current domain
        D3dFlowContextGawsSide
                  * nbrCntxt,   // context of neighbor domain
        int         nbrCell,    // cell in neighbor domain
        int         nbrLine,    // line in neighbor domain
        PntType     type,       // type of equation point
        int         numDependents   // #eq.s depending on this eq.
        );

    virtual void RefineSeries(void);

    virtual void SetDependentLevels(
        int         curSer      // current eq.series
        );

    virtual int ProcessCouplePoint(
        D3dFlowContextGawsSide
                  * curCntxt,   // context of current domain
        bool        leftToRight,// boolean L->R or Top->Bottom
        int         eqSerie,    // current eq.series
        int         curCell,    // cell in current domain
        int         curLine     // line in current domain
        );

    virtual void Solver(
        bool            leftToRight // #x or y dir
        );

};

