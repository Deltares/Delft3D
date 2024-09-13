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
// $Id: mapper.h 878 2011-10-07 12:58:46Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/mapper/mapper.h $
//------------------------------------------------------------------------------
//  Flow2D3D DD Mapper
//
//  Stef.Hummel@Deltares.NL
//  Erik.deGoede@deltares.nl
//  31 may 11
//-------------------------------------------------------------------------------


#pragma once


#include "context-mapside.h"
#include "map_messages.h"    // Messages for Mapper
#include "iterator.h"


//
// Min/max macro functions
// Note: These functions evaluate one of their arguments more than once,
// so they should not have any side effects.
//

#if defined (HAVE_CONFIG_H)
    #define MIN(A,B)    ((A) <= (B) ? (A) : (B))
    #define MAX(A,B)    ((A) >= (B) ? (A) : (B))
#endif

//
// Definition of real-var. types
//
typedef double      MapScalar;
typedef MapScalar*  MapScalarPntr;


/*
 * Enumeration for orientation of a velocity component
 */

typedef enum {
    Vel_Norm,       /* Normal velocity          */
    Vel_Tang,       /* Tangential velocity          */
    Vel_Zeta,       /* No velocity component defined
               (in case the equation type is Zeta)  */
    NR_VEL
} Vel;

 //
// Enumeration for variable types (equation to solve)
//

typedef enum {
    Seq_U,      /* U-velocity               */
    Seq_V,      /* V-velocity               */
    Seq_Zeta,       /* Water level              */
    Seq_Conc,       /* Constituents             */
    Seq_Turb,       /* Turbulence               */
    Seq_Zeta_ADI_St1,
    Seq_Zeta_ADI_St2,
    Seq_Conc_ADI_St1,
    Seq_Conc_ADI_St2,
    Seq_2DAD,
    Seq_RolUV,
    NR_SEQ
} Seq;


//
// Flags for Setting/Resetting KC?-vars
//

typedef enum {
    KC_Set,
    KC_Reset,
    NR_KC_SET
} KC_Flag;

//
// Enumeration for 'type of Context' (source or target)
//

typedef enum {
   CtxType_Source,
   CtxType_Target,
   NR_CTX_TYPES
}  CtxType;



//
// DELFT3D-FLOW Mapper Class
//

class D3dFlowMapper
{
    public:

    ////////////////////////
    //
    // PUBLIC FUNCTIONS
    //

    D3dFlowMapper();

    int Setup(
        Iterator * mapper,                // current mapper
        char     * configString,          // configuration string
        Iterator * neighbors[NR_CNTXTS],  // neighbor flow processes
        MemType    memType[NR_CNTXTS]     // SharedMem or Distributed dd-data
        );

    int CreateData(void);

    int DoMapping(void);

    ~D3dFlowMapper(void);

    protected:

    /////////////////////
    //
    // PROTECTED DATA
    //
    //

    //
    // Position of Mapper in the Hydra configuration
    //

    int         mapperObjID;                // object Id of current mapper
    int         nnId[NR_CNTXTS];            // neighbor object Ids

    char        confFile[_POSIX_PATH_MAX];  // configuration file name
    MapMess   * mapMess;                    // message storage

    FILE      * logFile;                    // file handle to log-file for this mapper

    //
    // Pointers to:
    // DELFT3D-FLOW context variables
    //

    D3dFlowContextMapSide *C[NR_CNTXTS];   // Vars for both contexts

    //
    // CONFIGURATION DATA:
    //

    //
    // Variables that are read from configfile.
    //
    MapScalar EpsMap[NR_EQ];        // Epsilon for Mappers conv. check
    int MaxIter_Vel;                // Max number of mapper iterations
                                    //     for momentum equations
    int MaxIter_Conc;               // Max number of mapper iterations
                                    //     for transport equation
    int MaxIter_2DAD;               // Max number of mapper iterations
                                    //     for 2D adv.diff equation
    int Ref[NR_CNTXTS];             // Refinement factor for
                                    //     each context

    EdgeType Edge[NR_CNTXTS];       // Edge types for both contexts

                                    // Per context:
    int FirstCell[NR_CNTXTS];       // Mapper First-Cells
    int LastCell[NR_CNTXTS];        // Mapper Last-Cells
    int NormalCell[NR_CNTXTS];      // Centre cell in normal dir ('pivot')

    //
    // Variables used for Mapper loops
    //

    int ctx,oCtx;                       // context counter (0-1)

    int mStart[NR_CNTXTS][NR_EQ];       // Start-indices for map-loop
    int nStart[NR_CNTXTS][NR_EQ];       //    in the current context
    int mEnd[NR_CNTXTS][NR_EQ];         // End-ndices for map-loo
    int nEnd[NR_CNTXTS][NR_EQ];         //    in the current context

    int mOthStart[NR_CNTXTS][NR_EQ];    // Related start-indices for
    int nOthStart[NR_CNTXTS][NR_EQ];    //    map-loop in other ctx
    int mOthEnd[NR_CNTXTS][NR_EQ];      // Related end-indices fo
    int nOthEnd[NR_CNTXTS][NR_EQ];      //    map-loop in other ctx

    int mStartMin[NR_CNTXTS];           // These indices describe the region with
    int nStartMin[NR_CNTXTS];           // computational cells (i.e. indices indepen-
    int mStripSize[NR_CNTXTS];          // dent of equation type) that are coupled
    int nStripSize[NR_CNTXTS];          // over the interface between two subdomains.

    int l2r,b2t;                        // boolean flags indicating
                                        // if it is a left2right or
                                        // a bottom to top mapper

    int m,n,k;                          // loop counters current Cntxt
    int oM,oN,oK;                       // loop counters other Cntxt
    int l;                              // loop counter constituents

    int fM,fN,fK;                       // loop counters refined loops
    int nHorRef;                        // number of M or N points in
                                        // a horizontally refined loop

    //
    // help vars for vertical interpolation in mapper loops
    //

    MapScalar   layThick,       // Layer thickness per layer
                totThick,       // Total thickness
                evalValue,      // (Evaluated) value per layer
                horAverage,     // Horizontal average
                vertAverage;    // Vertical average


    ////////////////////////
    //
    // PROTECTED FUNCTIONS
    //

    //
    // Configuration functions
    //

    void FMapLog(
        char      * format, // 'fprintf-format' for log
        ...                 // arguments of log message
        );

    void FMapLog2DIndex(
        char      * messStr,// message string
        int         ctx,    // context counter
        int         mIndex, // M index
        int         nIndex  // N index
        );

    int InitAndParseConfigString(
        char              * configString    // configuration file
        );

    int CheckConfig(void);

    int CheckConfigContext(
        int         ctx     // current context
        );

    int CheckConfigBothContexts(void);

    int GetStartCell(
        int         curCtx,     // current context
        int         eq          // equation type
        );

    virtual int GetEndCell(
        int         ctx,        // current context
        int         eq          // equation type
        );

    virtual int GetNormalCell(
        int         ctx,        // current context
        int         eq,         // equation type
        CtxType     type        // equation type
        );

    Vel GetVelocityOrientation(
        int         curCtx,     // current context
        int         eq          // equation type
        );

    virtual void CentreCellOtherContext(
        int         ctx,        // current context
        int         curCentre,  // current centre cell
        int       * otherCentre // O: centre cell other context
        );

    virtual int Other_MN_Centre(
        int         ctx,        // current context
        int         curCentre   // current centre cell
        );

    //
    // NextStep functions (overloaded to be able to provide more
    // than one possible next step).
    //

    int NextStep(           // Return: Step caller must go to
        int     myStep,     // I: Step caller just completed
        int     nnStep,     // I: Step neighbor(s) should go to
        int     myNxtStep   // I: Next step
        );

    int NextStep(           // Return: Step caller must go to
        int     myStep,     // I: Step caller just completed
        int     nnStep,     // I: Step neighbor(s) should go to
        int     myNxtStep,  // I: Next step
        int     myNxtStepAlt    // I: Next step Alternative
        );

    void SendDataToFlow(
         int    myStep,       // I: Step caller just completed
         int    neighborStep  // I: Step neighbor(s) should go to
             );
    int ReceiveDataFromFlow(void);


    //
    // Functions related to 'DELFT3D-FLOW Steps'
    //

    virtual void InitKc(
        Eq      eq          // Equation type
        );

    virtual void CheckDDbounds(
        Eq      eq          // Equation type
        );

    virtual void CheckDepthUV(
        Eq      eq          // Equation type
        );

    virtual void InitFlowVariables(void);

    virtual void CopyFlowVariables(
        Seq     seq             // Type of variable
        );

    virtual void CopyOuterNormalVelocities(
        Seq     seq             // Type of variable
        );

    virtual void CopyOuterTangentVelocities(
        Seq     seq             // Type of variable
        );

    virtual void CopySediment(
        Seq     seq             // Type of variable
        );

    virtual void Build(
        Seq     seq,            // Type of variable
        int     l               // conc. loop counter
        );

    virtual int Proces(
        Seq     seq,            // Type of variable
        int     l               // conc. loop counter
        );

    virtual int CheckConvergence(
        Seq     seq,                // Variable type
        int     l,                  // conc. loop counter
        int     resolve[NR_CNTXTS]  // which context must be
        );                          //     resolved?

    virtual void Adjust(
        Seq     seq,                // Type of variable
        int     l,                  // conc. loop counter
        int     resolve[NR_CNTXTS]  // which context must be
        );                          //     resolved?

    virtual double InterpolateNormalU1(
        int fineCtx,
        int coarseCtx,
        int fineN,
        int coarseM,
        int coarseN,
        int coarseK
        );

    virtual double InterpolateTangentialU1(
        int fineCtx,
        int coarseCtx,
        int fineM,
        int coarseM,
        int coarseN,
        int coarseK
        );

    virtual double InterpolateNormalV1(
        int fineCtx,
        int coarseCtx,
        int fineM,
        int coarseM,
        int coarseN,
        int coarseK
        );

    virtual double InterpolateTangentialV1(
        int fineCtx,
        int coarseCtx,
        int fineN,
        int coarseM,
        int coarseN,
        int coarseK
        );

    virtual double InterpolateS1(
        int fineCtx,
        int coarseCtx,
        int fineM,
        int fineN,
        int coarseM,
        int coarseN,
        int coarseK
        );
    virtual int CheckForDry();

    virtual void StateMachineAdiOrWang(
        int     transportUsed,      // boolean calculate transp.?
        int     sedimentUsed        // boolean calculate sediment?
        );

    virtual void FinishWang(void);

};


//
// CONVENIENCE MACRO'S  for:
//

//
// Logging the timestep / iteration counter
//

#define LOG_ITER(iter)      ON_DEBUG( DBLEV3+DBLEV6,        \
                FMapLog((char*)"\nIt %d ...\n",iter);       \
                )

#define cI2D(ctx,m,n,var)       *( (C[ctx]->var) + (m + (C[ctx]->mArrayOffset)) * (C[ctx]->nArraySize) + (n + (C[ctx]->nArrayOffset)))

#define cI3D(ctx,m,n,k,var)     *( (C[ctx]->var) + ((k) - 1) * (C[ctx]->mArraySize) * (C[ctx]->nArraySize) + \
                                    (m + (C[ctx]->mArrayOffset)) * (C[ctx]->nArraySize) + (n + (C[ctx]->nArrayOffset)))

#define cI4D(ctx,m,n,k,l,var)   *( (C[ctx]->var) + ((l) - 1) * (C[ctx]->mArraySize) * (C[ctx]->nArraySize) * (C[ctx]->kMax) + \
                        ((k) - 1) * (C[ctx]->mArraySize) * (C[ctx]->nArraySize) + \
                                    (m + (C[ctx]->mArrayOffset)) * (C[ctx]->nArraySize) + (n + (C[ctx]->nArrayOffset)))


//
// Set other context index
//

#define SET_OTHER_CTX       oCtx=1-ctx


//
// Static variables for each source that includes 'seq' functions:
//
// - Which variable to evaluate in build/check loops (this may only
//   be VarS_s1, for seq=Zeta(adi) )
// - Text for equation type
//

static const char * seqTxt[NR_SEQ] = {
    "U",
    "V",
    "Zeta",
    "Conc",
    "Turb",
    "Zeta_ADI_St1",
    "Zeta_ADI_St2",
    "Conc_ADI_St1",
    "Conc_ADI_St2"
    };


static const Eq location[NR_SEQ] = {
    Eq_U,
    Eq_V,
    Eq_Zeta,
    Eq_Zeta,
    Eq_Zeta,
    Eq_Zeta,
    Eq_Zeta,
    Eq_Zeta,
    Eq_Zeta
    };


int
ParseMapperConfigString (
    char      * configString,           // config string in DD-Bound file
    EdgeType    edgeType  [NR_CNTXTS],  // Out: edge types left / right domein
    int         firstCell [NR_CNTXTS],  // Out: first cell left / right domein
    int         lastCell  [NR_CNTXTS],  // Out: last cell left / right domein
    int         normalCell[NR_CNTXTS],  // Out: normal cell left / right domein
    int         refine    [NR_CNTXTS],  // Out: refinement of contexts
    int         echoRefinement          // In : 1: echo refinement to file
    );
