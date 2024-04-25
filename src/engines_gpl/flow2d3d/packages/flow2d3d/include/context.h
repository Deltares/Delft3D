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
// $Id: context.h 878 2011-10-07 12:58:46Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/mapper/context.h $
//------------------------------------------------------------------------------
//  Class: D3dFlowContext
//  Flow2D3D Context variables/functions
//
//  Stef.Hummel@Deltares.NL
//  Erik.deGoede@deltares.nl
//  Adri.Mourits@deltares.nl
//  30 may 11
//-------------------------------------------------------------------------------


#pragma once


//#include <ddexec.h>

#include "precision.h"

#include "flow_in_hydra.h"
#include "map_debug.h"
#include "varinfocoll.h"
#include "iterator.h"


//using namespace Hydra;


#define YET_TO_INIT     (-1)
#define LOG_CONTEXT     0



//
// When communicating all parameters, every state, change the code as follows:
// 1) file dd\mapping\context.h
//    in typedef enum {...} MapDistribGroup:
//    add the following line:
//    MapDistrib_All,
//
// 2) file dd\mapping\context_distrib_functs.cpp
//    change
//    #if 0
//    into
//    #if 1
//    at:
//        varInfoColl->AddGroup(MapDistrib_All); // TODO: exclude from get-max-num-bytes
//
// 3) file dd\mapping\flow_nxtstp.cpp
//    change
//          MapDistribGroup retVal = MapDistrib_NoGroup;
//          // MapDistribGroup retVal = MapDistrib_All;
//
//      #if 1
//    into
//          // MapDistribGroup retVal = MapDistrib_NoGroup;
//          MapDistribGroup retVal = MapDistrib_All;
//
//      #if 0
//
//


/*
 * Enumeration for orientation of manipulated edge
 */

typedef enum {
    Edge_Top,
    Edge_Bottom,
    Edge_Left,
    Edge_Right,
    NR_EDGETYPES
} EdgeType;


//
// Enumeration for location of var. or equation to be manipulated
//
typedef enum {
    Eq_U,       // on U-velocity points
    Eq_V,       // on V-velocity points
    Eq_Zeta,    // on Water level points
    NR_EQ
} Eq;


//
// Enumeration for tang./norm. vel points, used to distinguish
//      communciated strip sizes.
//
typedef enum {
    Points_Tangential,   // Tangential velocity points (along the mapped edge)
    Points_Normal,       // Normal velocity points (perpendiculare to the mapped edge)
    NR_POINT_TYPES
} PointType;


//
// Enumeration for dimensions of variables
//
typedef enum {
    Dim_1d,         // 1d, vertical (thickness)
    Dim_2d,         // 2d (e.g. zeta)
    Dim_3d,         // 3d (e.g. U/V-velocities)
    Dim_3dt,        // 3d beTween layers (e.g. W-velocities)
    Dim_3dLsedtt,   // 3d (total number of Sed.s as third dimension)
    Dim_4d,         // 4d (e.g. constituents)
    Dim_4dt,        // 4d beTween layers (e.g. turbulence)
    NR_DIM_TYPES
} DimType;


//
// Size of the var-info collection
//

#define NUM_BLOCK_INFO_S NR_DIM_TYPES*NR_EQ


//
// Enumeration for variables that are exchanged by means of the
// var-info collection.
//

enum{
    Var_Unknown = -1,
    VarKcs,       // 0    INTS:      2-dim., accessed with CI2D
    VarKcu,
    VarKcv,
    VarKfu,
    VarKfv,
    VarCfurou,    // 5    FLOATS:    2-dim. or 3-dim., accessed with CI2D/CI3D
    VarCfvrou,    //      VarCfurou and VarCfvrou 2 layers instead of KMAX
    VarDps,
    VarDpu,
    VarDpv,
    VarGuu,       // 10
    VarGvv,
    VarS0,
    VarS1,
    VarDicuv,
    VarVicuv,     // 15
    VarDepchg,
    VarZ0urou,
    VarZ0vrou,
    VarQxk,
    VarGsqs,      // 20
    VarQyk,
    VarSbuu,
    VarSbvv,
    VarU0,
    VarU1,        // 25
    VarV0,
    VarV1,
    VarWrkb4,
    VarR0,
    VarR1,        // 30
    VarWrkc4,
    VarThick,     //                 1-dim (0:kmax-1)
    VarUMean,
    VarVMean,
    VarKfuMin,    // 35              Z-Model, 2-dim
    VarKfuMx0,
    VarKfvMin,
    VarKfvMx0,
    VarKfuZ0,
    VarKfvZ0,     // 40
    VarWrkb17,
    VarQxkr,
    VarQykr,
    VarQxkw,
    VarQykw,      // 45
    NUM_VAR_IDS
};


//
// Support administration structure (to build the var info collection.)
//

typedef struct D3dBlockInfo_STR {
    int dim;
    int Offset;
    int Amount_D1;
    int Amount_D2;
    int Amount_D3;
    int Amount_D4;
    int Stride_D1;
    int Stride_D2;
    int Stride_D3;
    int Stride_D4;
    int Size;
} D3dBlockInfo;


//
// Enumeration for groups of data to be exchanged (by means of
// the var-info collection) between Map-side and Flow-side
//

typedef enum {
    MapDistrib_NoGroup,
    MapDistrib_Initial,
    MapDistrib_Initial2,
    MapDistrib_Build_UV,
    MapDistrib_Solve_UV,
    MapDistrib_Build_Conc,
    MapDistrib_Solve_Conc,
    MapDistrib_Finish_Wang,
    MapDistrib_Sediment,
    MapDistrib_Dry,
    MapDistrib_Roller_UV,
    MapDistrib_Build_2DAD,
    MapDistrib_Solve_2DAD,
    NUM_MAP_DISTRIB_TYPES
} MapDistribGroup;


//
// Admin for an equation point that has to be communicated
// between GAWS side and Flow Side
//

typedef struct GawsCommPointAdmin_STR {
    int                 m;              // m-index of point
    int                 n;              // n-index of point
    bool                l2r;            // L2R or B2T?
} GawsCommPointAdmin;


//
// Enumeration for groups of data to be exchanged (by means of
// the var-info collection) between Gaws-side and Flow-side
//

typedef enum {
    GawsDistrib_KcsFromFlow,
    GawsDistrib_AllVars,
    NUM_GAWS_DISTRIB_TYPES
} GawsDistribGroup;


//
// Number of float vars that are exchanged between Gaws and Flow
//
#define NUM_EXCHANGED_GAWS_VARS 4


/////////////////////////////////////////////////////////////////
// DELFT3D-FLOW Context Class
//


class D3dFlowContext
{
    public:

    ////////////////////////
    //
    // PUBLIC DATA
    //

    //
    // Dimensions of the context.
    // These vales will be set after the vars have been attached.
    //

    int     mMax    ;       // ( = *mmax)
    int     nMax    ;       // ( = *nmax)
    int     nMaxus  ;       // ( = *nmaxus)
    int     kMax    ;       // ( = *kmax)

    int     lStsci  ;       // ( = *lstsci )
    int     lTur    ;       // ( = *ltur )

    int     lSedtt  ;       // ( = *lsedtt )

    int     Zmodel  ;       // ( = *izmodl )

    int     Roller  ;       // ( = *iroll  )

    int     dDb     ;       // ( = *ddb )
    int     mMaxdb  ;       // ( = *mmaxdb )  mmax + 2 * ddbound
    int     nMaxdb  ;       // ( = *nmaxdb )  nmax + 2 * ddbound

    REAL_FP  Hdt    ;       // ( = *hdt )

    //
    // Settings for mapping:
    // - Type of edge for this context
    // - 'Refinement-layers' up and down.
    // - Pointer to other context.
    //

    EdgeType        edgeType;
    int             refUp;
    int             refDown;
    D3dFlowContext *oC;

    //
    // dimensions/offsets for array-access
    //
    int mStart[NR_EQ];  // Start/End of mapper loop per location type
    int mEnd[NR_EQ];    // (i.e. Start/End for U-, V- and Zeta-points)
    int nStart[NR_EQ];
    int nEnd[NR_EQ];

    int mStartMin;      // minimum of mStart[location-types]
    int nStartMin;      // minimum of nStart[location-types]
    int mStripSize;     // For Distributed: M*N strip to be communicated
    int nStripSize;     //   with the other size (flow->mapper, mapper->flow).

    int mArraySize;     // Actual M/N sizes of arrays in context:
    int nArraySize;     // - Mapside when Distributed: stripSize
                        // - Mapside when SharedMem  : (mMaxdb+4)*nMaxdb
                        // - Flowside                : (mMaxdb+4)*nMaxdb

    int mArrayOffset;   // Offset for determining position of <var>(m,n) in
    int nArrayOffset;   //   the array <var>
                        // - Mapside when Distributed: stripSize
                        // - Mapside when SharedMem  : (mMaxdb+4)*nMaxdb
                        // - Flowside                : (mMaxdb+4)*nMaxdb


    //
    // For distributed:
    // - data exchange type (SharedMem/Distributed)
    // - var info collection
    // - Support administration to build a var info collection
    //   for D3D-Flow data

    MemType             memType;
    VarInfoCollection * varInfoColl;
    D3dBlockInfo        d3dblockInfo[NR_DIM_TYPES][NR_POINT_TYPES];

    //
    // DELFT3D-FLOW Shared Memory data
    //

    int   * kcu     ;
    int   * kcv     ;
    int   * kcs     ;
    int   * kfu     ;
    int   * kfuz0   ;
    int   * kfumx0  ;
    int   * kfumin  ;
    int   * kfv     ;
    int   * kfvz0   ;
    int   * kfvmx0  ;
    int   * kfvmin  ;
    int   * kfs     ;
    int   * nmax    ;
    int   * nmaxus  ;
    int   * kmax    ;
    int   * mmax    ;
    int   * lstsci  ;
    int   * ltur    ;

    int   * lsedtt  ;     // Sediment

    int   * izmodl  ;

    int   * iroll   ;

    int   * ddb     ;
    int   * mmaxdb  ;
    int   * nmaxdb  ;

    REAL_FP * s1      ;
    REAL_FP * s0      ;
    REAL_FP * u1      ;
    REAL_FP * u0      ;
    REAL_FP * umean   ;
    REAL_FP * v1      ;
    REAL_FP * v0      ;
    REAL_FP * vmean   ;
    REAL_FP * r1      ;
    REAL_FP * r0      ;

    REAL_FP * thick   ;

    REAL_FP * guu     ;
    REAL_FP * gvv     ;
    REAL_FP * guv     ;
    REAL_FP * gvu     ;

    REAL_PREC * dps     ;

    REAL_FP * dpu     ;
    REAL_FP * dpv     ;

    REAL_FP * depchg  ;       // Sediment
    REAL_FP * sbuu    ;       // Sediment
    REAL_FP * sbvv    ;       // Sediment

    REAL_FP * qxkr    ;       // Roller
    REAL_FP * qykr    ;       // Roller
    REAL_FP * qxkw    ;       // Roller
    REAL_FP * qykw    ;       // Roller

    REAL_FP * wrka1   ;
    REAL_FP * wrka2   ;
    REAL_FP * wrka3   ;
    REAL_FP * wrka4   ;

    REAL_FP * wrkb4   ;
    REAL_FP * wrkb17  ;       // Roller

    REAL_FP * wrkc4   ;

    REAL_FP * cfurou  ;
    REAL_FP * cfvrou  ;
    REAL_FP * z0urou  ;
    REAL_FP * z0vrou  ;
    REAL_FP * vicuv   ;
    REAL_FP * dicuv   ;

    REAL_FP * qxk     ;
    REAL_FP * qyk     ;
    REAL_FP * gsqs    ;
    REAL_FP * hdt     ;


    ////////////////////////
    //
    // PUBLIC FUNCTIONS
    //

    D3dFlowContext(void);

    virtual ~D3dFlowContext(void);

    int GetContextId(void);

    virtual void SetOtherContext(
        D3dFlowContext * otherC // pointer to other context
        );


    void DumpEsmContext(void);

    Iterator * flowIterator;    //  D3dFlow iterator for this dd-data object

    Iterator * mapperIterator;  // Mapper iterator for this dd-data object

    Iterator * gawsIterator;    // GAWS iterator for this GAWS-side dd-data object

    int     contextID;          // esm context Id
    FILE  * logFile;            // file handle to logfile


    GawsCommPointAdmin  * commPointAdmins;  // Communcication points
                                            // Gaws-side / Flow side
    int numCommPoints;                      // #Communcication points


    ////////////////////////
    //
    // PROTECTED FUNCTIONS
    //

    virtual void SetSizesAndFlags(void);

    virtual void AttachVars(
        int     cntxtId     // context-id
        );

    int AttachContextVar(
        int     aContextId, // context Id
        void ** var,        // adress of var. pointer
        char  * name        // name of var in context
        );

    int CreateContextVar(
        void     ** var,    // adress of var. pointer
        char      * name,   // name of var in context
        int         size    // #bytes in var
        );

    void CMapLog(
        char      * format,     // I: 'fprintf-format' for print
        ...             // I: varargs
        );

    int FillBlockAdmin(void);
    void DumpBlockAdmin(void);
    void DetermineStripSize(void);
    void FillVarInfoCollection(void);

};


/////////////////////////////////////////////////////////////////
//
// Convenience macro's
//

//
// Macro for:
// - Attaching a var to an esm context
// - Creating a local copy of a var.
//
#if LOG_CONTEXT

#define ATTACH(cid,var,name)    \
    AttachContextVar( (cid) , (void **) &(var) , (name) ) ; \
    printf("CONTXT: %x (%s) \n", var, name);

#define CREATE_REAL_FP(var,name,size)    \
    { CreateContextVar((void **) &(var) , (name) , sizeof(REAL_FP) * size ) ; \
    printf("CONTXT-FLT: %x (%s) %d\n", var, name, size); }

#define CREATE_REAL_PREC(var,name,size)    \
    { CreateContextVar((void **) &(var) , (name) , sizeof(REAL_PREC) * size ) ; \
    printf("CONTXT-FLT: %x (%s) %d\n", var, name, size); }

#define CREATE_INT(var,name,size)    \
    { CreateContextVar((void **) &(var) , (name) , sizeof(int) * size ) ; \
    printf("CONTXT-INT: %x (%s) %d\n", var, name, size); }

#else

#define ATTACH(cid,var,name)    \
    AttachContextVar( (cid) , (void **) &(var) , (name) ) ;

#define CREATE_REAL_FP(var,name,size)    \
    CreateContextVar((void **) &(var) , (name) , sizeof(REAL_FP) * size ) ;

#define CREATE_REAL_PREC(var,name,size)    \
    CreateContextVar((void **) &(var) , (name) , sizeof(REAL_PREC) * size ) ;

#define CREATE_INT(var,name,size)    \
    CreateContextVar((void **) &(var) , (name) , sizeof(int) * size ) ;

#endif


#define FREEVAR(var)  if ( var != NULL ) { free(var) ; var = NULL; }


//
// Generic Macro for accessing the Flow variables from a context
//
// I1D is used for accessing a variable var(1:kmax)
// I1DT is used for accessing a variable var(0:kmax)
// I2D is used for accessing a variable var(1:nmmaxj)
// (O2D is the offset, used for debugging purposes)
// I3D is used for accessing a variable var(1:nmmaxj,kmax)
// (O3DT is the offset, used for debugging purposes)
// I3DT is used for accessing a variable var(1:nmmaxj,0:kmax)
// I4D is used for accessing a variable var(1:nmmaxj,kmax,lstci)
// (O4D is the offset, used for debugging purposes)
// I4DT is used for accessing a variable var(1:nmmaxj,0:kmax,lstci)
// (O4D is the offset, used for debugging purposes)
//

#define I1D(var,k)      *( (var) + (k) - 1 )

#define I1DT(var,k)     *( (var) + (k) )

#define I2D(var,m,n)        *( (var) + ((m) + (mArrayOffset)) * (nArraySize) + ((n) + (nArrayOffset)))

#define O2D(m,n)        (((m) + (mArrayOffset)) * nArraySize + ((n) + (nArrayOffset)))

#define I3D(var,m,n,k)      *( (var) + ((k) - 1) * (mArraySize) * (nArraySize) + \
                                    ((m) + (mArrayOffset)) * (nArraySize) + ((n) + (nArrayOffset)))

#define I3DT(var,m,n,k)     *( (var) + ((k)) * (mArraySize) * (nArraySize) + \
                                    ((m) + (mArrayOffset)) * (nArraySize) + ((n) + (nArrayOffset)))

#define O3DT(m,n,k)     ( ((k)) * (mArraySize) * (nArraySize) + \
                                    ((m) + (mArrayOffset)) * (nArraySize) + ((n) + (nArrayOffset)))

#define I4D(var,m,n,k,l)    *( (var) + ((l) - 1) * (mArraySize) * (nArraySize) * (kMax) + \
                        ((k) - 1) * (mArraySize) * (nArraySize) + \
                                    ((m) + (mArrayOffset)) * (nArraySize) + ((n) + (nArrayOffset)))

#define O4D(m,n,k,l)    ( ((l) - 1) * (mArraySize) * (nArraySize) * (kMax) + \
                        ((k) - 1) * (mArraySize) * (nArraySize) + \
                                    ((m) + (mArrayOffset)) * (nArraySize) + ((n) + (nArrayOffset)))

#define I4DT(var,m,n,k,l)    *( (var) + ((l) - 1) * (mArraySize) * (nArraySize) * ((kMax) + 1) + \
                        ((k)) * (mArraySize) * (nArraySize) + \
                                    ((m) + (mArrayOffset)) * (nArraySize) + ((n) + (nArrayOffset)))

#define O4DT(m,n,k,l)      ( ((l) - 1) * (mArraySize) * (nArraySize) * ((kMax) + 1) + \
                        ((k)) * (mArraySize) * (nArraySize) + \
                                    ((m) + (mArrayOffset)) * (nArraySize) + ((n) + (nArrayOffset)))

//
// Macro to get the adress of a Flow variable from a context
//

#define A2D(var,m,n)        ( (var) + ((m) + (mArrayOffset)) * (nArraySize) + ((n) + (nArrayOffset)))

