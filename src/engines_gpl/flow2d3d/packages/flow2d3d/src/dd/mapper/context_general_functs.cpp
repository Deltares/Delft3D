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
// $Id: context_general_functs.cpp 878 2011-10-07 12:58:46Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/mapper/context_general_functs.cpp $
//------------------------------------------------------------------------------
// Class: D3dFlowContext
// Context Attacher, base class (parent for Flow side Map side, Gaws Side)
// Functions for creating and initializing a context.
//
// Stef.Hummel@deltares.nl
// Erik.deGoede@deltares.nl
// Adri.Mourits@deltares.nl
//  31 may 11
//-------------------------------------------------------------------------------


#include "flow2d3d.h"


#define LOG_VALUES  0


//////////////////////////////////////////////////////////////////////
//
// D3dFlowContext:
//
// Public Functions on D3dFlowContext data
//


D3dFlowContext::D3dFlowContext(void)
{

    MAPDBG_FUN2("D3dFlowContext::D3dFlowContext");

    //
    // Init identifiers for this context
    //

    contextID   = YET_TO_INIT;

    //
    // Init var.s for data exchange
    //
    this->flowIterator   = NULL;
    this->mapperIterator = NULL;
    this->gawsIterator  = NULL;

    memType     = Mem_Unknown;
    varInfoColl = NULL;         // Default: no var-info coll.
                                //          (only used for Distributed).
    this->commPointAdmins = NULL;
    this->numCommPoints   = 0;

    //
    // Init settings for mapping
    // (default no refinement / edge type not known)
    //
    refUp    = 0;
    refDown  = 0;
    edgeType = NR_EDGETYPES; // i.e.: not yet defined

    //
    // Init D3D esm context variables (all pointers to zero)
    //

    mMax   = YET_TO_INIT;
    nMax   = YET_TO_INIT;
    nMaxus = YET_TO_INIT;
    kMax   = YET_TO_INIT;

    kcu     = NULL;     kcv     = NULL;     kcs     = NULL;

    kfu     = NULL;     kfv     = NULL;     kfs     = NULL;
    kfuz0   = NULL;     kfvz0   = NULL;
    kfumx0  = NULL;     kfvmx0  = NULL;
    kfumin  = NULL;     kfvmin  = NULL;

    qxkr    = NULL;
    qykr    = NULL;
    qxkw    = NULL;
    qykw    = NULL;

    nmax    = NULL;
    nmaxus  = NULL;
    mmax    = NULL;
    kmax    = NULL;

    lstsci  = NULL;
    ltur    = NULL;
    lsedtt  = NULL;       // Sediment

    izmodl  = NULL;       // flag for Z-model

    iroll   = NULL;       // flag for Roller model

    ddb     = NULL;
    mmaxdb  = NULL;
    nmaxdb  = NULL;

    thick   = NULL;

    hdt     = NULL;

    u0      = NULL;     v0      = NULL;     s0      = NULL;
    u1      = NULL;     v1      = NULL;     s1      = NULL;
    umean   = NULL;     vmean   = NULL;

    dpu     = NULL;     dpv     = NULL;     dps     = NULL;

    r0      = NULL;
    r1      = NULL;

    guu     = NULL;     guv     = NULL;
    gvv     = NULL;     gvu     = NULL;

    wrka1   = NULL;     wrka2   = NULL;     wrka3   = NULL;     wrka4   = NULL;
    wrkb4   = NULL;
    wrkb17  = NULL;
    wrkc4   = NULL;

    qxk     = NULL;
    qyk     = NULL;
    gsqs    = NULL;

    depchg = NULL;       // Sediment
    sbuu   = NULL;       // Sediment
    sbvv   = NULL;       // Sediment

}


D3dFlowContext::~D3dFlowContext(void)
{
    MAPDBG_FUN2("D3dFlowContext::~D3dFlowContext");

    // delete the variables that may have been created when running distributed

    if ( this->memType == Mem_Distributed )
    {

        //
        // Delete allocated mem that was created when running distributed
        //

        // Delete communication info

        if (this->varInfoColl != NULL) delete this->varInfoColl;
        if (this->commPointAdmins != NULL) delete this->commPointAdmins;

        // Delete variables

        FREEVAR( this->nmax     );
        FREEVAR( this->nmaxus   );
        FREEVAR( this->mmax     );
        FREEVAR( this->kmax     );
        FREEVAR( this->lstsci   );
        FREEVAR( this->ltur     );

        FREEVAR( this->lsedtt   );

        FREEVAR( this->izmodl   );

        FREEVAR( this->iroll    );

        FREEVAR( this->ddb      );
        FREEVAR( this->mmaxdb   );
        FREEVAR( this->nmaxdb   );

        FREEVAR( this->kcu      );
        FREEVAR( this->kcv      );
        FREEVAR( this->kcs      );
        FREEVAR( this->kfu      );
        FREEVAR( this->kfuz0    );
        FREEVAR( this->kfumx0   );
        FREEVAR( this->kfumin   );
        FREEVAR( this->kfv      );
        FREEVAR( this->kfvz0    );
        FREEVAR( this->kfvmx0   );
        FREEVAR( this->kfvmin   );
        FREEVAR( this->kfs      );

        FREEVAR( this->hdt      );

        FREEVAR( this->s1       );
        FREEVAR( this->s0       );
        FREEVAR( this->u1       );
        FREEVAR( this->u0       );
        FREEVAR( this->umean    );
        FREEVAR( this->v1       );
        FREEVAR( this->v0       );
        FREEVAR( this->vmean    );
        FREEVAR( this->r1       );
        FREEVAR( this->r0       );

        FREEVAR( this->thick    );

        FREEVAR( this->guu      );
        FREEVAR( this->gvv      );
        FREEVAR( this->guv      );
        FREEVAR( this->gvu      );

        FREEVAR( this->dps      );
        FREEVAR( this->dpu      );
        FREEVAR( this->dpv      );

        FREEVAR( this->depchg   );
        FREEVAR( this->sbuu     );
        FREEVAR( this->sbvv     );
        FREEVAR( this->qxkr     );
        FREEVAR( this->qykr     );
        FREEVAR( this->qxkw     );
        FREEVAR( this->qykw     );

        FREEVAR( this->wrka1    );
        FREEVAR( this->wrka2    );
        FREEVAR( this->wrka3    );
        FREEVAR( this->wrka4    );

        FREEVAR( this->wrkb4    );
        FREEVAR( this->wrkb17   );

        FREEVAR( this->wrkc4    );

        FREEVAR( this->cfurou   );
        FREEVAR( this->cfvrou   );
        FREEVAR( this->z0urou   );
        FREEVAR( this->z0vrou   );
        FREEVAR( this->vicuv    );
        FREEVAR( this->dicuv    );

        FREEVAR( this->qxk      );
        FREEVAR( this->qyk      );
        FREEVAR( this->gsqs     );

    }

}


int D3dFlowContext::GetContextId(void)
{
    MAPDBG_FUN("D3dFlowContext::GetContextId");

    if ( this->contextID == YET_TO_INIT )
    {
        throw new Exception (true, "Context not yet Initialized" );
    }

    return this->contextID;
}


void D3dFlowContext::AttachVars(
    int cntxtId         // context Id
    )
{
    MAPDBG_FUN2("D3dFlowContext::AttachVars");

    //
    //  Attach Integer Pointers
    //

    ATTACH( cntxtId,    nmax,   (char*)"NMAX"   );
    ATTACH( cntxtId,    nmaxus, (char*)"NMAXUS" );
    ATTACH( cntxtId,    mmax,   (char*)"MMAX"   );
    ATTACH( cntxtId,    kmax,   (char*)"KMAX"   );
    ATTACH( cntxtId,    lstsci, (char*)"lstsci" );
    ATTACH( cntxtId,    ltur,   (char*)"ltur"   );

    ATTACH( cntxtId,    lsedtt, (char*)"lsedtt" );

    ATTACH( cntxtId,    izmodl, (char*)"izmodl" );

    ATTACH( cntxtId,    iroll,  (char*)"iroll"  );

    ATTACH( cntxtId,    ddb,    (char*)"ddb"    );
    ATTACH( cntxtId,    mmaxdb, (char*)"mmaxdb" );
    ATTACH( cntxtId,    nmaxdb, (char*)"nmaxdb" );

    ATTACH( cntxtId,    kcu,    (char*)"kcu"    );
    ATTACH( cntxtId,    kcv,    (char*)"kcv"    );
    ATTACH( cntxtId,    kcs,    (char*)"kcs"    );
    ATTACH( cntxtId,    kfu,    (char*)"kfu"    );
    ATTACH( cntxtId,    kfuz0,  (char*)"kfuz0"  );
    ATTACH( cntxtId,    kfumx0, (char*)"kfumx0" );
    ATTACH( cntxtId,    kfumin, (char*)"kfumin" );
    ATTACH( cntxtId,    kfv,    (char*)"kfv"    );
    ATTACH( cntxtId,    kfvz0,  (char*)"kfvz0"  );
    ATTACH( cntxtId,    kfvmx0, (char*)"kfvmx0" );
    ATTACH( cntxtId,    kfvmin, (char*)"kfvmin" );
    ATTACH( cntxtId,    kfs,    (char*)"kfs"    );

    ATTACH( cntxtId,    qxkr  , (char*)"qxkr"   );
    ATTACH( cntxtId,    qykr  , (char*)"qykr"   );
    ATTACH( cntxtId,    qxkw  , (char*)"qxkw"   );
    ATTACH( cntxtId,    qykw  , (char*)"qykw"   );

    //
    //  Attach Real Pointers
    //

    ATTACH( cntxtId,    hdt,    (char*)"hdt" );

    ATTACH( cntxtId,    s1,     (char*)"s1"     );
    ATTACH( cntxtId,    s0,     (char*)"s0"     );
    ATTACH( cntxtId,    u1,     (char*)"u1"     );
    ATTACH( cntxtId,    u0,     (char*)"u0"     );
    ATTACH( cntxtId,    umean,  (char*)"umean"  );
    ATTACH( cntxtId,    v1,     (char*)"v1"     );
    ATTACH( cntxtId,    v0,     (char*)"v0"     );
    ATTACH( cntxtId,    vmean,  (char*)"vmean"  );
    ATTACH( cntxtId,    r1,     (char*)"r1"     );
    ATTACH( cntxtId,    r0,     (char*)"r0"     );

    ATTACH( cntxtId,    thick,  (char*)"thick"  );

    ATTACH( cntxtId,    guu,    (char*)"guu"    );
    ATTACH( cntxtId,    gvv,    (char*)"gvv"    );
    ATTACH( cntxtId,    guv,    (char*)"guv"    );
    ATTACH( cntxtId,    gvu,    (char*)"gvu"    );

    ATTACH( cntxtId,    dps,    (char*)"dps"    );
    ATTACH( cntxtId,    dpu,    (char*)"dpu"    );
    ATTACH( cntxtId,    dpv,    (char*)"dpv"    );

    ATTACH( cntxtId,    depchg, (char*)"depchg" );       // Sediment
    ATTACH( cntxtId,    sbuu,   (char*)"sbuu"   );       // Sediment
    ATTACH( cntxtId,    sbvv,   (char*)"sbvv"   );       // Sediment

    ATTACH( cntxtId,    wrka1,  (char*)"wrka1"  );
    ATTACH( cntxtId,    wrka2,  (char*)"wrka2"  );
    ATTACH( cntxtId,    wrka3,  (char*)"wrka3"  );
    ATTACH( cntxtId,    wrka4,  (char*)"wrka4"  );

    ATTACH( cntxtId,    wrkb4,  (char*)"wrkb4"  );
    ATTACH( cntxtId,    wrkb17, (char*)"wrkb17" );

    ATTACH( cntxtId,    wrkc4,  (char*)"wrkc4"  );

    ATTACH( cntxtId,    cfurou, (char*)"cfurou" );
    ATTACH( cntxtId,    cfvrou, (char*)"cfvrou" );
    ATTACH( cntxtId,    z0urou, (char*)"z0urou" );
    ATTACH( cntxtId,    z0vrou, (char*)"z0vrou" );
    ATTACH( cntxtId,    vicuv,  (char*)"vicuv"  );
    ATTACH( cntxtId,    dicuv,  (char*)"dicuv"  );

    ATTACH( cntxtId,    qxk,    (char*)"qxk" );
    ATTACH( cntxtId,    qyk,    (char*)"qyk" );
    ATTACH( cntxtId,    gsqs,   (char*)"gsqs" );


}


int D3dFlowContext::CreateContextVar(
    void     ** var,
    char      * name,
    int         size
    )
{
    int     retVal = HY_ERR;
    void      * ptr = NULL;

    MAPDBG_FUN3("D3dFlowContext::CreateContextVar");

    ptr = malloc(size);
    if ( ptr == NULL )
    {
        throw new Exception (true, "Var (%s) not created", name);
    }
    else
    {
        *var = ptr;
        retVal = HY_OK;
    }

    return retVal;
}


void D3dFlowContext::SetSizesAndFlags(void)
{
    MAPDBG_FUN2("D3dFlowContext::SetSizesAndFlags");

    //
    // Set frequently used vars for easy access
    //

    mMax   = *mmax;
    nMax   = *nmax;
    nMaxus = *nmaxus;
    kMax   = *kmax;

    lStsci = *lstsci;
    lTur   = *ltur;
    lSedtt = *lsedtt;
    Zmodel = *izmodl;
    Roller = *iroll;

    dDb   = *ddb;
    mMaxdb= *mmaxdb;
    nMaxdb= *nmaxdb;

    Hdt   = *hdt;

    //
    // Set array sizes for access macro's
    // m-size:  2 + dDb - 1;
    //          ( 2 extra columns, DD-edge, -1: Fortan->C )
    // n-size:      dDb - 1
    //          (                  DD-edge, -1: Fortan->C )
    //

    mArraySize   = mMaxdb+4;
    nArraySize   = nMaxdb;
    mArrayOffset = dDb + 1;
    nArrayOffset = dDb - 1;

#if LOG_VALUES
    printf("mMax = %d\n", mMax);
    printf("nMax = %d\n", nMax);
    printf("nMaxus = %d\n", nMaxus);
    printf("kMax = %d\n", kMax);

    printf("lStsci = %d\n", lStsci);
    printf("lTur = %d\n", lTur);
    printf("lSedtt = %d\n", lSedtt);

    printf("dDb = %d\n", dDb);
    printf("mMaxdb = %f\n", mMaxdb);
    printf("nMaxdb = %f\n", nMaxdb);

    printf("Hdt = %f\n", Hdt);

    printf("mArraySize = %d\n", this->mArraySize );
    printf("nArraySize = %d\n", this->nArraySize );
    printf("mArrayOffset = %d\n", this->mArrayOffset );
    printf("nArrayOffset = %d\n", this->nArrayOffset );
#endif


}


void D3dFlowContext::SetOtherContext(
    D3dFlowContext    * otherC      // pointer to other context
    )
{
    MAPDBG_FUN2("D3dFlowContext::SetOtherContext");

    //
    // Store pointer to other context
    // Check refinements
    //

    this->oC = otherC;

    if ( kMax > oC->kMax )
    {
        //
        // Current context is fine one.
        // Set 'upwards' and 'downwards' K-layers to be included
        // (the refinement factore minus one).
        // K-layers extend to up, so 'refDown' remains 0 (as intialized).
        //
        this->refUp   = (kMax/oC->kMax) - 1;
        ON_DEBUG( DBLEV7,
        CMapLog("Refined context, this->refUp: %d\n", this->refUp); )
    }
}


int D3dFlowContext::AttachContextVar(
    int     aContextId,
    void ** var,
    char  * name
    )
{
    int     retVal = HY_ERR;
    void      * ptr = NULL;

    MAPDBG_FUN3("MapContext::AttachContextVar");

    //
    // Check if var's are always attached to the same context
    //

    if ( this->contextID == YET_TO_INIT )
    {
        this->contextID = aContextId;
    }

    if ( aContextId != this->contextID )
    {
        throw new Exception (true, "Trying to attach var. to wrong context");
    }
    else
    {
        ptr = ESM_Alloc ( aContextId, name, 0 );
        if ( ptr == NULL )
        {
            throw new Exception (true, "Var (%s) not attached to Context (%d)", name, aContextId);
        }
        else
        {
            *var = ptr;
            retVal = HY_OK;
        }
    }

    return retVal;
}


void D3dFlowContext::DumpEsmContext(void)
{
    MAPDBG_FUN2("MapContext::DumpEsmContext");

    CMapLog((char*)"Dumping Shared Memory For Context %d\n", contextID);
    ESM_ListRegions (contextID,logFile);
}


void D3dFlowContext::CMapLog(
    char      * format,     /* I: 'fprintf-format' for print of log */
    ...             /* I: arguments of log message (should be
                    terminated with NULL)       */
    )
{
    char    name[100];  // filename
    va_list arguments;  // var-arg list

    if ( ! DoLoggingFor(Log_Context) )
    {
        return;
    }

    va_start ( arguments, format );

    if ( DoLoggingToFileFor(Log_Context) )
    {
        if (logFile == NULL)
        {
            sprintf(name, "context.%d", this->contextID);
            logFile = fopen(name,"w");
            if (logFile == NULL)
            {
                throw new Exception (true, "Couldn't open logfile %s", name);
            }
        }
        if ( logFile != NULL )
        {
            vfprintf ( logFile, format, arguments );
            fflush(logFile);
        }
    }
    else
    {
        vprintf ( format, arguments );
        fflush(stdout);
    }

    va_end ( arguments );

}
