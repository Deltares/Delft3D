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
// $Id: context-gawsside.cpp 878 2011-10-07 12:58:46Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/mapper/context-gawsside.cpp $
//------------------------------------------------------------------------------
//  Class: D3dFlowContextGawsSide
//  Context Attacher, Gaws side
//
//  Stef.Hummel@deltares.nl
//  Adri.Mourits@deltares.nl
//  1 jun 11
//-------------------------------------------------------------------------------


#include "flow2d3d.h"


#define LOG_GAWS_EXCHANGE 0

#if(LOG_GAWS_EXCHANGE)
#define ON_LOG_GAWS_EXCHANGE(code)   code
#else
#define ON_LOG_GAWS_EXCHANGE(code)
#endif




#define GAWS_COMM_POINTS_BLOCKSIZE  1000


//////////////////////////////////////////////////////////////////////
//
// D3dFlowContextGawsSide Class
//


//
// Public functions for creating and initializing a Gaws-side context.
//


D3dFlowContextGawsSide::D3dFlowContextGawsSide(void)
{
    MAPDBG_FUN2("D3dFlowContextGawsSide::D3dFlowContextGawsSide");
}


D3dFlowContextGawsSide::~D3dFlowContextGawsSide(void)
{
    MAPDBG_FUN2("D3dFlowContextMapSide::~D3dFlowContextGawsSide");
}


int D3dFlowContextGawsSide::Setup(
    Iterator * gaws,    // Gaws iterator
    Iterator * flow,    // D3dFlow iterator
    MemType aMemType    // (SharedMem | Distributed)
    )
{
    int retVal = HY_OK;

    MAPDBG_FUN2("D3dFlowContextGawsSide::Setup");

    this->gawsIterator = gaws;
    this->flowIterator = flow;
    this->memType      = aMemType;

    FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "GAWS \"%s\" WAITS for cID from flow \"%s\"",
                    this->gawsIterator->name, this->flowIterator->name);

    int incomingBlobID=0;
    Blob * cIdBlob = new Blob(&(this->contextID), sizeof(int));
    this->flowIterator->Receive(cIdBlob, &incomingBlobID);  // DDD-COMM: FLOW->GAWS, contextID
    delete cIdBlob;
    if ( incomingBlobID != F2G_Blob_ContextID )
    {
        throw new Exception (true, "D3dFlowContextGawsSide::Setup: unexpected blobID (%d /= %d)",
                    incomingBlobID, F2G_Blob_ContextID);
    }
    FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "GAWS \"%s\" RECEIVED cID from flow \"%s\"",
                    this->gawsIterator->name, this->flowIterator->name);

    if ( this->contextID == YET_TO_INIT )
    {
        throw new Exception (true, "GAWS didn't receive valid contextID from D3dFlow \"%s\"",
                        this->flowIterator->name);
    }

    //
    // In case of SharedMem:   Attach to esm context
    // In case of Distributed: Determine strips to be
    //                         communicated with flow size
    //

    if ( this->memType == Mem_Shared )
    {
        FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "Context (%d) connected", this->contextID);

        //
        //  Attach to size variables and to KCS Array
        //

        this->AttachVars(this->contextID);
        this->SetSizesAndFlags();
    }
    else
    {
        //
        //  Get sizes and KCS Array
        //

        this->ReceiveSizesFromFlow();                   // DDD-COMM: FLOW->GAWS, Subdomain sizes

        this->mArraySize   = this->mMaxdb+4;
        this->nArraySize   = this->nMaxdb;
        this->mArrayOffset = this->dDb + 1;
        this->nArrayOffset = this->dDb - 1;

        this->kcs = new int[this->mArraySize * this->nArraySize];

        FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "GAWS \"%s\" WAITS for KCS (%d*%d) from flow \"%s\"",
                        this->gawsIterator->name, this->mArraySize, this->nArraySize, this->flowIterator->name);

        Blob * kcsBlob = new Blob(this->kcs, this->mArraySize * this->nArraySize * sizeof(int) );
        this->flowIterator->Receive(kcsBlob, &incomingBlobID);           // DDD-COMM: FLOW->GAWS, KCS-array
        delete kcsBlob;
        if ( incomingBlobID != F2G_Blob_KCS_Array )
        {
            throw new Exception (true, "D3dFlowContextGawsSide::Setup: unexpected blobID (%d /= %d)",
                        incomingBlobID, F2G_Blob_KCS_Array);
        }
        FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "GAWS \"%s\" RECEIVED KCS from flow \"%s\"",
                        this->gawsIterator->name, this->flowIterator->name);

    }

    return retVal;
}


//
//
// Public functions for getting/setting a value of array in current context,
// given the M/N/K index (for integer Vars and FlowScalar Vars).
//


FlowScalar * D3dFlowContextGawsSide::Get_Adress(
    bool        l2r,            // L2R or B2T?
    int         m,              // M index
    int         n,              // N index
    GawsTerm    term            // get address of which term
    )
{
    FlowScalar        * retVal = NULL ; // pointer to FlowScalarVar
    FlowScalar        * var    = NULL ; // pointer to FlowScalarVar

    MAPDBG_FUN3("D3dFlowContextGawsSide::Get_Adress");

    switch ( term )
    {
        case GawsTerm_A:    var = wrka1;
            break;
        case GawsTerm_B:    var = wrka2;
            break;
        case GawsTerm_C:    var = wrka3;
            break;
        case GawsTerm_D:    var = wrka4;
            break;
    }

    if (var != NULL)
    {
        switch( this->memType )
        {
            case  Mem_Shared:

                retVal = A2D(var,m,n);
                break;

            case  Mem_Distributed:

                int i;
                for ( i = 0 ; ( i < this->numCommPoints ) && ( retVal == NULL ) ; i++ )
                {
                    if ( ( this->commPointAdmins[i].m   == m   ) &&
                         ( this->commPointAdmins[i].n   == n   ) &&
                         ( this->commPointAdmins[i].l2r == l2r )    )
                    {
                        retVal = var + i;
                    }
                }
                break;
        }
    }

    FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "D3dFlowContextGawsSide::Get_Adress for %d in \"%s\"(%s %2d,%2d): %x, value %12.5f",
                        term, this->flowIterator->name, (l2r ? "L2R" : "T2B"),
                        m, n, retVal, ( retVal == NULL ) ? 0.0 : *retVal );

    return retVal;
}


int D3dFlowContextGawsSide::Get_KCS_XorY(
    int  cell,           // cell-index on line
    int  line,           // line-index
    bool leftToRight     // boolean L->R or Top->Bottom
    )
{
    int     retVal = -1;    // return value

    MAPDBG_FUN3("D3dFlowContextGawsSide::Get_KCS_X_OR_Y");

    if ( leftToRight == 1 )
    {
            retVal = I2D(this->kcs,cell,line);
    }
    else
    {
            retVal = I2D(this->kcs,line,cell);
    }

    ON_DEBUG(DBLEV7,
        CMapLog("Get_KCS_X_OR_Y (line=%2d, cell=%2d), %s: %d\n",
        line,cell, ( leftToRight ? "leftToRight" : "topToBottom" ), retVal);
        )

    return retVal;
}


void D3dFlowContextGawsSide::SetupCommunication(void)
{

    MAPDBG_FUN2("D3dFlowContextGawsSide::SetupCommunication");

    if ( this->memType == Mem_Distributed )
    {
        //
        // Allocate the array 'strips' to be communicated
        //

        this->wrka1 = new REAL_FP[this->numCommPoints];
        this->wrka2 = new REAL_FP[this->numCommPoints];
        this->wrka3 = new REAL_FP[this->numCommPoints];
        this->wrka4 = new REAL_FP[this->numCommPoints];

        for ( int i = 0 ; i < this->numCommPoints ; i++ )
        {
            this->wrka1[i] = 0.0;
            this->wrka2[i] = 0.0;
            this->wrka3[i] = 0.0;
            this->wrka4[i] = 0.0;
        }

        //
        // Send information on Comm. Points
        // (Received by FlowSide->SetupForGaws)
        // - #communication points
        // - communication point administration
        //

        FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "GAWS \"%s\" SENDS numCommPoints to flow \"%s\"",
                        this->gawsIterator->name, this->flowIterator->name);

        Blob * numCommPointsBlob = new Blob(&(this->numCommPoints), sizeof(int));
        this->flowIterator->Send(numCommPointsBlob, G2F_Blob_NumCommPoints);     // DDD-COMM: GAWS->FLOW, #Comm.Points
        delete numCommPointsBlob;

        FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "GAWS \"%s\" HAS SENT numCommPoints (%d) to flow \"%s\"",
                        this->gawsIterator->name, this->numCommPoints, this->flowIterator->name);

        if (this->numCommPoints > 0)
        {
            FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "GAWS \"%s\" SENDS commPoints to flow \"%s\"",
                            this->gawsIterator->name, this->flowIterator->name);

            Blob * commPointsBlob = new Blob(this->commPointAdmins,
                                        this->numCommPoints * sizeof(GawsCommPointAdmin) );
            this->flowIterator->Send(commPointsBlob, G2F_Blob_CommPoints);       // DDD-COMM: GAWS->FLOW, Comm.Points
            delete commPointsBlob;

            FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "GAWS \"%s\" HAS SENT commPoints to flow \"%s\"",
                            this->gawsIterator->name, this->flowIterator->name);
        }
    }
}


void D3dFlowContextGawsSide::UpdateGawsToFlow(GawsDistribGroup distribGroup)
{
    // return; // TODO REACTIVATE

    MAPDBG_FUN2("D3dFlowContextGawsSide::UpdateGawsToFlow");

    if ( this->memType == Mem_Distributed )
    {
        if ( this->flowIterator == NULL )
        {
            throw new Exception (true, "UpdateGawsToFlow: flowIterator not set for Gaws \"%s\"",
                this->gawsIterator == NULL ? "????" : this->gawsIterator->name );
        }
        else
        {
            //
            // Gather bytes to be sent
            //
            if ( ( this->numCommPoints > 0 ) && ( distribGroup == GawsDistrib_AllVars ) )
            {
                int numBytes = this->numCommPoints * sizeof(REAL_FP) * NUM_EXCHANGED_GAWS_VARS;
                char * bytes = new char[numBytes];

                int numBufferedBytes = 0;
                numBufferedBytes += this->BufferVarOnCommPoints(bytes                   , this->wrka1, BufferAction_Fill);
                numBufferedBytes += this->BufferVarOnCommPoints(bytes + numBufferedBytes, this->wrka2, BufferAction_Fill);
                numBufferedBytes += this->BufferVarOnCommPoints(bytes + numBufferedBytes, this->wrka3, BufferAction_Fill);
                numBufferedBytes += this->BufferVarOnCommPoints(bytes + numBufferedBytes, this->wrka4, BufferAction_Fill);

                FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "GAWS \"%s\" SENDS Update to flow \"%s\"",
                                this->gawsIterator->name, this->flowIterator->name);

                Blob * sendBlob = new Blob(bytes, numBufferedBytes);
                this->flowIterator->Send(sendBlob, G2F_Blob_Update);   // ddd-comm: gaws->flow, update
                delete sendBlob;

                delete [] bytes;

                FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "GAWS \"%s\" HAS SENT Update to flow \"%s\"",
                                this->gawsIterator->name, this->flowIterator->name);

            }
        }

    }
}


void D3dFlowContextGawsSide::UpdateGawsFromFlow(GawsDistribGroup distribGroup)
{

    MAPDBG_FUN2("D3dFlowContextGawsSide::UpdateGawsFromFlow");

    if ( this->memType == Mem_Distributed )
    {
        if ( this->flowIterator == NULL )
        {
            throw new Exception (true, "UpdateGawsFromFlow: flowIterator not set for Gaws \"%s\"",
                this->gawsIterator == NULL ? "????" : this->gawsIterator->name );
        }
        else
        {
            //
            // Receive buffer with values on Comm. Points.
            //

            int numBytes = this->numCommPoints * sizeof(REAL_FP) * NUM_EXCHANGED_GAWS_VARS;
            char * bytes = new char[numBytes];

            FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "GAWS \"%s\" WAITS for Update from flow \"%s\"",
                            this->gawsIterator->name, this->flowIterator->name);

            int incomingBlobID=0;
            Blob * receiveBlob = new Blob(bytes, numBytes);
            this->flowIterator->Receive(receiveBlob, &incomingBlobID);  // ddd-comm: flow->gaws, update
            delete receiveBlob;
            if ( incomingBlobID != F2G_Blob_Update )
            {
                throw new Exception (true, "D3dFlowContextGawsSide::UpdateGawsFromFlow: unexpected blobID (%d /= %d)",
                            incomingBlobID, F2G_Blob_Update);
            }

            FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "GAWS \"%s\" RECEIVED Update from flow \"%s\"",
                            this->gawsIterator->name, this->flowIterator->name);

            // TODORE: check #expected with #received bytes

            //
            // Put received bytes on the right place in the work arrays
            //

            if ( ( this->numCommPoints > 0 ) && ( distribGroup == GawsDistrib_AllVars ) )
            {
                int numBytes = 0;

                numBytes += this->BufferVarOnCommPoints(bytes           , this->wrka1, BufferAction_Read);
                numBytes += this->BufferVarOnCommPoints(bytes + numBytes, this->wrka2, BufferAction_Read);
                numBytes += this->BufferVarOnCommPoints(bytes + numBytes, this->wrka3, BufferAction_Read);
                numBytes += this->BufferVarOnCommPoints(bytes + numBytes, this->wrka4, BufferAction_Read);
                // TODORE: check #received with #read bytes
            }

        delete [] bytes;
        }
    }
}


void D3dFlowContextGawsSide::AddCommPointAdmin(
    int                 m,              // m-index of point
    int                 n,              // n-index of point
    bool                l2r             // L2R or B2T?
    )
{

    if ( this->memType != Mem_Shared )
    {
        MAPDBG_FUN3("D3dFlowContextGawsSide::AddCommPointAdmin");

        if (this->numCommPoints == 0)
        {
            //
            // allocate initial block for communicated Gaws zeta points
            //
            this->commPointAdmins = new GawsCommPointAdmin[GAWS_COMM_POINTS_BLOCKSIZE];
        }
        else
        {
            if ( ( this->numCommPoints % GAWS_COMM_POINTS_BLOCKSIZE ) == 0 )
            {
                //
                // max num communicated Gaws zeta points reached, realloc.
                //
                GawsCommPointAdmin * oldCommPointAdmins = this->commPointAdmins;
                this->commPointAdmins = new GawsCommPointAdmin[this->numCommPoints+GAWS_COMM_POINTS_BLOCKSIZE];

                for ( int cp = 0 ; cp < this->numCommPoints ; cp++ )
                {
                    this->commPointAdmins[cp] = oldCommPointAdmins[cp];
                }
                delete [] oldCommPointAdmins;
            }
        }

                bool allreadyThere = false;
                for ( int cp = 0 ; cp < this->numCommPoints ; cp++ )
                {
                        if (this->commPointAdmins[cp].m   == m   &&
                                this->commPointAdmins[cp].n   == n   &&
                                this->commPointAdmins[cp].l2r == l2r )
                        {
                                allreadyThere = true;
                                break;
                        }
                }

                if ( ! allreadyThere )
                {
                        this->commPointAdmins[this->numCommPoints].m   = m   ;
                        this->commPointAdmins[this->numCommPoints].n   = n   ;
                        this->commPointAdmins[this->numCommPoints].l2r = l2r ;

                        FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "AddedCommPoint in \"%s\": %3d,%3d (type=%d)",
                                                                                this->flowIterator->name, m, n, l2r);
                        this->numCommPoints++;

                }

    }
}


//
// Protected functions for communication with flow
//

void D3dFlowContextGawsSide::ReceiveSizesFromFlow(void)
{
    MAPDBG_FUN2("D3dFlowContextGawsSide::ReceiveSizesFromFlow");

        if ( this->flowIterator == NULL )
    {
        throw new Exception (true, "ReceiveSizesFromFlow: flowIterator not set for Mapper \"%s\"",
            this->gawsIterator == NULL ? "????" : this->gawsIterator->name );
    }
    else
    {
        Flow2GawsSizes * sizes = new Flow2GawsSizes;

        FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "GAWS \"%s\" WAITS for sizes from flow \"%s\"",
                        this->gawsIterator->name, this->flowIterator->name);

        int incomingBlobID=0;
        Blob * sizesBlob = new Blob(sizes, sizeof(Flow2GawsSizes));
        this->flowIterator->Receive(sizesBlob, &incomingBlobID);     // ddd-comm: flow->gaws, sizes
        delete sizesBlob;
        if ( incomingBlobID != F2G_Blob_SubdomainSizes )
        {
            throw new Exception (true, "D3dFlowContextGawsSide::ReceiveSizesFromFlow: unexpected blobID (%d /= %d)",
                        incomingBlobID, F2G_Blob_SubdomainSizes);
        }

        FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "GAWS \"%s\" RECEIVED sizes from flow \"%s\"",
                        this->gawsIterator->name, this->flowIterator->name);

        this->mMax   = sizes->mMax  ;
        this->nMax   = sizes->nMax  ;
        this->nMaxus = sizes->nMaxus;
        this->kMax   = sizes->kMax  ;
        this->dDb    = sizes->dDb   ;
        this->mMaxdb = sizes->mMaxdb;
        this->nMaxdb = sizes->nMaxdb;

        FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "\"%s\"<-\"%s\": mMx=%d, nMx=%d, nMxus=%d, kMx=%d, dDb=%d, mMxdb=%d, nMxdb=%d",
                        this->gawsIterator->name, this->flowIterator->name,
                        mMax, nMax, nMaxus, kMax, dDb, mMaxdb, nMaxdb
                        );

        delete sizes;
    }
}


int D3dFlowContextGawsSide::BufferVarOnCommPoints(
    char          * buffer,        // buffer to be filled or read
    REAL_FP       * var,           // var (1D-array) to be stored or read
    BufferAction    bufferAction   // Fill or Read buffer
    )
{

    MAPDBG_FUN3("D3dFlowContextGawsSide::BufferVarOnCommPoints");

    int numBytes = 0;
    REAL_FP * copyTarget = (REAL_FP *) buffer;
    // return 0; // TODO REACTIVATE

    int i;
    for ( i = 0 ; i < this->numCommPoints ; i++ )
    {
        if ( bufferAction == BufferAction_Read )
        {
            * (var + i) = *copyTarget++;
            ON_LOG_GAWS_EXCHANGE(
                FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "GAWSBufferComm Read from \"%10s\" %x (%3i): %10.4f",
                        this->flowIterator->name, var, i, * (var + i)  );
                fflush(stdout);
                )
        }
        else
        {
            *copyTarget++ = * (var + i);
            ON_LOG_GAWS_EXCHANGE(
                FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "GAWSBufferComm Fill for  \"%10s\" %x (%3i): %10.4f",
                        this->flowIterator->name, var, i, * (var + i)  );
                fflush(stdout);
                )
        }
        numBytes += sizeof(REAL_FP);
    }
    return numBytes;

}
