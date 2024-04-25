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
// $Id: iterator.cpp 962 2011-10-31 21:52:47Z elshoff $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/iterator.cpp $
//------------------------------------------------------------------------------
//  d_hydro Flow2D3D Component
//  Iterator Class - IMPLEMENTATION
//
//  Irv.Elshoff@Deltares.NL
//  30 oct 11
//-------------------------------------------------------------------------------


#include "flow2d3d.h"


//-------------------------------------------------------------------------------
//  Constructor and destructor


Iterator::Iterator (
    DD * dd,
    const char * name,
    Blob * configblob,
    Category * category,
    IteratorFunction function
    ) {

    // Validate constructor arguments

    if (name == NULL || name[0] == '\0')
        throw new Exception (true, "Iterator does not have a name");
    if (strlen (name) >= DD::MAXSTRING)
        throw new Exception (true, "Iterator name is too long");
    if (dd->iteratorDict->Lookup (name) != (void *) Dictionary::NOTFOUND)
        throw new Exception (true, "Duplicate iterator name \"%s\"", name);
    if (category == NULL)
        throw new Exception (true, "No category for iterator \"%s\"", name);
    if (function == NULL)
        throw new Exception (true, "Null iterator function for \"%s\"", name);

    char * cname = category->name;
    long catid = (long) dd->categoryDict->Lookup (cname);
    if (catid == Dictionary::NOTFOUND)
        throw new Exception (true, "Cannot find category \"%s\" for iterator \"%s\"", cname, name);

    // Allocate new slot in configuration table

    if ((this->id = dd->numIterators++) >= DD::MAXITERATORS)
        throw new Exception (true, "Configuration iterator table is full (> %d entries)", DD::MAXITERATORS);

    // Fill in object members

    this->dd = dd;
    this->name = new char [strlen (name)+1];
    strcpy (this->name, name);

    int blobsize;
    if (configblob == NULL) {
        this->configblob = new Blob (NULL, 0);
        blobsize = 0;
        }
    else {
        this->configblob = configblob;
        blobsize = configblob->Size ();
        }

    this->all.count     = 0;
    this->all.neighbors = new List ();
    this->all.rewound   = false;
    this->category      = category;
    this->detached      = false;
    this->function      = function;
    this->neighDict     = new Dictionary ("Neighbor");
    this->node          = NULL;
    this->sync          = new Semaphore (this->name, 0, dd->log);
    this->value         = NULL;

    // Fill in config table entries

    dd->iterator[this->id].iterator = this;
    strcpy (dd->iterator[this->id].name, name);
    if (blobsize > 0)
        memcpy (dd->iterator[this->id].blob, configblob->Address (), blobsize);

    dd->iterator[this->id].blobsize = blobsize;
    dd->iterator[this->id].catID    = catid;
    dd->iterator[this->id].detached = false;
    dd->iterator[this->id].function = function;
    dd->iterator[this->id].thread   = false;

    // Add iterator to object dictionary

    dd->iteratorDict->Insert ((char *) name, (void *) this->id);

    dd->log->Write (Log::ITER_MAJOR, "Created iterator #%d \"%s\" in category \"%s\"", this->id, this->name, category->name);
    }


Iterator::~Iterator (
    void
    ) {

    // ToDo: Delete everything allocated, for formality only since
    // iterator objects are destroyed only at the end of a simpulation
    }


//-------------------------------------------------------------------------------
//  Neighbor-related functions


void
Iterator::AddNeigh (
    Iterator * neighbor
    ) {

    // If this iterator does not already have a neighbor in the category of
    // the new neighbor, create a new descriptor and put it in the dictionary.

    CatNeigh * catNeigh = (CatNeigh *) this->neighDict->Lookup (neighbor->category->name);

    if ((long) catNeigh == Dictionary::NOTFOUND) {
        catNeigh = new CatNeigh ();
        catNeigh->neighbors = new List ();
        catNeigh->count = 0;
        catNeigh->rewound = false;
        this->neighDict->Insert (neighbor->category->name, (void *) catNeigh);
        }

    // Add new neighbor to category and all-neighbor lists

    catNeigh->neighbors->Append (neighbor);
    catNeigh->count++;
    this->all.neighbors->Append (neighbor);
    this->all.count++;
    }


unsigned int
Iterator::NeighborCount (
    Category * category
    ) {

    CatNeigh * catNeigh;

    if (category == NULL)
        catNeigh = &this->all;
    else {
        catNeigh = (CatNeigh *) this->neighDict->Lookup (category->name);
        if ((long) catNeigh == Dictionary::NOTFOUND)
            return 0;
        }

    return catNeigh->count;
    }


void
Iterator::RewindNeighbors (
    Category * category
    ) {

    CatNeigh * catNeigh;

    if (category == NULL)
        catNeigh = &this->all;
    else {
        catNeigh = (CatNeigh *) this->neighDict->Lookup (category->name);
        if ((long) catNeigh == Dictionary::NOTFOUND) {
            this->dd->log->Write (Log::ALWAYS, "Iterator \"%s\" does not have neighbors in category \"%s\"", this->name, category->name);
            return;
            }
        }

    catNeigh->neighbors->Rewind ();
    catNeigh->rewound = true;
    }


Iterator *
Iterator::NextNeighbor (
    Category * category
    ) {

    CatNeigh * catNeigh;

    if (category == NULL)
        catNeigh = &this->all;
    else {
        catNeigh = (CatNeigh *) this->neighDict->Lookup (category->name);
        if ((long) catNeigh == Dictionary::NOTFOUND)
            throw new Exception (true, "Iterator \"%s\" does not have neighbors in category \"%s\"", this->name, category->name);
        }

    if (! catNeigh->rewound)
        throw new Exception (true, "Neighbor list of \"%s\" not rewound before Next in category \"%s\"", this->name, category->name);

    return (Iterator *) catNeigh->neighbors->Next ();
    }


//-------------------------------------------------------------------------------


void
Iterator::Detach (
    void
    ) {

    this->detached = true;
    this->dd->iterator[id].detached = true;
    this->dd->log->Write (Log::ITER_MINOR, "Detached iterator \"%s\"", this->name);
    }


void
Iterator::Place (
    Node * node
    ) {

    this->node = node;
    node->AddIterator (this);

    dd->log->Write (Log::ITER_MAJOR, "Placed iterator #%d \"%s\" on node %d", this->id, this->name, this->node->nodeID);
    }


int
Iterator::FindJoin (
    int     iter1,
    int     iter2
    ) {

    // ToDo:  Get rid of the ridiculously inefficent search

    for (int jid = 0 ; jid < this->dd->numJoins ; jid++)
        if (this->dd->join[jid].iter1 == iter1 && this->dd->join[jid].iter2 == iter2 ||
            this->dd->join[jid].iter1 == iter2 && this->dd->join[jid].iter2 == iter1)
            return jid;

    throw new Exception (true, "Internal error: Cannot find (%d,%d) in join table", iter1, iter2);
    return 0;
    }


//-------------------------------------------------------------------------------


void
Iterator::Send (
    Blob * message,
    int tag
    ) {

    // Find entry in join table for sender and receiver

    this->dd->log->Write (Log::ITER_MINOR, "IteratorSend called");
    int senderid = this->dd->DDGetThreadID ();
    if (this->id == senderid)
        throw new Exception (true, "Iterator \"%s\" in \"%s\" is trying to send itself a message",
                        this->name,
                        this->dd->category[this->dd->iterator[senderid].catID].name
                        );

    int jid = this->FindJoin (this->id, senderid);
    if (this->dd->join[jid].local) {
        // Determine which channel to use

        Channel * channel;
        if (this->id == this->dd->join[jid].iter1)
            channel = &this->dd->join[jid].a2b;
        else
            channel = &this->dd->join[jid].b2a;

        // Copy message into buffer and signal that there is a message

        this->SendMessage (message, tag, channel);
        channel->sync->VSem ();
        }

    else {
        this->dd->join[jid].stream->Send ((char *) message->Address(), message->Size());
        this->dd->join[jid].stream->Send ((char *) &tag, sizeof tag);
        }

    const char * lr = this->dd->join[jid].local ? "local" : "remote";
    Iterator * sender = this->dd->iterator[senderid].iterator;
    this->dd->log->Write (Log::DD_SENDRECV, "%s Send %s(%d) -> %s(%d) tag=%d, size=%d", lr, sender->name, sender->id, this->name, this->id, tag, message->Size());
    }


void
Iterator::Receive (
    Blob * message,
    int * ptag
    ) {

    int tag;

    this->dd->log->Write (Log::ITER_MINOR, "IteratorReceive called");
    int senderid = this->dd->DDGetThreadID ();
    if (this->id == senderid)
        throw new Exception (true, "Iterator \"%s\" in \"%s\" is trying to receive a message from itself",
                        this->name,
                        this->dd->category[this->dd->iterator[senderid].catID].name
                        );

    int jid = this->FindJoin (this->id, senderid);
    if (this->dd->join[jid].local) {
        // Determine which channel to use

        Channel * channel;
        if (this->id == this->dd->join[jid].iter2)
            channel = &this->dd->join[jid].a2b;
        else
            channel = &this->dd->join[jid].b2a;

        // Wait for buffer to be filled with a message, then copy data from buffer

        channel->sync->PSem ();
        this->ReceiveMessage (message, &tag, channel);
        }

    else {
        this->dd->join[jid].stream->Receive ((char *) message->Address(), message->Size());
        this->dd->join[jid].stream->Receive ((char *) &tag, sizeof tag);
        }

    if (ptag == NULL)
        tag = 0;
    else
        *ptag = tag;

    const char * lr = this->dd->join[jid].local ? "local" : "remote";
    Iterator * sender = this->dd->iterator[senderid].iterator;
    this->dd->log->Write (Log::DD_SENDRECV, "%s Receive %s(%d) -> %s(%d) tag=%d, size=%d", lr, this->name, this->id, sender->name, sender->id, tag, message->Size());
    }



//-------------------------------------------------------------------------------


void
Iterator::SendMessage (
    Blob * message,
    int tag,
    Channel * channel
    ) {

    if (pthread_mutex_lock (&channel->mutex) != 0)
        throw new Exception (true, "Cannot acquire mutex on local message channel for send");

    // Determine slot to use
    if (++channel->head == this->MESGBUFSIZE) channel->head = 0;
    int slot = channel->head;
    if (channel->head == channel->tail)
        throw new Exception (true, "Message buffer full (%d slots)", this->MESGBUFSIZE);

    // Release previous buffer
    if (channel->ring[slot].data != NULL)
        delete [] channel->ring[slot].data;

    // Copy data from user memory to fresh buffer
    char * buf = new char [message->Size()];
    memcpy (buf, message->Address (), message->Size());

    // Fill the next slot of the ring buffer
    channel->ring[slot].data = buf;
    channel->ring[slot].size = message->Size();
    channel->ring[slot].tag  = tag;

    if (pthread_mutex_unlock (&channel->mutex) != 0)
        throw new Exception (true, "Cannot relinquish mutex on local message channel for send");

    this->dd->log->Write (Log::DD_SENDRECV, "Put message of size %d in slot %d", message->Size(), slot);
    }


void
Iterator::ReceiveMessage (
    Blob * message,
    int *ptag,
    Channel * channel
    ) {

     if (pthread_mutex_lock (&channel->mutex) != 0)
        throw new Exception (true, "Cannot acquire mutex on local message channel for receive");

    // Determine slot to use
    if (++channel->tail == this->MESGBUFSIZE) channel->tail = 0;
    int slot = channel->tail;

    // Copy data from buffer to user memory and release buffer
    memcpy (message->Address (), channel->ring[slot].data, channel->ring[slot].size);
    // ToDo: set blob size?
    *ptag = channel->ring[slot].tag;

    if (pthread_mutex_unlock (&channel->mutex) != 0)
        throw new Exception (true, "Cannot relinquish mutex on local message channel for receive");

    this->dd->log->Write (Log::DD_SENDRECV, "Got message of size %d from slot %d", message->Size(), slot);
    }


//-------------------------------------------------------------------------------


void
Iterator::SetValue (
    void * value
    ) {

    this->value = value;
    }


void *
Iterator::GetValue (
    void
    ) {

    return this->value;
    }


//-------------------------------------------------------------------------------


void
Iterator::Ready (
    void
    ) {

    this->dd->log->Write (Log::ITER_MAJOR, "Iterator \"%s\" ready to enter simulation phase", this->name);
    this->dd->initSync->VSem ();
    this->sync->PSem ();
    }


//-------------------------------------------------------------------------------
//  Extra-object functions


Iterator *
IteratorSelf (
    void
    ) {

    FLOW2D3D->dd->log->Write (Log::ITER_MINOR, "IteratorSelf called");
    int iid = FLOW2D3D->dd->DDGetThreadID ();
    return FLOW2D3D->dd->iterator[iid].iterator;
    }


#include <typeinfo>
using namespace std;


void *
IteratorShell (
    void * argument
    ) {

    int iid = (long) argument;     // index in DD iterator table
    Iterator * iter = FLOW2D3D->dd->iterator[iid].iterator;

    try {
        if (pthread_setspecific (FLOW2D3D->dd->thiter, (void *) &iid) != 0)
            throw new Exception (true, "Pthreads error in IteratorShell: Cannot set thread-specific key for iterator thread, errno=%d", errno);

        char * hnpid = GetHostnamePID ();
        char *threadName = new char[strlen (hnpid) + 100];
        sprintf (threadName, "%s slave:%d %s", hnpid, iter->node->nodeID, iter->name);
        iter->dd->log->RegisterThread (threadName);
        delete [] hnpid;
        delete [] threadName;

        iter->dd->log->Write (Log::ITER_MAJOR, "IteratorShell \"%s\" \"%s\" is waiting for initialization signal",
                iter->category->name,
                iter->name
                );

        iter->sync->PSem ();

        Blob * configblob = new Blob (
                FLOW2D3D->dd->iterator[iid].blobsize == 0 ? NULL : FLOW2D3D->dd->iterator[iid].blob,
                FLOW2D3D->dd->iterator[iid].blobsize
                );

        iter->dd->log->Write (Log::ITER_MAJOR, "IteratorShell: Invoking function for \"%s\" \"%s\"...",
                iter->category->name,
                iter->name
                );

        FLOW2D3D->dd->iterator[iid].function (
                iter,
                iter->name,
                configblob
                );

        iter->dd->log->Write (Log::ITER_MAJOR, "IteratorShell: Function for \"%s\" \"%s\" has terminated",
                iter->category->name,
                iter->name
                );

        return NULL;
        }

    catch (exception& ex) {
        printf ("d_hydro ABORT: C++ Exception in iterator: %s\n", ex.what());
        exit (1);
        }

    catch (Exception *ex) {
        printf ("d_hydro ABORT: [Iterator] %s\n", ex->message);
        exit (1);
        // ToDo: signal main thread for an orderly shutdown
        }
    }
