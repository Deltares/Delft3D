//-------------------------------------------------------------------------------
//  DelftOnline -- Server::ClientConnection Routines
//
//  Irv.Elshoff@Deltares.NL
//  3 jul 12
//-------------------------------------------------------------------------------
//---- LGPL --------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2024.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation version 2.1.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, see <http://www.gnu.org/licenses/>.
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
// $Id$
// $HeadURL$


#include "dol.h"

namespace DOL {


//-------------------------------------------------------------------------------


static void *
clientServiceInitiator (
    void *
    );


//-------------------------------------------------------------------------------
//  Constructor/Destructor


Server::ClientConnection::ClientConnection (
    int                 clientID,
    Server *            server,
    int                 sock,
    struct sockaddr *   addr
    ) {

    server->clients->Append ((void *) this);
    server->status.clients++;

    struct sockaddr_in * inaddr = (struct sockaddr_in *) addr;

    this->clientID  = clientID;
    this->curDir    = server->rootDir;
    this->server    = server;
    this->log       = server->log;
    this->sock      = sock;
    this->port      = ntohs (inaddr->sin_port);
    this->seqn      = -1000;

    uint32_t ipaddr = ntohl (inaddr->sin_addr.s_addr);
    sprintf (this->ipaddr, "%u.%u.%u.%u",
                    ((uint8_t *) &ipaddr)[3],
                    ((uint8_t *) &ipaddr)[2],
                    ((uint8_t *) &ipaddr)[1],
                    ((uint8_t *) &ipaddr)[0]
                    );

    // Setup a mutex to wait on for step or stop

    if (pthread_mutex_init (&this->sync, NULL) != 0)
        throw new Exception (true, "Pthreads error: Cannot create Server::ClientConnection step/stop mutex: %s", strerror (errno));
    if (pthread_mutex_lock (&this->sync) != 0)
        throw new Exception (true, "Pthreads error: Cannot initially lock Server::ClientConnection step/stop mutex: %s", strerror (errno));

    // Allocate a message buffer for communication.  There is one buffer per client that is
    // used for both receives (requests) and sends (replies)

    this->mesg = (Message::Header *) new (std::nothrow) unsigned char [Message::maxMesgSize];
    if (this->mesg == NULL)
        throw server->Error (true, "Server::ClientConnection", "Cannot allocate IPC buffer (%d bytes)", Message::maxMesgSize);

    // Create a thread to service incoming client requests

    if (pthread_create (&this->serviceThread, NULL, &clientServiceInitiator, (void *) this) != 0)
        throw server->Error (true, "Server::ClientConnection", "Pthreads error: Cannot create client service thread, errno=%d", errno);
    }


Server::ClientConnection::~ClientConnection (
    void
    ) {

    delete [] this->mesg;
    this->server->status.clients--;
    this->server->clients->Delete ((void *) this);
    }


//-------------------------------------------------------------------------------
//  Client Service Thread


static void *
clientServiceInitiator (
    void * argument
    ) {

    Server::ClientConnection * cc = (Server::ClientConnection *) argument;
    try {
        cc->ServiceThread ();
        }
    catch (Exception * ex) {
        cc->log->Write (ERROR, "Connection thread for client %d terminating due to exception: %s", cc->clientID, ex->message);
        }

    delete cc;
    return NULL;
    }


void
Server::ClientConnection::ServiceThread (
    void
    ) {

    char logID [10];
    sprintf (logID, "CL%d", this->clientID);
    this->log->RegisterThread (logID);
    this->log->Write (INFO, "New client from %s:%u", this->ipaddr, this->port);

    // Get initial HELLO message from client. Contains key his key, which should match ours

    size_t received = 0;
    try {
        memset (this->mesg, 0, sizeof (Message::Header));
        received = Receive (this->sock, this->mesg);
        }
    catch (char * explanation) {
        throw new Exception (false, "Cannot receive HELLO message: %s", explanation);
        }

    if (this->mesg->magic != DOL_MAGIC_NUMBER)
        throw new Exception (false, "Client HELLO message contains an invalid magic number.  Do client and server versions match?");
    if (this->mesg->type != Message::HELLO)
        throw new Exception (false, "Initial client message is not HELLO");
    if (mesg->size != received - sizeof (Message::Header))
        throw new Exception (true, "HELLO message is wrong size");

    char * clientKey = this->mesg->payload;
    char * serverKey = &this->server->handle[strlen (this->server->handle) - URLkeyLength];

    // Verify key and tell client we're ready

    if (strcmp (clientKey, serverKey) != 0) {
        try {
            this->mesg->type = Message::GOODBYE;
            this->mesg->size = 0;
            this->mesg->seqn = --this->seqn;
            Send (this->sock, this->mesg);
            }
        catch (char * explanation) {
            throw new Exception (true, "Send of key mismatch GOODBYE fails: %s", explanation);
            }
        throw new Exception (false, "Client key does not match server key (\"%s\" != \"%s\"", clientKey, serverKey);
        }

    this->mesg->value = (uint64_t) this->clientID;
    this->mesg->size = 0;
    this->mesg->seqn = --this->seqn;

    try {
        Send (this->sock, this->mesg);
        }
    catch (char * explanation) {
        throw new Exception (true, "Send of HELLO ACK fails: %s", explanation);
        }

    // Process client requests

    while (true) {
        try {
            memset (this->mesg, 0, sizeof (Message::Header));
            received = Receive (this->sock, this->mesg);
            }
        catch (char * explanation) {
            throw new Exception (false, "Cannot receive request message: %s", explanation);
            }

        if (this->mesg->magic != DOL_MAGIC_NUMBER)
            throw new Exception (false, "Client request message contains an invalid magic number.  Do client and server versions match?");

        int value = ((unsigned int) this->mesg->value) & 0xFFFFFFFF;

        this->log->Write (TRACE, "ServiceThread got %s request", MessageTypeString (mesg->type));

        switch (this->mesg->type) {
            case Message::ARRAY_SHAPE: {
                this->SendArrayShape ();
                break;
                }

            case Message::CALL_FUNCTION: {
                this->CallFunction ();
                break;
                }

            case Message::CHANGE_DIR: {
                this->ChangeDirectory ();
                break;
                }

            case Message::GET_DATA: {
                this->SendData ();
                break;
                }

            case Message::GET_DESCRIPTION: {
                this->SendString (this->server->description);
                break;
                }

            case Message::GET_DIR: {
                this->SendDirectory ();
                break;
                }

            case Message::GET_ELEMENT: {
                this->SendDataElement ();
                break;
                }

            case Message::GET_FUNCTION: {
                this->SendFunction ();
                break;
                }

            case Message::GET_THREADCOUNT: {
                this->Reply (this->server->numThreads, 0);
                break;
                }

            case Message::GET_THREADNAME: {
                this->SendString (this->server->thr[value]->name);
                break;
                }

            case Message::GOODBYE: {
                this->Reply (0, 0);
                return;
                }

            case Message::PUT_DATA: {
                this->PutData ();
                break;
                }

            case Message::SERVER_STATUS: {
                if (sizeof (Status) > Message::maxPayload)
                    throw new Exception (true, "Status structure too big for message payload!");

                memcpy (this->mesg->payload, &this->server->status, sizeof (Status));
                this->Reply (0, sizeof (Status));
                break;
                }

            case Message::START: {
                this->Reply ((int) this->server->Start (), 0);
                break;
                }

            case Message::STEP: {
                if (this->server->Step (value, this))
                    this->Reply ((int) this->server->status.milestone, 0);
                else
                    this->Reply (-1, 0);

                break;
                }

            case Message::STOP: {
                if (this->server->Stop (this))
                    this->Reply ((int) this->server->status.milestone, 0);
                else
                    this->Reply (-1, 0);

                break;
                }

            case Message::TERMINATE: {
                bool terminate = this->server->Terminate (this->clientID);
                this->Reply ((int) terminate, 0);
                if (terminate) exit (1);
                break;
                }

            default:
                throw new Exception (true, "Client request header type is invalid");

            }
        }
    }


void
Server::ClientConnection::Reply (
    int value,
    size_t size
    ) {

    if (size > Message::maxPayload)
          throw new Exception (false, "Reply message exceeds send buffer capacity (%d > %d)", size, Message::maxPayload);

    this->mesg->value = (uint64_t) value;
    this->mesg->size = size;
    this->mesg->seqn = --this->seqn;

    try {
        Send (this->sock, this->mesg);
        }
    catch (char * explanation) {
        throw new Exception (true, "Send of reply to %s fails: %s", MessageTypeString (this->mesg->type), explanation);
        }
    }


//-------------------------------------------------------------------------------
//  Client Request Handling Routines


void
Server::ClientConnection::CallFunction (
    void
    ) {

    const char * pn = ResolvePathName (this->mesg->payload, this->curDir->pathname);
    Function * func = this->server->LookupFunction (pn);
    if (func == NULL) {
        this->Reply ((int) false, 0);
        return;
        }

    int argument = ((unsigned int) this->mesg->value) & 0xFFFFFFFF;
    int value = (*func->function) (func->context, &argument);

    this->Reply (value, 0);
    }


void
Server::ClientConnection::ChangeDirectory (
    void
    ) {

    const char * pn = ResolvePathName (this->mesg->payload, this->curDir->pathname);
    Directory * dir = this->server->LookupDirectory (pn);
    if (dir == NULL) {
        this->Reply ((int) false, 0);
        return;
        }

    size_t len = strlen (dir->pathname) + 1;
    if (len > Message::maxPayload) {
        this->Reply ((int) false, 0);
        throw new Exception (false, "Directory pathname exceeds send buffer capacity for %s (%d > %d)", MessageTypeString (this->mesg->type), len, Message::maxPayload);
        }

    this->curDir = dir;
    memcpy (this->mesg->payload, dir->pathname, len);
    this->Reply ((int) true, len);
    }


void
Server::ClientConnection::PutData (
    void
    ) {

    const char * pn = ResolvePathName (this->mesg->payload, this->curDir->pathname);
    Server::DataElement * elt = this->server->LookupDataElement (pn);
    if (elt == NULL) {
        this->Reply ((int) false, 0);
        return;
        }

    char * data = this->mesg->payload + (((unsigned int) this->mesg->value) & 0xFFFFFFFF);
    memcpy (elt->address, data, elt->size);
    this->Reply ((int) true, 0);
    }


void
Server::ClientConnection::SendArrayShape (
    void
    ) {

    const char * pn = ResolvePathName (this->mesg->payload, this->curDir->pathname);

    Server::ArrayShape * ash;
    try {
        ash = this->server->LookupArrayShape (pn);
        }

    catch (Exception * ex) {
        this->Reply ((int) false, 0);
        this->log->Write (ERROR, "ERROR: %s", ex->message);
        if (ex->fatal) throw ex;
        return;
        }

    this->Reply ((int) true, ash->Serialize (this->mesg->payload, Message::maxPayload));
    }


void
Server::ClientConnection::SendDirectory (
    void
    ) {

    const char * pn = ResolvePathName (this->mesg->payload, this->curDir->pathname);
    Server::Directory * dir = this->server->LookupDirectory (pn);
    if (dir == NULL) {
        this->Reply ((int) false, 0);
        return;
        }

    this->Reply ((int) true, dir->Serialize (this->mesg->payload, Message::maxPayload));
    }


void
Server::ClientConnection::SendDataElement (
    void
    ) {

    const char * pn = ResolvePathName (this->mesg->payload, this->curDir->pathname);
    Server::DataElement * elt = this->server->LookupDataElement (pn);
    if (elt == NULL) {
        this->Reply ((int) false, 0);
        return;
        }

    this->Reply ((int) true, elt->Serialize (this->mesg->payload, Message::maxPayload));
    }


void
Server::ClientConnection::SendData (
    void
    ) {

    const char * pn = ResolvePathName (this->mesg->payload, this->curDir->pathname);
    Server::DataElement * elt = this->server->LookupDataElement (pn);
    if (elt == NULL) {
        this->Reply ((int) false, 0);
        return;
        }

    if (elt->size > Message::maxPayload) {
        this->log->Write (ERROR, "Cannot send %s because message buffer is too small (%d < %d)", elt->pathname, Message::maxPayload, elt->size);
        this->Reply ((int) false, 0);
        return;
        }

    memcpy (this->mesg->payload, elt->address, elt->size);
    this->Reply ((int) true, elt->size);
    }


void
Server::ClientConnection::SendFunction (
    void
    ) {

    this->mesg->size = 0;

    const char * pn = ResolvePathName (this->mesg->payload, this->curDir->pathname);
    Server::Function * func = this->server->LookupFunction (pn);
    if (func == NULL) {
        this->Reply ((int) false, 0);
        return;
        }

    this->Reply ((int) true, func->Serialize (this->mesg->payload, Message::maxPayload));
    }


void
Server::ClientConnection::SendString (
    const char * string
    ) {

    size_t len = strlen (string) + 1;
    if (len > Message::maxPayload) {
        this->Reply ((int) false, 0);
        throw new Exception (false, "String exceeds send buffer capacity (%d > %d) for %s request", len, Message::maxPayload, MessageTypeString (this->mesg->type));
        }

    memcpy (this->mesg->payload, string, len);
    this->Reply ((int) true, len);
    }

}
