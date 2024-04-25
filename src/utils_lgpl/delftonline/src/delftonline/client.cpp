//-------------------------------------------------------------------------------
//  DelftOnline -- C++ Client API Routines
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


#if defined(WIN32) || defined(WIN64)
#   define vsnprintf _vsnprintf
#endif


namespace DOL {

bool Client::initialized = false;

void
Client::initialize (
    void
    ) {

    if (! Client::initialized) {
        Client::initialized = true;
#if defined (WIN32) || defined(WIN64)
        WORD versionRequested = MAKEWORD (1, 1);
        WSADATA wsaData;
        if (WSAStartup (versionRequested, &wsaData) != 0)
            throw new Exception (true, "Windows Socket API initialization fails");
#endif
        }
    }


//-------------------------------------------------------------------------------
//  Constructor/Destructor


Client::Client (
    const char *    url,
    int             verbosity,
    const char *    logfile
    ) {

    Client::initialize ();

    // Unpack the URL string

    if (url == NULL)
        throw new Exception (true, "Client constructor: Null URL");
    if (strstr (url, URLprefix) != url)
        throw new Exception (true, "Client constructor: Malformed DOL URL: Invalid prefix");

    char * myURL = strdup (url);
    char * hostname = myURL + strlen (URLprefix);
    char * colon = strchr (hostname, ':');
    if (colon == NULL)
        throw new Exception (true, "Client constructor: Malformed DOL URL: No port specification");

    *colon++ = '\0';
    char * slash = strchr (colon, '/');
    if (slash == NULL)
        throw new Exception (true, "Client constructor: Malformed DOL URL: No authentication key");

    int port = atoi (colon);

    *slash++ = '\0';
    char * key = slash;
    if (strlen (key) != URLkeyLength)
        throw new Exception (true, "Client constructor: Malformed DOL URL: Authentication key not the correct length");

    this->hostname  = strdup (hostname);
    this->port      = port;
    this->key       = strdup (key);

    free (myURL);

    // Connect to the DOL server

    if (! LookupHostname (this->hostname, &this->addr))
        throw new Exception (true, "Cannot resolve host \"%s\"", this->hostname);

    this->inaddr = (struct sockaddr_in *) &this->addr;
    this->inaddr->sin_family = AF_INET;
    this->inaddr->sin_port = htons (this->port);

    if ((this->sock = socket (PF_INET, SOCK_STREAM, IPPROTO_TCP)) == INVALID_SOCKET)
        throw new Exception (true, "Cannot create socket: (%d) %s\n", errno, strerror (errno));

    socklen_t addrlen = sizeof (struct sockaddr);
    if (connect (this->sock, &this->addr, addrlen) != 0)
        throw new Exception (true, "Cannot connect to server: (%d) %s", errno, strerror (errno));

    // Allocate a message buffer for communication; both sends (requests) and receives (replies)

    this->mesg = (Message::Header *) new (std::nothrow) unsigned char [Message::maxMesgSize];
    if (this->mesg == NULL)
        throw new Exception (true, "Cannot allocate IPC buffer (%d bytes)", Message::maxMesgSize);

    // Send key to server in a hello message

    this->seqn = 1000;

    try {
        size_t payloadSize = strlen (this->key) + 1;
        if (payloadSize > Message::maxPayload)
              throw new Exception (true, "Initial key-exchange message exceeds send buffer capacity!");

        this->mesg->type = Message::HELLO;
        this->mesg->magic = DOL_MAGIC_NUMBER;
        this->mesg->value = 0;
        this->mesg->size = payloadSize;
        this->mesg->seqn = ++this->seqn;
        memcpy (this->mesg->payload, this->key, payloadSize);

        Send (this->sock, this->mesg);
        }

    catch (char * explanation) {
        throw new Exception (true, "Cannot send HELLO message: %s", explanation);
        }

    // Get reply to hello message from the server

    try {
        memset (this->mesg, 0, sizeof (Message::Header));
        size_t received = Receive (this->sock, this->mesg);
        if (received != sizeof (Message::Header))
            throw new Exception (true, "HELLO reply has non-empty payload; expected %d, got %d bytes", sizeof (Message::Header), received);
        }

    catch (char * explanation) {
        throw new Exception (true, "Cannot receive HELLO reply: %s", explanation);
        }

    if (mesg->magic != DOL_MAGIC_NUMBER)
        throw new Exception (true, "Client HELLO message contains an invalid magic number.  Do client and server versions match?");
    if (mesg->type == Message::GOODBYE)
        throw new Exception (true, "Server does not accept key");
    if (mesg->type != Message::HELLO)
        throw new Exception (true, "Initial reply is not HELLO");

    this->clientID = ((unsigned int) mesg->value & 0xFFFFFFFF);

    // Setup a mutex to serialize client/server communications for multi-threaded clients

    if (pthread_mutex_init (&this->mutex, NULL) != 0)
        throw new Exception (true, "Cannot create server communication mutex");

    this->terminated = false;
    this->curDir = strdup ("/");
    }


Client::~Client (
    void
    ) {

    if (! this->terminated)
        CallServer (Message::GOODBYE);    // tell server we're leaving

    free (this->hostname);
    free (this->key);
    free (this->curDir);
    }


//-------------------------------------------------------------------------------
//  Remote Procedure Call Routine


char *
Client::CallServer (
    Message::Type   type,
    int *           value,
    const char *    argument,
    size_t          argsize,
    size_t *        replysize       // default NULL
    ) {

    size_t payloadSize = (argument == NULL) ? 0 : argsize;

    LOCK

    // Build and send request message

    this->mesg->type = type;
    this->mesg->magic = DOL_MAGIC_NUMBER;
    this->mesg->value = (uint64_t) ((value == NULL) ? 0 : *value);
    this->mesg->size = payloadSize;
    this->mesg->seqn = ++this->seqn;

    if (this->mesg->size > Message::maxPayload)
          throw new Exception (true, "%s request size exceeds send buffer capacity (%d > %d)", MessageTypeString (type), mesg->size, Message::maxPayload);

    memcpy (this->mesg->payload, argument, payloadSize);

    try {
        Send (this->sock, this->mesg);
        }
    catch (char * explanation) {
        throw new Exception (true, "Send of %s request fails: %s", MessageTypeString (type), explanation);
        }

    // Receive reply

    size_t received = 0;
    try {
        memset (this->mesg, 0, sizeof (Message::Header));
        received = Receive (this->sock, this->mesg);
        }
    catch (char * explanation) {
        throw new Exception (true, "Cannot receive server %s reply: %s", MessageTypeString (type), explanation);
        }

    if (mesg->magic != DOL_MAGIC_NUMBER)
        throw new Exception (true, "%s reply has wrong magic number", MessageTypeString (type));
    if (mesg->type != type)
        throw new Exception (true, "%s reply has wrong type (%s)", MessageTypeString (type), MessageTypeString (mesg->type));
    if (mesg->size != received - sizeof (Message::Header))
        throw new Exception (true, "%s reply has wrong message size [mesg->size = %d, received - sizeof (Message::Header) = %d]",
                        MessageTypeString (type),
                        mesg->size,
                        received - sizeof (Message::Header)
                        );

    if (value != NULL)
        *value = ((unsigned int) mesg->value & 0xFFFFFFFF);
    if (replysize != NULL)
        *replysize = mesg->size;

    char * reply = NULL;
    if (mesg->size > 0) {
        reply = new (std::nothrow) char [mesg->size];
        if (reply == NULL)
            throw new Exception (true, "Cannot allocate %d-byte string from reply to %s request", mesg->size, MessageTypeString (type));

        memcpy (reply, mesg->payload, mesg->size);
        }

    UNLOCK
    return reply;
    }


//-------------------------------------------------------------------------------
//  Client API - General API Functions


char *
Client::GetDescription (
    void
    ) {

    CheckIfTerminated ();
    char * reply = CallServer (Message::GET_DESCRIPTION);
    if (reply == NULL)
        return NULL;

    char * description = strdup (reply);
    delete [] reply;
    return description;
    }


//-------------------------------------------------------------------------------
//  Client API - Directory Functions


char *
Client::ChangeDirectory (
    const char * dirname
    ) {

    CheckIfTerminated ();

    char * reply = CallServer (Message::CHANGE_DIR, NULL, dirname, strlen (dirname) + 1);
    if (reply == NULL)
        return NULL;

    free (this->curDir);
    this->curDir = strdup (reply);
    delete [] reply;
    return strdup (this->curDir);
    }


char *
Client::PWD (
    void
    ) {

    CheckIfTerminated ();
    return strdup (this->curDir);
    }


//-------------------------------------------------------------------------------
//  Client API - Functions to Enumerate Threads


int
Client::GetThreadCount (
    void
    ) {

    CheckIfTerminated ();

    int count = 0;
    CallServer (Message::GET_THREADCOUNT, &count);
    return count;
    }


char *
Client::GetThreadName (
    int id
    ) {

    CheckIfTerminated ();
    char * reply = CallServer (Message::GET_THREADNAME, &id);
    char * name = strdup (reply);
    delete [] reply;
    return name;
    }


//-------------------------------------------------------------------------------
//  Client API - Get a DOL Directory


Client::Directory *
Client::GetDirectory (
    const char * dirname
    ) {

    CheckIfTerminated ();

    char * reply = CallServer (Message::GET_DIR, NULL, dirname, strlen (dirname) + 1);
    if (reply == NULL)
        throw new Exception (false, "Directory \"%s\" not found", dirname);

    Directory * dir = new Directory ();
    dir->Unserialize (reply);
    delete [] reply;
    return dir;
    }


//-------------------------------------------------------------------------------
//  Client API - Function to Get Detailed Information on an Array Shape


Client::ArrayShape *
Client::GetArrayShape (
    const char * arrayname
    ) {

    CheckIfTerminated ();

    int value;
    char * reply = CallServer (Message::ARRAY_SHAPE, &value, arrayname, strlen (arrayname) + 1);
    if (reply == NULL)
        return NULL;

    if ((bool) value == false) {
        delete [] reply;
        return NULL;
        }

    ArrayShape * ash = new ArrayShape ();
    ash->Unserialize (reply);
    delete [] reply;
    return ash;
    }


//-------------------------------------------------------------------------------
//  Client API - Function to Get Detailed Information on a Data Element


Client::DataElement *
Client::GetDataElement (
    const char * eltname
    ) {

    CheckIfTerminated ();

    size_t replysize;
    char * reply = CallServer (Message::GET_ELEMENT, NULL, eltname, strlen (eltname) + 1, &replysize);
    if (reply == NULL)
        return NULL;

    DataElement * elt = new DataElement ();
    elt->Unserialize (reply);
    delete [] reply;
    return elt;
    }


//-------------------------------------------------------------------------------
//  Client API - Function to Get Detailed Information on a Function


Client::Function *
Client::GetFunction (
    const char * funcname
    ) {

    CheckIfTerminated ();

    size_t replysize;
    char * reply = CallServer (Message::GET_FUNCTION, NULL, funcname, strlen (funcname) + 1, &replysize);
    if (reply == NULL)
        return NULL;

    Function * func = new Function ();
    func->Unserialize (reply);
    delete [] reply;
    return func;
    }


//-------------------------------------------------------------------------------
//  Client API - Data Access Functions


void
Client::GetData (
    const char *    eltname,
    unsigned char * buffer,
    size_t          size
    ) {

    CheckIfTerminated ();

    size_t replysize;
    char * reply = CallServer (Message::GET_DATA, NULL, eltname, strlen (eltname) + 1, &replysize);
    if (reply == NULL)
        throw new Exception (false, "Server does not return any data for element \"%s\"", eltname);

    if (replysize > size)
        throw new Exception (false, "Data returned by server for element \"%s\" does not fit in supplied buffer (%d < %d)\n", eltname, size, replysize);

    memcpy (buffer, reply, replysize);
    delete [] reply;
    }


void
Client::PutData (
    const char *            eltname,
    const unsigned char *   buffer,
    size_t                  size
    ) {

    CheckIfTerminated ();

    size_t namelen = strlen (eltname) + 1;
    char * sendbuf = new char [namelen + size];
    strcpy (sendbuf, eltname);
    memcpy (sendbuf + namelen, buffer, size);

    CallServer (Message::PUT_DATA, (int *) &namelen, sendbuf, namelen + size);
    delete [] sendbuf;
    }


int
Client::CallFunction (
    const char *  funcname,
    int     argument
    ) {

    CheckIfTerminated ();

    int result = argument;
    CallServer (Message::CALL_FUNCTION, &result, funcname, strlen (funcname) + 1);

    return result;
    }


//-------------------------------------------------------------------------------
//  Client API - Control Functions


bool
Client::Start (
    void
    ) {

    CheckIfTerminated ();

    int value;
    CallServer (Message::START, &value);
    return (bool) value;
    }


Milestone
Client::Step (
    int     steps
    ) {

    CheckIfTerminated ();

    int value = steps;
    CallServer (Message::STEP, &value);
    return (Milestone) value;
    }


Milestone
Client::Stop (
    void
    ) {

    CheckIfTerminated ();

    int value;
    CallServer (Message::STOP, &value);
    return (Milestone) value;
    }


bool
Client::Terminate (
    void
    ) {

    CheckIfTerminated ();

    int value;
    CallServer (Message::TERMINATE, &value);
    return this->terminated = (bool) value;
    }


//-------------------------------------------------------------------------------
//  Client API - Miscellaneous Functions


Status *
Client::ServerStatus (
    void
    ) {

    CheckIfTerminated ();

    Status * status = new Status;
    char * reply = CallServer (Message::SERVER_STATUS);
    memcpy (status, reply, sizeof (Status));
    delete [] reply;
    return status;
    }



//-------------------------------------------------------------------------------
//  Utility Functions


void
Client::CheckIfTerminated (
    void
    ) {

    if (this->terminated)
        throw new Exception (true, "Server has already been instructed to terminate");
    }

}
