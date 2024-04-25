 //-------------------------------------------------------------------------------
//  DelftOnline -- Global (Client/Server) Include File
//
//  This include file is very long, and contains not only API-related
//  definitions, but also (almost) all internal definitions.
//
//  Irv.Elshoff@Deltares.NL
//  2 jul 12
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

#pragma once

// The following definition is needed since VisualStudio2015 before including <pthread.h>:
#define HAVE_STRUCT_TIMESPEC

#include <pthread.h>
#include <stdio.h>


#if defined (WIN32) || defined (WIN64)
#   include <io.h>
#   include <winsock.h>
#   if defined (VS2008)
#      define uint64_t _ULonglong
#      define uint32_t u_long
#      define uint16_t u_short
#      define uint8_t  u_char
#   else
#      include <stdint.h>
#   endif
    // Undefine tokens we use, but Windows defines
#   undef IN
#   undef OUT
#   undef OPAQUE
#   undef ERROR

#   define DLL      __declspec(dllexport)
#   if !defined(STDCALL)
#      define STDCALL  __cdecl
#   endif

#   define ssize_t  int

#else
#   include <netinet/in.h>

#   define STDCALL
#   define DLL

#endif


class LinkedList;
class SortedBag;

namespace DOL {

class Client;
class DOL;
class Exception;
class Server;

class ClientConnection;
class Clock;
class Log;


//-------------------------------------------------------------------------------
//  Constants


#define URLprefix "dol://"

enum {
    MaxThreads          = 500,
    MaxPathNameLength   = 100,
    MAGIC_NUMBER        = 3705177,  // sanity checking and for client/server compatibility
    URLkeyLength        = 40,       // number of chars in key part of DOL handle URL
    TCPPORT_FIRST       = 51001,
    TCPPORT_LAST        = 51099,
    TCP_BACKLOG         = 10,       // number of pending TCP/IP connections
    VERSION_MAJOR       = 3,
    VERSION_MINOR       = 0
    };


#define DOL_MAGIC_NUMBER (VERSION_MAJOR * MAGIC_NUMBER + VERSION_MINOR)


typedef enum {
    OPAQUE          = 0,    // unspecified type; ToDo: implement
    INTEGER         = 1,    // in C++ int
    REAL            = 2,    // in C++ float
    DOUBLE          = 3,    // in C++ double
    DOUBLECOMPLEX   = 4,    // Fortran only
    COMPLEX         = 5,    // Fortran only
    LOGICAL         = 6,    // Fortran only
    CHARACTER       = 7     // Fortran only
        // ToDo: Add other C++ types such as long long, long double, etc.
    } BaseType;

typedef enum {
    C               = 1,    // C or C++
    FORTRAN         = 2     // Fortran-77, -90, 95, etc.
    } Language;

typedef enum {
    IN              = 1,    // put only
    OUT             = 2,    // get only
    INOUT           = 3,    // both get and put allowed
    } AccessMode;


typedef enum {
    SILENT          = 0,    // never any output (to stdout/stderr)
    ERROR           = 1,    // print serious error messages
    INFO            = 2,    // print errors, warnings and messages about major events
    TRACE           = 3,    // print lots of debugging information as well
    } Verbosity;


//-------------------------------------------------------------------------------
//  General Types and Classes


typedef int Milestone;

class Status {
    public:
        bool        allowControl;
        bool        running;
        int         generation;
        int         clients;
        int         distance;
        Milestone   milestone;
    };


class Barrier {
    public:
        Barrier (int numParticipants);
        ~Barrier (void);
        void Pass (void);

    private:
        int             numParticipants;    // total number of participants
        int             numArrived;         // number who have arrived at barrier this round
        pthread_mutex_t mutex;              // lock for access to this object
        pthread_cond_t  sync;               // for local synchronization
    };


class Message {
    public:
        typedef enum {
            ARRAY_SHAPE = 769020,
            CALL_FUNCTION,
            CHANGE_DIR,
            GET_DATA,
            GET_DESCRIPTION,
            GET_DIR,
            GET_ELEMENT,
            GET_FUNCTION,
            GET_THREADCOUNT,
            GET_THREADNAME,
            GOODBYE,
            HELLO,
            PUT_DATA,
            SERVER_STATUS,
            START,
            STEP,
            STOP,
            TERMINATE
           } Type;

        typedef struct {
            Type        type;               // what to do or what was done
            int         magic;              // magic number for sanity check
            uint64_t    value;              // simple payload
            size_t      size;               // additional payload size
            int         seqn;               // sequence number (for debugging only)
            char        payload[4];         // message payload (actual space is allocated and larger)
            } Header;

        static const size_t maxPayload = 1000*1000;    // size of buffer for client/server communication
        static const size_t maxMesgSize = sizeof (Header) + maxPayload;
    };


//-------------------------------------------------------------------------------
//  Server Class


class Server {
    public:
        typedef int (STDCALL *Callable) (void *, const int *);

        DLL Server (
            bool            startRunning,
            bool            allowControl,
            Verbosity       verbosity,
            const char *    logFile,
            uint16_t        firstPort = 0,
            uint16_t        lastPort = 0
            );

        DLL Server (
            const char *    serverURL
            );

        DLL ~Server (void);

        DLL char *
        Handle (
            void
            );

        DLL void
        RegisterThread (
            int             numthreads,
            const char *    name
            );

        DLL void
        UnregisterThread (
            void
            );

        DLL void
        SetDescription (
            const char *    description
            );

        DLL void
        CreateDirectory (
            const char *    pathname
            );

        DLL void
        ChangeDirectory (
            const char *    pathname
            );

        DLL void
        PublishArrayShape (
            const char *    directory,
            const char *    name,
            int             dimensionality,
            int             dimensions[]
            );

        DLL void
        Publish (
            const char *    directory,
            const char *    name,
            const char *    description,
            const char *    units,
            const char *    definedon,
            const char *    arrayshape,
            BaseType        basetype,
            void *          address,
            AccessMode      inout
            );

        DLL void
        PublishFunction (
            const char *    directory,
            const char *    name,
            const char *    description,
            Language        language,
            Callable        function,
            void *          context
            );

        DLL void
        Retract (
            const char *    directory,
            const char *    name
            );

        DLL void
        RetractArrayShape (
            const char *    directory,
            const char *    name
            );

        DLL void
        RetractFunction (
            const char *    directory,
            const char *    name
            );

        DLL bool
        waitOnStart (
            void
            );

        DLL void
        PassMilestone (
            Milestone       m
            );

    public:     // but not part of the DOL API, for internal use only
        class Directory {
            public:
                Directory (
                    const char * pathname,
                    Directory * parent
                    );

                ~Directory ();

                int
                Serialize (
                    char *  buffer,
                    int     bufsize
                    );

                char *          pathname;
                char *          name;
                Directory *     parent;

                LinkedList *    subdirs;
                LinkedList *    arrays;
                LinkedList *    elements;
                LinkedList *    functions;
            };

        class ClientConnection {
            public:
                ClientConnection (
                    int                 clientID,
                    Server *            server,
                    int                 sock,
                    struct sockaddr *   addr
                    );

                ~ClientConnection ();

                void
                ServiceThread (
                    void
                    );

                void
                Reply (
                    int     value,
                    size_t  size
                    );

                void CallFunction       (void);
                void ChangeDirectory    (void);
                void PutData            (void);
                void SendArrayShape     (void);
                void SendDirectory      (void);
                void SendData           (void);
                void SendDataElement    (void);
                void SendFunction       (void);
                void SendString         (const char * string);

            public:
                int         clientID;       // client sequence number
                Log *       log;
                pthread_mutex_t sync;      // for step/stop synchronization

            private:
                Server *    server;
                int         sock;
                uint16_t    port;
                char        ipaddr [17];
                pthread_t   serviceThread;
                Directory * curDir;
                Message::Header * mesg;     // fixed buffer (header + payload) for client/server communication
                int         seqn;           // message sequence number (for debugging purposes only)
            };

        void
        ClientGreeter (
            void
            );

        bool
        Start (
            void
            );

        bool
        Step (
            int steps,
            ClientConnection * client
            );

        bool
        Stop (
            ClientConnection * client
            );

        bool
        Terminate (
            int clientID
            );

    private:
        class ArrayShape {
            public:
                ArrayShape (
                    const char * name,
                    Directory * dir,
                    int         dimensionality,
                    int         dimensions[]
                    );

                ~ArrayShape ();

                size_t
                Serialize (
                    char *  buffer,
                    size_t  bufsize
                    );

                char *          name;
                char *          pathname;
                Directory *     dir;
                int             dimensionality;
                int *           dimensions;         // allocated array of dimensions
                int             numelements;        // product of dimensions
            };

        class DataElement {
            public:
                DataElement (
                    const char *    name,
                    Directory *     dir,
                    const char *    description,
                    const char *    units,
                    const char *    definedon,
                    ArrayShape *    ash,
                    BaseType        basetype,
                    AccessMode      inout,
                    void *          address
                    );

                ~DataElement ();

                size_t
                Serialize (
                    char *  buffer,
                    size_t  bufsize
                    );

                char *          name;
                char *          pathname;
                Directory *     dir;
                char *          description;
                char *          units;
                char *          definedon;
                ArrayShape *    arrayshape;
                BaseType        basetype;
                AccessMode      inout;
                unsigned int    arrayelements;
                size_t          size;
                void *          address;
            };

        class Function {
            public:
                Function (
                    const char *    name,
                    Directory *     dir,
                    const char *    description,
                    Callable        function,
                    void *          context
                    );

                ~Function ();

                size_t
                Serialize (
                    char *  buffer,
                    size_t  bufsize
                    );

                char *          name;
                char *          pathname;
                Directory *     dir;
                char *          description;
                Callable        function;
                void *          context;
            };

        class Thread {
            public:
                int             id;
                const char *    name;
                Directory *     curDir;
            };

        //-----------------------------------------------------------------------

        static bool initialized;    // for pan-instance initialization

        bool        startRunning;
        bool        ready;          // set to true when first thread passes milestone
        Status      status;
        Barrier *   barrier;
        Clock *     clock;
        Log *       log;

        char *      handle;         // URL
        char *      description;    // supplied by server program

        int         sock;           // TCP/IP socket for communication with clients
        struct sockaddr_in addr;    // TCP/IP protocol, host address, port

        pthread_key_t thread;       // place to store thread ID
        pthread_mutex_t mutex;      // for serializing access to this data structure
        pthread_cond_t  pause;      // used to stop at milestones at client request

        int         numThreads;    // number of server threads
        Thread *    thr [MaxThreads];

        enum {
            THID_FREE = -1,
            THID_FUNC = -2
            };

        pthread_t   clientGreeter;  // thread to accept incoming client connections

        Directory * rootDir;
        int         clientID;       // client sequence number
        LinkedList * clients;       // list of connected clients
        SortedBag * breakpoints;    // ordered list of milestones at which to stop

        //-----------------------------------------------------------------------

        void
        initialize (
            void
            );

        DLL int
        CallFunction (
            Callable    funcaddr,
            void *      context,
            int         argument,
            int         language,
            int         thid
            );

        Directory *
        GetDirectory (
            const char * directory,
            const char * moduleName
            );

        int
        GetThreadID (
            void
            );

        ArrayShape *
        LookupArrayShape (
            const char * pathname
            );

        DataElement *
        LookupDataElement (
            const char * pathname
            );

        Directory *
        LookupDirectory (
            const char * pathname
            );

        Function *
        LookupFunction (
            const char * pathname
            );

        void
        StopAt (
            Milestone milestone,
            ClientConnection * client
            );

        Exception *
        Error (
            bool fatal,
            const char * function,
            const char * reason,
            ...
            );
    };


//-------------------------------------------------------------------------------
//  Client Classes


class Client {
    public:
        class Directory;
        class ArrayShape;
        class DataElement;
        class Function;

        DLL Client (
            const char *    url,
            int             verbosity,
            const char *    logfile
            );

        DLL ~Client (
            void
            );

        //----  General informational methods

        DLL char *
        GetDescription (
            void
            );

        DLL void
        PrintContents (
            FILE * outfile
            );

        DLL int
        GetThreadCount (
            void
            );

        DLL char *
        GetThreadName (
            int thid
            );

        //----  DOL Object

        DLL Directory *
        GetDirectory (
            const char * dirname
            );

        DLL ArrayShape *
        GetArrayShape (
            const char * arrayname
            );

        DLL DataElement *
        GetDataElement (
            const char * eltname
            );

        DLL Function *
        GetFunction (
            const char * funcname
            );

        //----  Directory Functions

        DLL char *
        ChangeDirectory (
            const char * dirname
            );

        DLL char *
        PWD (
            void
            );

        //----  Data Access Functions

        DLL void
        GetData (
            const char *    name,
            unsigned char * buffer,
            size_t          size
            );

        DLL void
        PutData (
            const char *          name,
            const unsigned char * buffer,
            size_t                size
            );

        DLL int
        CallFunction (
            const char * funcname,
            int          argument
            );

        //----  Time-step Control Functions

        DLL bool
        Start (
            void
            );

        DLL Milestone
        Step (
            int steps
            );

        DLL Milestone
        Stop (
            void
            );

        DLL bool
        Terminate (
            void
            );

        //----  Miscellaneous

        DLL Status *
        ServerStatus (
            void
            );

    public:
        typedef struct {
            int         count;      // number of names in directory last queried
            char **     name;       // names in directory
            } List;

        class Directory {
            public:
                Directory (void);
                ~Directory (void);

                void
                Unserialize (
                    char *  buffer
                    );

            public:
                char *  pathname;
                List    subdirs;
                List    arrays;
                List    elements;
                List    functions;
            };

        class ArrayShape {
            public:
                ArrayShape (void);
                ~ArrayShape (void);

                void
                Unserialize (
                    char *  buffer
                    );

            public:
                char *          pathname;
                unsigned int    dimensionality;
                unsigned int *  dimensions;      // allocated array of dimensions
            };

        class DataElement {
            public:
                DLL DataElement (void);
                DLL ~DataElement (void);

                void
                Unserialize (
                    char *  buffer
                    );

            public:
                char *          pathname;
                char *          description;
                char *          units;
                char *          definedon;
                ArrayShape *    arrayshape;
                BaseType        basetype;
                AccessMode      inout;
                unsigned int    arrayelements;
                size_t          size;
            };

        class Function {
            public:
                DLL Function (void);
                DLL ~Function (void);

                void
                Unserialize (
                    char *  buffer
                    );

            public:
                char *          pathname;
                char *          description;
                void *          context;

            };

    private:
        static bool initialized;        // for pan-instance initialization

        char *          hostname;       // server hostname
        uint16_t        port;           // server TCP port
        char *          key;            // authentication key

        int             sock;           // socket for client/server communication
        struct sockaddr addr;           // server connection TCP/IP handle
        struct sockaddr_in * inaddr;    // pointer to same TCP/IP address

        int             clientID;       // server's identifier for me
        pthread_mutex_t mutex;          // for serializing communication with the server
        char *          curDir;         // pathname for my current directory on the server
        bool            terminated;     // server was told to terminate
        Message::Header * mesg;         // fixed buffer (header + payload) for client/server communication
        int             seqn;           // message sequence number (for debugging purposes only)

    private:
        void
        initialize (
            void
            );

        ArrayShape *
        getArrayShape (
            const char *    methodname
            );

        char *
        CallServer (
            Message::Type   type,
            int *           value     = NULL,
            const char *    argument  = NULL,
            size_t          argsize   = 0,
            size_t *        replysize = NULL
            );

        void
        CheckIfTerminated (
            void
            );

    };


//-------------------------------------------------------------------------------
//  Utility Functions


const char *
AccessModeString (
    AccessMode     accessmode
    );

unsigned int
BaseTypeSize (
    BaseType basetype
    );

const char *
BaseTypeString (
    BaseType basetype
    );


//-------------------------------------------------------------------------------
//  Exception Object, for when something goes wrong


class Exception {
    public:
        //static const unsigned int MaxErrorMesgLen = (10*1000);
        enum { MaxErrorMesgLen = (10*1000) };       // VisualStudio doesn't like const's

        bool    fatal;
        char *  message;

        DLL Exception (
            bool   fatal,
            const char * format,
            ...
            );

        DLL ~Exception (void);
    };

}

