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
// $Id: distributed.cpp 962 2011-10-31 21:52:47Z elshoff $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/distributed.cpp $
//------------------------------------------------------------------------------
//  d_hydro Flow2D3D Component
//  Domain Decomposition Distributed (MultiNode) Routines - IMPLEMENTATION
//
//  Irv.Elshoff@Deltares.NL
//  25 may 25
//------------------------------------------------------------------------------


#ifdef WIN32
#   define getpid GetCurrentProcessId
#endif

#include "flow2d3d.h"


//------------------------------------------------------------------------------


#define MAGIC   310967156       // random number

typedef enum {                  // message type
    TAG_HELLO = 9430,
    TAG_INITIAL,
    TAG_JOIN_LOCAL,
    TAG_JOIN_GLOBAL,
    TAG_FINAL,
    TAG_FINAL_ACK
    } MessageTag;


typedef struct {
    int         magic;          // sanity check
    MessageTag  tag;
    int         nodeID;         // ID of slave node
    int         numNodes;
    } MasterSlaveMessage;


typedef struct {
    int         magic;          // sanity check
    MessageTag  tag;
    int         numJoins;       // global number of joins
    struct {
        char        handle [Stream::MAXHANDLE]; // lead stream identity
        } join [DD::MAXJOINS];
    } JoinExchangeMessage;


//------------------------------------------------------------------------------


static void
masterStreamError (
    char * reason
    ) {

    throw new Exception (true, "MASTER Stream ERROR: %s", reason);
    }

static void
slaveStreamError (
    char * reason
    ) {

    throw new Exception (true, "SLAVE Stream ERROR: %s", reason);
    }

static void
joinStreamError (
    char * reason
    ) {

    throw new Exception (true, "Inter-iterator Stream ERROR: %s", reason);
    }

static void
masterStreamTrace (
    char * message
    ) {

    FLOW2D3D->log->Write (Log::DD_SENDRECV, "DD master stream: %s", message);
    }

static void
slaveStreamTrace (
    char * message
    ) {

    FLOW2D3D->log->Write (Log::DD_SENDRECV, "DD slave stream: %s", message);
    }

static void
joinStreamTrace (
    char * message
    ) {

    FLOW2D3D->log->Write (Log::DD_SENDRECV, "DD inter-iterator stream: %s", message);
    }


//------------------------------------------------------------------------------


void
DD::MasterProcess (
    void
    ) {

#if defined (WIN32)
    throw new Exception (true, "Remote execution is not supported on Microsoft Windows");
#else

    const char * hnpid = GetHostnamePID ();
    char threadName [strlen (hnpid) + 100];
    sprintf (threadName, "%s master", hnpid);
    this->log->RegisterThread (threadName);
    delete [] hnpid;
    this->log->Write (Log::ALWAYS, "Distributed DD master process started");

    const char * mainArgs = this->flow->DH->mainArgs;
    const char * exePath  = this->flow->DH->exePath;

    const char * remoteShellCommand = this->multiNode->GetAttrib ("remoteShellCommand");
    if (remoteShellCommand == NULL)
        remoteShellCommand = "ssh -n";

    // Get the current directory and LD_LIBRARY_PATH, which needs to be set on the remote nodes

    const char * pwd = getenv ("PWD");
    const char * libPath = getenv ("LD_LIBRARY_PATH");
    if (libPath == NULL) libPath = "";

    // Start slave processes on remote nodes

    for (int id = 0 ; id < this->nodeSet->numNodes ; id++) {
        Node * node = this->nodeSet->node[id];

        node->stream = new Stream (Stream::TCPIP, &masterStreamError, &masterStreamTrace);
        char * streamHandle = node->stream->LocalHandle ();

        // Build remote shell command string

        int maxLen = 10000;
        char remoteCommand [maxLen];
        int len = snprintf (remoteCommand, maxLen,
                    "%s %s 'cd %s; export LD_LIBRARY_PATH=%s; ulimit -c unlimited; %s -S %s %s' < /dev/null",
                        remoteShellCommand,
                        node->hostname,
                        pwd,
                        libPath,
                        exePath,
                        streamHandle,
                        mainArgs
                        );

        if (len >= maxLen)
            throw new Exception (true, "Internal error: Remote command line too long in MasterProcess!");

        // Execute command

        this->log->Write (Log::MAJOR, "Master spawning node %d slave process on \"%s\"", id, node->hostname);
        this->log->Write (Log::CONFIG_MINOR, "Master process executing shell command \"%s\"", remoteCommand);

        node->remotePID = fork ();
        if (node->remotePID == 0) {
            if (system (remoteCommand) == 0)
                exit (0);
            else {
                 this->log->Write (Log::ALWAYS, "Trouble running \"%s\"", remoteCommand);
                 exit (1);
                 }
            }
        }

    //  Make sure all of the slaves are actually running.  They fail quickly if the remote command
    //  cannot be executed, which is the most common fault.

    sleep (1);

    for (int id = 0 ; id < this->nodeSet->numNodes ; id++) {
        Node * node = this->nodeSet->node[id];
        int status;
        int rc = waitpid (node->remotePID, &status, WNOHANG);
        if (rc != 0 && WIFEXITED (status))
            throw new Exception (true, "Slave process for node %d on \"%s\" has exited prematurely", id, node->hostname);
        }

    //  Wait for HELLO messages indicating the nodes are ready.
    //  ToDo: time out and abort for silent slaves.

    for (int id = 0 ; id < this->nodeSet->numNodes ; id++) {
        Node * node = this->nodeSet->node[id];

        this->log->Write (Log::MAJOR, "Master connecting to node %d", id);
        node->stream->Connect ();

        MasterSlaveMessage mesg;
        node->stream->Receive ((char *) &mesg, sizeof mesg);

        if (mesg.magic != MAGIC)
            throw new Exception (true, "Got malformed message from slave %d", id);
        if (mesg.tag != TAG_HELLO)
            throw new Exception (true, "Expected HELLO message from slave %d but got something else", id);

        this->log->Write (Log::MAJOR, "Master got HELLO message from node %d", id);
        }

    //  Send each slave an initial message with it's node ID

    for (int id = 0 ; id < this->nodeSet->numNodes ; id++) {
        Node * node = this->nodeSet->node[id];

        MasterSlaveMessage mesg;
        mesg.magic = MAGIC;
        mesg.tag = TAG_INITIAL;
        mesg.nodeID = id;

        this->log->Write (Log::MINOR, "Master sending INITIAL message to node %d", id);
        node->stream->Send ((char *) &mesg, sizeof mesg);
        }

    //  Wait for a join exchange message from each slave.  Merge all non-null entries
    //  into a global join exchange table and send this back to the slaves.

    JoinExchangeMessage globalJoin;
    globalJoin.numJoins = -1;
    for (int jid = 0 ; jid < DD::MAXJOINS ; jid++)
        globalJoin.join[jid].handle[0] = '\0';

    for (int id = 0 ; id < this->nodeSet->numNodes ; id++) {
        Node * node = this->nodeSet->node[id];

        JoinExchangeMessage joinEx;

        this->log->Write (Log::MAJOR, "Master waiting for JOIN_LOCAL message from node %d", id);
        node->stream->Receive ((char *) &joinEx, sizeof joinEx);

        if (joinEx.magic != MAGIC)
            throw new Exception (true, "Got malformed message from slave %d", id);
        if (joinEx.tag != TAG_JOIN_LOCAL)
            throw new Exception (true, "Expected JOIN_LOCAL message from slave %d but got something else", id);

        this->log->Write (Log::MAJOR, "Master got JOIN_LOCAL message from node %d", id);

        if (globalJoin.numJoins < 0)
            globalJoin.numJoins = joinEx.numJoins;
        else if (globalJoin.numJoins != joinEx.numJoins)
            throw new Exception (true, "Inconsistent numJoins values JOIN_LOCAL messages (%d != %d)", globalJoin.numJoins, joinEx.numJoins);

        for (int jid = 0 ; jid < globalJoin.numJoins ; jid++)
            if (joinEx.join[jid].handle[0] != '\0')
                strcpy (globalJoin.join[jid].handle, joinEx.join[jid].handle);
        }

    globalJoin.magic = MAGIC;
    globalJoin.tag = TAG_JOIN_GLOBAL;

    for (int id = 0 ; id < this->nodeSet->numNodes ; id++) {
        Node * node = this->nodeSet->node[id];

        this->log->Write (Log::MAJOR, "Master sending JOIN_GLOAL message to node %d", id);
        node->stream->Send ((char *) &globalJoin, sizeof globalJoin);
        }

    //  At this point all slaves can proceed on their own.
    //  We just wait until they finish, or in the worst case crash.
    //  ToDo: If one crashes, kill the rest.

    this->log->Write (Log::ALWAYS, "Master process has done its work and waiting for all slaves to terminate");

    // Wait for all node remote shell processes to terminate

    for (int id = 0 ; id < this->nodeSet->numNodes ; id++) {
        Node * node = this->nodeSet->node[id];

        pid_t pid = waitpid (node->remotePID, NULL, 0);
        if (pid != node->remotePID)
            throw new Exception (true, "Internal error: Unexpected return value from waitpid in MasterProces!");

        delete node->stream;
        node->stream = NULL;
        }

    this->log->Write (Log::ALWAYS, "Master process has seen that all slaves have terminated, and is terminating");

#endif
    }


//------------------------------------------------------------------------------


static void *   leader_thread       (void *);
static void *   follower_thread     (void *);


void
DD::SlaveProcess (
    void
    ) {

#if defined (WIN32)
    throw new Exception (true, "Remote execution is not supported on Microsoft Windows");
#else

    char * hnpid = GetHostnamePID ();
    char threadName [strlen (hnpid) + 100];
    sprintf (threadName, "%s slave", hnpid);
    this->log->RegisterThread (threadName);

    // Connect to master and send HELLO message

    this->log->Write (Log::ALWAYS, "Slave process connecting to master on \"%s\" and sending HELLO", this->flow->DH->slaveArg);

    Stream * master = new Stream (Stream::TCPIP, this->flow->DH->slaveArg, &slaveStreamError, &slaveStreamTrace);

    MasterSlaveMessage mesg;
    mesg.magic = MAGIC;
    mesg.tag = TAG_HELLO;
    master->Send ((char *) &mesg, sizeof mesg);

    // Wait for INITIAL message with my node ID

    this->log->Write (Log::MAJOR, "Slave sent HELLO, now waiting for INITIAL message");
    master->Receive ((char *) &mesg, sizeof mesg);

    if (mesg.magic != MAGIC)
        throw new Exception (true, "Got malformed message from master");
    if (mesg.tag != TAG_INITIAL)
        throw new Exception (true, "Expected INITIAL message from master but got something else");

    this->nodeID = mesg.nodeID;

    sprintf (threadName, "%s slave:%d", hnpid, this->nodeID);
    this->log->RenameThread (threadName);
    this->log->Write (Log::MAJOR, "Slave got INITIAL message");
    delete [] hnpid;

    this->role = ROLE_SLAVE;

    this->CreateFixedEntities ();

    //  Read the DD configuration file (or get it from the Flow2D3D config tree)

    this->ReadConfig ();

    if (this->flow->flowol)
        this->flow->flowol->numSubdomains = this->numSubdomains;

    //  Create the minimum barrier iterator and join it to all mappers

    this->minbarCat = new Category (this, DD::minBarCategoryName);
    this->minbar = new Iterator (this, DD::minBarCategoryName, NULL, this->minbarCat, &MinimumBarrier_Function);
    this->minbar->Place  (this->nodeSet->node[0]);
    this->minbar->Detach ();

    this->mapperList->Rewind ();
    Iterator * mapper;
    while ((mapper = (Iterator *) this->mapperList->Next ()) != NULL)
        this->JoinIterators (mapper, this->minbar);

    //  Determine whether joins are local or semi-remote or completely remote.
    //  Local joins have both iterators on this node and communicate via shared memory.
    //  Semi-remote joins have one iterator on this node, the other on a different node.
    //  Streams are used to communicate.  If the first iterator is on this node, this
    //  node is the leader and creates the initial endpoint.  The handle is later sent
    //  to the other node via the master.  Completely remote joins are those in which
    //  both iterators reside on other nodes.  Nothing needs to be done for these.

    JoinExchangeMessage joinEx;
    joinEx.magic = MAGIC;
    joinEx.tag = TAG_JOIN_LOCAL;
    joinEx.numJoins = this->numJoins;

    for (int jid = 0 ; jid < DD::MAXJOINS ; jid++)
        joinEx.join[jid].handle[0] = '\0';

    for (int jid = 0 ; jid < this->numJoins ; jid++) {
        Iterator * iter1 = this->iterator [this->join[jid].iter1].iterator;
        Iterator * iter2 = this->iterator [this->join[jid].iter2].iterator;

        if (iter1->node->nodeID == this->nodeID) {
            if (iter2->node->nodeID == this->nodeID)
                this->join[jid].local = true;

            else {
                if (this->log->GetMask () & Log::DD_SENDRECV == 0)
                    this->join[jid].stream = new Stream (Stream::TCPIP, &joinStreamError);
                else
                    this->join[jid].stream = new Stream (Stream::TCPIP, &joinStreamError, &joinStreamTrace);

                strcpy (joinEx.join[jid].handle, this->join[jid].stream->LocalHandle ());
                this->join[jid].leader = true;
                this->log->Write (Log::CONFIG_MINOR, "Slave leading join %d; handle is \"%s\"", jid, joinEx.join[jid].handle);
                }
            }

        else if (iter2->node->nodeID == this->nodeID)
            this->join[jid].follower = true;
        }

    //  The result of the above loop is a join exchange table filled with stream
    //  handles for the streams we lead.  Send it to the master, who will then send
    //  us the union of all tables.

    master->Send ((char *) &joinEx, sizeof joinEx);
    this->log->Write (Log::MAJOR, "Slave sent local joing exchange table; waiting for global union");

    memset (&joinEx, 0, sizeof joinEx);
    master->Receive ((char *) &joinEx, sizeof joinEx);

    if (joinEx.magic != MAGIC)
        throw new Exception (true, "Got malformed message from master");
    if (joinEx.tag != TAG_JOIN_GLOBAL)
        throw new Exception (true, "Expected JOIN_GLOBAL message from master but got something else");
    if (joinEx.numJoins != this->numJoins)
        throw new Exception (true, "Inconsistent numJoins value JOIN_GLOBAL message");

    delete master;

    //  Start helper threads for the joins we lead

    this->leadFollow = new Semaphore ("leadFollow", 0, this->log);

    for (int jid = 0 ; jid < this->numJoins ; jid++) {
        if (this->join[jid].leader) {
            if (pthread_create (&this->join[jid].thid, NULL, &leader_thread, (void *) jid) != 0)
                throw new Exception (true, "Pthreads error: Cannot create leader thread %s", strerror(errno));

            this->leadFollow->PSem ();
            }
        }

    //  Start helper threads for the joins we follow

    for (int jid = 0 ; jid < this->numJoins ; jid++) {
        if (this->join[jid].follower) {
            if (joinEx.join[jid].handle[0] == '\0')
                throw new Exception (true, "Slot %d in JOIN_GLOBAL message is empty", jid);

            strcpy (this->join[jid].handle, joinEx.join[jid].handle);

            if (pthread_create (&this->join[jid].thid, NULL, &follower_thread, (void *) jid) != 0)
                throw new Exception (true, "Pthreads error: Cannot create follower thread %s", strerror(errno));
            }
        }

    // Wait for all leader and follower threads to terminate

    delete this->leadFollow;
    this->leadFollow = NULL;

    for (int jid = 0 ; jid < this->numJoins ; jid++) {
        if (this->join[jid].leader || this->join[jid].follower) {
            this->log->Write (Log::CONFIG_MINOR, "Waiting for termination of join %d %s thread", jid, this->join[jid].leader ? "leader" : "follower");
            if (pthread_join (this->join[jid].thid, NULL) != 0)
                throw new Exception (true, "Pthreads error: Cannot join with thread, errno=%d", errno);
            }
        }

    this->log->Write (Log::CONFIG_MINOR, "All slave join threads have terminated");

    // Create lists of categories and iterators

    this->categoryList = new List ();
    for (int catid = 0 ; catid < this->numCategories ; catid++) {
        this->categoryList->Append ((void *) this->category[catid].category);
        }

    this->iteratorList = new List ();
    for (int iterid = 0 ; iterid < this->numIterators ; iterid++) {
        this->iteratorList->Append ((void *) this->iterator[iterid].iterator);
        }

    this->PrintJoinTable ();

    this->InitializeLocalMessageBuffers ();

    this->StartLocalIteratorThreads ();     // returns when all have invoked their Ready methods

    if (this->mappersOnThisNode > 0)
        SetupMinimumBarrier ();

    this->RunSimulation ();

    this->log->Write (Log::ALWAYS, "Slave is terminating");

#endif

    }


//------------------------------------------------------------------------------


static void *
leader_thread (
    void * argument
    ) {

    int jid = ((long) argument);         // index in join table
    DD * dd = FLOW2D3D->dd;

    dd->log->Write (Log::CONFIG_MINOR, "Leader thread for join %d starting", jid);

    try {
        dd->leadFollow->VSem ();
        dd->join[jid].stream->Connect ();
        }

    catch (Exception * ex) {
        dd->log->Write (Log::ALWAYS, "Exception in leader thread for join %d: %s", jid, ex->message);
        exit (1);
        // ToDo: a more orderly shutdown
        }

    dd->log->Write (Log::CONFIG_MINOR, "Leader thread for join %d terminating", jid);
    return NULL;
    }


static void *
follower_thread (
    void * argument
    ) {

    int jid = ((long) argument);         // index in join table
    DD * dd = FLOW2D3D->dd;

    dd->log->Write (Log::CONFIG_MINOR, "Follower thread for join %d starting; handle = \"%s\"", jid, dd->join[jid].handle);

    try {
        if (dd->log->GetMask () & Log::DD_SENDRECV == 0)
            dd->join[jid].stream = new Stream (Stream::TCPIP, dd->join[jid].handle, &joinStreamError);
        else
            dd->join[jid].stream = new Stream (Stream::TCPIP, dd->join[jid].handle, &joinStreamError, &joinStreamTrace);
        }

    catch (Exception * ex) {
        dd->log->Write (Log::ALWAYS, "Exception in follower thread for join %d: %s", jid, ex->message);
        exit (1);
        // ToDo: a more orderly shutdown
        }

    dd->log->Write (Log::CONFIG_MINOR, "Follower thread for join %d terminating", jid);
    return NULL;
    }
