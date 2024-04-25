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
// $Id: dd.cpp 962 2011-10-31 21:52:47Z elshoff $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/dd.cpp $
//------------------------------------------------------------------------------
//  d_hydro Flow2D3D Component
//  Domain Decomposition Initialization - IMPLEMENTATION
//
//  Irv.Elshoff@Deltares.NL
//  6 mar 13
//------------------------------------------------------------------------------


#define DD_MAIN

#ifdef WIN32
#   define getpid GetCurrentProcessId
#endif

#include "flow2d3d.h"


//------------------------------------------------------------------------------
//  Local function declarations


static  void    check_md_file   (char *);
static  char *  process_name    (const char *, char *);
static  bool    readable_file   (const char *);


//------------------------------------------------------------------------------


DD::DD (
    Flow2D3D * flow,
    XmlTree *  config
    ) {

    this->flow   = flow;
    this->config = config;

    this->clock                 = this->flow->DH->clock;
    this->leadFollow            = NULL;
    this->log                   = this->flow->DH->log;
    this->mappersOnThisNode     = 0;
    this->multiNode             = NULL;
    this->nodeID                = -1;
    this->nodeSet               = new NodeSet ();
    this->numCategories         = 0;
    this->numSubdomains         = 0;
    this->numIterators          = 0;
    this->numJoins              = 0;
    this->numMappers            = 0;
    this->numNodes              = 0;
    this->role                  = ROLE_UNDEFINED;
    this->initSync              = NULL;

    this->dredgeCat             = NULL;
    this->dredgem               = NULL;
    this->gawsbar               = NULL;
    this->gawsbarCat            = NULL;
    this->initbar               = NULL;
    this->initbarCat            = NULL;
    this->minbar                = NULL;
    this->minbarCat             = NULL;
    this->rtc                   = NULL;
    this->rtcCat                = NULL;

    this->log->Write (Log::CONFIG_MINOR, "Sizeof DD object is %d bytes", sizeof (*this));

    this->log->Write (Log::MAJOR, "Flow2D3D initializing single-process multi-domain simulation");
    this->nodeSet->AddNodesFromString ("localhost");
    this->role = ROLE_SINGLE;

    /*
    ToDo: Get multi-node DD configuratino from XML file
    this->multiNode = this->config->Lookup ("MultiNode");
    if (this->multiNode == NULL) {
        this->log->Write (Log::MAJOR, "Flow2D3D initializing single-process multi-domain simulation");
        this->nodeSet->AddNodesFromString ("localhost");
        this->role = ROLE_SINGLE;
        }

    else {
        int localhost = this->multiNode->GetIntegerAttrib ("localhost");
        if (localhost > 0) {
            for (int i = 0 ; i < localhost ; i++)
                this->nodeSet->AddNodesFromString ("localhost");
            }

        else {
            const char * nodeListFileName = this->multiNode->GetAttrib ("file");
            if (nodeListFileName != NULL)
                this->nodeSet->AddNodesFromFile (nodeListFileName);
            else if (this->multiNode->charDataLen > 0) {
                this->nodeSet->AddNodesFromString (this->multiNode->charData);
                }
            else
                throw new Exception (true, "MultiNode DD specified, but no nodes specified");
            }

        this->numNodes = this->nodeSet->numNodes;

        if (this->flow->DH->slaveArg == NULL) {
            this->log->Write (Log::MAJOR, "Flow2D3D initializing master for multi-domain multi-node simulation on %d nodes", this->numNodes);
            this->role = ROLE_MASTER;
            }
        else {
            this->log->Write (Log::MAJOR, "Flow2D3D initializing a slave for multi-domain multi-node simulation");
            this->role = ROLE_SLAVE;
            }
        }
    */

#if defined (WIN32)
    if (this->multiNode)
        throw new Exception (true, "Multi-node DD simulations are not yet supported on Microsoft Windows platforms");
#endif

    this->nodeSet->CreateNodeTable ();
    this->numNodes = this->nodeSet->numNodes;

    this->InitThreads ();

    this->categoryDict = new Dictionary ("Category");
    this->iteratorDict = new Dictionary ("Iterator");
    this->joinDict     = new Dictionary ("Join");
    }


DD::~DD (
    void
    ) {

    delete this->nodeSet;
    delete this->categoryDict;
    delete this->iteratorDict;
    delete this->joinDict;
    }


//------------------------------------------------------------------------------


void
DD::Run (
    void
    ) {

    if (this->role == ROLE_SINGLE)
        this->SingleProcess ();
    else if (this->role == ROLE_MASTER)
        this->MasterProcess ();
    else if (this->role == ROLE_SLAVE)
        this->SlaveProcess ();
    }


//------------------------------------------------------------------------------


void
DD::SingleProcess (
    void
    ) {

    this->log->Write (Log::MAJOR, "Single-process DD started");
    this->masterStart = this->clock->Start ();
    this->role = ROLE_SINGLE;
    this->nodeID = 0;

    this->CreateFixedEntities ();

    // Read the DD configuration file (or get it from the Flow2D3D config tree)

    this->ReadConfig ();

    if (this->flow->flowol)
        this->flow->flowol->numSubdomains = this->numSubdomains;

    //  All joins are local, so set all flags to true in the join table

    for (int jid = 0 ; jid < this->numJoins ; jid++) {
        this->join[jid].local = true;
        }

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

    SetupMinimumBarrier ();

    this->RunSimulation ();

    this->log->Write (Log::MAJOR, "All iterators have terminated in single-process DD");
    }



//------------------------------------------------------------------------------


void
DD::CreateFixedEntities (
    void
    ) {

    // Create categories, lists, dictionaries and iterators for fixed (non-input dependent) entities
    // and place them on the first node

    this->processCat = new Category (this, DD::processCategoryName);
    this->processList = new List ();
    this->processDict = new Dictionary ("Process");

    this->mapperCat  = new Category (this, DD::mapperCategoryName);
    this->mapperList = new List ();

    this->initbarCat = new Category (this, DD::initBarCategoryName);
    this->initbar = new Iterator (this, DD::initBarCategoryName, NULL, this->initbarCat, &InitBarrier_Function);
    this->initbar->Place (this->nodeSet->node[0]);
    this->initbar->Detach ();

    this->gawsbarCat = new Category (this, DD::gawsBarCategoryName);
    this->gawsbar = new Iterator (this, DD::gawsBarCategoryName, NULL, this->gawsbarCat, &GawsBarrier_Function);
    this->gawsbar->Place (this->nodeSet->node[0]);
    this->gawsbar->Detach ();

    this->dredgeCat = new Category (this, DD::dredgeMergeCategoryName);
    this->dredgem = new Iterator (this, DD::dredgeMergeCategoryName, NULL, this->dredgeCat, &DredgeMerge_Function);
    this->dredgem->Place (this->nodeSet->node[0]);
    this->dredgem->Detach ();

    this->rtcCat = new Category (this, DD::rtcCategoryName);
    this->rtc = new Iterator (this, DD::rtcCategoryName, NULL, this->rtcCat, &Rtc_Function);
    this->rtc->Place     (this->nodeSet->node[0]);
    this->rtc->Detach ();
    }


//------------------------------------------------------------------------------


void
DD::ReadConfig (
    void
    ) {

    const char * ddbfile = this->config->GetElement ("ddbFile");
    if (ddbfile == NULL)
        throw new Exception (true, "DD-bounds file not specified in configuration file");

    this->log->Write (Log::CONFIG_MAJOR, "Reading DD-bounds file \"%s\"...", ddbfile);

    FILE * input;
    if ((input = fopen (ddbfile, "r")) == NULL)
        throw new Exception (true, "Cannot open DD-bounds file \"%s\"", ddbfile);

    char line [DD::MAXSTRING];
    if (fgets (line, DD::MAXSTRING, input) == NULL)
        throw new Exception (true, "Error reading first line of DD-bounds file");

    rewind (input);

    const char * xmlhead = "<?xml ";
    if (strncmp (line, xmlhead, strlen (xmlhead)) == 0) {
#if defined (WIN32)
        throw new Exception (true, "XML DD-bounds file is currently not supported on Windows");
#else
        throw new Exception (true, "XML DD-bounds file is currently not implemented (WIP)");        // ToDo
#endif
        }

    this->ReadDDBounds (input, ddbfile);
    fclose (input);

    this->log->Write (Log::CONFIG_MAJOR, "Done reading DD-bounds file");
    }


void
DD::ReadDDBounds (
    FILE *  input,
    const char *  filename
    ) {

    int     confline = 0;                   // input line number
    int     len;                            // length of line
    char    line    [MAXSTRING];            // input buffer
    char    left    [MAXSTRING];            // left-side mapper input file
    char    right   [MAXSTRING];            // right-side mapper input file
    int     l1, l2, l3, l4;                 // left-side bounds
    int     r1, r2, r3, r4;                 // right-side bounds
    char    leftname  [MAXSTRING];          // left-side process name
    char    rightname [MAXSTRING];          // right-side process name
    char    mapname   [MAXSTRING];          // mapper name
    int     nextNode = 0;                   // place for next new subdomain process

    int pos = 0;                            // position for continuations
    int mappercount = 0;

    while (fgets (&line[pos], MAXSTRING-pos, input) != NULL) {
        confline++;
        len = strlen (&line[pos]);

        if (line [pos+len-1] == '\\') {     // assemble continuation lines
            pos += len-1;
            continue;
            }

        pos = 0;                            // reset for next line
        char word [MAXSTRING];
        word [0] = '\0';
        sscanf (line, "%s", word);
        if (word [0] == '#')  continue;     // skip comment lines
        if (word [0] == '\0')  continue;    // skip empty lines
        String::Chomp (line);

        // Parse input line and generate process and mapper names

        if (sscanf (line, "%s %d %d %d %d %s %d %d %d %d", left, &l1, &l2, &l3, &l4, right, &r1, &r2, &r3, &r4) != 10)
            throw new Exception (true, "Syntax error in DD-bounds file on line %d", confline);

        check_md_file (left);
        check_md_file (right);

        sprintf (mapname, "M%d=%s=%s",
                    ++mappercount,
                    process_name (left,  leftname),
                    process_name (right, rightname)
                    );

        // Create process iterators when they are seen for the first time

        Iterator * leftprocess = (Iterator *) this->processDict->Lookup (leftname);
        if (leftprocess == (Iterator *) Dictionary::NOTFOUND) {
            leftprocess = this->AddProcess (leftname, left, nextNode++);
            nextNode %= this->numNodes;
            }

        Iterator * rightprocess = (Iterator *) this->processDict->Lookup (rightname);
        if (rightprocess == (Iterator *) Dictionary::NOTFOUND) {
            rightprocess = this->AddProcess (rightname, right, nextNode++);
            nextNode %= this->numNodes;
            }

        // Create mapper iterator on same node as left process

        this->AddMapper (mapname, line, leftprocess, rightprocess, leftprocess->node->nodeID);
        }
    }


//------------------------------------------------------------------------------


Iterator *
DD::AddProcess (
    const char *    name,
    const char *    config,
    int             nodeID
    ) {

    Blob * configblob = new Blob (config, strlen (config));
    Iterator * process = new Iterator (this, name, configblob, this->processCat, &FlowDD_Process);
    delete configblob;

    this->processDict->Insert (name, (void *) process);
    this->processList->Append ((void *) process);
    this->numSubdomains++;

    process->Place (this->nodeSet->node[nodeID]);

    this->JoinIterators (process, this->gawsbar);
    this->JoinIterators (process, this->initbar);
    this->JoinIterators (process, this->dredgem);
    this->JoinIterators (process, this->rtc);

    return process;
    }


Iterator *
DD::AddMapper (
    const char *    name,
    const char *    config,
    Iterator *      leftprocess,
    Iterator *      rightprocess,
    int             nodeID
    ) {

    int len = strlen (config);
    char * configcopy = new char [len+1];
    strcpy (configcopy, config);
    Blob * configblob = new Blob (configcopy, len+1);

    Iterator * mapper = new Iterator (this, name, configblob, this->mapperCat, &FlowDD_Mapper);
    this->mapperList->Append ((void *) mapper);

    mapper->Place (this->nodeSet->node[nodeID]);

    if (nodeID == this->nodeID)
        this->mappersOnThisNode++;

    this->JoinIterators (mapper, leftprocess);
    this->JoinIterators (mapper, rightprocess);

    return mapper;
    }


//------------------------------------------------------------------------------


void
DD::JoinIterators (
    Iterator * iter1,
    Iterator * iter2
    ) {

    // Validate function arguments

    if (iter1 == NULL || iter2 == NULL)
        throw new Exception (true, "Null iterator in JoinIterators");

    // Get IDs of the iterators to join

    const char * name1 = iter1->name;
    if (name1 == NULL) name1 = "";
    long id1 = (long) this->iteratorDict->Lookup (name1);
    if (id1 == Dictionary::NOTFOUND)
        throw new Exception (true, "Unknown iterator \"%s\" in JoinIterators", name1);

    const char * name2 = iter2->name;
    if (name2 == NULL) name2 = "";
    long id2 = (long) this->iteratorDict->Lookup (name2);
    if (id2 == Dictionary::NOTFOUND)
        throw new Exception (true, "Unknown iterator \"%s\" in JoinIterators", name2);

    // Make sure iterators are in different categories

    if (this->iterator[id1].catID == this->iterator[id2].catID)
        throw new Exception (true, "Iterators \"%s\" and \"%s\" are in the same category (#%d); cannot join", name1, name2, this->iterator[id2].catID);

    // Look for preexisting join with the same two iterators.

    int jid;
    for (jid = 0 ; jid < this->numJoins ; jid++) {
        if (id1 == this->join[jid].iter1 && id2 == this->join[jid].iter2 ||
            id1 == this->join[jid].iter2 && id2 == this->join[jid].iter1) {
            break;
            }
        }

    if (jid == this->numJoins) {    // not found, allocate new slot
        if (this->numJoins >= DD::MAXJOINS)
            throw new Exception (true, "Configuration join table is full (> %d entries)", DD::MAXJOINS);

        this->join[jid].iter1     = id1;
        this->join[jid].iter2     = id2;
        this->join[jid].local     = false;
        this->join[jid].leader    = false;
        this->join[jid].follower  = false;
        this->join[jid].stream    = NULL;
        this->join[jid].handle[0] = '\0';
        this->numJoins++;

        // Add string representation to dictionary for quick lookup
        char string [1000];
        sprintf (string, "%ld:%ld", min (id1, id2), max (id1, id2));
        this->joinDict->Insert (string, (void *) jid);
        }

    // Add neighbors to each other

    iter1->AddNeigh (iter2);
    iter2->AddNeigh (iter1);

    this->log->Write (Log::CONFIG_MINOR, "Joined \"%s\" and \"%s\"", name1, name2);
    }


void
DD::PrintJoinTable (
    void
    ) {

    for (int jid = 0 ; jid < this->numJoins ; jid++)
        this->log->Write (Log::CONFIG_MINOR, "JoinTable #%-2d %-6s iter1=%s:%d, iter2=%s:%d, handle=\"%s\" stream=0x%x",
                        jid,
                        (this->join[jid].local ? "local" : this->join[jid].leader ? "lead" : this->join[jid].follower ? "follow" : ""),
                        this->iterator[this->join[jid].iter1].name,
                        this->iterator[this->join[jid].iter1].iterator->node->nodeID,
                        this->iterator[this->join[jid].iter2].name,
                        this->iterator[this->join[jid].iter2].iterator->node->nodeID,
                        this->join[jid].handle,
                        this->join[jid].stream
                        );
    }


//------------------------------------------------------------------------------


static void
check_md_file (
    char * filename
    ) {

    // The DD-bounds file contains subdomain names of the form xxx.grd.
    // Reform to xxx.mdf and check that this exists.

    int i = strlen (filename);

    if (i >= (int) DD::MAXSTRING)
        throw new Exception (true, "File name in DD-bound file is too long");

    for ( ; i >= 0 && filename[i] != '.' ; i--);        // find last dot

    if (i <= 0)
        throw new Exception (true, "Improperly formed file name in DD-bound file");
    if (i >= (int) DD::MAXSTRING-4)
        throw new Exception (true, "MD-file name derived from DD-bound file will be too long");

    strncpy (filename+i, ".mdf", 4);

    if (! readable_file (filename))
        throw new Exception (true, "Cannot find subdomain input file \"%s\"", filename);
    }


static bool
readable_file (
    const char * filename
    ) {

    int file;
    struct stat filestat;

    if ((file = open (filename, O_RDONLY)) == -1)
        return false;   // cannot open

    if (fstat (file, &filestat) != 0) {
        close (file);
        return false; // cannot get status, impossible failure?
        }

    close (file);
#if defined (WIN32)
    if (filestat.st_mode & _S_IFDIR)
#else
    if (S_ISDIR (filestat.st_mode))
#endif
        return false;   // readable directory

    return true;
    }


static char *
process_name (
    const char * filename,
    char * processname
    ) {

    // Extract the word between the last slash and last dot from the first
    // argument and store the result in the second argument

    int dot = -1;
    int slash = -1;
    int i;

    for (i = strlen (filename) - 1 ; i >= 0 ; i--) {
        if (filename[i] == '.') {
            dot = i;
            break;
            }
        }

    for ( ; i >= 0 ; i--) {
        if (filename[i] == '/') {
            slash = i;
            break;
            }
        }

    if (dot == slash)
        strcpy (processname, filename);
    else if (dot == -1)
        strcpy (processname, &filename[slash+1]);
    else {
        strncpy (processname, &filename[slash+1], dot-slash-1);
        processname[dot-slash-1] = '\0';
        }

    return processname;
    }


//------------------------------------------------------------------------------


void
DD::InitializeLocalMessageBuffers (
    void
    ) {

    // Initialize message buffers for local send/receive

    Iterator::Channel channel;
    channel.head = 0;
    channel.tail = 0;

    for (int i = 0 ; i < Iterator::MESGBUFSIZE ; i++) {
        channel.ring[i].data = NULL;
        channel.ring[i].size = 0;
        channel.ring[i].tag = 0;
        }

    for (int jid = 0 ; jid < this->numJoins ; jid++) {
        if (this->join[jid].local) {
            char a2b_name [DD::MAXSTRING];
            sprintf (a2b_name, "channel-%d-%d", this->join[jid].iter1, this->join[jid].iter2);

            if (pthread_mutex_init (&channel.mutex, NULL) != 0)
                throw new Exception (true, "Cannot create mutex for local message channel");

            channel.sync = new Semaphore (a2b_name, 0, this->log);

            this->join[jid].a2b = channel;

            char b2a_name [MAXSTRING];
            sprintf (b2a_name, "channel-%d-%d", this->join[jid].iter2, this->join[jid].iter1);

            if (pthread_mutex_init (&channel.mutex, NULL) != 0)
                throw new Exception (true, "Cannot create mutex for local message channel");

            channel.sync = new Semaphore (b2a_name, 0, this->log);
            this->join[jid].b2a = channel;
            }
        }
    }


void
DD::StartLocalIteratorThreads (
    void
    ) {

    // Create semaphore for initialization phase and
    // start threads for all iterators that run on this node

    this->log->Write (Log::MAJOR, "Starting local iterator threads...");

    this->initSync = new Semaphore ("initsync", 0, this->log);

    int localiter = 0;  // number of iterators running on this node

    for (int iid = 0 ; iid < this->numIterators ; iid++) {
        if (this->iterator[iid].iterator->node->nodeID == this->nodeID) {
            localiter++;
            this->log->Write (Log::ITER_MAJOR, "Spawning thread for iterator #%d \"%s\" \"%s\"",
                                    iid,
                                    this->category[this->iterator[iid].catID].name,
                                    this->iterator[iid].name
                                    );

            if (pthread_create (&this->iterator[iid].thid, &this->thattr, &IteratorShell, (void *) iid) != 0)
                throw new Exception (true, "Pthreads error: Cannot create shell thread, errno=%d", errno);

            this->iterator[iid].thread = true;
            this->iterator[iid].iterator->sync->VSem ();

            // ToDo: collapsed fork/start loop.  OK? VSem sill needed?
            this->initSync->PSem ();
            }
        }

    if (localiter == 0)
        this->log->Write (Log::CONFIG_MAJOR, "This node does not have any iterators (unexpected!)");
    else
        this->log->Write (Log::CONFIG_MAJOR, "All %d iterators on this node have initialized", localiter);
    }


void
DD::RunSimulation (
    void
    ) {

    // Resume all iterators and let them run concurrently

    for (int iid = 0 ; iid < this->numIterators ; iid++) {
        if (this->iterator[iid].iterator->node->nodeID == this->nodeID) {
            this->iterator[iid].iterator->sync->VSem ();
            }
        }

    // Wait for undetached iterators to terminate

    for (int iid = 0 ; iid < this->numIterators ; iid++) {
        if (this->iterator[iid].iterator->node->nodeID == this->nodeID) {
            if (! this->iterator[iid].detached) {
                this->log->Write (Log::DETAIL, "Waiting for termination of iterator \"%s\"", this->iterator[iid].name);
                if (pthread_join (this->iterator[iid].thid, NULL) != 0)
                    throw new Exception (true, "Pthreads error: Cannot join with thread, errno=%d", errno);

                this->iterator[iid].thread = false;
                this->log->Write (Log::DETAIL, "Iterator \"%s\" has terminated", this->iterator[iid].name);
                }
            }
        }
    }


//------------------------------------------------------------------------------
//  Utility functions


char *
GetHostnamePID (
    void
    ) {

    // This routine gets the hostname and process ID and stores them in
    // an allocated buffer.  The caller is responsible for freeing the buffer.

    int bufsize = 100;
    char * hostnamePID = new char [bufsize];
    memset (hostnamePID, '\0', bufsize);

#ifdef WIN32
    // For some reason this goes wrong on Windows in example 02_domaindecomposition
    gethostname (hostnamePID, bufsize);
#else
    if (gethostname (hostnamePID, bufsize) != 0)
        throw new Exception (true, "Cannot get hostname for distributed DD");
#endif

    // Chop off domain name if it's present

    char * dot = strchr (hostnamePID, '.');
    if (dot != NULL) *dot = '\0';

    // Add process ID

    int len = strlen (hostnamePID);
    if (len + 16 >= bufsize)
        throw new Exception (true, "Hostname too long for buffer in distributed DD");

    sprintf (hostnamePID + len, ":%d", getpid ());
    return hostnamePID;
    }
