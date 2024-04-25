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
// $Id: dd.h 962 2011-10-31 21:52:47Z elshoff $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/dd.h $
//------------------------------------------------------------------------------
//  d_hydro Flow2D3D Component
//  Domain Decomposition Initialization - DEFINITIONS
//
//  Irv.Elshoff@Deltares.NL
//  31 oct 11
//------------------------------------------------------------------------------


#pragma once


class Blob;
class Category;
class DD;
class Dictionary;
class Iterator;
class List;
class Node;
class NodeSet;
class Semaphore;


//------------------------------------------------------------------------------
//  INCLUDES
//  Every include file need by DD is mentioned below.  The order is not
//  significant.  Each include file includes what it needs.  The "#prama once"
//  directives will topologically sort the definitions.  We choose a slight
//  inefficiency at compile time so that we don't have to worry about details.
//  Every include file must have a unique name.  The include paths are defined
//  as a central macro STANDARD_C_INCLUDES in the top-level common.am file.
//  All source files should only include "Flow2D3D.h" (not with angle brackets!)


// Other d_hydro includes

#include "precision.h"
#include "stream.h"

// Includes from  the "dd" directory

#include "blob.h"
#include "category.h"
#include "dictionary.h"
#include "iterator.h"
#include "list.h"
#include "node.h"
#include "semaphore.h"

// Includes from the "dd/iterators" directory

#include "dredgemerge.h"
#include "gawsbarrier.h"
#include "initbarrier.h"
#include "minimumbarrier.h"
#include "rtccomm.h"

// Includes from the "dd/mapper" directory

#include "context-flowside.h"
#include "context-gawsside.h"
#include "context-mapside.h"
#include "context.h"
#include "dd_messages.h"
#include "flow_in_hydra.h"
#include "flow_nxtstp.h"
#include "flow_steps_c.h"
#include "gaws.h"
#include "gaws_wang_eq.h"
#include "map_debug.h"
#include "map_messages.h"
#include "maploops.h"
#include "mapper.h"
#include "subdomGlobals.h"
#include "varinfocoll.h"


//------------------------------------------------------------------------------


class DD {
    public:
        DD (
            Flow2D3D * flow,
            XmlTree *  config
            );

        ~DD (
            void
            );

        void
        Run (
            void
            );

    public:
        enum {
            //INFINITY    = UINT_MAX,     // largest possible integer
            //MAXSTREAM       = 20,       // max host:port string length in bytes
            //MAXNODES        = 250,      // max number of parallel hosts

            MAXCATEGORIES   = 10,       // max number of categories
            MAXCONFIGBLOB   = 20000,    // max size of a config blob in bytes
            MAXITERATORS    = 250,      // max number of iterators
            MAXJOINS        = 200,      // max number of joins between iterators
            MAXSTRING       = 1000,     // max string length in bytes
            };

        typedef enum {
            ROLE_UNDEFINED = 12000,
            ROLE_SINGLE,
            ROLE_MASTER,
            ROLE_SLAVE
            } Role;

        struct {
            Category *  category;
            char        name [MAXSTRING];
            } category [MAXCATEGORIES];

        struct {
            Iterator *  iterator;           // object reference
            char        name [MAXSTRING];
            char        blob [MAXCONFIGBLOB];
            int         blobsize;
            IteratorFunction    function;
            int         catID;              // index in category table
            bool        detached;           // true => don't wait for iterator termination
            char        logname [MAXSTRING];// name (prefix) of log file
            FILE *      logfile;            // file descriptor for log file
            pthread_t   thid;               // Pthread ID on host node
            bool        thread;             // true => thid refers to a existing thread
            } iterator [MAXITERATORS];

        struct {
            int         iter1;              // index in iterator table
            int         iter2;              // index in iterator table
            bool        local;              // true if both iterators on same node
            bool        leader;             // true if this slave is leader of this join
            bool        follower;           // true if this slave is follower of this join
            Iterator::Channel     a2b;      // for local to local iterator
            Iterator::Channel     b2a;      // for local to local iterator
            Stream *    stream;             // for semi-remote communication
            char        handle [Stream::MAXHANDLE]; // lead stream identity
            pthread_t   thid;               // Pthread ID of lead/follow helper thread
            bool        thread;             // true => thid refers to a existing thread
            } join [MAXJOINS];

    public:
        int     DDGetThreadID     (void);

    public:
        Flow2D3D *  flow;               // reference to overall Flow object
        pthread_key_t   thiter;         // contains thread iterator index
        Role        role;

        int         numCategories;
        int         numSubdomains;
        int         numIterators;
        int         numJoins;
        int         numMappers;

        int         nodeID;             // identity of this DD node [0..numNodes-1]
        int         numNodes;           // total number of nodes

        Dictionary *    categoryDict;   // name to config table index
        Dictionary *    iteratorDict;   // name to config table index
        Dictionary *    joinDict;       // join list for quick lookup
        Dictionary *    processDict;    // name to process (iterator? ToDo)

        Log *           log;            // logging facility
        Semaphore *     initSync;       // for ready serialization
        Semaphore *     leadFollow;     // for synchronization of slave join lead/follow threads

        Iterator *      dredgem;
        Iterator *      gawsbar;
        Iterator *      initbar;
        Iterator *      minbar;
        Iterator *      rtc;

        static const char dredgeMergeCategoryName [];
        static const char gawsBarCategoryName     [];
        static const char initBarCategoryName     [];
        static const char mapperCategoryName      [];
        static const char minBarCategoryName      [];
        static const char processCategoryName     [];
        static const char rtcCategoryName         [];

    private:
        void    CreateFixedEntities             (void);
        void    InitThreads                     (void);
        void    InitializeLocalMessageBuffers   (void);
        void    MasterProcess                   (void);
        void    PrintJoinTable                  (void);
        void    ReadConfig                      (void);
        void    RunSimulation                   (void);
        void    SingleProcess                   (void);
        void    SlaveProcess                    (void);
        void    StartLocalIteratorThreads       (void);

        void
        ReadDDBounds (
            FILE *  input,
            const char *  filename
            );

        Iterator *
        AddProcess (
            const char *    name,
            const char *    config,
            int             node
            );

        Iterator *
        AddMapper (
            const char *    name,
            const char *    config,
            Iterator *      leftprocess,
            Iterator *      rightprocess,
            int             node
            );

        void
        JoinIterators (
            Iterator * iter1,
            Iterator * iter2
            );

    private:
        Clock *     clock;              // timing facility
        XmlTree *   config;             // top level DD confiruration
        XmlTree *   multiNode;          // multi-node sub-configuration
        NodeSet *   nodeSet;            // info on all nodes in mult-node mode
        int         mappersOnThisNode;  // run SetupMinimumBarrier if > 0

        pthread_attr_t  thattr;         // attributes for thread creation

        enum {    // special thread self (thiter) values
            ID_MAIN = -12000,
            ID_MASTER,
            ID_SLAVE
            };

        Clock::Timestamp masterStart;   // time master/single DD process started

        Category *      dredgeCat;
        Category *      gawsbarCat;
        Category *      initbarCat;
        Category *      minbarCat;
        Category *      mapperCat;
        Category *      processCat;
        Category *      rtcCat;

        List *          categoryList;
        List *          iteratorList;
        List *          mapperList;
        List *          processList;
        };


//------------------------------------------------------------------------------


#define ROLENAME(R) ( \
            (R) == DD::ROLE_SINGLE  ? "single" : \
            (R) == DD::ROLE_MASTER  ? "master" : \
            (R) == DD::ROLE_SLAVE   ? "slave"  : \
            "undefined" \
            )


//------------------------------------------------------------------------------
//  Iterator functions implemented in dd/mapper subdirectory


void
FlowDD_Process (
    Iterator * self,
    const char * name,
    Blob * configblob
    );


void
FlowDD_Mapper (
    Iterator * self,
    const char * name,
    Blob * configblob
    );


//------------------------------------------------------------------------------
//  Utility functions


char *
GetHostnamePID (
    void
    );


//------------------------------------------------------------------------------


#ifdef DD_MAIN
    // String constant initialization
    const char DD::dredgeMergeCategoryName  [] = "dredgeMerge";
    const char DD::gawsBarCategoryName      [] = "gawsBarrier";
    const char DD::initBarCategoryName      [] = "initBarrier";
    const char DD::mapperCategoryName       [] = "mappers";
    const char DD::minBarCategoryName       [] = "minBarrier";
    const char DD::processCategoryName      [] = "processes";
    const char DD::rtcCategoryName          [] = "rtc";
#endif
