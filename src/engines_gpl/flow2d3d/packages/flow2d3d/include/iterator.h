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
// $Id: iterator.h 962 2011-10-31 21:52:47Z elshoff $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/iterator.h $
//------------------------------------------------------------------------------
//  d_hydro Flow2D3D Component - Hydra Executive
//  Iterator Class - DEFINITIONS
//
//  Irv.Elshoff@Deltares.NL
//  25 oct 11
//-------------------------------------------------------------------------------


#pragma once

#include "flow2d3d.h"


typedef void (*IteratorFunction) (
    Iterator *      self,
    const char *    name,
    Blob *          configblob
    );


class Iterator {
    public:
        Iterator (
            DD * dd,
            const char * name,
            Blob * configblob,
            Category * category,
            IteratorFunction function
            );

        ~Iterator (
            void
            );

        void            Detach          (void);
        void            Ready           (void);
        void            SetValue        (void * value);
        void *          GetValue        (void);

        unsigned int    NeighborCount   (Category * category = NULL);
        void            RewindNeighbors (Category * category = NULL);
        Iterator *      NextNeighbor    (Category * category = NULL);

        void            Place           (Node * node);

        void            Send            (Blob * message, int tag = 0);
        void            Receive         (Blob * message, int * ptag = NULL);

        void            AddNeigh        (Iterator *);    // internal use only

    public:
        static const int MESGBUFSIZE = 10;

        typedef struct {
            struct {
                char *  data;
                int     size;
                int     tag;
                } ring [MESGBUFSIZE];       // local temporary storage for message

            int     head;                   // start of queue
            int     tail;                   // end of queue
            Semaphore * sync;               // for local to local sync
            pthread_mutex_t mutex;          // protection for ring buffer
            } Channel;

    public:
        DD *            dd;                 // domain decomposition object
        int             id;                 // index in DD iterator table
        char *          name;               // string for logging
        Blob *          configblob;         // configuration
        Category *      category;           // which category iterator belongs to
        Node *          node;               // pointer to node that hosts the iterator
        Semaphore *     sync;               // syncronization on host node
        void *          value;              // user-defined iterator-specific value

    private:
        int
        FindJoin (
            int     iter1,
            int     iter2
            );

        void
            SendMessage (
                Blob *,
                int,
                Channel *
                );
        void
            ReceiveMessage (
                Blob *,
                int *,
                Channel *
                );

    private:
        IteratorFunction    function;
        bool                detached;

        // Neighbor data:
        // CatNeigh is a descriptor for each category if neighbors.
        // A descriptor contains a list of neighbors of a given category.
        // The list contains pointers to iterators.
        // Neighdict is a dictionary that maps a category name to a descriptor.

        typedef struct {
            int     count;          // number of neighbors on list
            List *  neighbors;      // list of neighbors
            bool    rewound;        // false until first rewind
            } CatNeigh;

        Dictionary *    neighDict;
        CatNeigh        all;        // all neighbors for NextNeighbor (NULL)
    };


//-------------------------------------------------------------------------------
//  Extra-object functions


Iterator *
IteratorSelf (
    void
    );


void *
IteratorShell (
    void * argument
    );
