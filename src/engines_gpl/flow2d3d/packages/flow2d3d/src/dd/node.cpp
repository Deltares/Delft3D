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
// $Id: node.cpp 932 2011-10-25 09:41:59Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/node.cpp $
//------------------------------------------------------------------------------
//  d_hydro Flow2D3D Component
//  Domain Decomposition MultiNode Support - IMPLEMENTATION
//
//  Irv.Elshoff@Deltares.NL
//  6 jun 11
//------------------------------------------------------------------------------


#include "flow2d3d.h"


//------------------------------------------------------------------------------
//  Node constructor and destructor


Node::Node (
    int nodeID,
    const char * hostname
    ) {

    this->nodeID        = nodeID;
    this->hostname      = strdup (hostname);
    this->stream        = NULL;
    this->remotePID     = 0;

    this->iterators     = new List ();
    this->numIterators  = 0;
    }


Node::~Node (
    void
    ) {

    free((void *)this->hostname);
    }


//------------------------------------------------------------------------------
//  Node addtion routines


void
Node::AddIterator (
    Iterator * iterator
    ) {

    this->iterators->Append ((void *) iterator);
    this->numIterators++;
    }


//------------------------------------------------------------------------------
//  NodeSet constructor and destructor


NodeSet::NodeSet (
    void
    ) {

    this->nodeList      = new List ();
    this->node          = NULL;
    this->numNodes      = 0;
    }


NodeSet::~NodeSet (
    void
    ) {

    if (this->nodeList != NULL) {
        this->nodeList->Rewind ();
        Node * node;
        while ((node = (Node *) this->nodeList->Next ()) != NULL)
            delete node;

        delete this->nodeList;
        }

    else if (this->node != NULL) {
        for (int id = 0 ; id < this->numNodes ; id++)
            delete this->node[id];

        delete [] this->node;
        }
    }


//------------------------------------------------------------------------------
//  NodeSet addtion routines


void
NodeSet::AddNodesFromFile (
    const char * nodeListFileName
    ) {

    FILE * nodeListFile = fopen (nodeListFileName, "r");
    if (nodeListFile == NULL)
        throw new Exception (true, "Cannot open node list file \"%s\": %s", nodeListFileName, strerror (errno));

    char line [DD::MAXSTRING];
    while (fgets (line, sizeof line, nodeListFile) != NULL) {
        String::Chomp (line);
        if (strlen (line) > 0)
            this->AddNode (line);
        }
    }


void
NodeSet::AddNodesFromString (
    const char * nodeListString
    ) {

    // A node list is a whitespace-separated string of host names with an
    // optional colon-delimited repetion count.  Get rid of the whitespace here.

    char * list = strdup (nodeListString);
    String::Tidy (list);
    String::CollapseAllWhitespace (list);

    char * nodename = list;
    char * separator;
    while ((separator = strchr (nodename, ' ')) != NULL) {
        *separator = '\0';
        this->AddNode (nodename);
        nodename = separator + 1;
        }

    if (*nodename != '\0')
        this->AddNode (nodename);

    free (list);
    }


void
NodeSet::AddNode (
    const char * nodeSpecification
    ) {

    // Add one or more nodes to the node set.
    // A node specification is a hostname (or IP address) followed by
    // an optional colon-spearate repetion count.

    char * nodespec = strdup (nodeSpecification);
    char * colon = strchr (nodespec, ':');
    int count;
    if (colon == NULL)
        count = 1;
    else {
        *colon = '\0';
        count = atoi (colon+1);
        }

    for (int i = 0 ; i < count ; i++) {
        this->nodeList->Append ((void *) new Node (this->numNodes, nodespec));
        this->numNodes++;
        }

    free (nodespec);
    }


void
NodeSet::CreateNodeTable (
    void
    ) {

    // This routine is called when the last node has been added.  The list is
    // converted to a table for easier (and faster) access.

    this->node = new Node * [this->numNodes];

    this->nodeList->Rewind ();
    for (int id = 0 ; id < this->numNodes ; id++) {
        Node * node = (Node *) this->nodeList->Next ();
        if (node == NULL)
            throw new Exception (true, "Internal error: Premature end of node list in NodeSet::CreateNodeTable!");

        this->node[id] = node;
        }

    delete this->nodeList;
    this->nodeList = NULL;
    }
