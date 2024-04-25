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
// $Id: node.h 932 2011-10-25 09:41:59Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/node.h $
//------------------------------------------------------------------------------
//  d_hydro Flow2D3D Component
//  Domain Decomposition MultiNode Support - DEFINITIONS
//
//  Irv.Elshoff@Deltares.NL
//  6 jun 11
//------------------------------------------------------------------------------


#include "flow2d3d.h"


//------------------------------------------------------------------------------
//  Class Node represents a single node in a multi-node DD simulation


class Node {
    public:
        Node (
            int nodeID,
            const char * hostname
            );

        ~Node (
            void
            );

        void
        AddIterator (
            Iterator * iterator
            );

    public:
        int         nodeID;             // ID of node (starting at 0)
        const char * hostname;          // short name, FQDN, or IP address
        Stream *    stream;             // stream with which to communicate with the node
        pid_t       remotePID;          // ID of remote shell process on master host

        List *      iterators;          // list of iterators hosted on this node
        int         numIterators;       // number of   iterators

    private:

    };


//------------------------------------------------------------------------------
//  Class NodeSet represents all nodes in a multi-node DD simulation


class NodeSet {
    public:
        NodeSet (
            void
            );

        ~NodeSet (
            void
            );

        void
        AddNodesFromFile (
            const char * nodeListFileName
            );

        void
        AddNodesFromString (
            const char * nodeListString
            );

        void
        CreateNodeTable (
            void
            );

    public:
        Node **     node;               // table of Node pointers (allocated by CreateNodeTable)
        int         numNodes;           // number of nodes

    private:
        void
        AddNode (
            const char * nodeSpecification
            );

    private:
        List *      nodeList;           // list of distributed nodes (initial phase)

    };
