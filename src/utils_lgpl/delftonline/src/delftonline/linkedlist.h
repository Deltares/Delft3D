//------------------------------------------------------------------------------
//  DelftOnline
//  Linked List - DEFINITIONS
//
//  Irv.Elshoff@Deltares.NL
//  27 apr 12
//------------------------------------------------------------------------------
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

class LinkedList {
    public:
        LinkedList      (void);
        ~LinkedList     (void);

        void    Append  (void * value);
        void    Insert  (void * value);
        bool    Delete  (void * value);
        void    Rewind  (void);
        void *  Next    (void);
        int     Count   (void);

    private:
        typedef struct node_struct {
            void * value;
            struct node_struct * next;
            } * node;

        node    head;       // first element
        node    tail;       // last element
        node    current;    // element pointer for Rewind/Next
        int     count;      // number of elements in the list
    };
