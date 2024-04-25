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
// $Id: list.cpp 932 2011-10-25 09:41:59Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/list.cpp $
//------------------------------------------------------------------------------
//  d_hydro Flow2D3D Component
//  Linked List - Implementation
//
//  Irv.Elshoff@Deltares.NL
//  27 may 11
//-------------------------------------------------------------------------------


#include "flow2d3d.h"


//------------------------------------------------------------------------------
//  Construct and destructor


List::List (
    void
    ) {

    this->head = NULL;
    this->tail = NULL;
    this->current = NULL;
    }


List::~List (
    void
    ) {
    }


//------------------------------------------------------------------------------
//  Routines to add a new element


void
List::Append (
    void *  value
    ) {

    node n = new struct node_struct;
    n->value = value;
    n->next = NULL;

    if (this->head == NULL)
        this->head = n;

    if (this->tail != NULL)
        this->tail->next = n;

    this->tail = n;
    }


void
List::Insert (
    void *  value
    ) {

    node n = new struct node_struct;
    n->value = value;
    n->next = this->head;

    if (this->tail == NULL)
        this->tail = n;

    this->head = n;
    }


//------------------------------------------------------------------------------
//  Routine to delete an element
//  Simple linear search is inefficient for long lists, but most are short


bool
List::Delete (
    void *  value
    ) {

    node n;
    node lastn = NULL;

    for (n = this->head ; n != NULL ; n = n->next) {
        if (value == n->value) {
            if (n == this->head)
                this->head = n->next;
            else
                lastn->next = n->next;

            if (n == this->tail)
                this->tail = lastn;

            return true;
            }

        lastn = n;
        }

    return false;
    }


//------------------------------------------------------------------------------
//  Routines to run through the list


void
List::Rewind (
    void
    ) {
    this->current = this->head;
    }


void *
List::Next (
    void
    ) {

    void * value;

    if (this->current == NULL)
        value = NULL;

    else {
        value = this->current->value;
        this->current = this->current->next;
        }

    return value;
    }
