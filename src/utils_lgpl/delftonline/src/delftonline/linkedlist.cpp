//------------------------------------------------------------------------------
//  DelftOnline
//  Linked List - Implementation
//
//  Irv.Elshoff@Deltares.NL
//  25 may 12
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


#include "linkedlist.h"

#define NULL 0


//------------------------------------------------------------------------------
//  Construct and destructor


LinkedList::LinkedList (
    void
    ) {

    this->head    = NULL;
    this->tail    = NULL;
    this->current = NULL;
    this->count   = 0;
    }


LinkedList::~LinkedList (
    void
    ) {
    }


//------------------------------------------------------------------------------
//  Routines to add a new element


void
LinkedList::Append (
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
    this->count++;
    }


void
LinkedList::Insert (
    void *  value
    ) {

    node n = new struct node_struct;
    n->value = value;
    n->next = this->head;

    if (this->tail == NULL)
        this->tail = n;

    this->head = n;
    this->count++;
    }


//------------------------------------------------------------------------------
//  Routine to delete an element and return true if successful.
//  Simple linear search is inefficient for long lists, but most are short


bool
LinkedList::Delete (
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

            this->count--;
            return true;
            }

        lastn = n;
        }

    return false;
    }


//------------------------------------------------------------------------------
//  Routines to run through the list


void
LinkedList::Rewind (
    void
    ) {
    this->current = this->head;
    }


void *
LinkedList::Next (
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


int
LinkedList::Count (
    void
    ) {

    return this->count;
    }
