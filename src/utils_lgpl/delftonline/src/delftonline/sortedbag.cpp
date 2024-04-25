//------------------------------------------------------------------------------
//  DelftOnline - Sorted Bag of Tagged Non-negative Integers - IMPLEMENTTATION
//
//  Irv.Elshoff@Deltares.NL
//  26 may 12
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


#include "sortedbag.h"


//------------------------------------------------------------------------------
//  Construct and destructor


SortedBag::SortedBag (
    void
    ) {

    this->size = this->allocationSize;

    this->values = new int    [this->size+1];
    this->tags   = new void * [this->size+1];

    for (int i = 0 ; i < this->size+1 ; i++) {
        this->values[i] = -1;
        this->tags[i]   = NULL;
        }

    this->count = 0;
    }


SortedBag::~SortedBag (
    void
    ) {

    delete [] this->values;
    delete [] this->tags;
    }


//------------------------------------------------------------------------------


void
SortedBag::Add (
    int value,
    void * tag      // default NULL
    ) {

    if (this->count == this->size) {
        // The fixed-size array has become too small.  Make a larger one
        // and copy over the existing values, then get rid of the small array.

        int newsize = this->size + this->allocationSize;
        int * newvalues = new int    [newsize+1];
        void ** newtags = new void * [newsize+1];

        for (int i = 0 ; i < this->size ; i++) {
            newvalues[i] = this->values[i];
            newtags[i]   = this->tags[i];
            }

        delete [] this->values;
        delete [] this->tags;

        for (int i = this->size ; i < newsize+1 ; i++) {
            newvalues[i] = -1;
            newtags[i] = NULL;
            }

        this->size = newsize;
        this->values = newvalues;
        this->tags = newtags;
        }

    for (int i = 0 ; i < this->count ; i++) {
        if (value <= this->values[i] || this->values[i] == -1) {
            if (value <= this->values[i])
                for (int j = this->count ; j > i ; j--) {       // move everything up one slot to make room for the new entry
                    this->values[j] = this->values[j-1];
                    this->tags[j] = this->tags[j-1];
                    }

            this->values[i] = value;
            this->tags[i] = tag;
            this->count++;
            return;
            }
        }

    this->values [this->count] = value;
    this->tags   [this->count] = tag;
    this->count++;
    }


bool
SortedBag::Delete (
    int value,
    void * tag      // default NULL
    ) {

    if (this->values[0] == -1)
        return false;

    for (int i = 0 ; i < this->count ; i++) {
        if (value == this->values[i] && (tag == NULL || tag == this->tags[i])) {
            for (int j = i ; j < this->count ; j++) {       // move everything down one slot
                this->values[j] = this->values[j+1];
                this->tags[j] = this->tags[j+1];
                }

            this->count--;
            return true;
            }
        }

    return false;
    }


int
SortedBag::Min (
    void ** tag     // default NULL
    ) {

    if (tag != NULL)
        *tag = this->tags[0];

    return this->values[0];
    }
