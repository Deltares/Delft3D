//------------------------------------------------------------------------------
//  DelftOnline
//  Dictionary Class - IMPLEMENTATION
//
//  ToDo: Reinstate hashing
//
//  Irv.Elshoff@Deltares.NL
//  26 apr 12
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


#include "dictionary.h"


//------------------------------------------------------------------------------
//  A dictionary implemented as a hash table
//  Keys are strings, values arbitrary pointers


Dictionary::Dictionary (
    const char * name
    ) {

    this->name = name;

    for (int i = 0 ; i < SIZE ; i++) {
        this->table[i].key   = NULL;
        this->table[i].value = (void *) NOTFOUND;
        }

    sprintf (this->fullMessage, "Dictionary full (%d slots)", SIZE);
    }


Dictionary::~Dictionary (
    void
    ) {

    for (int i = 0 ; i < SIZE ; i++)
        if (this->table[i].key != NULL)
            delete [] this->table[i].key;
    }


char *
Dictionary::Insert (
    const char *  key,
    void *  value
    ) {

    //int hashcode = hash (key, SIZE);
    int i;

    for (i = 0 ; i < SIZE ; i++) {
        if (this->table[i].key == NULL) {       // free slot
            this->table[i].key = new char [strlen (key) + 1];
            strcpy (this->table[i].key, key);
            this->table[i].value = value;
            return NULL;
            }

        if (strcmp (this->table[i].key, key) == 0)
            return (char *) "Duplicate key";

        i = (i+1) % SIZE;
        }

    return this->fullMessage;
    }


void *
Dictionary::Lookup (
    const char *  key
    ) {

    //int hashcode = hash (key, SIZE);

    for (int i = 0 ; i < SIZE ; i++) {
        if (this->table[i].key == NULL) // free slot
            return (void *) NOTFOUND;
        else if (strcmp (this->table[i].key, key) == 0)   // key found
            return this->table[i].value;

        i = (i+1) % SIZE;
        }

    return (void *) NOTFOUND;    // table full and key not found
    }


//------------------------------------------------------------------------------


/*
int
Dictionary::hash (
    char *  key,
    int size
    ) {

    int     hashcode = 0;

    for (unsigned int i = 0 ; i < strlen (key) ; i++) {
        hashcode = (hashcode << 3) + (int) key[i];
        if (i % 7 == 0)
            hashcode %= size;
        }

    return hashcode % size;
    }
*/

