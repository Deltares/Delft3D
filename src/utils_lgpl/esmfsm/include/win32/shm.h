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
/* ------------------------------------------------------------------
   Emulation of shared memory simply using malloc() and free()
   Define the prototypes and macros
------------------------------------------------------------------ */

/* Dummy macros, but make them behave "intelligently" anyway */
#define IPC_CREAT 1
#define IPC_RMID  2

#define SHM_R     4
#define SHM_W     8

#define SEM_A    16
#define SEM_R    32

/*
typedef unsigned long key_t ;
*/
typedef long key_t ;

struct shmid_ds
{
   int dummy ;
} ;
struct sembuf
{
   int value1 ;
   int value2 ;
   int value3 ;
} ;

int   shmget( key_t ipc_key, size_t blocksize, int flags ) ;
int   shmctl( int key_index, int flags, struct shmid_ds *buffer ) ;
void *shmat(  int key_index, struct shmid_ds *buffer, int flags ) ;
int   shmdt(  struct shmid_ds *buffer ) ;

int   semget( key_t ipc_key, int initial, int flags ) ;
int   semctl( key_t ipc_key, int value, int flags ) ;
int   semop ( key_t ipc_key, struct sembuf *buffer, int value ) ;

int   getpid( void ) ;
