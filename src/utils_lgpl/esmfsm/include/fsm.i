!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  $Id$
!  $HeadURL$
!-------------------------------------------------------------------------------
!   Delft-FSM (FORTRAN Shared Memory)
!   Definitions for Fortran programs
!
!   Irv.Elshoff@deltares.nl
!   3 aug 06
!
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
!   API Functions and Subroutines
!
    integer     fsmini
    external    fsmini

    integer(kind=pntrsize) :: makptr
    external    makptr

    integer(kind=pntrsize) :: getptr
    external    getptr

    integer     relptr
    external    relptr

    integer     prtkey
    external    prtkey

    integer     fsmerr
    external    fsmerr
    
    integer     esminif
    external    esminif   
    
    integer     esmcreatef
    external    esmcreatef

!-------------------------------------------------------------------------------
!   FSM Pointer Type Definitions

    integer (kind=4) ibuf
    real    (kind=4) rbuf
    real    (kind=8) dbuf       ! double precision
    complex (kind=8) dcbuf      ! double complex
    complex (kind=4) cbuf
    logical          lbuf
    character(len=1) chbuf

    integer    &
       ityp,   &
       rtyp,   &
       dtyp,   &
       dctyp,  &
       ctyp,   &
       ltyp,   &
       chtyp

    parameter (     &
        ityp  = 1,  &
        rtyp  = 2,  &
        dtyp  = 3,  &
        dctyp = 4,  &
        ctyp  = 5,  &
        ltyp  = 6,  &
        chtyp = 7   &
        )


!-------------------------------------------------------------------------------
!   Pseudo-storage for each data type.  An array is declared so that it can
!   be misused by indexing it using the pointers returned from ESM.

    common /dynmem/     &
        ibuf  (0:0),    &
        rbuf  (0:0),    &
        dbuf  (0:0),    &
        dcbuf (0:0),    &
        cbuf  (0:0),    &
        lbuf  (0:0),    &
        chbuf (0:0)


!-------------------------------------------------------------------------------
!   Flags for FSMINI (coupled with definitions in "esm.h")

    integer             &
        FSM_SILENT,     &
        FSM_TRACE

    parameter (                 &
        FSM_SILENT      = 1,    &
        FSM_TRACE       = 2     &
        )


!-------------------------------------------------------------------------------
!   Background Information:
!   Jan Mooiman/Arjen Markus wrote:
!
!   Adresses and memory allocation are as follows
!   Memory layout:
!   bit pattern, every <x> is a byte
!   base adress is multiple of
!   memory needed for number (double needs 8 bytes, denoted by -)
!   type
!
!   xxxx xxxx xxxx xxxx xxxx xxxx xxxx xxxx xxxx xxxx xxxxx xxxx
!   16   4    8    4    16   4    8    4    16   4    8     4
!   ---- ---- --------- ------------------- --------- ----- ----
!   int  real double    double complex      complex   logi  char
!   4    4    8         16                  8         4     1
!
!
!   base        multiple of
!   ------------------------
!   loc(ibuf)   16              !!!!!!!!! This is a very important assumption
!   loc(rbof)    4
!   loc(dbuf)    8
!   loc(dcbuf)  16
!   loc(cbuf)   16
!   loc(lbuf)    8
!   loc(chbuf)   4
!
!   Example:
!   The length of a double complex is 16, therefor the base adress for
!   double complex have to be a multiple of 16.
