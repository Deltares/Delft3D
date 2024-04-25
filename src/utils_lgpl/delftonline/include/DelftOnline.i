!--------------------------------------------------------------------------------
!   DelftOnline -- Constant Definitions for Fortran
!
!   Irv.Elshoff@wldelft.nl
!   4 apr 07
!
!--------------------------------------------------------------------------------
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


!-----  DataElement BaseType

integer, parameter ::   DOL_OPAQUE          = 0
integer, parameter ::   DOL_INTEGER         = 1
integer, parameter ::   DOL_REAL            = 2
integer, parameter ::   DOL_DOUBLE          = 3
integer, parameter ::   DOL_DOUBLECOMPLEX   = 4
integer, parameter ::   DOL_COMPLEX         = 5
integer, parameter ::   DOL_LOGICAL         = 6
integer, parameter ::   DOL_CHARACTER       = 7

!-----  Called Function Language

integer, parameter ::   DOL_C               = 1
integer, parameter ::   DOL_FORTRAN         = 2

!-----  Read/Write Access

integer, parameter ::   DOL_IN              = 1
integer, parameter ::   DOL_OUT             = 2
integer, parameter ::   DOL_INOUT           = 3

