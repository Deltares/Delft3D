module m_GlobalParameters
!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.             
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
!  
!  
!-------------------------------------------------------------------------------
   
   use MessageHandling
   use m_node
   
   
   public
   
   double precision                 :: gravity                       = 9.81d0
   logical                          :: anyGroundLayer                = .false.
   logical                          :: anySummerDike                 = .false.
   !TODO: Remove these drying and flooding options and also in all compute structure subroutines. This should be handled by Hu(L)
   double precision                 :: summerDikeTransitionHeight    = 0.50d0 !read from CrossSectionDefinitions.ini 
   double precision                 :: strucalfa                     = 0.9d0
   double precision                 :: sl                            = 0.01d0        !< width at top of closed profile (Preisman lock)
   double precision                 :: pi                            = 3.141592653589793d0
   double precision                 :: ThresholdForPreismannLock     = 0.02d0
   double precision                 :: minSectionLength              = 1.0
   double precision, public         :: missingvalue                  =-999.999d0
   double precision, public         :: flow1d_eps10                  = 1d-10
   integer, public                  :: maxlenpar                     = 100000   
   
   type, public :: t_chainage2cross
      integer :: c1 = -1           !< cross section index 1
      integer :: c2 = -1           !< cross section index 2
      double precision :: f        !< fraction: c_loc = f * c1 + (1-f)*c2
      double precision :: distance !< geometric distance between two cross sections
   end type
 
   type t_filenames
      character(len=255) :: cross_section_definitions    = ' ' !< 1d cross section definitions
      character(len=255) :: cross_section_locations      = ' ' !< 1d cross section locations
      character(len=1024):: roughness                    = ' ' !< 1d roughness files
      character(len=255) :: roughnessdir                 = ' ' !< location of roughness files
      character(len=255) :: storage_nodes                = ' ' !< 1d cross section retention manhole definitions
      character(len=255) :: structures                   = ' ' !< structure file
   end type

   ! Structure Types
   integer, public, parameter              :: ST_UNSET         = -1
   integer, public, parameter              :: ST_WEIR          =  2
   integer, public, parameter              :: ST_ORIFICE       =  3
   integer, public, parameter              :: ST_PUMP          =  4
   integer, public, parameter              :: ST_GATE          =  5
   integer, public, parameter              :: ST_GENERAL_ST    =  6
   integer, public, parameter              :: ST_UNI_WEIR      =  7
   integer, public, parameter              :: ST_DAMBREAK      =  8
   integer, public, parameter              :: ST_CULVERT       =  9
   integer, public, parameter              :: ST_BRIDGE        = 10
   integer, public, parameter              :: ST_COMPOUND      = 11
   integer, public, parameter              :: ST_LONGCULVERT   = 12
   integer, public, parameter              :: ST_DAM           = 13
   integer, public, parameter              :: ST_OBS_STATION   = 14
   integer, public, parameter              :: ST_CROSS_SECTION = 15
   integer, public, parameter              :: ST_RUNUP_GAUGE   = 16
   integer, public, parameter              :: ST_SOURCE_SINK   = 17
   integer, public, parameter              :: ST_GATEGEN       = 18
   integer, public, parameter              :: ST_LATERAL       = 19

   integer, public, parameter              :: ST_MAX_TYPE      = 19 !< Max id of structure types. The preceding ids must be lower than or equal to this.

   ! Flow geometry / computational grid
   integer, public, parameter              :: INDTP_1D      = 1  !< Type code for flow nodes that are 1D
   integer, public, parameter              :: INDTP_2D      = 2  !< Type code for flow nodes that are 2D
   integer, public, parameter              :: INDTP_ALL     = 3  !< Type code for flow nodes that are 1D or 2D
   
end module m_GlobalParameters                                 
