!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
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

      subroutine write_hyd_step(hyd, itime)

      use hydmod
      implicit none

      type(t_hyd)         :: hyd           ! description of the hydrodynamics
      integer             :: itime         ! relative time in file

      ! local

      character(len=20)   :: valnam(1)     ! name of value to be written
      integer             :: notim         ! number of output steps

      notim = 2 ! in fact we do not know but not constant so set to 2

      valnam(1) = 'volume'
      call write_data ( hyd%file_vol, itime, notim, hyd%noseg, 0, 0, 1, 1, 0, valnam, hyd%volume,2)

      valnam(1) = 'area'
      call write_data ( hyd%file_are, itime, notim, hyd%noq1, hyd%noq2, hyd%noq3, 1, 1, 0, valnam, hyd%area,1)

      valnam(1) = 'flow'
      call write_data ( hyd%file_flo, itime, notim, hyd%noq1, hyd%noq2, hyd%noq3, 1, 1, 0, valnam, hyd%flow,1)

      if ( hyd%sal_present ) then
         valnam(1) = 'salinity'
         call write_data ( hyd%file_sal, itime, notim, hyd%noseg, 0, 0, 1, 1, 0, valnam, hyd%sal,1)
      endif

      if ( hyd%tem_present ) then
         valnam(1) = 'temperature'
         call write_data ( hyd%file_tem, itime, notim, hyd%noseg, 0, 0, 1, 1, 0, valnam, hyd%tem,1)
      endif

      if ( hyd%tau_present ) then
         valnam(1) = 'tau'
         call write_data ( hyd%file_tau, itime, notim, hyd%noseg, 0, 0, 1, 1, 0, valnam, hyd%tau,1)
      endif

      if ( hyd%vdf_present ) then
         valnam(1) = 'vdf'
         call write_data ( hyd%file_vdf, itime, notim, hyd%noseg, 0, 0, 1, 1, 0, valnam, hyd%vdf,1)
      endif

      return
      end
