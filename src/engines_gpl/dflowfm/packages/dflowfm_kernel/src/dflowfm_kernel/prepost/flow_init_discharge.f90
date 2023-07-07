!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2023.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! 
! 

subroutine flow_init_discharge()
   use properties
   implicit none
   type(TREE_DATA), pointer :: dis_ptr

   character(len=64) :: dis_type
   character(len=1024) :: rec

         rec = ' '
         ! [discharge]
         call prop_get(dis_ptr, '', 'id', rec)
         call prop_get(dis_ptr, '', 'polylinefile', rec)
         call prop_get(dis_ptr, '', 'type', dis_type) ! normal, momentum, walking, in-out
         !call prop_get(dis_ptr, '', 'interpolation', rec) ! linear, block

         !if (.not. success .or. len_trim(rec) == 0) then
         !   write(msgbuf, '(a,a,a)') 'Required field ''crest_level'' missing in weir ''', trim(strid), '''.'
         !   call warn_flush()
         !   cycle
         !end if
         !read(rec, *, iostat = ierr) tmpval
         !if (ierr /= 0) then ! No number, so check for timeseries filename
         !   if (trim(rec) == 'REALTIME') then
         !      success = .true.
         !      ! zcgen(1, 1+kx, ..) should be filled via DLL's API
         !      write(msgbuf, '(a,a,a)') 'Control for weir ''', trim(strid), ''', crest_level set to REALTIME.'
         !      call dbg_flush()
         !   else
         !      qid = 'generalstructure' ! TODO: werkt dit als je de losse quantities (crest/gateloweredge/width) dezelfde id geeft, maar wel netjes correct veschillende offset?
         !      fnam = trim(rec)
         !      ! Time-interpolated value will be placed in zcdam(n) when calling ec_gettimespacevalue.
         !      success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, fnam, uniform, spaceandtime, 'O', targetIndex=(n-1)*kx+1)
         !   end if
         !else
         !   zcdam((n-1)*kx+1) = tmpval ! Constant value for always, set it now already.
         !end if
         !
         !tmpval = dmiss
         !call prop_get(str_ptr, '', 'lat_contr_coeff', tmpval)
         !! TODO: Herman/Jaco: this is not relevant anymore, using width (gate only)??
         !
         !nweirgen = nweirgen+1
         !weir2cgen(nweirgen) = n ! Mapping from 1:nweirgen to underlying generalstructure --> (1:ncgensg)


end subroutine flow_init_discharge
