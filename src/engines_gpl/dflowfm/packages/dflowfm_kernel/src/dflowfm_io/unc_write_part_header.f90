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

!> write particles header to netcdf file
subroutine unc_write_part_header(ifile,id_timedim,id_partdim,id_parttime,id_partx,id_party,id_partz)
   use m_particles
   use netcdf
    use unstruc_netcdf, only: unc_addcoordatts, check_error
   use m_flow, only: kmx
   use m_sferic, only: jsferic
   use unstruc_messages
   use m_missing
   implicit none

   integer, intent(in)    :: ifile  !< output file identifier
   integer, intent(in)    :: id_timedim
   integer, intent(inout) :: id_partdim, id_parttime, id_partx, id_party, id_partz

   character(len=128)     :: mesg

   integer                :: ierr
   integer                :: jaInDefine

   if ( japart.eq.0 ) then
      return
   end if

   ! Put dataset in define mode (possibly again) to add dimensions and variables.
   ierr = nf90_redef(ifile)
   if (ierr == nf90_eindefine) jaInDefine = 1 ! Was still in define mode.
   if (ierr /= nf90_noerr .and. ierr /= nf90_eindefine) then
       call mess(LEVEL_ERROR, 'Could not put header in flow geometry file.')
       call check_error(ierr)
       return
   end if

   ierr = nf90_def_dim(ifile, 'particles', NpartOut, id_partdim)

   ierr = nf90_def_var(ifile, 'particles_time', nf90_double, id_timedim, id_parttime)
   ierr = nf90_put_att(ifile, id_parttime, 'long_name', 'particles time')

   ierr = nf90_def_var(ifile, 'particles_x_coordinate', nf90_double, (/ id_partdim, id_timedim /), id_partx)
   ierr = nf90_def_var(ifile, 'particles_y_coordinate', nf90_double, (/ id_partdim, id_timedim /), id_party)
   ierr = unc_addcoordatts(ifile, id_partx, id_party, jsferic)
   ierr = nf90_put_att(ifile, id_partx, 'long_name', 'x-coordinate of particles')
   ierr = nf90_put_att(ifile, id_party, 'long_name', 'y-coordinate of particles')
   ierr = nf90_put_att(ifile, id_partx, '_FillValue', dmiss)
   ierr = nf90_put_att(ifile, id_party, '_FillValue', dmiss)


   if ( kmx.gt.0 ) then
      ierr = nf90_def_var(ifile, 'particle_z_coordinate', nf90_double, (/ id_partdim, id_timedim /), id_partz)
      ierr = nf90_put_att(ifile, id_partz, 'long_name', 'z-coordinate of particle')
   end if

   ! Leave the dataset in the same mode as we got it.
   if (jaInDefine == 1) then
       ierr = nf90_redef(ifile)
   end if

   return
end subroutine unc_write_part_header
