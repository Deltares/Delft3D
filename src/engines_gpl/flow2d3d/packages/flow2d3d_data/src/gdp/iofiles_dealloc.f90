subroutine iofiles_dealloc(gdp)
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
!!--description-----------------------------------------------------------------
!
! NONE
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
!
! Global variables
!
!
! Local variables
!
    integer                        :: i
    integer                        :: ig
    integer                        :: istat
    type(datagroup), pointer       :: group
!
!! executable statements -------------------------------------------------------
!
    do i = 1, FILOUT_CNT
       do ig = 1, MAXNR_GROUP
          group => gdp%iofiles(i)%group(ig)
          if (associated(group%elm_ndims )) deallocate (group%elm_ndims , stat=istat)
          if (associated(group%elm_dims  )) deallocate (group%elm_dims  , stat=istat)
          if (associated(group%elm_size  )) deallocate (group%elm_size  , stat=istat)
          if (associated(group%elm_type  )) deallocate (group%elm_type  , stat=istat)
          if (associated(group%elm_unit  )) deallocate (group%elm_unit  , stat=istat)
          if (associated(group%elm_name  )) deallocate (group%elm_name  , stat=istat)
          if (associated(group%elm_qty   )) deallocate (group%elm_qty   , stat=istat)
          if (associated(group%elm_lname )) deallocate (group%elm_lname , stat=istat)
          if (associated(group%elm_sname )) deallocate (group%elm_sname , stat=istat)
          if (associated(group%elm_attrib)) deallocate (group%elm_attrib, stat=istat)
       enddo
       deallocate (gdp%iofiles(i)%group, stat=istat)
       !
       deallocate (gdp%iofiles(i)%dim_name, stat=istat)
       deallocate (gdp%iofiles(i)%dim_id, stat=istat)
       deallocate (gdp%iofiles(i)%dim_length, stat=istat)
       !
       deallocate (gdp%iofiles(i)%att_name, stat=istat)
       deallocate (gdp%iofiles(i)%att_vtype, stat=istat)
       deallocate (gdp%iofiles(i)%att_ival, stat=istat)
       deallocate (gdp%iofiles(i)%att_rval, stat=istat)
       deallocate (gdp%iofiles(i)%att_cval, stat=istat)
       !
       deallocate (gdp%iofiles(i)%acl_label, stat=istat)
       deallocate (gdp%iofiles(i)%acl_attrib, stat=istat)
    enddo
    deallocate (gdp%iofiles, stat=istat)
end subroutine iofiles_dealloc
