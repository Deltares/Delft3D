subroutine iofiles_alloc(gdp)
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
    integer                          :: i
    integer                          :: ig
!
!! executable statements -------------------------------------------------------
!
    allocate (gdp%iofiles(FILOUT_CNT))
    do i = 1, FILOUT_CNT
       select case (i)
       case (FILOUT_DIA)
         gdp%iofiles(i)%filetype = FTYPE_ASCII
       case (FILOUT_HIS)
         gdp%iofiles(i)%filetype = FTYPE_NEFIS
       case (FILOUT_MAP)
         gdp%iofiles(i)%filetype = FTYPE_NEFIS
       case (FILOUT_COM)
         gdp%iofiles(i)%filetype = FTYPE_NEFIS
       case (FILOUT_FOU)
         gdp%iofiles(i)%filetype = FTYPE_ASCII
       case (FILOUT_DRO)
         gdp%iofiles(i)%filetype = FTYPE_NEFIS
       end select
       gdp%iofiles(i)%filename = ' '
       !
       allocate (gdp%iofiles(i)%dim_name(0))
       allocate (gdp%iofiles(i)%dim_id(0))
       allocate (gdp%iofiles(i)%dim_length(0))
       !
       allocate (gdp%iofiles(i)%att_name(0))
       allocate (gdp%iofiles(i)%att_vtype(0))
       allocate (gdp%iofiles(i)%att_ival(0))
       allocate (gdp%iofiles(i)%att_rval(0))
       allocate (gdp%iofiles(i)%att_cval(0))
       !
       allocate (gdp%iofiles(i)%acl_label(0))
       allocate (gdp%iofiles(i)%acl_attrib(0,0))
       !
       allocate (gdp%iofiles(i)%group(MAXNR_GROUP))
       do ig = 1, MAXNR_GROUP
          gdp%iofiles(i)%group(ig)%first   = .true.
          gdp%iofiles(i)%group(ig)%name    = ' '
          gdp%iofiles(i)%group(ig)%celidt  = 1
          gdp%iofiles(i)%group(ig)%grp_dim = 0
          gdp%iofiles(i)%group(ig)%nelmx   = 0
          nullify(gdp%iofiles(i)%group(ig)%elm_ndims )
          nullify(gdp%iofiles(i)%group(ig)%elm_dims  )
          nullify(gdp%iofiles(i)%group(ig)%elm_size  )
          nullify(gdp%iofiles(i)%group(ig)%elm_type  )
          nullify(gdp%iofiles(i)%group(ig)%elm_unit  )
          nullify(gdp%iofiles(i)%group(ig)%elm_name  )
          nullify(gdp%iofiles(i)%group(ig)%elm_qty   )
          nullify(gdp%iofiles(i)%group(ig)%elm_lname )
          nullify(gdp%iofiles(i)%group(ig)%elm_sname )
          nullify(gdp%iofiles(i)%group(ig)%elm_attrib)
       enddo
    enddo
end subroutine iofiles_alloc
