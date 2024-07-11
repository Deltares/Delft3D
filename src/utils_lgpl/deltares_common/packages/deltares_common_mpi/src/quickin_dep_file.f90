module quickin_dep_file
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
!  
!  
!-------------------------------------------------------------------------------

private

public read_qndep

interface read_qndep
   module procedure read_qndep_single
   module procedure read_qndep_double
end interface read_qndep

contains

subroutine read_qndep_single(lundia    ,error     ,fildep    ,fmttmp    ,array     , &
                           & num_rows      ,num_columns      ,iocond    )
!!--description----------------------------------------------------------------- 
! 
!    Function: Reads the depth values from the attribute file in single precision
! 
!!--pseudo code and references-------------------------------------------------- 
! NONE 
!!--declarations---------------------------------------------------------------- 
    use precision 
    use grid_dimens_module 
    use message_module
    use system_utils, only: exifil
    use dfparall 
    ! 
    implicit none 
! 
! Global variables 
! 
    integer                                                                          :: lundia !  unit number for diagnostic file
    integer                                                            , intent(in)  :: num_rows   !  size of 1st dimension
    integer                                                            , intent(in)  :: num_columns   !  size of 2nd dimension
    logical                                                            , intent(out) :: error  !  Flag=TRUE if an error is encountered 
    real(sp), dimension(:,:)                                           , intent(out) :: array  !  data array to fill
    character(*)                                                                     :: fildep !  Name of the relevant file 
    character(11)                                                      , intent(in)  :: fmttmp !  Fornat switch for the attribute file 
    integer                                                            , intent(out) :: iocond !  Return value of iostat condition  
! 
! Local variables 
! 
    integer                               :: ierr
    integer                               :: luntmp ! Unit number for attribute file  
    integer                               :: m 
    integer                               :: n 
    character(300)                        :: errmsg ! Character string containing the error message to be written to file. The message depends on the error.
! 
!! executable statements ------------------------------------------------------- 
! 
    error = .false.
    ! 
    open (newunit=luntmp, file = fildep, form = fmttmp, status = 'old') 
    ! 
    if (fmttmp(1:2) == 'un') then 
       ! 
       ! unformatted file 
       ! read per N index, all M values in array array 
       ! end of error in file = not ok 
       ! 
       do n = 1, num_rows
          read (luntmp, iostat = iocond) (array(n, m), m = 1, num_columns)
          if (iocond /= 0) then 
             if (iocond < 0) then 
                errmsg = PREMATURE_EOF // trim(fildep)
                call write_error(errmsg, unit=lundia)
             else 
                errmsg = FILE_READ_ERROR // trim(fildep)
                call write_error(errmsg, unit=lundia)
             endif
             errmsg = ' '
             error = .true. 
             goto 9999 
          endif 
          ! 
          ! If a NaN is read -> error
          ! 
          do m = 1, num_columns
              if ( isnan(array(n, m)) ) then  
                 errmsg = 'NaN found in file ' // fildep 
                 error = .true. 
                 goto 9999 
             endif 
         enddo 
       enddo
    else
       ! 
       ! Freeformatted file 
       ! Skip lines starting with a '*' 
       ! 
       call skipstarlines(luntmp) 
       ! 
       ! read per N index, all M values in array 
       ! End of error in file = not ok 
       ! 
       do n = 1, num_rows
          read (luntmp, *, iostat = iocond) (array(n, m), m = 1, num_columns)
          if (iocond /= 0) then 
             if (iocond < 0) then 
                errmsg = PREMATURE_EOF // trim(fildep)
                call write_error(errmsg, unit=lundia)
             else 
                errmsg = FILE_READ_ERROR // trim(fildep)
                call write_error(errmsg, unit=lundia)
             endif 
             error = .true. 
             goto 9999 
          endif 
          ! 
          ! If a NaN is read -> error
          ! 
          do m = 1, num_columns
             if ( isnan(array(n, m)) ) then  
                errmsg = 'NaN found in file ' // fildep 
                error = .true. 
                goto 9999 
             endif 
          enddo 
       enddo 
    endif 
    ! 
    ! Stop reading file 
    ! 
9999  continue 
    ! 
    ! Close file 
    ! 
    close (luntmp) 
end subroutine read_qndep_single


subroutine read_qndep_double(lundia    ,error     ,fildep    ,fmttmp    ,array     , &
                           & num_rows      ,num_columns      ,iocond    )
!!--description----------------------------------------------------------------- 
! 
!    Function: Reads the depth values from the attribute file in single precision
! 
!!--pseudo code and references-------------------------------------------------- 
! NONE 
!!--declarations---------------------------------------------------------------- 
    use precision 
    use grid_dimens_module 
    use message_module
    use system_utils, only: exifil
    use dfparall 
    ! 
    implicit none 
! 
! Global variables 
! 
    integer                                                                          :: lundia !  unit number for diagnostic file
    integer                                                            , intent(in)  :: num_rows   !  size of 1st dimension
    integer                                                            , intent(in)  :: num_columns   !  size of 2nd dimension
    logical                                                            , intent(out) :: error  !  Flag=TRUE if an error is encountered 
    real(hp), dimension(:,:)                                           , intent(out) :: array  !  data array to fill
    character(*)                                                                     :: fildep !  Name of the relevant file 
    character(11)                                                      , intent(in)  :: fmttmp !  Fornat switch for the attribute file 
    integer                                                            , intent(out) :: iocond !  Return value of iostat condition  
! 
! Local variables 
! 
    integer                               :: ierr
    integer                               :: luntmp ! Unit number for attribute file  
    integer                               :: m 
    integer                               :: n 
    character(300)                        :: errmsg ! Character string containing the error message to be written to file. The message depends on the error.
! 
!! executable statements ------------------------------------------------------- 
! 
    error = .false.
    ! 
    open (newunit=luntmp, file = fildep, form = fmttmp, status = 'old') 
    ! 
    if (fmttmp(1:2) == 'un') then 
       ! 
       ! unformatted file 
       ! read per N index, all M values in array array 
       ! end of error in file = not ok 
       ! 
       do n = 1, num_rows
          read (luntmp, iostat = iocond) (array(n, m), m = 1, num_columns)
          if (iocond /= 0) then 
             if (iocond < 0) then 
                errmsg = PREMATURE_EOF // trim(fildep)
                call write_error(errmsg, unit=lundia)
             else 
                errmsg = FILE_READ_ERROR // trim(fildep)
                call write_error(errmsg, unit=lundia)
             endif
             errmsg = ' '
             error = .true. 
             goto 9999 
          endif 
          ! 
          ! If a NaN is read -> error
          ! 
          do m = 1, num_columns
              if ( isnan(array(n, m)) ) then  
                 errmsg = 'NaN found in file ' // fildep 
                 error = .true. 
                 goto 9999 
             endif 
         enddo 
       enddo
    else
       ! 
       ! Freeformatted file 
       ! Skip lines starting with a '*' 
       ! 
       call skipstarlines(luntmp) 
       ! 
       ! read per N index, all M values in array 
       ! End of error in file = not ok 
       ! 
       do n = 1, num_rows
          read (luntmp, *, iostat = iocond) (array(n, m), m = 1, num_columns)
          if (iocond /= 0) then 
             if (iocond < 0) then 
                errmsg = PREMATURE_EOF // trim(fildep)
                call write_error(errmsg, unit=lundia)
             else 
                errmsg = FILE_READ_ERROR // trim(fildep)
                call write_error(errmsg, unit=lundia)
             endif 
             error = .true. 
             goto 9999 
          endif 
          ! 
          ! If a NaN is read -> error
          ! 
          do m = 1, num_columns
             if ( isnan(array(n, m)) ) then  
                errmsg = 'NaN found in file ' // fildep 
                error = .true. 
                goto 9999 
             endif 
          enddo 
       enddo 
    endif 
    ! 
    ! Stop reading file 
    ! 
9999  continue 
    ! 
    ! Close file 
    ! 
    close (luntmp) 
end subroutine read_qndep_double

end module quickin_dep_file
