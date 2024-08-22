module nan_check_module
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
!!--module description----------------------------------------------------------
!
! This module provides a subroutine nan_check for 1D, 2D, 3D and 4D,
! in single, double and flexible precision.
!
!!--module declarations---------------------------------------------------------
implicit none
interface nan_check
  module procedure nan_check_1D_sp
  module procedure nan_check_2D_sp
  module procedure nan_check_3D_sp
  module procedure nan_check_4D_sp
  module procedure nan_check_1D_dp
  module procedure nan_check_2D_dp
  module procedure nan_check_3D_dp
  module procedure nan_check_4D_dp
end interface

contains

logical function nan_check_1D_sp(field, fieldnam, lundia, ilb) result (retval)
   use ieee_arithmetic, only: ieee_is_nan

!!--description-----------------------------------------------------------------
!
!    Function: - Checks a 1D single precision array on NaN's.
!
!!--declarations----------------------------------------------------------------
    !
    ! Arguments
    !
    real(kind=4), dimension(:)               , intent(in)    :: field
    character(len=*)                         , intent(in)    :: fieldnam
    integer                                  , intent(in)    :: lundia
    integer, optional                        , intent(in)    :: ilb
    !
    ! Local variables
    !
    integer                                :: i
    integer                                :: i0
    character(len=256)                     :: message
!
!! executable statements -------------------------------------------------------
!
    retval = .true.
    if (present(ilb)) then
       i0 = ilb - 1
    else
       i0 = 0
    endif
    do i = lbound(field,1), ubound(field,1)
       if (ieee_is_nan(field(i))) then
          write(message,*) 'NaN found in ', fieldnam, &
               ' at (i) = (', i0+i, ')'
          write(lundia,*) '*** ERROR', trim(message)
          retval = .false.
          return
       endif
    enddo
end function nan_check_1D_sp

logical function nan_check_2D_sp(field, fieldnam, lundia, nlb, mlb) result (retval)
   use ieee_arithmetic, only: ieee_is_nan

!!--description-----------------------------------------------------------------
!
!    Function: - Checks a 2D single precision array on NaN's.
!
!!--declarations----------------------------------------------------------------
    !
    ! Arguments
    !
    real(kind=4), dimension(:,:)             , intent(in)    :: field
    character(len=*)                         , intent(in)    :: fieldnam
    integer                                  , intent(in)    :: lundia
    integer, optional                        , intent(in)    :: nlb
    integer, optional                        , intent(in)    :: mlb
    !
    ! Local variables
    !
    integer                                  :: m, n
    integer                                  :: m0, n0
    character(len=256)                       :: message
!
!! executable statements -------------------------------------------------------
!
    retval = .true.
    if (present(nlb)) then
       n0 = nlb-1
    else
       n0 = 0
    endif
    if (present(mlb)) then
       m0 = mlb-1
    else
       m0 = 0
    endif
    do m = lbound(field,2), ubound(field,2)
       do n = lbound(field,1), ubound(field,1)
          if (ieee_is_nan(field(n,m))) then
             write(message,*) 'NaN found in ', fieldnam, &
              ' at (n,m) = (', n0+n, ',' , m0+m, ')'
             write(lundia,*) '*** ERROR', trim(message)
             retval = .false.
             return
          endif
       enddo
    enddo
end function nan_check_2D_sp

logical function nan_check_3D_sp(field, fieldnam, lundia, nlb, mlb, klb) result (retval)
   use ieee_arithmetic, only: ieee_is_nan

!!--description-----------------------------------------------------------------
!
!    Function: - Checks a 3D single precision array on NaN's.
!
!!--declarations----------------------------------------------------------------
    !
    ! Arguments
    !
    real(kind=4), dimension(:,:,:)           , intent(in)    :: field
    character(len=*)                         , intent(in)    :: fieldnam
    integer                                  , intent(in)    :: lundia
    integer, optional                        , intent(in)    :: nlb
    integer, optional                        , intent(in)    :: mlb
    integer, optional                        , intent(in)    :: klb
    !
    ! Local variables
    !
    integer                                    :: k, m, n
    integer                                    :: k0, m0, n0
    character(len=256)                         :: message
!
!! executable statements -------------------------------------------------------
!
    retval = .true.
    if (present(nlb)) then
       n0 = nlb-1
    else
       n0 = 0
    endif
    if (present(mlb)) then
       m0 = mlb-1
    else
       m0 = 0
    endif
    if (present(klb)) then
       k0 = klb-1
    else
       k0 = 0
    endif
    do k = lbound(field,3), ubound(field,3)
       do m = lbound(field,2), ubound(field,2)
          do n = lbound(field,1), ubound(field,1)
            if (ieee_is_nan(field(n,m,k))) then
              write(message,*) 'NaN found in ', fieldnam, &
                  ' at (n,m,k) = (', n0+n, ',', m0+m, ',', k0+k, ')'
              write(lundia,*) '*** ERROR', trim(message)
              retval = .false.
              return
            endif
          enddo
        enddo
     enddo
    end function nan_check_3D_sp

logical function nan_check_4D_sp(field, fieldnam, lundia, nlb, mlb, klb, llb) result (retval)
   use ieee_arithmetic, only: ieee_is_nan

!!--description-----------------------------------------------------------------
!
!    Function: - Checks a 4D single precision array on NaN's.
!
!!--declarations----------------------------------------------------------------
    !
    ! Arguments
    !
    real(kind=4), dimension(:,:,:,:)         , intent(in)    :: field
    character(len=*)                         , intent(in)    :: fieldnam
    integer                                  , intent(in)    :: lundia
    integer, optional                        , intent(in)    :: nlb
    integer, optional                        , intent(in)    :: mlb
    integer, optional                        , intent(in)    :: klb
    integer, optional                        , intent(in)    :: llb
    !
    ! Local variables
    !
    integer                                      :: k, l, m, n
    integer                                      :: k0, l0, m0, n0
    character(len=256)                           :: message
!
!! executable statements -------------------------------------------------------
!
    retval = .true.
    if (present(nlb)) then
       n0 = nlb-1
    else
       n0 = 0
    endif
    if (present(mlb)) then
       m0 = mlb-1
    else
       m0 = 0
    endif
    if (present(klb)) then
       k0 = klb-1
    else
       k0 = 0
    endif
    if (present(llb)) then
       l0 = llb-1
    else
       l0 = 0
    endif
    do l = lbound(field,4), ubound(field,4)
       do k = lbound(field,3), ubound(field,3)
          do m = lbound(field,2), ubound(field,2)
             do n = lbound(field,1), ubound(field,1)
                if (ieee_is_nan(field(n,m,k,l))) then
                   write(message,*) 'NaN found in ', fieldnam, &
                      ' at (n,m,k,l) = (', n0+n, ',', m0+m, ',', k0+k, ',', l0+l, ')'
                   write(lundia,*) '*** ERROR', trim(message)
                   retval = .false.
                   return
                endif
             enddo
          enddo
       enddo
    enddo
    end function nan_check_4D_sp

logical function nan_check_1D_dp(field, fieldnam, lundia, ilb) result (retval)
   use ieee_arithmetic, only: ieee_is_nan

!!--description-----------------------------------------------------------------
!
!    Function: - Checks a 1D double precision array on NaN's.
!
!!--declarations----------------------------------------------------------------
    !
    ! Arguments
    !
    real(kind=8), dimension(:)               , intent(in)    :: field
    character(len=*)                         , intent(in)    :: fieldnam
    integer                                  , intent(in)    :: lundia
    integer, optional                        , intent(in)    :: ilb
    !
    ! Local variables
    !
    integer                                :: i
    integer                                :: i0
    character(len=256)                     :: message
!
!! executable statements -------------------------------------------------------
!
    retval = .true.
    if (present(ilb)) then
       i0 = ilb-1
    else
       i0 = 0
    endif
    do i = lbound(field,1), ubound(field,1)
       if (ieee_is_nan(field(i))) then
          write(message,*) 'NaN found in ', fieldnam, &
               ' at (i) = (', i0+i, ')'
          write(lundia,*) '*** ERROR', trim(message)
          retval = .false.
          return
       endif
    enddo
end function nan_check_1D_dp

logical function nan_check_2D_dp(field, fieldnam, lundia, nlb, mlb) result (retval)
   use ieee_arithmetic, only: ieee_is_nan

!!--description-----------------------------------------------------------------
!
!    Function: - Checks a 2D double precision array on NaN's.
!
!!--declarations----------------------------------------------------------------
    !
    ! Arguments
    !
    real(kind=8), dimension(:,:)             , intent(in)    :: field
    character(len=*)                         , intent(in)    :: fieldnam
    integer                                  , intent(in)    :: lundia
    integer, optional                        , intent(in)    :: nlb
    integer, optional                        , intent(in)    :: mlb
    !
    ! Local variables
    !
    integer                                  :: m, n
    integer                                  :: m0, n0
    character(len=256)                       :: message
!
!! executable statements -------------------------------------------------------
!
    retval = .true.
    if (present(nlb)) then
       n0 = nlb-1
    else
       n0 = 0
    endif
    if (present(mlb)) then
       m0 = mlb-1
    else
       m0 = 0
    endif
    do m = lbound(field,2), ubound(field,2)
       do n = lbound(field,1), ubound(field,1)
          if (ieee_is_nan(field(n,m))) then
             write(message,*) 'NaN found in ', fieldnam, &
              ' at (n,m) = (', n0+n, ',' , m0+m, ')'
             write(lundia,*) '*** ERROR', trim(message)
             retval = .false.
             return
          endif
       enddo
    enddo
end function nan_check_2D_dp

logical function nan_check_3D_dp(field, fieldnam, lundia, nlb, mlb, klb) result (retval)
   use ieee_arithmetic, only: ieee_is_nan

!!--description-----------------------------------------------------------------
!
!    Function: - Checks a 3D double precision array on NaN's.
!
!!--declarations----------------------------------------------------------------
    !
    ! Arguments
    !
    real(kind=8), dimension(:,:,:)           , intent(in)    :: field
    character(len=*)                         , intent(in)    :: fieldnam
    integer                                  , intent(in)    :: lundia
    integer, optional                        , intent(in)    :: nlb
    integer, optional                        , intent(in)    :: mlb
    integer, optional                        , intent(in)    :: klb
    !
    ! Local variables
    !
    integer                                    :: k, m, n
    integer                                    :: k0, m0, n0
    character(len=256)                         :: message
!
!! executable statements -------------------------------------------------------
!
    retval = .true.
    if (present(nlb)) then
       n0 = nlb-1
    else
       n0 = 0
    endif
    if (present(mlb)) then
       m0 = mlb-1
    else
       m0 = 0
    endif
    if (present(klb)) then
       k0 = klb-1
    else
       k0 = 0
    endif
    do k = lbound(field,3), ubound(field,3)
       do m = lbound(field,2), ubound(field,2)
          do n = lbound(field,1), ubound(field,1)
            if (ieee_is_nan(field(n,m,k))) then
              write(message,*) 'NaN found in ', fieldnam, &
                  ' at (n,m,k) = (', n0+n, ',', m0+m, ',', k0+k, ')'
              write(lundia,*) '*** ERROR', trim(message)
              retval = .false.
              return
            endif
          enddo
        enddo
     enddo
    end function nan_check_3D_dp

logical function nan_check_4D_dp(field, fieldnam, lundia, nlb, mlb, klb, llb) result (retval)
   use ieee_arithmetic, only: ieee_is_nan

!!--description-----------------------------------------------------------------
!
!    Function: - Checks a 4D double precision array on NaN's.
!
!!--declarations----------------------------------------------------------------
    !
    ! Arguments
    !
    real(kind=8), dimension(:,:,:,:)         , intent(in)    :: field
    character(len=*)                         , intent(in)    :: fieldnam
    integer                                  , intent(in)    :: lundia
    integer, optional                        , intent(in)    :: nlb
    integer, optional                        , intent(in)    :: mlb
    integer, optional                        , intent(in)    :: klb
    integer, optional                        , intent(in)    :: llb
    !
    ! Local variables
    !
    integer                                      :: k, l, m, n
    integer                                      :: k0, l0, m0, n0
    character(len=256)                           :: message
!
!! executable statements -------------------------------------------------------
!
    retval = .true.
    if (present(nlb)) then
       n0 = nlb-1
    else
       n0 = 0
    endif
    if (present(mlb)) then
       m0 = mlb-1
    else
       m0 = 0
    endif
    if (present(klb)) then
       k0 = klb-1
    else
       k0 = 0
    endif
    if (present(llb)) then
       l0 = llb-1
    else
       l0 = 0
    endif
    do l = lbound(field,4), ubound(field,4)
       do k = lbound(field,3), ubound(field,3)
          do m = lbound(field,2), ubound(field,2)
             do n = lbound(field,1), ubound(field,1)
                if (ieee_is_nan(field(n,m,k,l))) then
                   write(message,*) 'NaN found in ', fieldnam, &
                      ' at (n,m,k,l) = (', n0+n, ',', m0+m, ',', k0+k, ',', l0+l, ')'
                   write(lundia,*) '*** ERROR', trim(message)
                   retval = .false.
                   return
                endif
             enddo
          enddo
       enddo
    enddo
    end function nan_check_4D_dp

end module nan_check_module
