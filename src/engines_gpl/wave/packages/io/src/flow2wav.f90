subroutine flow2wav(u1      ,v1        ,alfas     , &
                & guu       ,gvv       ,mmax      ,nmax      ,kcs       , &
                & kfu       ,kfv       ,alpb      ,clbot     )
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
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Global variables
!
    integer, intent(in)                         :: mmax
    integer, intent(in)                         :: nmax
    integer, dimension(mmax, nmax), intent(in)  :: kcs
    integer, dimension(mmax, nmax), intent(in)  :: kfu
    integer, dimension(mmax, nmax), intent(in)  :: kfv
    logical, intent(in)                         :: clbot
    real, intent(in)                            :: alpb
    real, dimension(mmax, nmax), intent(in)     :: alfas
    real, dimension(mmax, nmax), intent(in)     :: guu
    real, dimension(mmax, nmax), intent(in)     :: gvv
    real, dimension(mmax, nmax)                 :: u1
    real, dimension(mmax, nmax)                 :: v1
!
!
! Local variables
!
    real, dimension(:,:), allocatable :: uzeta
    real, dimension(:,:), allocatable :: vzeta
    integer                        :: ierr
    integer                        :: m
    integer                        :: m1
    integer                        :: n
    integer                        :: n1
    integer                        :: kenmu
    integer                        :: kenmv
    real                           :: alfa
    real                           :: beta
    real                           :: csalfa
    real                           :: degrad
    real                           :: eps
    real                           :: snalfa
    real                           :: ugem
    real                           :: vgem
    real                           :: vmag
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    ! parameters:
    !
    ! name     type     length      i o   description
    ! ====     ====     ======      ===   ===========
    ! alpb     real     1           *     angle of bottom grid w.r.t. problem
    !                                     coordinate system
    ! guu      real     mmax*nmax   *     coeff. arrays g-eta-eta
    ! gvv      real     mmax*nmax   *     coeff. arrays g-ksi-ksi
    ! kcs      integer  mmax*nmax   *     0/1/2 non-active / active /
    !                                     boundary water-level point
    ! kfu      integer  mmax*nmax   *     0/1 non-active / active u-point
    ! kfv      integer  mmax*nmax   *     0/1 non-active / active v-point
    ! mmax     integer  1           *     m-size of the grid
    ! nmax     integer  1           *     n-size of the grid
    ! u1       real     mmax*nmax   *     array with u-velocity in u-point
    ! uzeta    real     mmax*nmax     *   array with x-velocity in zetapoint
    ! v1       real     mmax*nmax   *     array with v-velocity in v-point
    ! vzeta    real     mmax*nmax     *   array with y-velocity in zetapoint
    ! xcor     real     mmax*nmax   *     x-coordinate here used in depth
    !                                     point
    ! ycor     real     mmax*nmax   *     y-coordinate here used in depth
    !                                     point
    !
    ! important local variables:
    !
    ! name     type     lenght            description
    ! ====     ====     ======            ===========
    ! beta     real     1                 angle between u and v vector
    ! eps      real     1                 small value for real value tests
    ! guugem   real     1                 g-eta-eta in zeta point
    ! gvvgem   real     1                 g-ksi-ksi in zeta point
    ! ugem     real     1                 u- velocity in zeta point in
    !                                     transformed plane
    ! vgem     real     1                 v- velocity in zeta point in
    !                                     transformed plane
    ! xeta     real     1                 x-coordinate for eta line for
    !                                     transformation to physical plane
    ! xksi     real     1                 x-coordinate for ksi line for
    !                                     transformation to physical plane
    ! yeta     real     1                 y-coordinate for eta line for
    !                                     transformation to physical plane
    ! yksi     real     1                 y-coordinate for ksi line for
    !                                     transformation to physical plane
    ! vmag     real     1                 magnitude of velocity
    !
    !
    !
     allocate(uzeta(mmax,nmax))
     allocate(vzeta(mmax,nmax))

    !-----initialisation
    !
    eps = 0.
    degrad = atan(1.)/45.
    !
    !-----arrays
    !     For vectorplots the defaultvalue is <eps>.
    !
    do n = 1, nmax
       do m = 1, mmax
          uzeta(m, n) = eps
          vzeta(m, n) = eps
       enddo
    enddo
    !
    !-----calculate u end v at zeta points
    !     note: inside the irocol table guu and gvv are
    !           never .le. eps so this check can be skipped.
    !
    do n = 1, nmax
       do m = 1, mmax
          m1 = max(m - 1, 1)
          n1 = max(n - 1, 1)
          if (guu(m1, n)<=eps .or. gvv(m, n1)<=eps) then
             uzeta(m, n) = eps
             vzeta(m, n) = eps
          else
             kenmu      = max(1, kfu(m,n)+kfu(m1,n ))
             kenmv      = max(1, kfv(m,n)+kfv(m ,n1))
             ugem       = (u1(m,n)*kfu(m,n) + u1(m1,n )*kfu(m1,n )) / kenmu
             vgem       = (v1(m,n)*kfv(m,n) + v1(m ,n1)*kfv(m ,n1)) / kenmv
             csalfa     = cos(alfas(m, n)*degrad)
             snalfa     = sin(alfas(m, n)*degrad)
             uzeta(m,n) = ugem*csalfa - vgem*snalfa
             vzeta(m,n) = ugem*snalfa + vgem*csalfa
          endif
       enddo
    enddo
    !
    ! Orientate velocities on bottom grid of Hiswa by rotating velocities
    ! over alpb
    !
    if (clbot .or. abs(alpb)<.001) goto 400
    alfa = alpb*atan(1.)*4./180.
    do n = 1, nmax
       do m = 1, mmax
          if (kfu(m, n)/=0 .and. kfv(m, n)/=0) then
             if (abs(vzeta(m, n))>.0 .or. abs(uzeta(m, n))>.0) then
                beta = atan2(vzeta(m, n), uzeta(m, n))
                vmag = sqrt(vzeta(m, n)*vzeta(m, n) + uzeta(m, n)*uzeta(m, n))
                uzeta(m, n) = cos(beta - alfa)*vmag
                vzeta(m, n) = sin(beta - alfa)*vmag
             endif
          endif
       enddo
    enddo

    400 continue
   
    ! Assign uzeta to u1, vzeta to v1
    u1 = uzeta
    v1 = vzeta
    
    ! Deallocate uzeta, vzeta
    deallocate(uzeta, stat=ierr)
    deallocate(vzeta, stat=ierr)


end subroutine flow2wav  
