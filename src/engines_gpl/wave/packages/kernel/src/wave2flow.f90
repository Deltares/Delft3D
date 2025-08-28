subroutine wave2flow (fof,fg)
!
! Head routine for calling read_bot
!
use swan_flow_grid_maps
implicit none
type (output_fields)            :: fof      ! flow output fields
type (grid)                     :: fg       ! flow grid
       call wav2tr (fof%fx  ,fof%fy   ,fg%alfas  ,fg%guu  ,fg%gvv     ,     &
                 &  fg%mmax ,fg%nmax  ,fg%kcs  )
       call wav2tr (fof%mx  ,fof%my   ,fg%alfas  ,fg%guu  ,fg%gvv     ,     &
                 &  fg%mmax ,fg%nmax  ,fg%kcs  )
       call dircor( fof%dir ,fg%alfas, fg%npts )
end subroutine wave2flow

subroutine wav2tr(fx        ,fy        ,alfas     ,guu       ,gvv       , &
                & mmax      ,nmax      ,kcs       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
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
!     Subroutine wav2tr converts vector fx,fy  (wave forces or mass
!     fluxes) given in cartesian orientation in water level points to
!     components in u- and v-direction in u- and v-points; results are
!     stored in fx,fy
!
!     Programmer: Dano Roelvink
!     Date:       20-12-94
!
!     This routine only works in conjunction with FLOW 2.40 and
!     upward.
!
!     Modification related to spherical coordinate system: no use is
!     made of xcor and ycor, instead alfas is used. Loop 50,51 added,
!     loop 100,110 modified. 19/8/2003
!
!     Grid convention:
!
!
! nmax   | 0 | 0    | 0 | 0
!        + - + -    + - + -
! nmax-1 | 0 | 0    | 0 | 0
!        + - + -    + - + -           + water level point (fx,fy input)
!                                     | v point      (fv, fy on output)
!                                     - u point      (fu, fx on output)
!   2    | 0 | 0    | 0 | 0           0 bottom point (also xcor,ycor)
!        + - + -    + - + -
!   1    | 0 | 0    | 0 | 0
!        + - + -    + - + -
!
!         1   2   mmax-1 mmax
!
!     The water level points with kcs>0 all have defined coordinates and
!     can thus contain realistic values of fx and fy initially.
!     Note: the user must ensure that the computational domain(s) of
!     HISWA encompass(es) all relevant water level points.
!
!     The water level points at n=1, n=nmax, m=1 and m=mmax DO contain
!     realistic fx and fy values if kcs>0.
!
!     The mask array kcs can attain the following values:
!
!          0   closed boundary
!          1   active point
!          2   open boundary
!
!     The fu or fv values are determined in the velocity points by
!     interpolation between the two neighbouring water level points. The
!     combination of the two related kcs-values determines whether or
!     not to set fu or fv to zero:
!
!          0  0    outside comp. domain:        set to 0
!          0  1    closed boundary:             set to 0
!          1  0    closed boundary:             set to 0
!          2  2    along open boundary:         set to 0
!
!          1  1    active cell                  interpolate
!          1  2    open boundary                interpolate
!          2  1    open boundary                interpolate
!
!     The fu or fv value between two water level points is set to zero
!     if one of the kcs values equals 0 or both equal 2, or if the
!     corresponding guu or gvv is zero.
!
!     The velocity points on n=nmax and m=mmax are dummy and are set to
!     zero
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Global variables
!
    integer                       , intent(in)  :: mmax
    integer                       , intent(in)  :: nmax
    integer, dimension(mmax, nmax), intent(in)  :: kcs
    real   , dimension(mmax, nmax), intent(in)  :: alfas
    real   , dimension(mmax, nmax)              :: fx
    real   , dimension(mmax, nmax)              :: fy
    real   , dimension(mmax, nmax), intent(in)  :: guu
    real   , dimension(mmax, nmax), intent(in)  :: gvv
!
! Local variables
!
    integer :: m
    integer :: mp1
    integer :: n
    integer :: np1
    real    :: csalfa
    real    :: degrad
    real    :: fu
    real    :: fv
    real    :: snalfa
!
!! executable statements -------------------------------------------------------
!
    degrad = atan(1.)/45.
    !
    do n = 1, nmax
       do m = 1, mmax
          !
          ! First transform from cartesian directions to grid directions
          ! using alfas
          !
          if (kcs(m, n)>0) then
             csalfa = cos(alfas(m, n)*degrad)
             snalfa = sin(alfas(m, n)*degrad)
             fu = fx(m, n)*csalfa + fy(m, n)*snalfa
             fv = -fx(m, n)*snalfa + fy(m, n)*csalfa
             fx(m, n) = fu
             fy(m, n) = fv
          else
             fx(m, n) = 0.
             fy(m, n) = 0.
          endif
       enddo
    enddo
    !
    do n = 1, nmax - 1
       do m = 1, mmax - 1
          mp1 = m + 1
          np1 = n + 1
          !
          ! u-component
          !
          if (guu(m, n)<1.E-6 .or. kcs(m, n)==0 .or. kcs(mp1, n)==0 .or.        &
            & (kcs(m, n)==2 .and. kcs(mp1, n)==2)) then
             fu = 0.
          else
             fu = 0.5*(fx(m, n) + fx(mp1, n))
          endif
          !
          ! v-component
          !
          if (gvv(m, n)<1.E-6 .or. kcs(m, n)==0 .or. kcs(m, np1)==0 .or.        &
            & (kcs(m, n)==2 .and. kcs(m, np1)==2)) then
             fv = 0.
          else
             fv = 0.5*(fy(m, n) + fy(m, np1))
          endif
          !
          ! store results in fx,fy
          !
          fx(m, n) = fu
          fy(m, n) = fv
       enddo
    enddo
    do m = 1, mmax
       !
       ! mirror values at boundaries
       !
       fy(m, nmax - 1) = fy(m, nmax - 2)
       fy(m, 1) = fy(m, 2)
    enddo
    do n = 1, nmax
       fx(mmax - 1, n) = fx(mmax - 2, n)
       fx(1, n) = fx(2, n)
    enddo
end subroutine wav2tr
