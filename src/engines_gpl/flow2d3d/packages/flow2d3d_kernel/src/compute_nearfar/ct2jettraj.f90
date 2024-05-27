subroutine ct2jettraj (no_modules, nrow, modules, modstart, x_cor  , y_cor , z_cor , s_cor  , h_cor   , b_cor   ,&
                                                            x_jet  , y_jet , z_jet , s_jet  , h_jet   , b_jet   ,&
                                                            xz     , yz    , dps   , s1     , nm_diff ,&
                                                            no_val , taua  , idis   , gdp     )
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
!  $Id: ct2jettraj.f90 5888 2016-02-24 10:14:54Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160128_34357_NearField_Coupling/src/engines_gpl/flow2d3d/packages/kernel/src/compute_nearfar/ct2jettraj.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Compute the jet trajectory in "world" coordinates from cortim results
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    ! They replace the  include igd / include igp lines
    !
!
! Global variables
!
    integer                                                    , intent(in)    :: idis
    integer                                                    , intent(in)    :: no_modules
    integer                                                    , intent(in)    :: nrow
    integer                                                    , intent(in)    :: nm_diff
    integer                                                    , intent(out)   :: no_val
    integer    , dimension(nrow)                               , intent(in)    :: modstart
    real(fp)                                                   , intent(inout) :: taua
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)    :: s1
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)    :: xz
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)    :: yz
    real(fp)   , dimension(nrow)                               , intent(in)    :: x_cor
    real(fp)   , dimension(nrow)                               , intent(in)    :: y_cor
    real(fp)   , dimension(nrow)                               , intent(in)    :: z_cor
    real(fp)   , dimension(nrow)                               , intent(in)    :: s_cor
    real(fp)   , dimension(nrow)                               , intent(in)    :: h_cor
    real(fp)   , dimension(nrow)                               , intent(in)    :: b_cor
    real(fp)   , dimension(nrow)                               , intent(inout) :: x_jet
    real(fp)   , dimension(nrow)                               , intent(inout) :: y_jet
    real(fp)   , dimension(nrow)                               , intent(inout) :: z_jet
    real(fp)   , dimension(nrow)                               , intent(inout) :: h_jet
    real(fp)   , dimension(nrow)                               , intent(inout) :: b_jet
    real(fp)   , dimension(nrow)                               , intent(inout) :: s_jet
    real(prec) , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)    :: dps
    character*256 , dimension(no_modules)                      , intent(in)    :: modules
!
! Local variables
!
    integer                                                                    :: ival
    integer                                                                    :: imodule
    integer                                                                    :: istart
    integer                                                                    :: iend
    integer                                                                    :: i
    integer                                                                    :: irow
    real(fp)                                                                   :: deg2rad
!
!! executable statements -------------------------------------------------------
!
    !
    deg2rad = acos(-1.0_fp)/180.0_fp
    ival    = 0

    do imodule = 1, no_modules
       istart = modstart(imodule)
       if (imodule < no_modules) then
          iend = modstart(imodule + 1) - 1
       else
          iend = nrow
       endif

       select case (modules (imodule))

          case ('mod110')
             !
             ! corjet module
             !
             do i = istart,iend
                ival = ival + 1
                x_jet(ival)  = xz(nm_diff) + x_cor(i)*cos(taua*deg2rad) - y_cor(i)*sin(taua*deg2rad)
                y_jet(ival)  = yz(nm_diff) + x_cor(i)*sin(taua*deg2rad) + y_cor(i)*cos(taua*deg2rad)
                !
               ! Temporarily, for V en V case eysink, lwer discharge point by 4 cm
                !
!               z_jet(ival)  = real(dps(nm_diff),fp) - z_cor(i)
                z_jet(ival)  = real(dps(nm_diff),fp) - z_cor(i) + 0.04_fp
                s_jet(ival)  = s_cor(i)
                h_jet(ival)  = h_cor(i)
                b_jet(ival)  = b_cor(i)
                if (i < iend) then
                   if (z_cor(i) > z_cor(i+1)) then
                      exit
                   endif
                endif
             enddo
          case ('mod132')
             !
             ! Cormix1:
             ! layer boundary impingment/upstream spreading
             !
             ival = ival + 1
             x_jet(ival) = xz(nm_diff) + x_cor(i)*cos(taua*deg2rad) - y_cor(i)*sin(taua*deg2rad)
             y_jet(ival) = yz(nm_diff) + x_cor(i)*sin(taua*deg2rad) + y_cor(i)*cos(taua*deg2rad)
             z_jet(ival) = real(dps(nm_diff),fp) - z_cor(iend)
             s_jet(ival) = s_cor(iend)
          case ('mod141')
             !
             ! Cormix1:
             ! Bouyant ambient spreading (just take second point and ignore the remainder, this is considered the
             !                            coupling point for now at least)
             ival = ival + 1
             x_jet(ival) = xz(nm_diff) + x_cor(i)*cos(taua*deg2rad) - y_cor(i)*sin(taua*deg2rad)
             y_jet(ival) = yz(nm_diff) + x_cor(i)*sin(taua*deg2rad) + y_cor(i)*cos(taua*deg2rad)
             z_jet(ival) = real(dps(nm_diff),fp) - z_cor(istart + 1)
             s_jet(ival) = s_cor(istart + 1)
             h_jet(ival) = h_cor(istart + 1)
          case ('mod310')
             !
             ! Cormix3: (does not wotk properly yet! Angles!)
             ! Corsurf, module310 Bouyant surface jet (reference level for cormix = water level in this case)
             !                    also plume width relevant, store in b_jet
             !
             do i = istart, iend
                ival = ival + 1
                x_jet(ival) = xz(nm_diff) + x_cor(i)*cos(taua*deg2rad) - y_cor(i)*sin(taua*deg2rad)
                y_jet(ival) = yz(nm_diff) + x_cor(i)*sin(taua*deg2rad) + y_cor(i)*cos(taua*deg2rad)
                z_jet(ival) = -1.0_fp*s1(nm_diff) + z_cor(i)
                s_jet(ival) = s_cor(i)
                h_jet(ival) = h_cor(i)
                b_jet(ival) = b_cor(i)
                enddo
          case  ('mod275','mod274')
             !
             ! Cormix2
             ! Staged perpendicular diffuser in a strong current (mod275)
             ! Acceleration zone of staged diffuser              (mod274)
             !
             do i = istart, iend
                ival = ival + 1
                x_jet(ival) = xz(nm_diff) + x_cor(i)*cos(taua*deg2rad) - y_cor(i)*sin(taua*deg2rad)
                y_jet(ival) = yz(nm_diff) + x_cor(i)*sin(taua*deg2rad) + y_cor(i)*cos(taua*deg2rad)
                z_jet(ival) = real(dps(nm_diff),fp) - z_cor(i)
                s_jet(ival) = s_cor(i)
                h_jet(ival) = h_cor(i)
                b_jet(ival) = b_cor(i)
             enddo
!         case  ('mod252')
             !
             ! Cormix2
             ! DIFFUSER INDUCED PLUME IN WEAK CROSS-FLOW
             ! Only include phase 1 (well-mixed); use height as indicator for phase1
             !
!            do i = istart, iend
!               if (h_cor(i) == h_cor(istart)) then
!                  ival = ival + 1
!                  x_jet(ival) = xz(nm_diff) + x_cor(i)*cos(taua*deg2rad) - y_cor(i)*sin(taua*deg2rad)
!                  y_jet(ival) = yz(nm_diff) + x_cor(i)*sin(taua*deg2rad) + y_cor(i)*cos(taua*deg2rad)
!                  z_jet(ival) = real(dps(nm_diff),fp) - z_cor(i)
!                  s_jet(ival) = s_cor(i)
!                  h_jet(ival) = h_cor(i)
!                  b_jet(ival) = b_cor(i)
!               endif
!            enddo
!         case  ('mod241')
             !
             ! Cormix2
             ! Ambient Bouyant spreafiffuser in a strong current (mod275)
             ! Acceleration zone of staged diffuser              (mod274)
             !
!            do i = istart, iend
!               if (h_cor(i) > 0.5_fp*h_cor(istart)) then
!                  ival = ival + 1
!                  x_jet(ival) = xz(nm_diff) + x_cor(i)*cos(taua*deg2rad) - y_cor(i)*sin(taua*deg2rad)
!                  y_jet(ival) = yz(nm_diff) + x_cor(i)*sin(taua*deg2rad) + y_cor(i)*cos(taua*deg2rad)
!                  z_jet(ival) = real(dps(nm_diff),fp) - z_cor(i)
!                  s_jet(ival) = s_cor(i)
!                  h_jet(ival) = h_cor(i)
!                   b_jet(ival) = b_cor(i)
!                endif
!             enddo
       end select
    enddo

    no_val = ival

end subroutine ct2jettraj

