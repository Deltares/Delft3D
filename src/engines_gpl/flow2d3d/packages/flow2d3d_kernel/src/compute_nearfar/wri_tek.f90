subroutine wri_tek    (x_jet  , y_jet  , z_jet , no_val , xstart ,xend  ,ystart ,yend  ,filename,&
                     & bv_jet , bh_jet , s_jet , linkinf, gdp    )
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
!  $Id: wri_tek.f90 5888 2016-02-24 10:14:54Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160128_34357_NearField_Coupling/src/engines_gpl/flow2d3d/packages/kernel/src/compute_nearfar/wri_tek.f90 $
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
    integer                                                     , intent(in)    :: no_val
    real(fp)                                                    , intent(in)    :: xstart
    real(fp)                                                    , intent(in)    :: xend
    real(fp)                                                    , intent(in)    :: ystart
    real(fp)                                                    , intent(in)    :: yend
    real(fp)   , dimension(8)                                   , intent(in)    :: linkinf
    real(fp)   , dimension(no_val)                              , intent(in)    :: x_jet
    real(fp)   , dimension(no_val)                              , intent(in)    :: y_jet
    real(fp)   , dimension(no_val)                              , intent(in)    :: z_jet
    real(fp)   , dimension(no_val)                              , intent(in)    :: bv_jet
    real(fp)   , dimension(no_val)                              , intent(in)    :: bh_jet
    real(fp)   , dimension(no_val)                              , intent(in)    :: s_jet
   character*256                                                , intent(in)    :: filename
!
! Local variables
!
    integer                                                                    :: ival
    integer                                                                    :: n
    integer                                                                    :: m
    integer                                                                    :: luntmp
    integer                                                                    :: newlun

!
!! executable statements -------------------------------------------------------
!
    luntmp = newlun(gdp)
    open (luntmp,file=filename,status='unknown')

    write (luntmp,'(a,f12.6)')'* Salinity    of the discharge : ',linkinf(1)
    write (luntmp,'(a,f12.6)')'* Temperature of the discharge : ',linkinf(2)
    write (luntmp,'(a,f12.6)')'* Density     of the discharge : ',linkinf(3)
    write (luntmp,'(a,f12.6)')'* Ambient flow velocity (m/s)  : ',linkinf(4)
    write (luntmp,'(a,f12.6)')'* Ambient flow direction (o)   : ',linkinf(5)
    write (luntmp,'(a,f12.6)')'* Relative flow direction (o)  : ',linkinf(6)

    call nm_to_n_and_m(nint(linkinf(7)),n,m,gdp)
    write (luntmp,'(a,i8)') '* M coordinate start point     : ',m
    write (luntmp,'(a,i8)') '* N coordinate start point     : ',n
    call nm_to_n_and_m(nint(linkinf(8)),n,m,gdp)
    write (luntmp,'(a,i8)') '* M coordinate end   point     : ',m
    write (luntmp,'(a,i8)') '* N coordinate end   point     : ',n

    write (luntmp,'(''* Column  1: x-coordinate jet centre line'')')
    write (luntmp,'(''* Column  2: y-coordinate jet centre line'')')
    write (luntmp,'(''* Column  3: z-coordinate jet centre line'')')
    write (luntmp,'(''* Column  4: vertical half width (bv)'')')
    write (luntmp,'(''* Column  5: horizontal half width (bh)'')')
    write (luntmp,'(''* Column  6: jet centre line dilution (s)'')')
    write (luntmp,'(''BL01'')')
    write (luntmp,'(2i6)') no_val,6

    do ival = 1, no_val
       write (luntmp,'(6f12.3)') x_jet (ival), y_jet(ival),-1.0_fp*z_jet(ival), bv_jet(ival), &
                               & bh_jet(ival), s_jet(ival)
    enddo

    write (luntmp,'(''* Column  1: x-coordinate jet start point'')')
    write (luntmp,'(''* Column  2: y-coordinate jet start point'')')
    write (luntmp,'(''* Column  3: z-coordinate jet start point'')')
    write (luntmp,'(''BL02'')')
    write (luntmp,'(2i6)') 1,3
    write (luntmp,'(3f12.3)') x_jet(1), y_jet(1),-1.0_fp*z_jet(1)

    write (luntmp,'(''* Column  1: x-coordinate jet end point'')')
    write (luntmp,'(''* Column  2: y-coordinate jet end point'')')
    write (luntmp,'(''* Column  3: z-coordinate jet end point'')')
    write (luntmp,'(''BL03'')')
    write (luntmp,'(2i6)') 1,3
    write (luntmp,'(3f12.3)') x_jet(no_val), y_jet(no_val),-1.0_fp*z_jet(no_val)

    write (luntmp,'(''BL04'')')
    write (luntmp,'(2i6)') 2,2
    write (luntmp,'(2f12.3)') xstart,ystart
    write (luntmp,'(2f12.3)') xend  ,yend

    close (luntmp)

end subroutine wri_tek
