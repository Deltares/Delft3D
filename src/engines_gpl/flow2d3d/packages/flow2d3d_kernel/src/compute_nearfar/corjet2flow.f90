subroutine corjet2flow(thick  ,kmax  ,dps   ,s0    ,disch_nf ,sour_nf , &
                     & lstsci ,lsal  ,ltem  ,xz    ,yz       ,nmmax   , &
                     & kcs    ,time  ,gdp   )
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
!  $Id: corjet2flow.f90 6194 2016-06-09 16:29:28Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160128_34357_NearField_Coupling/src/engines_gpl/flow2d3d/packages/kernel/src/compute_nearfar/corjet2flow.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Converts cormix output to delft3d sources
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
    integer ,dimension(:)           , pointer :: m_diff
    integer ,dimension(:)           , pointer :: n_diff
    real(fp),dimension(:)           , pointer :: q_diff
    real(fp),dimension(:,:)         , pointer :: const_diff
!
! Global variables
!
    integer                                                    , intent(in)  :: kmax     !  Description and declaration in tricom.igs
    integer                                                    , intent(in)  :: lstsci   !  Description and declaration in tricom.igs
    integer                                                    , intent(in)  :: lsal     !  Description and declaration in tricom.igs
    integer                                                    , intent(in)  :: ltem     !  Description and declaration in tricom.igs
    integer                                                    , intent(in)  :: nmmax    !  Description and declaration in tricom.igs
    real(fp)                                                   , intent(in)  :: time
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kcs      !  Description and declaration in
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: s0       !  Description and declaration in rjdim.f90 gs
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: xz       !  Description and declaration in rjdim.f90 gs
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: yz       !  Description and declaration in rjdim.f90
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(out) :: disch_nf !  Description and declaration in rjdim.f90
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax,lstsci) , intent(out) :: sour_nf  !  Description and declaration in
    real(fp)   , dimension(kmax)                               , intent(in)  :: thick    !  Description and declaration in rjdim.f90 gs
    real(prec) , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: dps      !  Description and declaration in rjdim.f90
!
! Local variables
!
    integer                                     :: ierror
    integer                                     :: nm_diff
    integer                      , external     :: newlun
    integer                                     :: luntmp
    integer                                     :: iocond
    integer                                     :: irow
    integer                                     :: itel
    integer                                     :: nrow
    real(fp)                                    :: xx
    real(fp)                                    :: yy
    real(fp)                                    :: zz
    real(fp)                                    :: ss
    real(fp)      , dimension(:) , allocatable  :: x_jet
    real(fp)      , dimension(:) , allocatable  :: y_jet
    real(fp)      , dimension(:) , allocatable  :: z_jet
    real(fp)      , dimension(:) , allocatable  :: s_jet
    real(fp)      , dimension(:) , allocatable  :: v_jet
    real(fp)      , dimension(:) , allocatable  :: b_jet
    real(fp)      , dimension(:) , allocatable  :: h_jet
    character(80)                               :: record
!
!! executable statements -------------------------------------------------------
!
    m_diff         => gdp%gdnfl%m_diff
    n_diff         => gdp%gdnfl%n_diff
    q_diff         => gdp%gdnfl%q_diff
    const_diff     => gdp%gdnfl%const_diff
    !
    allocate (x_jet(1000), stat=ierror)
    allocate (y_jet(1000), stat=ierror)
    allocate (z_jet(1000), stat=ierror)
    allocate (s_jet(1000), stat=ierror)
    !
    call n_and_m_to_nm(n_diff(1), m_diff(1), nm_diff, gdp)
    !
    ! Open cormix output file and read jet characteristics end of near field
    ! end of corjet computation
    !
    luntmp = newlun (gdp)
    open (luntmp,file='corjet.out',status='old')
    record (1:13) = '             '
    !
    do while (record (6:13) .ne. 'X      Y')
       read (luntmp,'(a80)') record
    enddo
    itel = 1
    read (record(1:47),*,iostat = iocond) x_jet(itel), y_jet(itel), z_jet(itel), s_jet(itel)
    !
    do while (record(2:13) .ne. '------------')
       read (luntmp,'(a80)') record
       read (record(1:47),*,iostat = iocond) xx, yy, zz, ss
       if (iocond == 0) then
          itel = itel + 1
          if (itel == 1000) stop 'Enlarge allocatable arrays in cormix2flow'
          x_jet(itel) = xx
          y_jet(itel) = yy
          z_jet(itel) = zz
          s_jet(itel) = ss
       endif
    enddo
    !
    nrow = itel - 1
    !
    ! Determine angle jet end near field
    !
    do irow = 1, nrow
       x_jet(irow) = xz(nm_diff) + x_jet(irow)
       y_jet(irow) = yz(nm_diff) + y_jet(irow)
       z_jet(irow) = real(dps(nm_diff),fp) - z_jet(irow)
    enddo
    !
    ! Fill sources and sinks following the Desa Method of Prof. Lee
    !
    !call desa(x_jet   ,y_jet    ,z_jet   ,s_jet   ,nrow    , &
    !        & kcs     ,xz       ,yz      ,dps     ,s0      , &
    !        & nmmax   ,thick    ,kmax    ,lstsci  ,lsal    , &
    !        & ltem    ,disch_nf ,sour_nf ,b_jet   ,h_jet   , &
    !        v_jet     ,1        ,gdp      )
    !
    ! Deallocate temporary arrays
    !
    deallocate (x_jet, stat=ierror)
    deallocate (y_jet, stat=ierror)
    deallocate (z_jet, stat=ierror)
    deallocate (s_jet, stat=ierror)
    !
    close(luntmp)
end subroutine corjet2flow
