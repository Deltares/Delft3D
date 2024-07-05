subroutine cortim2flow(thick  ,kmax  ,dps   ,s0    ,r0       ,          &
                     & lstsci ,lsal  ,ltem  ,xz    ,yz       ,nmmax   , &
                     & kcs    ,filename     ,taua            ,idis    , &
                     & linkinf,gdp     )
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
!  $Id: cortim2flow.f90 5913 2016-03-02 09:03:59Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160128_34357_NearField_Coupling/src/engines_gpl/flow2d3d/packages/kernel/src/compute_nearfar/cortim2flow.f90 $
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
    integer ,dimension(:)          , pointer :: m_diff
    integer ,dimension(:)          , pointer :: n_diff
!
! Global variables
!
    integer                                                      , intent(in)  :: idis
    integer                                                      , intent(in)  :: kmax     !  Description and declaration in tricom.igs
    integer                                                      , intent(in)  :: lstsci   !  Description and declaration in tricom.igs
    integer                                                      , intent(in)  :: lsal     !  Description and declaration in tricom.igs
    integer                                                      , intent(in)  :: ltem     !  Description and declaration in tricom.igs
    integer                                                      , intent(in)  :: nmmax    !  Description and declaration in tricom.igs
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kcs      !  Description and declaration in
    real(fp)                                                     , intent(in)  :: taua
    real(fp)     , dimension(8)                                  , intent(in)  :: linkinf
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: s0       !  Description and declaration in rjdim.f90 gs
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: xz       !  Description and declaration in rjdim.f90 gs
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: yz       !  Description and declaration in rjdim.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub,kmax,lstsci)  , intent(in)  :: r0       !  Description and declaration in rjdim.f90
    real(fp)     , dimension(kmax)                               , intent(in)  :: thick    !  Description and declaration in rjdim.f90 gs
    real(prec)   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: dps      !  Description and declaration in rjdim.f90
    character*256,dimension(3)                                   , intent(in)  :: filename
!
! Local variables
!
    integer                                     :: ierror
    integer                                     :: nm_diff
    integer                      , external     :: newlun
    integer                                     :: no_modules
    integer                                     :: luntmp
    integer                                     :: iocond
    integer                                     :: irow
    integer                                     :: itel
    integer                                     :: nrow
    integer                                     :: no_val
    integer       , dimension(:) , allocatable  :: modstart
    real(fp)                                    :: xx
    real(fp)                                    :: yy
    real(fp)                                    :: zz
    real(fp)                                    :: ss
    real(fp)                                    :: xstart
    real(fp)                                    :: xend
    real(fp)                                    :: ystart
    real(fp)                                    :: yend
    real(fp)      , dimension(:) , allocatable  :: x_cor
    real(fp)      , dimension(:) , allocatable  :: y_cor
    real(fp)      , dimension(:) , allocatable  :: z_cor
    real(fp)      , dimension(:) , allocatable  :: s_cor
    real(fp)      , dimension(:) , allocatable  :: bv_cor
    real(fp)      , dimension(:) , allocatable  :: bh_cor
    real(fp)      , dimension(:) , allocatable  :: v_cor
    real(fp)      , dimension(:) , allocatable  :: x_jet
    real(fp)      , dimension(:) , allocatable  :: y_jet
    real(fp)      , dimension(:) , allocatable  :: z_jet
    real(fp)      , dimension(:) , allocatable  :: s_jet
    real(fp)      , dimension(:) , allocatable  :: bv_jet
    real(fp)      , dimension(:) , allocatable  :: bh_jet

    character*256 , dimension(:) , allocatable  :: modules
!
!! executable statements -------------------------------------------------------
!
    m_diff         => gdp%gdnfl%m_diff
    n_diff         => gdp%gdnfl%n_diff

    call n_and_m_to_nm(n_diff(idis), m_diff(idis), nm_diff, gdp)

    !
    ! Open cormix output file
    !

    luntmp = newlun (gdp)
    open (luntmp,file=filename(2),status='old')

    !
    ! Determine the number of modules used by Cormix and the number of jut/plume trajectory values
    ! Allocate temporary arrays
    !

    call cortim_no_modules (luntmp,no_modules,nrow)
    rewind luntmp

    allocate (x_cor   (nrow), stat=ierror)
    allocate (y_cor   (nrow), stat=ierror)
    allocate (z_cor   (nrow), stat=ierror)
    allocate (s_cor   (nrow), stat=ierror)
    allocate (bv_cor  (nrow), stat=ierror)
    allocate (bh_cor  (nrow), stat=ierror)
    allocate (v_cor   (nrow), stat=ierror)
    allocate (x_jet   (nrow), stat=ierror)
    allocate (y_jet   (nrow), stat=ierror)
    allocate (z_jet   (nrow), stat=ierror)
    allocate (s_jet   (nrow), stat=ierror)
    allocate (bv_jet  (nrow), stat=ierror)
    allocate (bh_jet  (nrow), stat=ierror)
    allocate (modules (no_modules), stat=ierror)
    allocate (modstart(no_modules), stat=ierror)

    !
    ! Read cormix jet/plume trajectory and belonging characteristics
    ! Allocate temporary arrays
    !

    call cortim_xyzs (luntmp,no_modules, nrow, modules,modstart,x_cor,y_cor,z_cor,s_cor,bv_cor,bh_cor,v_cor)

    close (luntmp)

    !
    ! Cortim results to jet trajectory (in real world coordinates)
    !

    call ct2jettraj (no_modules, nrow, modules, modstart, x_cor  , y_cor  , z_cor , s_cor  , bv_cor , bh_cor , &
                                                          x_jet  , y_jet  , z_jet , s_jet  , bv_jet , bh_jet , &
                                                          xz     , yz     , dps   , s0     , nm_diff,&
                                                          no_val , taua           , idis   , gdp    )

    !
    ! Fill sources and sinks following the Desa Method of Prof. Lee
    !



    call desa_kepco(x_jet   ,y_jet    ,z_jet   ,s_jet   ,no_val  , &
	                & kcs     ,xz       ,yz      ,dps     ,s0      , &
	                & nmmax   ,thick    ,kmax    ,lstsci  ,lsal    , &
	                & ltem    ,bv_jet  ,bh_jet   ,idis    , &
                    & xstart  ,xend    ,ystart   ,yend    ,r0      , &
	                & linkinf ,gdp     )
!
! Fill sources and sinks following the Desa Method of Prof. Lee
!
!
!
!
!   call desa(x_jet   ,y_jet    ,z_jet   ,s_jet   ,no_val  , &
!                & kcs     ,xz       ,yz      ,dps     ,s0      , &
!	             & nmmax   ,thick    ,kmax    ,lstsci  ,lsal    , &
!	             & ltem    ,bv_jet  ,bh_jet   ,idis    , &
!                & xstart  ,xend    ,ystart   ,yend    ,r0      , &
!                & linkinf ,gdp     )

    !
    ! Temporarily, write cormix trajectory to tekal file for postprocessing
    !

    call wri_tek (x_jet  , y_jet  , z_jet , no_val  , xstart  , xend  , ystart  , yend  , filename(3), &
                & bv_jet , bh_jet , s_jet , linkinf , gdp     )


    !
    ! Deallocate temporary arrays
    !

    deallocate (x_cor, stat=ierror)
    deallocate (y_cor, stat=ierror)
    deallocate (z_cor, stat=ierror)
    deallocate (s_cor, stat=ierror)
    deallocate (bv_cor, stat=ierror)
    deallocate (bh_cor, stat=ierror)
    deallocate (v_cor, stat=ierror)
    deallocate (x_jet, stat=ierror)
    deallocate (y_jet, stat=ierror)
    deallocate (z_jet, stat=ierror)
    deallocate (s_jet, stat=ierror)
    deallocate (bv_jet, stat=ierror)
    deallocate (bh_jet, stat=ierror)
    deallocate (modules, stat=ierror)
    deallocate (modstart, stat=ierror)
    !
end subroutine cortim2flow
