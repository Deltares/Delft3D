subroutine jet3d2flow(thick  ,kmax   ,dps    ,s0     ,r0       ,          &
                    & lstsci ,lsal   ,ltem   ,xz     ,yz       ,nmmax   , &
                    & kcs    ,flwang ,sign   ,time   ,linkinf  ,gdp     )
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
!    Function: Converts Jet3D output to delft3d sources following the DESA
!              methodology of Joseph Lee
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
!
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
    integer, dimension(:)           , pointer :: m_diff
    integer, dimension(:)           , pointer :: n_diff
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
    real(fp)                                                                 :: sign     !  Description and declaration in tricom.igs
    real(fp)   ,                                                 intent(in)  :: flwang   !  Description and declaration in esm_alloc_real.f90 gs
    real(fp)     , dimension(8)                                , intent(in)  :: linkinf
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: s0       !  Description and declaration in esm_alloc_real.f90 gs
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: xz       !  Description and declaration in esm_alloc_real.f90 gs
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: yz       !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub,kmax,lstsci)  , intent(in)  :: r0
    real(fp)   , dimension(kmax)                               , intent(in)  :: thick    !  Description and declaration in esm_alloc_real.f90 gs
    real(prec) , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: dps      !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer                                  :: ierror
    integer                                  :: irow
    integer                   , external     :: newlun
    integer                                  :: nm_diff
    integer                                  :: nrow
    integer                                  :: luntmp
    real(fp)                                 :: deg2rad
    real(fp)                                 :: pi
    real(fp) , dimension(4)                  :: rdum
    real(fp)                                 :: xxx
    real(fp)                                 :: yyy
    real(fp) , dimension(:)   , allocatable  :: x_jet
    real(fp) , dimension(:)   , allocatable  :: y_jet
    real(fp) , dimension(:)   , allocatable  :: z_jet
    real(fp) , dimension(:)   , allocatable  :: b_jet
    real(fp) , dimension(:)   , allocatable  :: h_jet
    real(fp) , dimension(:)   , allocatable  :: s_jet
    real(fp) , dimension(:)   , allocatable  :: v_jet
!
!! executable statements -------------------------------------------------------
!
    m_diff         => gdp%gdnfl%m_diff
    n_diff         => gdp%gdnfl%n_diff
    !
    pi      = acos(-1.0_fp)
    deg2rad = pi/180.0_fp
    !
    call n_and_m_to_nm(n_diff(1), m_diff(1), nm_diff, gdp)
    !
    ! Open Jet3d output file and read jet characteristics end of near field
    !
    open (newunit=luntmp,file='str3dtek.xxx',status='old')
    !
    call skipstarlines (luntmp)
    read (luntmp,'( )',end = 999)
    !
    read (luntmp,*) nrow
    !
    allocate (x_jet(nrow), stat=ierror)
    allocate (y_jet(nrow), stat=ierror)
    allocate (z_jet(nrow), stat=ierror)
    allocate (b_jet(nrow), stat=ierror)
    allocate (h_jet(nrow), stat=ierror)
    allocate (s_jet(nrow), stat=ierror)
    allocate (v_jet(nrow), stat=ierror)

    x_jet = 0.0_fp
    y_jet = 0.0_fp
    z_jet = 0.0_fp
    b_jet = 0.0_fp
    h_jet = 0.0_fp
    s_jet = 0.0_fp
    v_jet = 0.0_fp

    !
    do irow = 1, nrow
       read (luntmp,*)       rdum(1)    , x_jet(irow), y_jet(irow), z_jet(irow), &
                           & s_jet(irow), b_jet(irow), v_jet(irow)
    enddo
    !
    ! Rising or sinking plume: determine terminal level
    !
    irow = 2
    if (z_jet(2) >= z_jet(1)) then
       do while (z_jet(irow) >= z_jet(irow-1).and. irow < nrow)
          irow = irow + 1
       enddo
    elseif (z_jet(2) <= z_jet(1)) then
       do while (z_jet(irow) <= z_jet(irow-1) .and. irow < nrow)
          irow = irow + 1
       enddo
    endif
    !
    nrow = irow
    !
    ! Jet3d relative to main flow direction, convert coordinates back to orignal coordinate system
    !
    do irow = 1, nrow
       xxx = x_jet(irow)*cos(flwang*deg2rad) - sign*y_jet(irow)*sin(flwang*deg2rad)
       yyy = x_jet(irow)*sin(flwang*deg2rad) + sign*y_jet(irow)*cos(flwang*deg2rad)
       x_jet(irow) = xz(nm_diff) + xxx
       y_jet(irow) = yz(nm_diff) + yyy
       z_jet(irow) = real(dps(nm_diff),fp) - z_jet(irow)
    enddo
    !
    ! Fill sources and sinks following the Desa Method of Prof. Lee
    !
    !call desa(x_jet      ,y_jet       ,z_jet     ,s_jet      ,nrow    , &
    !        & kcs        ,xz          ,yz        ,dps        ,s0      , &
    !        & nmmax      ,thick       ,kmax      ,lstsci     ,lsal    , &
    !        & ltem       ,h_jet       ,b_jet     ,v_jet      ,1       , &
    !        & rdum(1)    ,rdum(2)     ,rdum(3)   ,rdum(4)    ,r0      , &
    !        & linkinf ,gdp     )
    !
    ! Deallocate temporary arrays
    !
    deallocate (x_jet, stat=ierror)
    deallocate (y_jet, stat=ierror)
    deallocate (z_jet, stat=ierror)
    deallocate (b_jet, stat=ierror)
    deallocate (h_jet, stat=ierror)
    deallocate (s_jet, stat=ierror)
    deallocate (v_jet, stat=ierror)
    !
999 continue
    !
    close (luntmp)
    !
end subroutine jet3d2flow
