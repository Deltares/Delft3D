subroutine updbct(lundia, filnam, ntoftoq, nto, kcd, kmax, hydrbc, tprofu, error, gdp)
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
!    Function: Reads the time dependent hydrodynamic BC using tables module
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use handles
    use flow_tables
    use globaldata
    use m_openda_exchange_items, only : get_openda_buffer
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)                         , pointer :: timhr
    integer                          , pointer :: julday
    integer                          , pointer :: htype
    integer                          , pointer :: timerec
    integer, dimension(:)            , pointer :: bct_order
    integer                          , pointer :: gntoftoq
    integer, dimension(:)            , pointer :: ext_bnd
    type (handletype)                , pointer :: tseriesfile
    !
    ! Global variables
    !
    integer                      , intent(in)  :: kcd     ! Description and declaration in dimens.igs
    integer                      , intent(in)  :: kmax    ! Description and declaration in esm_alloc_int.f90
    integer                      , intent(in)  :: lundia  ! Description and declaration in inout.igs
    character(len=*)             , intent(in)  :: filnam  ! filename of TMP bct file
    integer                      , intent(in)  :: ntoftoq ! Number of open boundary sections before the time series type:
                                                          ! ntoftoq = ntof + ntoq
    integer                      , intent(in)  :: nto     ! Description and declaration in esm_alloc_int.f90
    real(fp), dimension(4, nto, kcd)           :: hydrbc  ! Description and declaration in esm_alloc_real.f90
    character(20), dimension(nto), intent(in)  :: tprofu  ! Description and declaration in esm_alloc_char.f90
    logical                      , intent(out) :: error   ! Flag=TRUE if an error is encountered
!
! Local variables
!
    integer                                    :: istat       ! error code allocate
    integer                                    :: irec        ! record number of time array
    integer                                    :: ito         ! Index number of open boundary loc.
    integer                                    :: j           ! loop counter
    integer                                    :: k           ! loop counter
    integer                                    :: minrec      ! minimum of irec
    integer                                    :: tablenumber
    real(fp), dimension(:), allocatable        :: work        ! work array
!
!! executable statements -------------------------------------------------------
!
    julday       => gdp%gdinttim%julday
    timhr        => gdp%gdinttim%timhr
    tseriesfile  => gdp%gdinibct%tseriesfile
    htype        => gdp%gdinibct%tseriesfile%htype
    timerec      => gdp%gdinibct%timerec
    bct_order    => gdp%gdbcdat%bct_order
    gntoftoq     => gdp%gdbcdat%gntoftoq
    ext_bnd      => gdp%gdbcdat%ext_bnd
    !
    error = .false.
    if (htype == -999) then
       !
       ! in case of:
       ! - reuse TMP files
       ! - parallel running and this is not the master
       !
       call flw_readtable(tseriesfile, filnam, julday, gdp)
    endif
    !
    minrec = huge(minrec)
    do ito = ntoftoq + 1, nto
       if (ext_bnd(ito) == 1) cycle
       irec = max(1, timerec)
       !
       ! See big comment block about gntoftoq in rdbndd.f90
       !
       tablenumber = bct_order(ito) - gntoftoq
       if (tprofu(ito) == '3d-profile') then
          if (.not. allocated(work)) then
             allocate(work(2*kmax), stat=istat)
             if (istat /= 0) then
                call prterr(lundia, 'U021', 'updbct: memory alloc error')
                error = .true.
                return
             endif
          endif
          call flw_gettabledata(tseriesfile, tablenumber, 1, 2*kmax, irec, work, timhr, julday, gdp)
          do j = 1, 2
             do k = 1, kmax
                hydrbc(j, ito, k) = work(k + (j-1)*kmax)
             enddo
          enddo
       else
          call flw_gettabledata(tseriesfile, tablenumber , 1, 2, irec, hydrbc(1:2,ito,1), timhr, julday, gdp)
       endif
       minrec = min(minrec, irec)
       !
       ! Adjust boundaries by OpenDA if necessary
       !
       call get_openda_buffer('bound_HQ', ito, 2, kmax, hydrbc(1:2,ito,1:kmax))
    enddo
    !
    timerec = minrec
    if (allocated(work)) deallocate(work)
end subroutine updbct
