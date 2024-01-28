subroutine incmeteo(timhr  ,grdang ,windu  ,windv ,patm   , &
                    kcs    ,alfas  ,windcd ,                &
                    windsu ,windsv ,w10mag ,gdp   )
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
!    Function: - Update meteo related items
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use meteo
    !use ec_module
    use precision
    use dfparall
    use globaldata
    use m_openda_exchange_items, only : get_openda_buffer, get_openda_buffersize
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer        , pointer :: nmax
    integer        , pointer :: mmax
    integer        , pointer :: nlb
    integer        , pointer :: nub
    integer        , pointer :: mlb
    integer        , pointer :: mub
    integer        , pointer :: nmaxus
    integer        , pointer :: kc
    integer        , pointer :: itdate
    real(fp)       , pointer :: tzone
    !type(tECHandle), pointer :: ECHandle
    !integer        , pointer :: patmECItemId
    !integer        , pointer :: uwindECItemId
    !integer        , pointer :: vwindECItemId
    real(hp)       , pointer :: dtimmin       ! Current timestep (in min) in high precision
!
! Global variables
!
    integer    , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: kcs
    real(fp)                                                        , intent(in)  :: timhr
    real(fp)                                                        , intent(in)  :: grdang
    real(fp)   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: alfas
    real(fp)   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: windcd
    real(fp)   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: windu
    real(fp)   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: windv
    real(fp)   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: windsu
    real(fp)   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: windsv
    real(fp)   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: patm
    real(fp)   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: w10mag
!
! Local variables
!
    integer                                   :: igrid
    integer                                   :: gugridsize
    integer                                   :: gvgridsize
    logical                                   :: success
    real(fp)                                  :: time
    real(fp)   ,allocatable, dimension(:,:)   :: gridunoise
    real(fp)   ,allocatable, dimension(:,:)   :: gridvnoise
    integer                                   :: nm_pos ! indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
!
!! executable statements -------------------------------------------------------
!
    nmax          => gdp%d%nmax
    mmax          => gdp%d%mmax
    nlb           => gdp%d%nlb
    nub           => gdp%d%nub
    mlb           => gdp%d%mlb
    mub           => gdp%d%mub
    nmaxus        => gdp%d%nmaxus
    kc            => gdp%d%kc
    itdate        => gdp%gdexttim%itdate
    tzone         => gdp%gdexttim%tzone
    !ECHandle      => gdp%gd_ECHandle
    !patmECItemId  => gdp%patmECItemId
    !uwindECItemId => gdp%uwindECItemId
    !vwindECItemId => gdp%vwindECItemId
    dtimmin       => gdp%gdinttim%dtimmin
    !
    time          = timhr*60.0  ! time in minutes
    nm_pos        = 1
    !
    ! update all meteo items (if necessary)
    !
    !if (patmECItemId == -1) then
       success = meteoupdate(gdp%runid, itdate, tzone, time)
       call checkmeteoresult(success, gdp)
       !
       ! update wind arrays
       !
       !
       ! First: check if wind noise grid on curvilinear grid is available 
       !
       gugridsize = get_openda_buffersize('windgu')
       if (gugridsize > 0) then
          allocate(gridunoise(gugridsize,3))
          gridunoise = 0.0_fp
          do igrid = 1, gugridsize
             call get_openda_buffer('windgu'  , igrid, 1, 1, gridunoise(igrid,1))       
             call get_openda_buffer('x_windgu', igrid, 1, 1, gridunoise(igrid,2))  
             call get_openda_buffer('y_windgu', igrid, 1, 1, gridunoise(igrid,3))               
          enddo
          !
          ! if gridnoise is not empty, call getmeteoval with extra argument: triple array to be interpolated.  
          !
          success = getmeteoval(gdp%runid, 'windu', time, gdp%gdparall%mfg, gdp%gdparall%nfg,   &
                              & nlb, nub, mlb, mub, windu, gugridsize,gridunoise)
          deallocate(gridunoise)
       else
          success = getmeteoval(gdp%runid, 'windu', time, gdp%gdparall%mfg, gdp%gdparall%nfg,   &
                              & nlb, nub, mlb, mub, windu, 0)
       endif  
       call checkmeteoresult(success, gdp)
       !
       gvgridsize = get_openda_buffersize('windgv')
       if (gvgridsize > 0) then
          allocate(gridvnoise(gvgridsize,3))
          gridvnoise = 0.0
          do igrid = 1, gvgridsize
             call get_openda_buffer('windgv', igrid, 1, 1, gridvnoise(igrid,1))       
             call get_openda_buffer('x_windgv', igrid, 1, 1, gridvnoise(igrid,2))  
             call get_openda_buffer('y_windgv', igrid, 1, 1, gridvnoise(igrid,3))               
          enddo
          !
          ! if gridnoise is not empty, call getmeteoval with extra argument: triple array to be interpolated.  
          !
          success = getmeteoval(gdp%runid, 'windv', time, gdp%gdparall%mfg, gdp%gdparall%nfg, &
                                nlb, nub, mlb, mub, windv, gvgridsize, gridvnoise)
          deallocate(gridvnoise)
       else
          success = getmeteoval(gdp%runid, 'windv', time, gdp%gdparall%mfg, gdp%gdparall%nfg, &
          nlb, nub, mlb, mub, windv, 0)       
       endif          
       call checkmeteoresult(success, gdp)
       !
       success = getmeteoval(gdp%runid,  'patm', time, gdp%gdparall%mfg, gdp%gdparall%nfg, nlb, nub, mlb, mub,  patm, 0)
       call checkmeteoresult(success, gdp)
    !else
    !   success = getVal(ECHandle, patmECItemId , dtimmin,  patm, nlb, nub, mlb, mub)
    !   call checkResult(ECHandle, success)
    !   success = getVal(ECHandle, uwindECItemId, dtimmin, windu, nlb, nub, mlb, mub)
    !   call checkResult(ECHandle, success)
    !   success = getVal(ECHandle, vwindECItemId, dtimmin, windv, nlb, nub, mlb, mub)
    !   call checkResult(ECHandle, success)
    !endif
    !
    ! Get possible input from OpenDA. Here: scalar adjustment of windfiles. Noise on an entire grid
    ! has already been precessed above, inside the getmeteoval call.
    !
    call get_openda_buffer('windu', 1, nub-nlb+1, mub-mlb+1, windu)
    call get_openda_buffer('windv', 1, nub-nlb+1, mub-mlb+1, windv)
    !
    ! Exchange data between partitions
    ! NB: wind velocities might not be required
    !     Patm required as gradient calculated in cucnp
    !
    call dfexchg(patm,  1, 1, dfloat, nm_pos, gdp)    
    call dfexchg(windu, 1, 1, dfloat, nm_pos, gdp)    
    call dfexchg(windv, 1, 1, dfloat, nm_pos, gdp)    
    call windtostress(mmax ,nmax ,nmaxus, grdang, kcs, w10mag, windu, windv, windsu, windsv, windcd, gdp)
    !
    ! Exchange data between partitions
    !
    call dfexchg(windsu, 1, 1, dfloat, nm_pos, gdp)
    call dfexchg(windsv, 1, 1, dfloat, nm_pos, gdp)
    call windtogridc (mmax  ,nmax  ,nmaxus,kcs   ,alfas ,windsu,windsv       ,gdp)
    !
    ! Exchange data between partitions
    !
    call dfexchg(windsu, 1, 1, dfloat, nm_pos, gdp)
    call dfexchg(windsv, 1, 1, dfloat, nm_pos, gdp)
end subroutine incmeteo
