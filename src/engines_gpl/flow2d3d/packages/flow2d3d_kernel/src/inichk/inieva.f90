subroutine inieva(runid     ,cyclic    ,timnow    ,evaint    ,j         , &
                & nmmaxj    ,nmmax     ,evap      ,precip    ,gdp       )
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
!    Function: Reads the time dependent data for rainfall /
!              evaporation model from file for the first time
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use string_module
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer     , pointer :: it0eva
    integer     , pointer :: it1eva
    real(fp)    , pointer :: dt
    integer     , pointer :: luneva
    real(fp)    , pointer :: evapor
    real(fp)    , pointer :: devapo
    real(fp)    , pointer :: precipt
    real(fp)    , pointer :: dpreci
    real(fp)    , pointer :: train
    real(fp)    , pointer :: dtrain
    logical     , pointer :: fleva
!
! Global variables
!
    integer                                                 :: j      !!  Begin pointer for arrays which have been transformed into 1D arrays. Due to the shift in the 2nd (M-) index, J = -2*NMAX + 1
    integer                                   , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer                                                 :: nmmaxj !  Description and declaration in dimens.igs
    logical                                                 :: cyclic !!  Flag = TRUE if cyclic system assumed
    real(fp)                                                :: timnow !!  Current timestep (ITSTRT * dt)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(out) :: evap   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(out) :: precip !  Description and declaration in esm_alloc_real.f90
    character(*)                                            :: runid
    character(1)                              , intent(in)  :: evaint !  Description and declaration in tricom.igs
!
! Local variables
!
    integer           :: itfac    ! Interpolation factor 
    integer           :: lrid     ! Length of character string runid 
    integer           :: newlun
    integer           :: nm       ! Loop counter for NMMAX 
    logical           :: first    ! Help var. It is always set to TRUE before calling the relevant routines for the time dependent data, because they are activated here for the first time 
    logical           :: inteva   ! Interpolation method between consecu- tive rain/evaporation data: N = No     interpolation. Y = Linear interpolation. 
    logical           :: opend    ! Help flag = TRUE when file is still open (DELFT3D) and 
    character(256)    :: filnam   ! Help var. for file name 
!
!! executable statements -------------------------------------------------------
!
    fleva       => gdp%gdtmpfil%fleva
    evapor      => gdp%gdheat%evapor
    devapo      => gdp%gdheat%devapo
    precipt     => gdp%gdheat%precipt
    dpreci      => gdp%gdheat%dpreci
    train       => gdp%gdheat%train
    dtrain      => gdp%gdheat%dtrain
    luneva      => gdp%gdluntmp%luneva
    dt          => gdp%gdexttim%dt
    it0eva      => gdp%gdinttim%it0eva
    it1eva      => gdp%gdinttim%it1eva
    !
    inteva = .false.
    if (evaint=='Y') inteva = .true.
    !
    ! initilisation global parameters, initial pressure and common
    !     values of HEAT
    !
    precipt = 0.0_fp
    dpreci  = 0.0_fp
    evapor  = 0.0_fp
    devapo  = 0.0_fp
    train   = 0.0_fp
    dtrain  = 0.0_fp
    !
    ! define length of RUNID
    !
    call remove_leading_spaces(runid     ,lrid      )
    !
    ! Time dependent rainfall / evaporation
    ! Only if KEVA > 0 FLEVA = .true. (See READMD)
    !
    if (fleva) then
       filnam = 'TMP_' // runid(:lrid) // '.eva'
       !
       ! Test if file is already opened (multi entry Delft3D)
       !
       inquire (file = filnam(:8 + lrid), opened = opend)
       if (.not.opend) then
          open (newunit=luneva, file = filnam(:8 + lrid), form = 'unformatted',         &
               & status = 'old')
       endif
       !
       ! Always rewind file, reset time parameters and read new time
       ! dependent input
       !
       rewind (luneva)
       it0eva = -1
       it1eva = -1
       !
       ! Read new time dep. input
       !
       first = .true.
       call updeva(luneva    ,timnow    ,dt        ,inteva    ,first     , &
                 & gdp       )
       !
       !
       ! Interpolate between IT0EVA and TIMNOW for rainfall/evaporation
       ! data when interpolation is requested
       !
       if (inteva) then
          itfac   = (int(timnow) - it0eva)*2
          precipt = precipt + dpreci*itfac
          evapor  = evapor + devapo*itfac
          train   = train + dtrain*itfac
       endif
       !
       ! Update evaporation and rainfall (start value for both block function as
       ! step interpolation)
       !
       do nm = 1, nmmax
          evap  (nm) = evapor
          precip(nm) = precipt
       enddo
    endif
end subroutine inieva
