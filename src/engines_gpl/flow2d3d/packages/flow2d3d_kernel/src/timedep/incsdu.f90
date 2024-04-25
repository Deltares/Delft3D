subroutine incsdu(timhr  ,dps  ,s1  ,kcs  ,kfs,  gdp    )
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
!    Function: Determine increments and update the current time
!              dependent value for subsidence/uplift data (if lfsdu
!              = true) 
! Method used: At each time step (if lfsdu=true) the difference between the 
!              values at the previous call are added to the depth values
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use meteo
    use dfparall
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    logical               , pointer :: sdufirst
    logical               , pointer :: lfsdus1
    integer               , pointer :: itdate
    real(fp)              , pointer :: tzone
    real(fp), dimension(:), pointer :: sdu_t0
    real(fp), dimension(:), pointer :: sdu_tn
    real(fp), dimension(:), pointer :: sdu_tp
!
! Global variables 
!
    real(fp)                                      , intent(in)  :: timhr
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)              :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)              :: s1     !  Description and declaration in esm_alloc_real.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: kfs    !  Description and declaration in esm_alloc_int.f90
!
! Local variables
!
    integer    :: nm
    logical    :: success
!
!! executable statements -------------------------------------------------------
!
    sdufirst  =>   gdp%gdsdu%sdufirst
    sdu_t0    =>   gdp%gdsdu%sdu_t0
    sdu_tp    =>   gdp%gdsdu%sdu_tp
    sdu_tn    =>   gdp%gdsdu%sdu_tn
    itdate    =>   gdp%gdexttim%itdate
    tzone     =>   gdp%gdexttim%tzone
    lfsdus1   =>   gdp%gdprocs%lfsdus1
    !
    if (.not. sdufirst) then 
        ! set previous level (tp) before setting new value (tn)
        do nm = gdp%d%nmlb, gdp%d%nmub 
           sdu_tp(nm) = sdu_tn(nm)
        enddo   
    endif    

    success = meteoupdate(gdp%runid, itdate, tzone, timhr*60.0_fp)
    success = getmeteoval(gdp%runid, 'sdu', timhr*60.0_fp, gdp%gdparall%mfg, gdp%gdparall%nfg, &
                           & gdp%d%nlb, gdp%d%nub, gdp%d%mlb, gdp%d%mub, sdu_tn , 0)
    call checkmeteoresult(success, gdp)
    if (sdufirst) then
        do nm = gdp%d%nmlb, gdp%d%nmub 
            sdu_tp(nm) = sdu_tn(nm)
            sdu_t0(nm) = sdu_tn(nm) 
        enddo     
        sdufirst = .false.
    endif    
    do nm = gdp%d%nmlb, gdp%d%nmub 
       !
       !  Update dps point 
       !
       if (kcs(nm) /= 0) then 
           dps(nm) = dps(nm) - sdu_tn(nm) + sdu_tp(nm)
           if (kfs(nm) == 0 .or. lfsdus1) then 
               s1(nm)  = s1(nm)  + sdu_tn(nm) - sdu_tp(nm)                  
           endif 
       endif     
    enddo
    !
    !
end subroutine incsdu
