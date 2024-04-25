subroutine cnvbub(kmax    ,nsrcd     ,nsrc      ,nbub      ,nxbub    , &
                & icx     ,icy       ,namsrc    ,mnksrc    , &
                & disch   ,gdp       )
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
! This routine converts the discharges of the sources (including bubble screens)
! to the new positions in the DISCH array.
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
    !
    real(fp), dimension(:)   , pointer :: cpdis
    logical , dimension(:)   , pointer :: flbub
!
! Global variables
!
    integer                          , intent(in)  :: icx    ! Increment in the X-dir., if ICX= NMAX
                                                             ! then computation proceeds in the X-
                                                             ! dir. If icx=1 then computation pro-
                                                             ! ceeds in the Y-dir. 
    integer                          , intent(in)  :: icy    ! Increment in the Y-dir. (see ICX)
    integer                          , intent(in)  :: kmax
    integer                          , intent(in)  :: nbub   ! Description and declaration in dimens.igs
    integer                          , intent(in)  :: nsrc   ! Description and declaration in dimens.igs
    integer                          , intent(in)  :: nsrcd  ! Description and declaration in dimens.igs
    integer                          , intent(in)  :: nxbub  ! Description and declaration in dimens.igs
    integer      , dimension(7, nsrc), intent(in)  :: mnksrc !  Description and declaration in esm_alloc_int.f90
    real(fp)     , dimension(nsrc)                 :: disch  ! Description and declaration in esm_alloc_real.f90
    character(20), dimension(nsrc)   , intent(in)  :: namsrc !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer       :: ibub
    integer       :: ii
    integer       :: icount
    integer       :: idis
    integer       :: k
    integer       :: nstbub
    character(20) :: chulp  ! Help charatcter string
!
!! executable statements -------------------------------------------------------
!
    cpdis  => gdp%gdbubble%cpdis
    flbub  => gdp%gdbubble%flbub
    !
    ! nstbub is the first discharge that is a bubble point 
    !
    nstbub = nsrcd - nxbub + 1
    !
    ! When new bubble discharge volumes are read in incdis,
    !   the volumes are placed in disch, per bubblescreen one value!
    !   These values are copied into cpdis
    ! When NO new bubble discharge volumes are read,
    !   rubbish values are copied to cpdis!
    !   be sure that they are not used to update array disch
    !
    do ibub = 1,nsrcd
       cpdis(ibub) = disch(ibub) 
    enddo
    icount = 0
    chulp  = namsrc(nstbub)
    idis   = nstbub
    do ibub = nstbub,nsrc
       !
       ! count the number of bubble points, having the same name (belonging to the same bubblescreen)
       ! assumption: all points having the same name are in order in the discharge arrays
       !
       if (ibub == nsrc) then
          !
          ! Last bubble point:
          ! this point and the icount previous points belong to the same bubblescreen
          ! (only) if discharge volume is changed, update their disch array
          ! Set disch to zero when it is outside this domain
          !
          if (flbub(idis)) then
             do ii = ibub-icount,ibub
                if (mnksrc(6, ii) == -1 ) then
                   disch(ii) = 0.0_fp
                else
                   disch(ii) = real(kmax,fp) * cpdis(idis) / real(icount+1,fp)
                endif
             enddo
          endif
       elseif (namsrc(ibub) /= chulp) then
          !
          ! This is the first bubble point having another name
          ! (only) if discharge volume is changed, update the icount previous points, excluding this one
          ! Set disch to zero when it is outside this domain
          !
          if (flbub(idis)) then
             do ii = ibub-icount,ibub-1
                if (mnksrc(6, ii) == -1 ) then
                   disch(ii) = 0.0_fp
                else
                   disch(ii) = real(kmax,fp) * cpdis(idis) / real(icount,fp)
                endif
             enddo
          endif
          !
          ! next bubblescreen, next bubblescreen name, start counting (current one is the first)
          idis   = idis + 1
          chulp  = namsrc(ibub)
          icount = 1
       else
          !
          ! This bubble point has the same name as the previous one
          ! Continue counting until all are found
          !
          icount = icount + 1
       endif
    enddo
end subroutine cnvbub
