subroutine initwaqpar(gdp)
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
! NONE
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
    integer           , pointer :: itwqff
    integer           , pointer :: itwqfi
    integer           , pointer :: itwqfl
    integer           , pointer :: lunvol
    integer           , pointer :: lunare
    integer           , pointer :: lunflo
    integer           , pointer :: lunsal
    integer           , pointer :: luntem
    integer           , pointer :: lunvdf
    integer           , pointer :: luntau
    integer           , pointer :: lunsrctmp
    integer           , pointer :: lunwlk
    integer           , pointer :: lunsrc
    integer           , pointer :: lunkmk
    logical           , pointer :: first_cf
    logical           , pointer :: firsttime
    logical           , pointer :: firstwaq
    logical           , pointer :: waqfil
    logical           , pointer :: waqol
    character(256)    , pointer :: flaggr
!
!! executable statements -------------------------------------------------------
!
    itwqff     => gdp%gdwaqpar%itwqff
    itwqfi     => gdp%gdwaqpar%itwqfi
    itwqfl     => gdp%gdwaqpar%itwqfl
    lunvol     => gdp%gdwaqpar%lunvol
    lunare     => gdp%gdwaqpar%lunare
    lunflo     => gdp%gdwaqpar%lunflo
    lunsal     => gdp%gdwaqpar%lunsal
    luntem     => gdp%gdwaqpar%luntem
    lunvdf     => gdp%gdwaqpar%lunvdf
    luntau     => gdp%gdwaqpar%luntau
    lunsrctmp  => gdp%gdwaqpar%lunsrctmp
    lunwlk     => gdp%gdwaqpar%lunwlk
    lunsrc     => gdp%gdwaqpar%lunsrc
    lunkmk     => gdp%gdwaqpar%lunkmk
    first_cf   => gdp%gdwaqpar%first_cf
    firsttime  => gdp%gdwaqpar%firsttime
    firstwaq   => gdp%gdwaqpar%firstwaq
    waqfil     => gdp%gdwaqpar%waqfil
    waqol      => gdp%gdwaqpar%waqol
    flaggr     => gdp%gdwaqpar%flaggr
    !
    first_cf  = .true.
    firsttime = .true.
    firstwaq  = .true.
    waqfil    = .false.
    waqol     = .false.
    !
    itwqff    = 0
    itwqfi    = 0
    itwqfl    = 0
    lunvol    = 0
    lunare    = 0
    lunflo    = 0
    lunsal    = 0
    luntem    = 0
    lunvdf    = 0
    luntau    = 0
    lunsrctmp = 0
    lunwlk    = 0
    lunsrc    = 0
    lunkmk    = 0
    !
    nullify(gdp%gdwaqpar%quwaq)
    nullify(gdp%gdwaqpar%qvwaq)
    nullify(gdp%gdwaqpar%qwwaq)
    nullify(gdp%gdwaqpar%discumwaq)
    nullify(gdp%gdwaqpar%ifrmto)
    nullify(gdp%gdwaqpar%isaggr)
    nullify(gdp%gdwaqpar%iqaggr)
    nullify(gdp%gdwaqpar%ilaggr)
    nullify(gdp%gdwaqpar%ifsmax)
    nullify(gdp%gdwaqpar%vol)
    nullify(gdp%gdwaqpar%sag)
    nullify(gdp%gdwaqpar%vol2)
    nullify(gdp%gdwaqpar%sag2)
    nullify(gdp%gdwaqpar%qag)
    nullify(gdp%gdwaqpar%horsurf)
    nullify(gdp%gdwaqpar%kmk)
    nullify(gdp%gdwaqpar%ksrwaq)
    nullify(gdp%gdwaqpar%loads)
    nullify(gdp%gdwaqpar%iwlk)
    nullify(gdp%gdwaqpar%lunsed)
    nullify(gdp%gdwaqpar%lunsedflx)
    nullify(gdp%gdwaqpar%cumsedflx)
    nullify(gdp%gdwaqpar%cumresflx)
    !
    flaggr = ' '
end subroutine initwaqpar
