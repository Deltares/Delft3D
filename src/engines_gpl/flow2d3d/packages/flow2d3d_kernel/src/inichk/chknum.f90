subroutine chknum(lundia    ,error     ,roumet    ,rouflo    ,gdp)
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
!    Function: - Checks the numerical parameters
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use time_module
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                , pointer :: itdate
    real(fp)               , pointer :: timjan
    real(fp)               , pointer :: dryflc
    integer                , pointer :: iter1
    character(6)           , pointer :: momsol
!
! Global variables
!
    integer                   :: lundia  !  Description and declaration in inout.igs
    logical     , intent(out) :: error   !!  Flag=TRUE if an error is encountered
    character(1), intent(in)  :: roumet  !!  Bed stress formulation specified:
                                         !!   C : Chezy    W : White Colebrook
                                         !!   M : Manning  Z : roughness par.
    character(4), intent(out) :: rouflo  !  Description and declaration in esm_alloc_char.f90
    character(4)              :: message
!
! Local variables
!
    integer :: defaultiter
    integer :: itjan1
    integer :: julday
    integer :: juljan
!
!! executable statements -------------------------------------------------------
!
    itdate     => gdp%gdexttim%itdate
    timjan     => gdp%gdheat%timjan
    dryflc     => gdp%gdnumeco%dryflc
    iter1      => gdp%gdnumeco%iter1
    momsol     => gdp%gdnumeco%momsol
    !
    rouflo = '    '
    !
    ! Check bottom friction coefficients
    ! Other values than M, C, W and Z are not allowed
    !
    if (roumet=='M') then
       rouflo = 'MANN'
    elseif (roumet=='C') then
       rouflo = 'CHEZ'
    elseif (roumet=='W') then
       rouflo = 'WHIT'
    elseif (roumet=='Z') then
       rouflo = 'Z   '
    else
       error = .true.
       call prterr(lundia    ,'V045'    ,' '       )
    endif
    !
    ! Define TIMJAN
    !
    itjan1 = itdate - mod(itdate, 10000) + 0101
    julday = ymd2jul(itdate)
    juljan = ymd2jul(itjan1)
    timjan = (julday - juljan)*24.
    !
    ! check ITER1, only warning will be issued
    !
    if (momsol == 'flood ') then
       defaultiter = 1
    else
       defaultiter = 2
    endif
    if (iter1 < defaultiter) then
       iter1 = defaultiter
       write(message,'(i0)') iter1
       call prterr(lundia    ,'V071'    ,trim(message)    )
    endif
    if (iter1 > defaultiter) then
       write(message,'(i0)') iter1
       call prterr(lundia    ,'U005'    ,trim(message)  )
    endif
    !
    ! Check DRYFLC, if DRYFLC < 0.02 m an inconsistency for drying and
    ! flooding versus calibration of 1/H**2 in CUCNP(2) can occure
    ! Therefore will be set on 0.02
    !
    if (dryflc <= 0.0) then
       call prterr(lundia    ,'V074'    ,' '       )
       dryflc = 0.02
    endif
    if (dryflc > 1.0) then
       call prterr(lundia    ,'V073'    ,' '       )
    endif
end subroutine chknum
