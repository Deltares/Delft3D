subroutine rdscour(lundia    ,error     ,nmmax     ,gdp       )
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
! Manage User Input from file = scour.inp
! Requests: LOAD  open and read file, save data
! TAKE  store data
! GIVE  return data
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                , pointer :: nof
    integer, dimension(:)  , pointer :: nmapp
    integer, dimension(:)  , pointer :: nmref
    logical                , pointer :: scour
    real(fp), dimension(:) , pointer :: factor
    real(fp)               , pointer :: slope
!
! Global variables
!
    integer                                  , intent(in)  :: nmmax
    integer                                  , intent(in)  :: lundia
    logical                                  , intent(out) :: error
!
! Local variables
!
    integer                              :: dummy
    integer                              :: i
    integer                              :: inp
    integer                              :: iost    ! IO-errorcodes
    integer                              :: m
    integer                              :: n
    integer, dimension(:,:), allocatable :: nmappin
    integer, dimension(:,:), allocatable :: nmrefin
    integer, external                    :: newlun
    logical                              :: lex
    character(256)                       :: errmsg
    character(256)                       :: flname
!
!! executable statements -------------------------------------------------------
!
    nof        => gdp%gdscour%nof
    scour      => gdp%gdscour%scour
    slope      => gdp%gdscour%slope
    !
    error  = .false.
    !
    nof = 0
    !
    ! locate 'Scour' record for attribute file containing parameters
    ! for transport formulation
    !
    flname = ' '
    call prop_get_string(gdp%mdfile_ptr, '*', 'Scour', flname)
    !
    if (flname == ' ') then
       ! no scour
    else
       inquire (file = flname, exist = lex)
       if (lex) then
          open (newunit=inp, file = flname,status = 'old', iostat = iost)
          if (iost/=0) then
             call prterr(lundia, 'G004', trim(flname))
             error = .true.
             return
          endif
          read (inp, *, iostat = iost) i
          if (iost/=0 .or. i==0) then
             goto 9999
          else
             scour   = .true.
             errmsg  = 'Scouring flag = ON'
             call prterr(lundia    ,'G051'    ,errmsg    )
          endif
          !
          ! read slope
          !
          read (inp, *) slope
          !
          ! Dummy read to get nof records ( = points)
          !
          read (inp, *, iostat = iost) m
          do while (iost==0)
             nof = nof + 1
             read (inp, *, iostat = iost) m
          enddo
          !
          ! Now the required memory is known
          ! Allocate arrays in the gdp structure
          !
                       allocate (nmappin(2, nof), stat = iost)
          if (iost==0) allocate (nmrefin(2, nof), stat = iost)
          if (iost==0) allocate (gdp%gdscour%nmapp(nof), stat = iost)
          if (iost==0) allocate (gdp%gdscour%nmref(nof), stat = iost)
          if (iost==0) allocate (gdp%gdscour%factor(nof), stat = iost)
          if (iost==0) allocate (gdp%gdscour%tauv(nmmax), stat = iost)
          if (iost==0) allocate (gdp%gdscour%depchange(nmmax), stat = iost)
          if (iost/=0) then
             call prterr(lundia, 'U021', 'Rdscour: memory alloc error')
             error = .true.
             return
          endif
          !
          ! make sure local pointers point to the allocated memory
          !
          nmapp      => gdp%gdscour%nmapp
          nmref      => gdp%gdscour%nmref
          factor     => gdp%gdscour%factor
          gdp%gdscour%tauv = 0.0_fp
          !
          rewind (inp)
          read (inp, *) dummy
          read (inp, *) dummy
          do n = 1, nof
             read (inp, *) (nmrefin(i,n), i = 1,2), &
                         & (nmappin(i,n), i = 1,2), &
                         & factor(n)
          enddo
          close (inp)
          !
          ! convert nmappin(n,nof), nmappin(m,nof) to nmapp(nof) nm point
          ! convert nmrefin(n,nof), nmrefin(m,nof) to nmref(nof) nm point
          !
          do n = 1, nof
             call n_and_m_to_nm(nmappin(2,n), nmappin(1,n), nmapp(n), gdp)
             call n_and_m_to_nm(nmrefin(2,n), nmrefin(1,n), nmref(n), gdp)
          enddo
          deallocate (nmappin)
          deallocate (nmrefin)
       else
          errmsg = 'Scour file '//trim(flname)//' does not exist'
          call prterr(lundia    ,'U021'    ,errmsg    )
          goto 9999
       endif
    endif
 9999 continue
end subroutine rdscour
