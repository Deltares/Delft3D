subroutine dimsedconst(lundia    ,error     ,sedim     ,const     , &
                     & lsed      ,lsedtot   ,lconst    ,gdp       )
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
! - Reads sediment dimension from an attribute file
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use string_module
    use globaldata
    use message_module
    use m_rdsed
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    character(20)    , dimension(:)     , pointer :: namsed
    integer          , dimension(:)     , pointer :: sedtyp
    type (sedpar_type)                  , pointer :: gdsedpar
!
! Global variables
!
    integer , intent(out) :: lconst
    integer , intent(out) :: lsed    ! Description and declaration in esm_alloc_int.f90
    integer , intent(out) :: lsedtot ! Description and declaration in esm_alloc_int.f90
    integer               :: lundia  ! Description and declaration in inout.igs
    logical , intent(in)  :: const    
    logical , intent(out) :: error
    logical , intent(out) :: sedim   ! Description and declaration in procs.igs
!
! Local variables
!
    integer                                                :: i
    integer                                                :: istat
    integer                                                :: j
    integer                                                :: tmptyp        ! Temporary storage for type of sediment fraction
    logical                                                :: found
    character(6)                                           :: keyword
    character(20)                                          :: versionstring
    character(20)                                          :: namc          ! Name of a constituent as read from md-file
    character(20)                                          :: nami          ! Default name of sediment fraction i
    character(20)                                          :: tmpnam        ! Temporary storage for name of sediment fraction
    character(20)   , dimension(:) , allocatable           :: namconst      ! Names of the constituents as read in md-file
    character(20)                                          :: sedtyptmp     ! Sediment type in sed-file
    character(80)                                          :: parname
    character(256)                                         :: filsed
    character(300)                                         :: message
    type(tree_data)                            , pointer   :: sed_ptr
    type(tree_data)                            , pointer   :: asedblock_ptr
!
!! executable statements -------------------------------------------------------
!
    lconst        = 0
    lsed          = 0
    istat         = 0
    sedim         = .false.
    error         = .false.
    !
    ! scan Namc## lines for the number of constituents lconst
    !
    keyword= 'Namc  '
    if (const) then
       do i = 1, 99
          if (i < 10) then
             write (keyword(5:5), '(i1)') i
          else
             write (keyword(5:6), '(i2)') i
          endif
          namc = ' '
          call prop_get_string(gdp%mdfile_ptr, '*', keyword, namc)
          if (namc == ' ') then
             exit
          else
             lconst = i
          endif
       enddo
    endif
    !
    allocate(namconst(lconst))
    namconst = ' '
    !
    ! populate namconst array
    !
    keyword= 'Namc  '
    do i = 1, lconst
       if (i<10) then
          write (keyword(5:5), '(i1)') i
       else
          write (keyword(5:6), '(i2)') i
       endif
       call prop_get_string(gdp%mdfile_ptr, '*', keyword, namconst(i))
       call small(namconst(i) ,999 )
    enddo
    !
    ! locate 'Filsed' record; file containing sediment parameters
    !
    filsed = ' '
    call prop_get_string(gdp%mdfile_ptr, '*', 'Filsed', filsed)
    if (filsed /= ' ') then
       sedim = .true.
    else
       !
       ! no sediment, so nothing to do here
       !
       return
    endif
    !
    ! Create Sediment branch in input tree
    !
    call tree_create_node( gdp%input_tree, 'Sediment Input', sed_ptr )
    call tree_put_data( sed_ptr, transfer(trim(filsed),node_value), 'STRING' )
    !
    ! Read sediment file, count number of suspended fractions and total number of fractions,
    ! allocate and fill sediment name and type arrays.
    !
    call count_sed(lundia    ,error     ,lsed      ,lsedtot   , &
                  & filsed    ,gdp%gdsedpar         ,sed_ptr   )
    if (error) return
    !
    namsed => gdp%gdsedpar%namsed
    sedtyp => gdp%gdsedpar%sedtyp
    !
    ! verify that the first lsed constituent names match the lsed suspended sediment fractions
    !
    if (lsed>lconst) then
       message = 'Number of suspended sediment fractions should not be greater than the number of constituents.'
       call write_error(message, unit=lundia)
       error = .true.
       return
    endif
    !
    do i = 1,lsed
       namc = namconst(i)
       write(nami,'(a,1i0)') 'sed.fraction ',i
       found = .false.
       j = i-1
       do while (.not.found .and. j<lsed)
          j = j+1
          if (strcmpi(namc,namsed(j))) then
             found = .true.
          endif
       end do
       if (found) then
          !
          ! make sure that the i-th sediment name matches the i-th constituent
          ! array indices need to match later on
          !
          if (j/=i) then
             !
             ! reorder sediment names (and types!) in morphology module
             ! constituent order cannot be changed because of initial and restart conditions
             !
             tmpnam = namsed(i)
             tmptyp = sedtyp(i)
             !
             namsed(i) = namsed(j)
             sedtyp(i) = sedtyp(j)
             !
             namsed(j) = tmpnam
             sedtyp(j) = tmptyp
          endif
       elseif (strcmpi(namsed(i),nami)) then
           !
           ! automatic name in case version 0 or 1 .sed files, adopt the name of the constituent as specified in the .mdf file
           !
           namsed(i) = namc
       else
          write(message,'(A,I0,A)') 'Names of first ',lsed,' constituent(s) should match the names of the suspended sediment fractions'
          call write_error(message, unit=lundia)
          write(message,'(A,I0,3A)') 'Name of constituent ',i,' (',trim(namc),') does not match name of any suspended sediment fraction'
          call write_error(message, unit=lundia)
          error = .true.
          return
       endif
    enddo
    !
end subroutine dimsedconst
