subroutine chksit(lundia    ,error     ,nostat    ,ntruv     ,ntru      , &
                & nmax      ,mmax      ,nmaxus    ,kcu       ,kcv       , &
                & kcs       ,gdp       )
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
!    Function: - Checks the location of the following points (if
!                specified) :
!                Stations and Cross sections
!                These points should/must lie on the active pnts.
!              - Checks whether a Cross section contains only one
!                point (not allowed)
!              - Sorts the following order of the cross sections
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use dfparall
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer       , dimension(:, :) , pointer :: mnit
    integer       , dimension(:, :) , pointer :: mnstat
    character(20) , dimension(:)    , pointer :: namst
    character(20) , dimension(:)    , pointer :: namtra
!
! Global variables
!
    integer                                                                           :: lundia    ! Description and declaration in inout.igs
    integer                                                             , intent(in)  :: mmax      ! Description and declaration in esm_alloc_int.f90
    integer                                                                           :: nmax      ! Description and declaration in esm_alloc_int.f90
    integer                                                             , intent(in)  :: nmaxus    ! Description and declaration in esm_alloc_int.f90
    integer                                                             , intent(in)  :: nostat    ! Description and declaration in dimens.igs
    integer                                                                           :: ntru      ! Description and declaration in dimens.igs
    integer                                                             , intent(in)  :: ntruv     ! Description and declaration in dimens.igs
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) , intent(in)  :: kcs       ! Description and declaration in esm_alloc_int.f90
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) , intent(in)  :: kcu       ! Description and declaration in esm_alloc_int.f90
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) , intent(in)  :: kcv       ! Description and declaration in esm_alloc_int.f90
    logical                                                             , intent(out) :: error     ! Flag=TRUE if an error is encountered
!
!
! Local variables
!
    integer                        :: i            ! Help variable
    integer                        :: iline        ! Help variable for remembering original input line of cross section in .crs file
    integer                        :: luntst       ! Help variable
    integer                        :: m            ! Current M-index of the active point in the current computational ROW 
    integer                        :: m1           ! Help variable
    integer                        :: m2           ! Help variable
    integer                        :: mmm          ! Help variable
    integer                        :: mmx          ! Help variable
    integer                        :: n            ! Current N-index of the active point in the current computational COLUMN 
    integer                        :: n1           ! Help variable
    integer                        :: n2           ! Help variable 
    integer                        :: nbuit        ! Help variable summing up the cross section points which lie on inactive velocity points
    integer                        :: newlun
    integer                        :: nnm          ! Help variable
    integer                        :: nnx          ! Help variable
    integer                        :: nr           ! Sequence number of open boundary points 
    character(20)                  :: hlpnam       ! Help variable 
    character(45)                  :: errmsg       ! Character variable containing the error message to be written to file. 
                                                   ! The message depends on the error. 
!
!
!! executable statements -------------------------------------------------------
!
    mnit               => gdp%gdstations%mnit
    mnstat             => gdp%gdstations%mnstat
    namst              => gdp%gdstations%namst
    namtra             => gdp%gdstations%namtra
    !
    ! initialize global parameters
    !
    ntru = 0
    !
    ! initialize local parameters
    !
    hlpnam = ' '
    errmsg = ' '
    !
    ! check position stations (nostat > 0)
    ! when outside boundaries, then warning
    !
    if (nostat>0) then
       do nr = 1, nostat
          m = mnstat(1, nr)
          n = mnstat(2, nr)
          if (m<1 .or. m>mmax .or. n<1 .or. n>nmaxus) then
             errmsg = 'Station definition ' // namst(nr)
             call prterr(lundia    ,'U007'    ,errmsg(:39)          )
             !
             write (lundia, '(20x,'' (m,n) = '',2i4)') m, n
             error = .true.
          elseif (kcs(n, m)==0) then
             call prterr(lundia    ,'V050'    ,namst(nr) )
             !
             write (lundia, '(20x,'' (m,n) = '',2i4)') m, n
          else
          endif
       enddo
    endif
    !
    ! check position cross-sections
    ! when outside boundaries, then warning
    !
    if (ntruv>0) then
       do nr = 1, ntruv
          m1 = mnit(1, nr)
          n1 = mnit(2, nr)
          m2 = mnit(3, nr)
          n2 = mnit(4, nr)
          nbuit = 0
          if (m1==m2) then
             nnm = min(n1, n2)
             nnx = max(n1, n2)
             !
             ! check cross sections only a single point, then error
             !
             if (nnm==nnx .and. .not. parll) then
                !
                ! parallel: this is taken care of
                ! not parallel: still going wrong?
                !
                call prterr(lundia    ,'V055'    ,namtra(nr))
                error = .true.
                cycle
             endif
             if (m1<1 .or. m1>mmax .or. nnm<1 .or. nnx>nmaxus) then
                errmsg = 'Cross Section definition ' // namtra(nr)
                call prterr(lundia    ,'U007'    ,errmsg    )
                !
                write (lundia, '(20x,'' (m,n1-n2) = '',3i4)') m1, n1, n2
                error = .true.
                cycle
             endif
             !
             do n = nnm, nnx
                if (kcu(n, m1)==0) then
                   if (nbuit==0) then
                      call prterr(lundia    ,'V054'    ,namtra(nr))
                   !
                   endif
                   write (lundia, '(20x,'' (m1,n) = '',2i4)') m1, n
                   nbuit = nbuit + 1
                endif
             enddo
          elseif (n1==n2) then
             mmm = min(m1, m2)
             mmx = max(m1, m2)
             if (mmm<1 .or. mmx>mmax .or. n1<1 .or. n1>nmaxus) then
                errmsg = 'Cross Section definition ' // namtra(nr)
                call prterr(lundia    ,'U007'    ,errmsg    )
                !
                write (lundia, '(20x,'' (m1-m2,n) = '',3i4)') m1, m2, n1
                error = .true.
                cycle
             endif
             do m = mmm, mmx
                if (kcv(n1, m)==0) then
                   if (nbuit==0) then
                      call prterr(lundia    ,'V054'    ,namtra(nr))
                   !
                   endif
                   write (lundia, '(20x,'' (m,n1) = '',2i4)') m, n1
                   nbuit = nbuit + 1
                endif
             enddo
          else
             call prterr(lundia    ,'V053'    ,namtra(nr))
             !
             error = .true.
          endif
       enddo
       !
       ! reorganize mnit array and namtra array: first all U,
       ! later all V
       !
       ntru = 1
       i = ntru
       do while (i <= ntruv)
          !
          ! test first for U points (m1=m2)
          !
          if (mnit(1,i) == mnit(3,i)) then
             i = ntru
             ntru = ntru + 1
          endif
          i = i + 1
       enddo
       ntru = ntru - 1
    endif
end subroutine chksit
