subroutine get_wind(wavetime, windu, windv, mmax, nmax, filnam)
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
    use wave_data
    !
    implicit none
!
! Local parameters
!
    integer, parameter :: nelmt = 1
    integer, parameter :: nelm  = 3
!
! Global variables
!
    integer                       , intent(in)  :: mmax
    integer                       , intent(in)  :: nmax
    real,    dimension(mmax, nmax), intent(out) :: windu
    real,    dimension(mmax, nmax), intent(out) :: windv
    type(wave_time_type)                        :: wavetime
!
! Local variables
!
    integer                              :: celidt
    integer                              :: timcur
    integer                              :: error
    integer                              :: ielem
    integer                              :: ierr
    integer                              :: ifind
    integer                              :: m
    integer                              :: n
    integer                              :: ntcur
    integer, dimension(1)                :: ival
    integer, dimension(6, nelmt)         :: elmdmst
    integer, dimension(6, nelm)          :: elmdms
    integer, dimension(nelmt)            :: nbytst
    integer, dimension(nelm)             :: nbyts
    real   , dimension(:,:), allocatable :: rbuff
    logical                              :: wrswch
    character(10), dimension(nelmt)      :: elmuntt
    character(10), dimension(nelm)       :: elmunt
    character(16)                        :: grpnamt
    character(16)                        :: grpnam
    character(16), dimension(nelmt)      :: elmnmst
    character(16), dimension(nelm)       :: elmnms
    character(16), dimension(nelmt)      :: elmqtyt
    character(16), dimension(nelm)       :: elmqty
    character(16), dimension(nelmt)      :: elmtpst
    character(16), dimension(nelm)       :: elmtps
    character(37)                        :: filnam
    character(64), dimension(nelmt)      :: elmdest
    character(64), dimension(nelm)       :: elmdes
    !
    data grpnamt/'CURNT'/, elmnmst/'NTCUR'/, elmtpst/'INTEGER'/, nbytst/4/
    !
    data grpnam/'WIND'/
    data elmnms/'TIMCUR', 'WINDU', 'WINDV'/
    data elmtps/'INTEGER', 2*'REAL'/
    data nbyts/nelm*4/
!
!! executable statements -------------------------------------------------------
!
    call filldm(elmdmst, 1, 1, 1   , 0   , 0, 0, 0)
    call filldm(elmdms , 1, 1, 1   , 0   , 0, 0, 0)
    call filldm(elmdms , 2, 2, nmax, mmax, 0, 0, 0)
    call filldm(elmdms , 3, 2, nmax, mmax, 0, 0, 0)
    !
    ! Allocate array
    !
    allocate (rbuff (nmax,mmax))    ! Note com-file dimensions are nmax,mmax
    !
    ! Set celidt:
    ! - get ntcur
    ! - while wavetime%timtscale /= timcur:
    !      celidt = celidt+1
    !      get timcur from cell=celidt
    !   endwhile
    !
    !
    ! ntcur
    !
    ielem  = 1
    wrswch = .false.
    celidt = 1
    call putgti(filnam        , grpnamt , nelmt  , elmnmst, elmdmst, &
              & elmqtyt       , elmuntt , elmdest, elmtpst, nbytst , &
              & elmnmst(ielem), celidt  , wrswch , error  , ival  )
    if (error /= 0) goto 1000
    ntcur  = ival(1)
    !
    ! set celidt
    !
    ielem  = 1
    wrswch = .false.
    ifind  = 0
    celidt = 0
    ! =>
  100 continue
        celidt = celidt + 1
        if (celidt>ntcur .and. ifind==0) then
           write (*,'(4x,a,f15.3)') 'Specified time not found on com-file, group WIND ', wavetime%timmin
           error = -100
           goto 1000
        endif
        !
        ! timcur
        !
        call putgti(filnam       , grpnam, nelm  , elmnms, elmdms, &
                  & elmqty       , elmunt, elmdes, elmtps, nbyts , &
                  & elmnms(ielem), celidt, wrswch, error , ival  )
        if (error /= 0) goto 1000
        timcur = ival(1)
        if (wavetime%timtscale/=timcur .and. ntcur>1) goto 100
    ! <=
    !
    ! windu
    !
    wrswch = .false.
    ielem  = 2
    call putgtr(filnam       , grpnam, nelm  , elmnms, elmdms, &
              & elmqty       , elmunt, elmdes, elmtps, nbyts , &
              & elmnms(ielem), celidt, wrswch, error , rbuff )
    do m = 1, mmax
       do n = 1, nmax 
          windu(m, n) = rbuff(n, m)
       enddo
    enddo
    !
    ! windv
    !
    wrswch = .false.
    ielem  = 3
    call putgtr(filnam       , grpnam, nelm  , elmnms, elmdms, &
              & elmqty       , elmunt, elmdes, elmtps, nbyts , &
              & elmnms(ielem), celidt, wrswch, error , rbuff )
    do m = 1, mmax
       do n = 1, nmax 
          windv(m, n) = rbuff(n, m)
       enddo
    enddo
 1000 continue
    deallocate (rbuff, stat=ierr)
    if (error /= 0) then
       write(*,'(2a)') '*** ERROR: Unable to read wind velocities from file ', trim(filnam)
       call wavestop(1, '*** ERROR: Unable to read wind velocities from file '//trim(filnam))
    endif
end subroutine get_wind
