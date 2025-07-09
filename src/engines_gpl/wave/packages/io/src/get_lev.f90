subroutine get_lev(wavetime, s1, mmax, nmax, filnam)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
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
    integer, parameter :: nelmx = 7
    integer, parameter :: nelmt = 1
!
! Global variables
!
    integer                      , intent(in)  :: mmax
    integer                      , intent(in)  :: nmax
    real   , dimension(mmax,nmax), intent(out) :: s1
    type(wave_time_type)                       :: wavetime
!
! Local variables
!
    integer                            :: celidt
    integer                            :: timcur
    integer                            :: error
    integer                            :: ielem
    integer                            :: ierr
    integer                            :: ifind
    integer                            :: m
    integer                            :: n
    integer                            :: ntcur
    integer, dimension(1)              :: ival
    integer, dimension(6, nelmt)       :: elmdmt
    integer, dimension(6, nelmx)       :: elmdms
    integer, dimension(nelmt)          :: nbytst
    integer, dimension(nelmx)          :: nbytsg
    real, dimension(:,:), allocatable  :: rbuff
    logical                            :: wrswch
    character(10), dimension(nelmt)    :: elmutt
    character(10), dimension(nelmx)    :: elmunt
    character(16)                      :: grpnam
    character(16)                      :: grpnmt
    character(16), dimension(nelmt)    :: elmnmt
    character(16), dimension(nelmt)    :: elmqtt
    character(16), dimension(nelmt)    :: elmtpt
    character(16), dimension(nelmx)    :: elmnms
    character(16), dimension(nelmx)    :: elmqty
    character(16), dimension(nelmx)    :: elmtps
    character(37)                      :: filnam
    character(64), dimension(nelmt)    :: elmdet
    character(64), dimension(nelmx)    :: elmdes
    !
    data grpnam/'CURTIM'/, elmnms/'TIMCUR', 'QU', 'QV', 'S1', 'U1', 'V1',       &
       & 'RSP'/, elmtps/1*'INTEGER', 6*'REAL'/, nbytsg/nelmx*4/
    data grpnmt/'CURNT'/, elmnmt/'NTCUR'/, elmtpt/1*'INTEGER'/nbytst/nelmt*4/
!
!! executable statements -------------------------------------------------------
!
    allocate(rbuff(nmax,mmax))

    call filldm(elmdmt    ,1         ,1         ,1         ,0         , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,1         ,1         ,1         ,0         , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,2         ,2         ,nmax    ,mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,3         ,2         ,nmax    ,mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,4         ,2         ,nmax    ,mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,5         ,2         ,nmax    ,mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,6         ,2         ,nmax    ,mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,7         ,2         ,nmax    ,mmax      , &
              & 0         ,0         ,0         )
    !
    !
    ielem  = 1
    wrswch = .false.
    celidt = 1
    call putgti(filnam    ,grpnmt    ,nelmt     ,elmnmt    ,elmdmt    , &
              & elmqtt    ,elmutt    ,elmdet    ,elmtpt    ,nbytst    , &
              & elmnmt(ielem)        ,celidt    ,wrswch    ,error     ,ival      )
    if (error/=0) goto 300
    ntcur = ival(1)
    ielem = 1
    ifind = 0
    celidt = 0
  100 continue
    celidt = celidt + 1
    if (celidt>ntcur .and. ifind==0) then
       write (*,'(4x,a,f15.3)') 'Specified time not found on com-file, group CURTIM ', wavetime%timmin
       error = -100
       goto 300
    endif
    call putgti(filnam    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,ival      )
    if (error/=0) goto 300
    timcur = ival(1)
    if (wavetime%timtscale/=timcur .and. ntcur>1) goto 100
    wrswch = .false.
    write (*,'(4x,a,f15.3,a)') 'Time ', wavetime%timmin, ' found on com-file, group CURTIM'
    
    ielem = 4
    call putgtr(filnam    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
    do n = 1, nmax
       do m = 1, mmax
          s1(m, n) = rbuff(n, m)
       enddo
    enddo
  300 continue
  deallocate(rbuff, stat=ierr)
  if (error /= 0) then
     write(*,'(2a)') '*** ERROR: Unable to read water level from file ', trim(filnam)
     call wavestop(1, '*** ERROR: Unable to read water level from file '//trim(filnam))
  endif
end subroutine get_lev
