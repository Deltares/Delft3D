subroutine get_dep (dps    ,mmax     ,nmax   ,filnam)

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
! When running online with Delft3D-FLOW, Delft3D-FLOW writes DPS to group BOTTIM,
! element DP!!!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Local parameters
!
    integer, parameter :: nelmx = 2
    integer, parameter :: nelmt = 1
!
! Global variables
!
    character(73)                            :: filnam  ! filename com-file
    integer                                  :: mmax    ! number of columns
    integer                    , intent(in)  :: nmax    ! number of rows
    real, dimension(mmax, nmax), intent(out) :: dps     ! depth in water level points
    real, dimension(:,:),allocatable         :: rbuff   ! read buffer
!
! Local variables
!
    integer                         :: celidt
    integer                         :: error
    integer                         :: ielem
    integer                         :: ierr
    integer                         :: m
    integer                         :: n
    integer, dimension(1)           :: ival
    integer, dimension(6, nelmt)    :: elmdmt
    integer, dimension(6, nelmx)    :: elmdms
    integer, dimension(nelmt)       :: nbytst
    integer, dimension(nelmx)       :: nbytsg
    logical                         :: wrswch
    character(10), dimension(nelmt) :: elmuns
    character(10), dimension(nelmx) :: elmunt
    character(16)                   :: grpnam
    character(16)                   :: grpnmt
    character(16), dimension(nelmt) :: elmnmt
    character(16), dimension(nelmt) :: elmqtt
    character(16), dimension(nelmt) :: elmtpt
    character(16), dimension(nelmx) :: elmnms
    character(16), dimension(nelmx) :: elmqty
    character(16), dimension(nelmx) :: elmtps
    character(64), dimension(nelmt) :: elmdet
    character(64), dimension(nelmx) :: elmdes
    !
    data grpnam/'BOTTIM'/, elmnms/'TIMBOT', 'DP'/, elmtps/'INTEGER', 'REAL'/,   &
       & nbytsg/nelmx*4/
    data grpnmt/'BOTNT'/, elmnmt/'NTBOT'/, elmtpt/'INTEGER'/, nbytst/nelmt*4/
!
!! executable statements -------------------------------------------------------
!
    allocate(rbuff(nmax,mmax))
    wrswch = .false.
    ielem = 1
    celidt = 1
    call filldm(elmdmt    ,1         ,1         ,1         ,0         , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,1         ,1         ,1         ,0         , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,2         ,2         ,nmax      ,mmax      , &
              & 0         ,0         ,0         )
    call putgti(filnam    ,grpnmt    ,nelmt     ,elmnmt    ,elmdmt    , &
              & elmqtt    ,elmuns    ,elmdet    ,elmtpt    ,nbytst    , &
              & elmnmt(ielem)        ,celidt    ,wrswch    ,error     ,ival      )
    if (error/=0) goto 200
    celidt = ival(1)
    ielem = 1
    call putgti(filnam    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,ival      )
    if (error/=0) goto 200
    !
    ! using the first bottom found on the com-file
    ! printing the number of flowsteps since reference date may be confusing
    ! write (*, '(4x,a,i0,a)') 'Bottom on time ', ival(1), ' found'
    !
    ielem = 2
    call putgtr(filnam    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
    do n = 1, nmax
       do m = 1, mmax
          dps(m, n) = rbuff(n, m)
       enddo
    enddo
  200 continue
    deallocate(rbuff, stat=ierr)
end subroutine get_dep
