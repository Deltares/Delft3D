subroutine get_cur(wavetime  ,kfu       ,kfv       ,u1        ,v1         , &
                 & mmax      ,nmax      ,kmax      ,filnam    ,layer_model, &
                 & flowVelocityType     ,dps       ,s1)
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
    integer, parameter :: nelmx = 9
    integer, parameter :: nelmt = 1
    integer, parameter :: nelmk = 3
!
! Global variables
!
    integer                       , intent(in)  :: mmax
    integer                       , intent(in)  :: nmax
    integer                       , intent(in)  :: kmax
    integer, dimension(mmax, nmax), intent(out) :: kfu
    integer, dimension(mmax, nmax), intent(out) :: kfv
    real   , dimension(mmax, nmax), intent(in)  :: dps
    real   , dimension(mmax, nmax), intent(in)  :: s1
    real   , dimension(mmax, nmax), intent(out) :: u1
    real   , dimension(mmax, nmax), intent(out) :: v1
    type(wave_time_type)                        :: wavetime
    integer                                     :: flowVelocityType
    character(*)                                :: layer_model
!
! Local variables
!
    integer      , dimension(:, :), allocatable  :: ibuff
    integer                                      :: ierr
    integer                                      :: celidt
    integer                                      :: timcur
    integer                                      :: error
    integer                                      :: ielem
    integer                                      :: ifind
    integer                                      :: k
    integer                                      :: m
    integer                                      :: n
    integer                                      :: ntcur
    integer      , dimension(1)                  :: ival
    integer      , dimension(6, nelmk)           :: elmdmk
    integer      , dimension(6, nelmt)           :: elmdmt
    integer      , dimension(6, nelmx)           :: elmdms
    integer      , dimension(6, nelmx)           :: elmdmw
    integer      , dimension(6, 1)               :: elmdm_sigma
    integer      , dimension(nelmk)              :: nbytsk
    integer      , dimension(nelmt)              :: nbytst
    integer      , dimension(nelmx)              :: nbytsg
    integer      , dimension(1)                  :: nbyts_sigma
    real         , dimension(:,:,:), allocatable :: rbuff
    real         , dimension(:,:)  , allocatable :: rlabda
    real         , dimension(:)    , allocatable :: thick
    real         , dimension(:,:,:), allocatable :: dzu1
    real         , dimension(:,:,:), allocatable :: dzv1
    logical                                      :: wrswch
    character(10), dimension(nelmk)              :: elmunk
    character(10), dimension(nelmt)              :: elmutt
    character(10), dimension(nelmx)              :: elmunt
    character(16)                                :: grpnam
    character(16)                                :: grpnmk
    character(16)                                :: grpnmt
    character(16)                                :: grpnm_sigma
    character(16), dimension(nelmk)              :: elmnmk
    character(16), dimension(nelmk)              :: elmqtk
    character(16), dimension(nelmk)              :: elmtpk
    character(16), dimension(nelmt)              :: elmnmt
    character(16), dimension(nelmt)              :: elmqtt
    character(16), dimension(nelmt)              :: elmtpt
    character(16), dimension(nelmx)              :: elmnms
    character(16), dimension(1)                  :: elmnm_sigma
    character(16), dimension(nelmx)              :: elmqty
    character(16), dimension(nelmx)              :: elmtps
    character(16), dimension(1)                  :: elmtp_sigma
    character(37)                                :: filnam
    character(64), dimension(nelmk)              :: elmdek
    character(64), dimension(nelmt)              :: elmdet
    character(64), dimension(nelmx)              :: elmdes
    !
    data grpnmt/'CURNT'/, elmnmt/'NTCUR'/, elmtpt/'INTEGER'/, nbytst/4/
    data grpnam/'CURTIM'/, elmnms/'TIMCUR', 'QU', 'QV', 'S1', 'U1', 'V1',       &
       & 'RSP', 'DZU1', 'DZV1'/, elmtps/'INTEGER', 8*'REAL'/, nbytsg/nelmx*4/
    data grpnmk/'KENMTIM'/, elmnmk/'TIMCUR', 'KFU ', 'KFV '/,                   &
        & elmtpk/nelmk*'INTEGER'/, nbytsk/nelmk*4/
    data grpnm_sigma/'GRID'/, elmnm_sigma/'THICK'/, elmtp_sigma/'REAL'/, nbyts_sigma/4/
!
!! executable statements -------------------------------------------------------
!
    call filldm(elmdmt    ,1         ,1         ,1         ,0         , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,1         ,1         ,1         ,0         , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,2         ,2         ,nmax      ,mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,3         ,2         ,nmax      ,mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,4         ,2         ,nmax      ,mmax      , &
              & 0         ,0         ,0         )
    if (kmax>1) then
       call filldm(elmdms    ,5         ,3         ,nmax      ,mmax      , &
                 & kmax      ,0         ,0         )
       call filldm(elmdms    ,6         ,3         ,nmax      ,mmax      , &
                 & kmax      ,0         ,0         )
    else
       call filldm(elmdms    ,5         ,2         ,nmax      ,mmax      , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,6         ,2         ,nmax      ,mmax      , &
                 & 0         ,0         ,0         )
    endif
    call filldm(elmdms    ,7         ,2         ,nmax      ,mmax      , &
              & 0         ,0         ,0         )
    if (kmax>1) then
       call filldm(elmdms    ,8         ,3         ,nmax      ,mmax      , &
                 & kmax      ,0         ,0         )
       call filldm(elmdms    ,9         ,3         ,nmax      ,mmax      , &
                 & kmax      ,0         ,0         )
    else
       call filldm(elmdms    ,8         ,2         ,nmax      ,mmax      , &
                 & 0         ,0         ,0         )
       call filldm(elmdms    ,9         ,2         ,nmax      ,mmax      , &
                 & 0         ,0         ,0         )
    endif
    call filldm(elmdmk    ,1         ,1         ,1         ,0         , &
              & 0         ,0         ,0         )
    call filldm(elmdmk    ,2         ,2         ,nmax      ,mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdmk    ,3         ,2         ,nmax      ,mmax      , &
              & 0         ,0         ,0         )

    call filldm(elmdm_sigma,1        ,1         ,kmax      ,0         , &
              & 0         ,0         ,0         )
    !
    ! Allocate arrays
    allocate (rbuff (nmax,mmax,kmax))    ! Note com-file dimensions are nmax,mmax
    allocate (ibuff (nmax,mmax))         ! Note com-file dimensions are nmax,mmax
    allocate (thick (kmax))              ! Thickness of the sigma layers
    allocate (dzu1  (mmax,nmax,kmax))    ! Note com-file dimensions are nmax,mmax
    allocate (dzv1  (mmax,nmax,kmax))    ! Note com-file dimensions are nmax,mmax
    !
    ielem  = 1
    wrswch = .false.
    celidt = 1
    call putgti(filnam    ,grpnmt    ,nelmt     ,elmnmt    ,elmdmt    , &
              & elmqtt    ,elmutt    ,elmdet    ,elmtpt    ,nbytst    , &
              & elmnmt(ielem)        ,celidt    ,wrswch    ,error     ,ival      )
    if (error/=0) goto 1000

    ielem = 1
    call putgtr(filnam    ,grpnm_sigma,nelmx    ,elmnm_sigma ,elmdm_sigma, &
              & elmqty    ,elmunt    ,elmdes    ,elmtp_sigma ,nbytsg     , &
              & elmnm_sigma(ielem)   ,celidt    ,wrswch    ,error     ,thick     )
    !
    ntcur = ival(1)
    ielem = 1
    ifind = 0
    celidt = 0
  100 continue
    celidt = celidt + 1
    if (celidt>ntcur .and. ifind==0) then
       write (*,'(4x,a,f15.3)') 'Specified time not found on com-file, group CURTIM ', wavetime%timmin
       error = -100
       goto 1000
    endif
    call putgti(filnam    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,ival      )
    if (error/=0) goto 1000
    timcur = ival(1)
    if (wavetime%timtscale/=timcur .and. ntcur>1) goto 100
    wrswch = .false.
    ielem = 2
    call putgti(filnam    ,grpnmk    ,nelmk     ,elmnmk    ,elmdmk    , &
              & elmqtk    ,elmunk    ,elmdek    ,elmtpk    ,nbytsk    , &
              & elmnmk(ielem)        ,celidt    ,wrswch    ,error     ,ibuff     )
    do m = 1, mmax
       do n = 1, nmax
          kfu(m, n) = ibuff(n, m)
       enddo
    enddo
    ielem = 3
    call putgti(filnam    ,grpnmk    ,nelmk     ,elmnmk    ,elmdmk    , &
              & elmqtk    ,elmunk    ,elmdek    ,elmtpk    ,nbytsk    , &
              & elmnmk(ielem)        ,celidt    ,wrswch    ,error     ,ibuff     )
    do m = 1, mmax
       do n = 1, nmax
          kfv(m, n) = ibuff(n, m)
       enddo
    enddo

    ielem = 8
    call putgtr(filnam    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
    do m = 1, mmax
       do n = 1, nmax
          do k = 1, kmax
             dzu1(m, n, k) = rbuff(n, m, k)
          enddo
       enddo
    enddo

    ielem = 9
    call putgtr(filnam    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
    do m = 1, mmax
       do n = 1, nmax
          do k = 1, kmax
             dzv1(m, n, k) = rbuff(n, m, k)
          enddo
       enddo
    enddo
    
    ielem = 5
    call putgtr(filnam    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
    !
    ! Compute flow velocity U1 depending on layer_model and flowVelocityType
    !
    call compvel(wavetime ,layer_model ,flowVelocityType ,kfu       ,kfv     , &
               & u1       ,mmax        ,nmax             ,kmax      ,filnam  , &
               & dps      ,s1          ,thick            ,dzu1      ,rbuff   )

    ielem = 6
    call putgtr(filnam    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
    !
    ! Compute flow velocity V1 depending on layer_model and flowVelocityType
    !
    call compvel(wavetime ,layer_model ,flowVelocityType ,kfu       ,kfv     , &
               & v1       ,mmax        ,nmax             ,kmax      ,filnam  , &
               & dps      ,s1          ,thick            ,dzv1      ,rbuff   )

 1000 continue
    deallocate (rbuff, stat=ierr)
    deallocate (ibuff, stat=ierr)
    deallocate (thick, stat=ierr)
    deallocate (dzu1 , stat=ierr)
    deallocate (dzv1 , stat=ierr)
  if (error /= 0) then
     write(*,'(2a)') '*** ERROR: Unable to read velocities from file ', trim(filnam)
     call wavestop(1, '*** ERROR: Unable to read velocities from file '//trim(filnam))
  endif
end subroutine get_cur
