subroutine wavcur2d(wavetime  ,layer_model ,kfu       ,kfv       , &
                  & u1        ,mmax        ,nmax      ,kmax      , &
                  & filnam    ,dps         ,s1        ,thick     , &
                  & dzu1      ,rbuffu       )
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
    use precision
    use wave_data
    !
    implicit none
!
! Local parameters
!
    integer, parameter :: nelmx = 14 
!
! Global variables
!
    integer                             , intent(in)  :: mmax
    integer                             , intent(in)  :: nmax
    integer                             , intent(in)  :: kmax
    integer, dimension(mmax, nmax)      , intent(in)  :: kfu
    integer, dimension(mmax, nmax)      , intent(in)  :: kfv
    real   , dimension(mmax, nmax)      , intent(in)  :: dps
    real   , dimension(mmax, nmax)      , intent(in)  :: s1
    real   , dimension(kmax)            , intent(in)  :: thick
    real   , dimension(mmax, nmax)      , intent(out) :: u1
    real   , dimension(mmax, nmax, kmax), intent(in)  :: dzu1
    real   , dimension(nmax, mmax, kmax), intent(in)  :: rbuffu
    character(37)                       , intent(in)  :: filnam
    character(*)                        , intent(in)  :: layer_model
    type(wave_time_type)                , intent(in)  :: wavetime
!
! Local variables
!
    integer                                 :: celidt
    integer                                 :: error
    integer                                 :: ielem
    integer                                 :: ierr
    integer                                 :: ifind
    integer                                 :: iwave
    integer                                 :: k
    integer                                 :: kd
    integer                                 :: m
    integer                                 :: md
    integer                                 :: n
    integer                                 :: nd
    integer                                 :: nelems
    integer                                 :: ntcur
    integer, dimension(1)                   :: ival
    integer, dimension(6, nelmx)            :: elmdms
    integer, dimension(nelmx)               :: nbytsg
    real                                    :: cosharg
    real                                    :: dep
    real                                    :: eps
    real                                    :: pi
    real, dimension(:,:), allocatable       :: rlabda
    real, dimension(kmax)                   :: sig
    real                                    :: timcur
    real                                    :: waveku
    real                                    :: wght
    real                                    :: wghtsum
    real                                    :: z
    logical                                 :: wrswch
    logical, dimension(1)                   :: lval
    character(10), dimension(nelmx)         :: elmunt
    character(16), dimension(2)             :: grpnam
    character(16), dimension(nelmx)         :: elmnms
    character(16), dimension(nelmx)         :: elmqty
    character(16), dimension(nelmx)         :: elmtps
    character(64), dimension(nelmx)         :: elmdes
    !
    data grpnam/'WAVNT', 'WAVTIM'/
    data elmnms/'NTWAV', 'SWFLUX', 'TIMWAV', 'HRMS', 'TP  ', 'DIR   ', 'DISS',  &
        & 'FX  ', 'FY  ', 'MX  ', 'MY    ', 'TPS', 'UBOT', 'WLEN' /
    data elmqty/nelmx*' '/
    data elmtps/'INTEGER', 'LOGICAL', 'INTEGER', 11*'REAL'/
    data nbytsg/14*4/ 
!
!! executable statements -------------------------------------------------------
!
    if (kmax == 1) then
       do m = 1, mmax
          do n = 1, nmax
             u1(m,n) = rbuffu(n,m,1)
          enddo
       enddo
       return
    endif
    pi  = 4.0*atan(1.0)
    eps = 1.0e-6
    !
    call filldm(elmdms    ,1         ,1         ,1   , 0         , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,2         ,1         ,1   , 0         , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,3         ,1         ,1   , 0         , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,4         ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,5         ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,6         ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,7         ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,8         ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,9         ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,10        ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,11        ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,12        ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,13        ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,14        ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )     !
    ! Allocate arrays
    allocate (rlabda  (nmax,mmax))
    !
    ! Read the number of fields written to the com-file from group(1)
    !
    ielem  = 1
    wrswch = .false.
    celidt = 1
    nelems = 1
    call putgti(filnam    ,grpnam(1) ,nelems    ,elmnms    ,elmdms    , &
              & elmqty(1) ,elmunt(1) ,elmdes(1) ,elmtps    ,nbytsg    , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,ival      ) 
    if (error/=0) then
       write(*,'(2a)') '*** WARNING: Unable to read group ''WAVNT'' from file ',trim(filnam)
       write(*,'(a)')  '             This is normal at first WAVE calculation'
       write(*,'(a)')  '             Using depth avaraged FLOW velocity'
       goto 900
    endif
    !
    ! Get the correct field (according to time)
    !
    ntcur  = ival(1)
    ielem  = 3
    ifind  = 0
    celidt = 0
    nelems = nelmx
  100 continue
    celidt = celidt + 1
    if (celidt>ntcur .and. ifind==0) then
       write (*,'(4x,a,f15.3)') '*** ERROR: Specified time not found on com-file, group CURTIM ', wavetime%timmin
       error = -100
       goto 1000
    endif
    call putgti(filnam    ,grpnam(2) ,nelems    ,elmnms(3) ,elmdms(1, 3)         , &
              & elmqty(3) ,elmunt(3) ,elmdes(3) ,elmtps(3) ,nbytsg(3) , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,ival      ) 
    if (error/=0) goto 1000
    timcur = ival(1)
    if (wavetime%timtscale/=timcur .and. ntcur>1) goto 100
    !
    ! Ready to read waveLength
    ! note: rlabda has dimensions(n,m)!
    !
    wrswch = .false.
    ielem  = 14
    call putgtr(filnam    ,grpnam(2) ,nelems    ,elmnms(3) ,elmdms(1, 3)         , &
              & elmqty(3) ,elmunt(3) ,elmdes(3) ,elmtps(3) ,nbytsg(3) , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rlabda     ) 
    !
    if (layer_model(1:11) == 'SIGMA-MODEL') then
       !
       ! FLOW model used Sigma-layering in the vertical
       !
       sig(1) = -thick(1) / 2.0_fp
       do k=2,kmax
          sig(k) = -(sum(thick(1:k-1)) + thick(k)/2.0_fp)
       enddo
       do m = 1, mmax
          do n = 1, nmax
             md = max(m-1, 1)
             nd = max(n-1, 1)
             u1(m,n) = 0.0
             !
             ! compute weighted depth-averaged velocity for use in SWAN
             ! note: velocities are (averaged!) in zeta-points, wave pars are in zeta-points
             ! note: this produces ONE velocity based on "significant" wavelength
             !       really, each spectral component should have its own "equivalent" velocity...
             !       can't do that using SWAN.
             !
             if (kfu(m,n) /= 0 .or. kfu(md,n) /= 0 .or. kfv(m,n) /= 0 .or. kfv(m,nd) /= 0 ) then
                if (rlabda(n,m) > 0.1) then
                   waveku = 2.0 * pi / rlabda(n,m)
                else
                   waveku = 99.0
                endif
                wghtsum = 0.0
                do k = 1, kmax
                   !
                   ! z is 0 at bed and H at surface
                   !
                   z = (1.0+sig(k)) * (s1(m,n)+dps(m,n))
                   !
                   ! weight velocities according to Dingemans(1997)
                   !
                   cosharg = 2.0*waveku*z
                   if (cosharg > 50.0) then
                      !
                      ! very "deep" water
                      ! use surface velocity
                      !
                      u1(m,n) = rbuffu(n,m,1)
                      wghtsum = 1.0
                      exit
                   endif
                   wght = cosh(cosharg)
                   wght = wght * thick(k)
                   u1(m,n) = u1(m,n) + rbuffu(n,m,k)*wght
                   wghtsum = wghtsum + wght
                enddo
                u1(m,n) = u1(m,n) / max(eps, wghtsum)
             endif
          enddo
       enddo
    elseif (layer_model(1:7) == 'Z-MODEL') then
       !
       ! FLOW model used Z-layering in the vertical
       !
       do m = 1, mmax
          do n = 1, nmax
             md = max(m-1, 1)
             nd = max(n-1, 1)
             u1(m,n) = 0.0
             !
             ! compute weighted depth-averaged velocity for use in SWAN
             ! note: velocities are (averaged!) in zeta-points, wave pars are in zeta-points
             ! note: this produces ONE velocity based on "significant" wavelength
             !       really, each spectral component should have its own "equivalent" velocity...
             !       can't do that using SWAN.
             !
             if (kfu(m,n) /= 0 .or. kfu(md,n) /= 0 .or. kfv(m,n) /= 0 .or. kfv(m,nd) /= 0 ) then
                dep = max(0.01, s1(m,n)+dps(m,n))
                if (rlabda(n,m) > 0.1) then
                   waveku = 2.0 * pi / rlabda(n,m)
                else
                   waveku = 99.0
                endif
                wghtsum = 0.0
                z       = -0.5*dzu1(m,n,1)
                do k = 1, kmax
                   if (dzu1(m,n,k) > 0.0) then
                      !
                      ! z is 0 at bed and H at surface
                      !
                      kd = max(1,k-1)
                      z  = z + 0.5*(dzu1(m,n,k)+dzu1(m,n,kd))
                      !
                      ! weight velocities according to Dingemans(1997)
                      !
                      cosharg = 2.0*waveku*z
                      if (cosharg > 50.0) then
                         !
                         ! very "deep" water
                         ! use surface velocity
                         !
                         u1(m,n) = rbuffu(n,m,1)
                         wghtsum = 1.0
                         exit
                      endif
                      wght = cosh(cosharg)
                      wght = wght * dzu1(m,n,k) / dep
                      u1(m,n) = u1(m,n) + rbuffu(n,m,k)*wght
                      wghtsum = wghtsum + wght
                   endif
                enddo
                u1(m,n) = u1(m,n) / max(eps, wghtsum)
             endif
          enddo
       enddo
    else
       !
       ! Erroneous vertical layering definition found on COM-FILE
       !
       write(*, '(2a)') '*** ERROR: Erroneous vertical layering definition found on COM-FILE: LAYER_MODEL = ', trim(layer_model)
       call wavestop(1, '*** ERROR: Erroneous vertical layering definition found on COM-FILE: LAYER_MODEL = '//trim(layer_model))
    endif
    !
    ! Normal end
    ! skip error solving part
    !
    goto 1000
  900 continue
    !
    ! Error. Probably not able to read WAVNT/WAVTIM from com-file
    ! Use depth averaged velocity
    !
    if (layer_model(1:11) == 'SIGMA-MODEL') then
       do m = 1, mmax
          do n = 1, nmax
             u1(m, n) = 0.0
             do k = 1,kmax
                u1(m,n) = u1(m,n) + thick(k) * rbuffu(n,m,k)
             enddo
          enddo
       enddo
    elseif (layer_model(1:7) == 'Z-MODEL') then
       do m = 1, mmax
          do n = 1, nmax
             u1(m, n) = 0.0
             dep      = 0.0 
             do k = 1, kmax
                u1(m, n) = u1(m, n) + dzu1(m, n, k) * rbuffu(n, m, k)
                dep      = dep + dzu1(m, n, k)
             enddo
             dep     = max(dep, 0.01)
             u1(m,n) = u1(m,n)/dep
          enddo
       enddo
    else
       !
       ! Erroneous vertical layering definition found on COM-FILE
       !
       write(*, '(2a)') '*** ERROR: Erroneous vertical layering definition found on COM-FILE: LAYER_MODEL = ', trim(layer_model)
       call wavestop(1, '*** ERROR: Erroneous vertical layering definition found on COM-FILE: LAYER_MODEL = '//trim(layer_model))
    endif
    !
    ! assuming error is solved
    !
    error = 0
 1000 continue
    deallocate (rlabda, stat=ierr)
    if (error /= 0) then
       write(*,'(2a)') '*** ERROR: Unable to read/calculate wave-dependent flow velocities from file ', trim(filnam)
       call wavestop(1, '*** ERROR: Unable to read/calculate wave-dependent flow velocities from file '//trim(filnam))
    endif
end subroutine wavcur2d
