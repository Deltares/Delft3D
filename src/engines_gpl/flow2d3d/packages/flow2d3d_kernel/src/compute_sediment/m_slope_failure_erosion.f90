module m_slope_failure_erosion
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
!-------------------------------------------------------------------------------
implicit none
private
!
! public routines
!
public slope_failure_erosion
!
contains 
!   
!
! ---------------START OF SLOPE FAILURE EROSION MECHANISM-------------
! Implementation of slope failure bank erosion mechanism.
!
! Notes:
!
! probably only valid for single layer model or fixed thickness transport 
! layer otherwise layer thickness = 0 in dry areas? doesn't seem to be 
! highlighted as a problem for ThetSD approach to bank erosion though so 
! might be ok...?
!
! possible issue if initial bed topography does not satisfy repose
! slope in areas where there is a non-zero sediment thickness
! i.e. areas where the bed is not set inerodible) - if slopes are 
! too severe algorithm could go through active layer in first
! timestep - this could potentially cause sediment shortage issues
! if composition of underlayers is significantly different to surface
!
subroutine slope_failure_erosion(&
           !input
           icx,icy,nmmax,kcu,kcv,kcs,kfs,dps,s1,lsedtot,gsqs,guu,guv,gvu,gvv,dtmor,&
           !input/output
           sbuu,sbvv, &
           !global
           gdp)

    use precision, only: fp, prec
    use globaldata, only: globdat
    
    type(globdat),target :: gdp

    integer                                            , intent(in)    :: icx
    integer                                            , intent(in)    :: icy
    integer                                            , intent(in)    :: nmmax  !  Description and declaration in dimens.igs
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)    :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)    :: kcv    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)    :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)    :: kcs    !  Description and declaration in esm_alloc_int.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)       , intent(in)    :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)    :: s1     !  Description and declaration in esm_alloc_real.f90
    integer                                            , intent(in)    :: lsedtot!  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)    :: gsqs   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)    :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)    :: guv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)    :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)    :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp)                                           , intent(in)    :: dtmor
    !input/output
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, lsedtot), intent(inout) :: sbuu   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, lsedtot), intent(inout) :: sbvv   !  Description and declaration in esm_alloc_real.f90
    
    
    real(fp), dimension(:,:)             , pointer :: dbodsd
    real(fp)                             , pointer :: repose
    real(fp)                             , pointer :: dryrepose
    real(fp)                             , pointer :: reposeredfac
    real(fp)                             , pointer :: reposemaxdz
    integer , dimension(:)               , pointer :: kfsed
    real(fp), dimension(:,:)             , pointer :: fixfac
    real(fp), dimension(:,:)             , pointer :: frac
    real(fp), dimension(:)               , pointer :: dzduu     ! local slope in u direction at u velocity point 
                                                                ! (positive slope = downwards) [m/m]
    real(fp), dimension(:)               , pointer :: dzdvv     ! local slope in v direction at v velocity point
                                                                ! (positive slope = downwards) [m/m]
    real(fp)                             , pointer :: sedthr
    real(fp)      , dimension(:)         , pointer :: cdryb

!
! Local variables
!
    logical  :: enabling_nmu
    logical  :: enabling_nmd
    logical  :: enabling_num
    logical  :: enabling_ndm
    real(fp) :: h1_nm
    real(fp) :: h1_neigh
    real(fp) :: h1_min
    real(fp) :: limitslope
    integer :: nm, nmu, num, nmd, ndm, l
    real(fp) :: totfixfrac
    real(fp) :: localslope0 ! magnitude of local slope vector out of cell nm [m/m]
    real(fp) :: localslope ! magnitude of local slope vector out of cell nm [m/m]
    real(fp) :: areafac
    real(fp) :: excessslope  ! excess slope steeper than repose
    real(fp) :: dvol      ! sediment volume flux due to bank erosion [m3]
    real(fp) :: dvolfrac
!
!! executable statements -------------------------------------------------------
!
    dbodsd              => gdp%gderosed%dbodsd
    kfsed               => gdp%gderosed%kfsed
    fixfac              => gdp%gderosed%fixfac
    frac                => gdp%gderosed%frac
    dzduu               => gdp%gderosed%e_dzdn
    dzdvv               => gdp%gderosed%e_dzdt
    !
    repose              => gdp%gdmorpar%repose
    dryrepose           => gdp%gdmorpar%dryrepose
    reposeredfac        => gdp%gdmorpar%reposeredfac
    reposemaxdz         => gdp%gdmorpar%reposemaxdz
    sedthr              => gdp%gdmorpar%sedthr
    !
    cdryb               => gdp%gdsedpar%cdryb
    

IF (repose > 0.0_fp) then !if slope erosion turned on
   !
   ! Calculate sediment flux due to slope failure
   !
   DO nm = 1, nmmax
      ! If this is a cell in which sediment processes are active then ...
      if (abs(kcs(nm))*kfs(nm)*kfsed(nm) /= 1) cycle
       
      nmu = nm + icx
      num = nm + icy
      nmd = nm - icx
      ndm = nm - icy
      
      ! calculate flux INTO nm from nmu
      !
      ! only calculate slope erosion if sediment available to erode out of cell nmu 
      ! and cell nmu is active for sediment transport 
      ! and there is sediment available in nmu to erode
      enabling_nmu = (kcs(nmu) /= 0 .and. kcs(nmu)<3 .and. abs(kcu(nm))==1)
      enabling_nmd = (kcs(nmd) /= 0 .and. kcs(nmd)<3 .and. abs(kcu(nmd))==1)
      enabling_num = (kcs(num) /= 0 .and. kcs(num)<3 .and. abs(kcv(nm))==1)
      enabling_ndm = (kcs(ndm) /= 0 .and. kcs(ndm)<3 .and. abs(kcv(ndm))==1)
      
      h1_nm   = real(dps(nm),fp) + s1(nm)
         
      IF (enabling_nmu) THEN
         totfixfrac = 0.0_fp
         do l = 1, lsedtot
            totfixfrac = totfixfrac + fixfac(nmu, l) * frac(nmu, l)
         enddo
         localslope0 = dzduu(nm)*dzduu(nm)
         
         h1_neigh   = real(dps(nmu),fp) + s1(nmu)
         h1_min = min(h1_nm, h1_neigh)
         if(h1_min >= SedThr)then
             limitslope = repose
         else
             limitslope = dryrepose
         endif
         
         ! BLOC 1 - uses localslope from dzduu(nm) and dzdvv(nm)
         localslope = localslope0
         if(enabling_num ) localslope = localslope + dzdvv(nm)*dzdvv(nm)   ! add 
         localslope=sqrt(localslope)
         IF (localslope > limitslope .and. dzduu(nm) < 0.0_fp .and. totfixfrac > 1.0E-7) THEN !active slope erosion occuring into nm
            
            ! calculate flux out of nmu to nm
            excessslope = (localslope - limitslope)
            areafac = gsqs(nm) * gsqs(nmu) / (gsqs(nm) + gsqs(nmu))
            dvol = excessslope * gvu(nm)  / ReposeRedFac 
            dvol = min(dvol, reposemaxdz ) *areafac * (- dzduu(nm)) / localslope    !dzduu(nm) is negative
            ! incorporate flux into dbodsd, sbuu and sbvv
            DO l = 1, lsedtot
               dvolfrac = dvol * frac(nmu,l) * cdryb(l) * fixfac(nmu, l)
               dbodsd(l,nm) = dbodsd(l,nm) + dvolfrac / gsqs(nm)
               dbodsd(l,nmu) = dbodsd(l,nmu) - dvolfrac / gsqs(nmu) 
               sbuu(nm, l) = sbuu(nm, l) - dvolfrac/(dtmor*guu(nm))
            ENDDO
         ENDIF
         ! BLOC 2 - uses localslope from dzduu(nm) and dzdvv(ndm)
         localslope = localslope0
         if(enabling_ndm) localslope = localslope + dzdvv(ndm)*dzdvv(ndm)   ! add 
         localslope=sqrt(localslope)
         IF (localslope > limitslope .and. dzduu(nm) < 0.0_fp .and. totfixfrac > 1.0E-7) THEN !active slope erosion occuring into nm
            
            ! calculate flux out of nmu to nm
            excessslope = (localslope - limitslope)
            areafac = gsqs(nm) * gsqs(nmu) / (gsqs(nm) + gsqs(nmu))
            dvol = excessslope * gvu(nm)  / ReposeRedFac 
            dvol = min(dvol, reposemaxdz ) *areafac * ( - dzduu(nm)) / localslope    !dzduu(nm) is negative
            ! incorporate flux into dbodsd, sbuu and sbvv
            DO l = 1, lsedtot
               dvolfrac = dvol * frac(nmu,l) * cdryb(l) * fixfac(nmu, l)
               dbodsd(l,nm) = dbodsd(l,nm) + dvolfrac / gsqs(nm)
               dbodsd(l,nmu) = dbodsd(l,nmu) - dvolfrac / gsqs(nmu) 
               sbuu(nm, l) = sbuu(nm, l) - dvolfrac/(dtmor*guu(nm))
            ENDDO
         ENDIF
      ENDIF
      
      ! calculate flux INTO nm from num
      !
      ! only calculate slope erosion if sediment available to erode out of cell num 
      ! and cell num is active for sediment transport 
      ! and there is sediment available in num to erode
      IF (enabling_num) THEN    
         totfixfrac = 0.0_fp
         do l = 1, lsedtot
            totfixfrac = totfixfrac + fixfac(num, l) * frac(num, l)
         enddo
         localslope0 = dzdvv(nm)*dzdvv(nm)
         
         h1_neigh   = real(dps(num),fp) + s1(num)
         h1_min = min(h1_nm, h1_neigh)
         if(h1_min >= SedThr)then
             limitslope = repose
         else
             limitslope = dryrepose
         endif
         
         ! BLOC 1 - uses localslope from dzdvv(nm) and dzduu(nm)
         localslope = localslope0
         if(enabling_nmu) localslope = localslope + dzduu(nm)*dzduu(nm)   ! add 
         localslope=sqrt(localslope)
         IF (localslope > limitslope .and. dzdvv(nm) < 0.0_fp .and. totfixfrac > 1.0E-7) THEN !active slope erosion occuring into nm
            ! calculate flux out of num to nm
            excessslope = (localslope - limitslope)
            areafac = gsqs(nm) * gsqs(num) / (gsqs(nm) + gsqs(num))
            dvol = excessslope * guv(nm) / ReposeRedFac 
            dvol = min(dvol, reposemaxdz )  * areafac * (-dzdvv(nm)) / localslope     ! dzdvv(nm) is negative
            ! incorporate flux into dbodsd, sbuu and sbvv
            DO l = 1, lsedtot
               dvolfrac = dvol * frac(num,l) * cdryb(l) * fixfac(num, l)
              dbodsd(l,nm) = dbodsd(l,nm) + dvolfrac / gsqs(nm)
               dbodsd(l,num) = dbodsd(l,num) - dvolfrac / gsqs(num) 
               sbvv(nm, l) = sbvv(nm, l) - dvolfrac/(dtmor*gvv(nm))
            ENDDO
         ENDIF
         ! BLOC 2 - uses localslope from dzdvv(nm) and dzduu(nmd)
         localslope = localslope0
         if(enabling_nmd) localslope = localslope + dzduu(nmd)*dzduu(nmd)   ! add 
         localslope=sqrt(localslope)
         IF (localslope > limitslope .and. dzdvv(nm) < 0.0_fp .and. totfixfrac > 1.0E-7) THEN !active slope erosion occuring into nm
            ! calculate flux out of num to nm
            excessslope = (localslope - limitslope)
            areafac = gsqs(nm) * gsqs(num) / (gsqs(nm) + gsqs(num))
            dvol = excessslope * guv(nm)  / ReposeRedFac 
            dvol = min(dvol, reposemaxdz )  * areafac * (-dzdvv(nm)) / localslope     ! dzdvv(nm) is negative
            ! incorporate flux into dbodsd, sbuu and sbvv
            DO l = 1, lsedtot
               dvolfrac = dvol * frac(num,l) * cdryb(l) * fixfac(num, l)
              dbodsd(l,nm) = dbodsd(l,nm) + dvolfrac / gsqs(nm)
               dbodsd(l,num) = dbodsd(l,num) - dvolfrac / gsqs(num) 
               sbvv(nm, l) = sbvv(nm, l) - dvolfrac/(dtmor*gvv(nm))
            ENDDO
         ENDIF
      ENDIF
      ! calculate flux INTO nm from nmd
      !
      ! only calculate slope erosion if sediment available to erode out of cell nmd 
      ! and cell nmd is active for sediment transport 
      ! and there is sediment available in nmd to erode
      IF (enabling_nmd) THEN
         totfixfrac = 0.0_fp
         do l = 1, lsedtot
            totfixfrac = totfixfrac + fixfac(nmd, l) * frac(nmd, l)
         enddo
         localslope0 = dzduu(nmd)*dzduu(nmd)
         
         h1_neigh   = real(dps(nmd),fp) + s1(nmd)
         h1_min = min(h1_nm, h1_neigh)
         if(h1_min >= SedThr)then
             limitslope = repose
         else
             limitslope = dryrepose
         endif
         
         ! BLOC 1 - uses localslope from dzduu(nmd) and dzdvv(nm)
         localslope = localslope0
         if(enabling_num ) localslope = localslope + dzdvv(nm)*dzdvv(nm)   ! add 
         localslope=sqrt(localslope)
         IF (localslope > limitslope .and. dzduu(nmd) > 0.0_fp .and. totfixfrac > 1.0E-7) THEN !active slope erosion occuring into nm
            !
            ! calculate flux out of nmd to nm
            excessslope = (localslope - limitslope)
            areafac = gsqs(nm) * gsqs(nmd) / (gsqs(nm) + gsqs(nmd))
            dvol = excessslope * gvu(nmd) / ReposeRedFac
            dvol = min(dvol, reposemaxdz )  * areafac * dzduu(nmd) / localslope  !dzduu(nmd) is positive
            ! incorporate flux into dbodsd, sbuu and sbvv
            DO l = 1, lsedtot
               dvolfrac = dvol * frac(nmd,l) * cdryb(l) * fixfac(nmd, l)
               dbodsd(l,nm) = dbodsd(l,nm) + dvolfrac / gsqs(nm)
               dbodsd(l,nmd) = dbodsd(l,nmd) - dvolfrac / gsqs(nmd) 
               sbuu(nmd, l) = sbuu(nmd, l) + dvolfrac/(dtmor*guu(nmd))    
            ENDDO
         ENDIF
         ! BLOC 2 - uses localslope from dzduu(nmd) and dzdvv(ndm)
         localslope = localslope0
         if(enabling_ndm) localslope = localslope + dzdvv(ndm)*dzdvv(ndm)   ! add 
         localslope=sqrt(localslope)
        IF (localslope > limitslope .and. dzduu(nmd) > 0.0_fp .and. totfixfrac > 1.0E-7) THEN !active slope erosion occuring into nm
            !
            ! calculate flux out of nmd to nm
            excessslope = (localslope - limitslope)
            areafac = gsqs(nm) * gsqs(nmd) / (gsqs(nm) + gsqs(nmd))
            dvol = excessslope * gvu(nmd) / ReposeRedFac
            dvol = min(dvol, reposemaxdz ) * areafac * dzduu(nmd) / localslope !dzduu(nmd) is positive
            ! incorporate flux into dbodsd, sbuu and sbvv
            DO l = 1, lsedtot
               dvolfrac = dvol * frac(nmd,l) * cdryb(l) * fixfac(nmd, l)
               dbodsd(l,nm) = dbodsd(l,nm) + dvolfrac / gsqs(nm)
               dbodsd(l,nmd) = dbodsd(l,nmd) - dvolfrac / gsqs(nmd) 
               sbuu(nmd, l) = sbuu(nmd, l) + dvolfrac/(dtmor*guu(nmd))    
            ENDDO
         ENDIF
      ENDIF
      
      ! calculate flux INTO nm from ndm
      !
      ! only calculate slope erosion if sediment available to erode out of cell ndm 
      ! and cell ndm is active for sediment transport 
      ! and there is sediment available in ndm to erode
      IF (enabling_ndm) THEN
         totfixfrac = 0.0_fp
         do l = 1, lsedtot
            totfixfrac = totfixfrac + fixfac(ndm, l) * frac(ndm, l)
         enddo
         localslope0 = dzdvv(ndm)*dzdvv(ndm)
         
         h1_neigh  = real(dps(ndm),fp) + s1(ndm)
         h1_min = min(h1_nm, h1_neigh)
         if(h1_min >= SedThr)then
             limitslope = repose
         else
             limitslope = dryrepose
         endif
         
         ! BLOC 1 - uses localslope from dzdvv(ndm) and dzduu(nm)
         localslope = localslope0
         if(enabling_nmu) localslope = localslope + dzduu(nm)*dzduu(nm)   ! add 
         localslope=sqrt(localslope)
         
         IF (localslope > limitslope .and. dzdvv(ndm) > 0.0_fp .and. totfixfrac > 1.0E-7) THEN !active slope erosion occuring into nm
            !
            ! calculate flux out of ndm to nm
            excessslope = (localslope - limitslope)
            areafac = gsqs(nm) * gsqs(ndm) / (gsqs(nm) + gsqs(ndm))
            dvol = excessslope * guv(ndm) / ReposeRedFac 
            dvol = min(dvol, reposemaxdz ) * areafac * dzdvv(ndm) / localslope  !dzdvv(ndm) is positive
            DO l = 1, lsedtot
               dvolfrac = dvol * frac(ndm,l) * cdryb(l) * fixfac(ndm, l)
               dbodsd(l,nm) = dbodsd(l,nm) + dvolfrac / gsqs(nm)
               dbodsd(l,ndm) = dbodsd(l,ndm) - dvolfrac / gsqs(ndm) 
               sbvv(ndm, l) = sbvv(ndm, l) + dvolfrac/(dtmor*gvv(ndm))
            ENDDO
         ENDIF
         ! BLOC 2 - uses localslope from dzdvv(ndm) and dzduu(nmd)
         localslope = localslope0
         if(enabling_nmd) localslope = localslope + dzduu(nmd)*dzduu(nmd)   ! add 
         localslope=sqrt(localslope)
         IF (localslope > limitslope .and. dzdvv(ndm) > 0.0_fp .and. totfixfrac > 1.0E-7) THEN !active slope erosion occuring into nm
            !
            ! calculate flux out of ndm to nm
            excessslope = (localslope - limitslope)
            areafac = gsqs(nm) * gsqs(ndm) / (gsqs(nm) + gsqs(ndm))
           dvol = excessslope * guv(ndm) / ReposeRedFac
            dvol = min(dvol, reposemaxdz )  * areafac * dzdvv(ndm) / localslope  !dzdvv(ndm) is positive
            ! incorporate flux into dbodsd, sbuu and sbvv
            DO l = 1, lsedtot
               dvolfrac = dvol * frac(ndm,l) * cdryb(l) * fixfac(ndm, l)
               dbodsd(l,nm) = dbodsd(l,nm) + dvolfrac / gsqs(nm)
               dbodsd(l,ndm) = dbodsd(l,ndm) - dvolfrac / gsqs(ndm) 
               sbvv(ndm, l) = sbvv(ndm, l) + dvolfrac/(dtmor*gvv(ndm))
            ENDDO
         ENDIF
      
      
      ENDIF
   ENDDO ! nm
ENDIF !repose > 0
! --------------------------END OF SLOPE FAILURE EROSION MECHANISM----------------

end subroutine slope_failure_erosion

end module