subroutine INTERPvATuPOINT(vINTu,v1,ghostv1,inSTENCILu,kcs,aguu,gsqs,icx,icy,kmax,nmmax,nst,nlb,nub,mlb,mub,nmlb,nmub, gdp)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011.                                     
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
!  Ndryact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  $Id: My_intersec.f90 
!  $HeadURL:
!!--description-----------------------------------------------------------------
!
!   Function:   Interpolate the values of a velocity point on the location of the other velocity points
!
!   Author: Alberto Canestrelli
!               
!!--declarations----------------------------------------------------------------
!
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer, pointer :: typeEXTRAPstencil
    logical, pointer :: periodSURFACE
    logical, pointer :: twoCELLSperiod
    logical, pointer :: perCIRC
!
! global variables
!
!    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: u0      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmlb:nmub)                                      , intent(in)    :: gsqs  
    real(fp), dimension(nmlb:nmub)                                      , intent(in)    :: aguu
    integer , dimension(nmlb:nmub)                                      , intent(in)    :: inSTENCILu
    integer , dimension(nmlb:nmub)                                      , intent(in)    :: kcs
    integer , dimension(nmlb:nmub)                                      , intent(in)    :: ghostv1
    real(fp), dimension(nmlb:nmub,kmax)                                 , intent(inout) :: v1
    real(fp), dimension(nmlb:nmub,kmax)                                 , intent(out)   :: vINTu
    integer                                                             , intent(in)    :: nmmax
!    integer                                                             , intent(in)    :: mmax   !  Description and declaration in iidim.f90
!    integer                                                             , intent(in)    :: nmax   !  Description and declaration in iidim.f90
!    integer                                                             , intent(in)    :: nmaxus   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: kmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nst   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nlb
    integer                                                             , intent(in)    :: nub
    integer                                                             , intent(in)    :: mlb
    integer                                                             , intent(in)    :: mub
    integer                                                             , intent(in)    :: nmlb
    integer                                                             , intent(in)    :: nmub
    integer                                                             , intent(in)    :: icx
    integer                                                             , intent(in)    :: icy
!
! local variables
!
  integer                    :: i,cont
  integer                    :: j
  integer                    :: m
  integer                    :: n,nm,ndm,nmu,ndmu,nmOK,num,nmd,numd
  real(fp)                   :: vvv(1:kmax),vel1(1:kmax),vel2(1:kmax),vel3(1:kmax),vel4(1:kmax)
  real(fp)                   :: areaFORTHad,areaFORTH,area,mask(4)
!
! executable statements -------------------------------------------------------
! 
    typeEXTRAPstencil => gdp%gdimbound%typeEXTRAPstencil
    periodSURFACE     => gdp%gdimbound%periodSURFACE
    twoCELLSperiod    => gdp%gdimbound%twoCELLSperiod
    perCIRC           => gdp%gdimbound%perCIRC
!
! this might be optimized by creating a matrix usedSTENCILu and usedSTENCILv in find_BI_IP.f90 that has 1 
! if the velocity point is used in any stencil and zero if never used. And here add: if usedSTENCILu(n,m) ==1 then
!
!
!  extrapolate tangential velocity at the boundary, in order to have some sort of tranmissive behaviour
!
   if (typeEXTRAPstencil.ge.1.and..not.periodSURFACE) then ! if periodic tangential velocity is already extrapolated
      ndm  = -icy
      nmu  =  icx
      ndmu =  icx - icy
      do nm = 1, nmmax !-nmax, maybe -nub !I should skip last column, but i dont have nmax here
         nmu  = nmu  + 1
         ndm  = ndm  + 1
         ndmu = ndmu + 1
         ! extrapolate  tangential velocity v1 hortogonally to the boundary
         !if (kcs(nm) == 2 .and. kcs(num) == 1) then
         !   v1(nm,1:kmax)  = v1(num,1:kmax)
         !   v1(nmd,1:kmax) = v1(numd,1:kmax)
        ! endif
         if (kcs(nm) == 2 .and. kcs(nmu) == 1) then
            v1(nm,1:kmax)  = v1(nmu,1:kmax)
            v1(ndm,1:kmax) = v1(ndmu,1:kmax)
         elseif (kcs(nm) == 1 .and. kcs(nmu) == 2) then
            v1(nmu,1:kmax)  = v1(nm,1:kmax)
            v1(ndmu,1:kmax) = v1(ndm,1:kmax)
         endif
      enddo
   endif
!
!  compute interpolation of v at u point:
!
   ndm  = -icy
   nmu  =  icx
   ndmu =  icx - icy
   do nm = 1, nmmax
      nmu  = nmu  + 1
      ndm  = ndm  + 1
      ndmu = ndmu + 1
      if (inSTENCILu(nm) == 1) then !.and.aguu(nm).gt.0.5_fp) then  !will be replaced by: if inSTENCILu(n,m) ==1  (see also if (kcs(nm).eq.2.and.kcs(nmu).eq.2) below)
         !bilinear interpolation (I used geometric visualization http://en.wikipedia.org/wiki/Bilinear_interpolation )
         areaFORTHad = gsqs(nmu)*0.25_fp
         areaFORTH   = gsqs(nm )*0.25_fp
         mask(1:4) = 0
         !check if some v points are on dry bank. In this case use Hertmann approx to compute their values
         nmOK = nm
         if (ghostV1(nmOK).eq.2.or.ghostV1(nmOK).eq.4) then !ghostV1(nmOK).eq.1.or.
           vvv(:) = 0._fp
           cont =0
           if (ghostV1(nmOK+icy).eq.0) then 
              cont = cont+1
              vvv(:) = vvv(:) + v1(nmOK+icy,:)
           endif
           if (ghostV1(nmOK-icy).eq.0) then 
              cont = cont+1
              vvv(:) = vvv(:) + v1(nmOK-icy,:)
           endif
           if (ghostV1(nmOK+icx).eq.0) then 
              cont = cont+1
              vvv(:) = vvv(:) + v1(nmOK+icx,:)
           endif
           if (ghostV1(nmOK-icx).eq.0) then 
              cont = cont+1
              vvv(:) = vvv(:) + v1(nmOK-icx,:)
           endif
           if (cont>0) then
              vel1(:) = vvv(:)/cont
              mask(1) = 1._fp
           else
              write(*,*) 'No point for Hartmann approx in nm=',nm  ! note: this could be ok, remove it
              mask(1) = 0._fp
              !call d3stop(1, gdp)
           endif
         else
           vel1(:) = v1(nmOK,:) 
           mask(1) = 1._fp
         endif
         nmOK = nmu
         if (ghostV1(nmOK).eq.2.or.ghostV1(nmOK).eq.4) then !ghostV1(nmOK).eq.1.or.
           vvv(:) = 0._fp
           cont =0
           if (ghostV1(nmOK+icy).eq.0) then 
              cont = cont+1
              vvv(:) = vvv(:) + v1(nmOK+icy,:)
           endif
           if (ghostV1(nmOK-icy).eq.0) then 
              cont = cont+1
              vvv(:) = vvv(:) + v1(nmOK-icy,:)
           endif
           if (ghostV1(nmOK+icx).eq.0) then 
              cont = cont+1
              vvv(:) = vvv(:) + v1(nmOK+icx,:)
           endif
           if (ghostV1(nmOK-icx).eq.0) then 
              cont = cont+1
              vvv(:) = vvv(:) + v1(nmOK-icx,:)
           endif
           if (cont>0) then
              vel2(:) = vvv(:)/cont
              mask(2) = 1._fp
           else
              write(*,*) 'No point for Hartmann approx in nm=',nm  ! note: this could be ok, remove it
              mask(2) = 0._fp
              !call d3stop(1, gdp)
           endif
         else
           vel2(:) = v1(nmOK,:) 
           mask(2) = 1._fp
         endif
         nmOK = ndmu
         if (ghostV1(nmOK).eq.2.or.ghostV1(nmOK).eq.4) then !ghostV1(nmOK).eq.1.or.
           vvv(:) = 0._fp
           cont =0
           if (ghostV1(nmOK+icy).eq.0) then 
              cont = cont+1
              vvv(:) = vvv(:) + v1(nmOK+icy,:)
           endif
           if (ghostV1(nmOK-icy).eq.0) then 
              cont = cont+1
              vvv(:) = vvv(:) + v1(nmOK-icy,:)
           endif
           if (ghostV1(nmOK+icx).eq.0) then 
              cont = cont+1
              vvv(:) = vvv(:) + v1(nmOK+icx,:)
           endif
           if (ghostV1(nmOK-icx).eq.0) then 
              cont = cont+1
              vvv(:) = vvv(:) + v1(nmOK-icx,:)
           endif
           if (cont>0) then
              vel3(:) = vvv(:)/cont
              mask(3) = 1._fp
           else
              write(*,*) 'No point for Hartmann approx in nm=',nm  ! note: this could be ok, remove it
              mask(3) = 0._fp
              !call d3stop(1, gdp)
           endif
         else
           vel3(:) = v1(nmOK,:) 
           mask(3) = 1._fp
         endif
         nmOK = ndm
         if (ghostV1(nmOK).eq.2.or.ghostV1(nmOK).eq.4) then !ghostV1(nmOK).eq.1.or.
           vvv(:) = 0._fp
           cont =0
           if (ghostV1(nmOK+icy).eq.0) then 
              cont = cont+1
              vvv(:) = vvv(:) + v1(nmOK+icy,:)
           endif
           if (ghostV1(nmOK-icy).eq.0) then 
              cont = cont+1
              vvv(:) = vvv(:) + v1(nmOK-icy,:)
           endif
           if (ghostV1(nmOK+icx).eq.0) then 
              cont = cont+1
              vvv(:) = vvv(:) + v1(nmOK+icx,:)
           endif
           if (ghostV1(nmOK-icx).eq.0) then 
              cont = cont+1
              vvv(:) = vvv(:) + v1(nmOK-icx,:)
           endif
           if (cont>0) then
              vel4(:) = vvv(:)/cont
              mask(4) = 1._fp
           else
              write(*,*) 'No point for Hartmann approx in nm=',nm  ! note: this could be ok, remove it
              mask(4) = 0._fp
              !call d3stop(1, gdp)
           endif
         else
           vel4(:) = v1(nmOK,:) 
           mask(4) = 1._fp
         endif
!
!                  
!                 vel1         vel2
!        -----o-----|-----o-----|----o----|
!             |           |          |
!             | AREA(n,m) |  AREAad  |       areaFORTH   = AREA/4
!             |           -u1        -       areaFORTHad = AREAad/4
!             |           |          |  
!             |           |          |  
!        -----o-----|-----o-----|----o----|
!                 vel4         vel3
! 
!        note: when one or more mask value is zero dont know if it is theoretically correct, but figure of wikipedia page makes still sense to me, it has the same geometrical interpretation but now  only with three points ( I should probably do a trinlinear interpolation instead)
         area = areaFORTH*(mask(2)+mask(3))+ areaFORTHad*(mask(1)+mask(4))
         vINTu(nm,:)  = ((vel1(:)* mask(1)+ vel4(:)*mask(4))*areaFORTHad + (vel2(:)*mask(2)+ vel3(:)*mask(3))*areaFORTH )  /area !note if areaFORTHad is small vol2 and vel3 have more weight since they are closer
!
    !  elseif (kcs(nm).eq.2.and.kcs(nmu).eq.2) then ! this is needed for the circular testcase (kfu between two kcs=2 is zero). Alternatively extrapolate value from internal domain but then you can have a boundary with  kfu between two kcs=2 is zero that is dry (like internally the lower radius in the cisrcular test case) and no hertmann points are found and we have to find a way to include it
    !     vINTu(nm,:)  = 0._fp  
      endif
   enddo
   if (periodSURFACE) THEN
      if (icy==1 ) then !along x, ie. v is interpolated at u points
         call periodic_vATuPOINTS(vINTu,nlb,nub,mlb,mub,kmax, gdp)
      else !along y, ie. u is interpolated at v points
         call periodic_uATvPOINTS(vINTu,nlb,nub,mlb,mub,kmax, gdp)
      endif
      IF(.not.twoCELLSperiod.and.perCIRC) THEN
         write(*,*) 'Warning: there is no space in the halo to make v at u point periodic'
         !call d3stop(1, gdp)
      ENDIF
   endif
 RETURN 
 END

 !alternativa interpolazione classica non bilineare

!   ndm  = -icy
!   nmu  =  icx
!   ndmu =  icx - icy
!   do nm = 1, nmmax
!      nmu  = nmu  + 1
!      ndm  = ndm  + 1
!      ndmu = ndmu + 1
!      if (kfs(nm) == 1) then 
!         svvv = max(kfv(ndm) + kfv(ndmu) + kfv(nm) + kfv(nmu), 1)
!         vvv(nm,:)  = (v1(ndm, :)*kfv(ndm) + v1(ndmu, :)*kfv(ndmu)&
!            & + v1(nm, :)*kfv(nm) + v1(nmu, :)*kfv(nmu))  &
!            & /svvv
!      endif
!   enddo
