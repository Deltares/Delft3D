      subroutine wrwaqvol ( nmax   , mmax   , kmax   , nlb    , nub    , &
     &                      mlb    , mub    , itim   , vol1   , kcs    , &
     &                      kfsmin , kfsmax , gsqs   , lsal   , ltem   , &
     &                      lsed   , conc   , tau    , vdiff  , isaggr , &
     &                      noseg  , vol    , sag    , con    , zmodel , &
     &                      ilaggr , mode   , lunvol , lunsal , luntem , &
     &                      lunsed , lunvdf , luntau , itdate , cmsedf , &
     &                      cmresf , lunsdf , naccum )
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
      use precision
      implicit none
!
!           Global variables
!
      integer( 4), intent(in   ) :: nmax                    !!  Dimension of first index in 2d arrays
      integer( 4), intent(in   ) :: mmax                    !!  Dimension of second index in 2d arrays
      integer( 4), intent(in   ) :: kmax                    !!  number of layers
      integer( 4), intent(in   ) :: nlb                     !!  Lower bound of all n dimensions
      integer( 4), intent(in   ) :: nub                     !!  Upper bound of all n dimensions
      integer( 4), intent(in   ) :: mlb                     !!  Lower bound of all m dimensions
      integer( 4), intent(in   ) :: mub                     !!  Upper bound of all m dimensions
      integer( 4), intent(in   ) :: itim                    !!  Time in the file in seconds
      real   (fp), intent(in   ) :: vol1  (nlb:nub,mlb:mub,1:kmax)    !!  Array with volume values
      integer( 4), intent(in   ) :: kcs   (nlb:nub,mlb:mub) !!  Fixed prop. of computational volumes
      integer( 4), intent(in   ) :: kfsmin(nlb:nub,mlb:mub) !!  Variable lowest active layer (z-model-only)
      integer( 4), intent(in   ) :: kfsmax(nlb:nub,mlb:mub) !!  Variable highest active layer (z-model-only)
      real   (fp), intent(in   ) :: gsqs  (nlb:nub,mlb:mub) !!  Horizontal surface area of the seg
      integer( 4), intent(in   ) :: lsal                    !!  Substance nr salinity
      integer( 4), intent(in   ) :: ltem                    !!  Substance nr temperature
      integer( 4), intent(in   ) :: lsed                    !!  Number of substances sediment
      real   (fp), intent(in   ) :: conc  (nlb:nub,mlb:mub,  kmax,*)  !!  substances array
      real   (fp), intent(in   ) :: tau   (nlb:nub,mlb:mub) !!  Tau at the bottom only
      real   (fp), intent(in   ) :: vdiff (nlb:nub,mlb:mub,0:kmax)    !!  Vertical diffusion
      integer( 4), intent(in   ) :: isaggr(nmax,mmax,kmax)  !!  horizontal grid aggregation pointer
      integer( 4), intent(in   ) :: noseg                   !!  number of segments
      real   ( 4), intent(  out) :: vol   (0:noseg)         !!  resulting volume array
      real   ( 4), intent(  out) :: sag   (0:noseg,max(lsed,1))       !!  second aggregation array for segments
      real   ( 4), intent(  out) :: con   (0:noseg)         !!  resulting concentration
      logical    , intent(in   ) :: zmodel                  !!  true if z-model feature is used
      integer( 4), intent(in   ) :: ilaggr(kmax)            !!  vertical layer aggregation array
      integer( 4), intent(in   ) :: mode                    !!  0 for initialisation, 1 normal, 2 finalisation
      integer( 4), intent(in   ) :: lunvol                  !!  file unit number to an output file
      integer( 4), intent(in   ) :: lunsal                  !!  file unit number to an output file
      integer( 4), intent(in   ) :: luntem                  !!  file unit number to an output file
      integer( 4), intent(in   ) :: lunsed(lsed)            !!  file unit numbers to sediment output files
      integer( 4), intent(in   ) :: lunvdf                  !!  file unit number to an output file
      integer( 4), intent(in   ) :: luntau                  !!  file unit number to an output file
      integer( 4), intent(in   ) :: itdate                  !!  reference time in YYYYMMDD
      real   (fp), intent(inout) :: cmsedf(nlb:nub,mlb:mub,lsed) !!  Array with cumulative sedimentation fluxes
      real   (fp), intent(inout) :: cmresf(nlb:nub,mlb:mub,lsed) !!  Array with cumulative resuspension fluxes
      integer( 4), intent(in   ) :: lunsdf(lsed, 2)         !!  file unit numbers to sediment flux output files
      integer( 4), intent(in   ) :: naccum                  !!  number of fluxes accumulated
!
!           Local variables
!
      integer  ( 4) i, j, ij, k, l, ml, iseg
      integer  ( 4) ilay
      integer  ( 4) kfmin, kfmax
      real     ( 4) amin
      real     ( 4) flx2day
!
!! executable statements -------------------------------------------------------
!
!        write the volumes

      vol = 0.0
      do k = 1,kmax
         do j = 1,mmax
            do i = 1,nmax
               if (kcs(i,j) .eq. 1) then
                  if ( zmodel .and. k .lt. kfsmin(i,j) ) cycle  ! not all layers may be active
                  iseg = isaggr(i,j,k)
                  if ( vol1(i,j,k) .gt. 1.0E-25 ) then
                     vol(iseg) = vol(iseg) + vol1(i,j,k)
                  endif
               endif
            enddo
         enddo
      enddo
      write ( lunvol ) itim, vol(1:noseg)

!        write the salinities

      if ( lsal .gt. 0 ) then
         con = 0.0
         do k = 1,kmax
            do j = 1,mmax
               do i = 1,nmax
                  if (kcs(i,j) .eq. 1) then
                     if ( zmodel .and. k .lt. kfsmin(i,j) ) cycle  ! not all layers may be active
                     iseg = isaggr(i,j,k)
                     if ( vol1(i,j,k) .gt. 1.0E-25 ) then
                        con(iseg) = con(iseg) + conc(i,j,k,lsal)*vol1(i,j,k)
                     endif
                  endif
               enddo
            enddo
         enddo
         do i = 1,noseg
            if ( vol(i) .gt. 1.0E-25 ) then
               con(i) = con(i) / vol(i)
            endif
         enddo
         write ( lunsal ) itim, con(1:noseg)
      endif

!        write the temperatures

      if ( ltem .gt. 0 ) then
         con = 0.0
         do k = 1,kmax
            do j = 1,mmax
               do i = 1,nmax
                  if (kcs(i,j) .eq. 1) then
                     if ( zmodel .and. k .lt. kfsmin(i,j) ) cycle  ! not all layers may be active
                     iseg = isaggr(i,j,k)
                     if ( vol1(i,j,k) .gt. 1.0E-25 ) then
                        con(iseg) = con(iseg) + conc(i,j,k,ltem)*vol1(i,j,k)
                     endif
                  endif
               enddo
            enddo
         enddo
         do i = 1,noseg
            if ( vol(i) .gt. 1.0E-25 ) then
               con(i) = con(i) / vol(i)
            endif
         enddo
         write ( luntem ) itim, con(1:noseg)
      endif

!        write the vertical diffusions

      if ( ilaggr(kmax) .gt. 1 ) then
         if ( mode .eq. 0 ) then                                                 ! this is the time of the next record
            write ( lunvdf ) itim                                                ! vdf in FLOW always TO   this time,
         else                                                                    ! vdf in WAQ  always FROM this time
            sag(:,1) = 0.0
            con = 0.0
            do j = 1,mmax
               do i = 1,nmax
                  if (kcs(i,j) .eq. 1) then
                     if ( zmodel ) then
                        kfmin = kfsmin(i,j)
                        kfmax = kfsmax(i,j)
                        do ilay = 1, ilaggr(kmax)-1                              ! the waq layers
                           if ( kmax-ilaggr(ilay+1)-1 .gt. kfmax ) cycle         ! waq layer above top
                           if ( kmax-ilaggr(ilay  )   .lt. kfmin ) cycle         ! waq layer below bottom
                           amin = 0.0
                           do k = ilaggr(ilay), ilaggr(ilay+1)-1
                              if ( kmax-k .ge. kfmax ) cycle
                              if ( kmax-k .lt. kfmin ) cycle
                              if ( vdiff(i,j,kmax-k) .lt. amin .or. amin .eq. 0.0 ) amin = vdiff(i,j,kmax-k)
                           enddo
                           iseg        = isaggr(i,j,kmax-ilaggr(ilay)+1)
                           con(iseg)   = con(iseg) + gsqs(i,j)*amin
                           sag(iseg,1) = sag(iseg,1) + gsqs(i,j)
                        enddo
                     else
                        do ilay = 1, ilaggr(kmax)-1                              ! the waq layers
                           amin = 0.0
                           do k = ilaggr(ilay), ilaggr(ilay+1)-1
                              if ( vdiff(i,j,k) .lt. amin .or. amin .eq. 0.0 ) amin = vdiff(i,j,k)
                           enddo
                           iseg        = isaggr(i,j,ilaggr(ilay))
                           con(  iseg) = con(  iseg) + gsqs(i,j)*amin
                           sag(iseg,1) = sag(iseg,1) + gsqs(i,j)
                        enddo
                     endif
                  endif
               enddo
            enddo
            do i = 1,noseg
               if ( sag(i,1) .gt. 1.0E-25 ) con(i) = con(i)/sag(i,1)
            enddo
            write ( lunvdf ) con(1:noseg), itim
            if ( mode .eq. 2 ) write ( lunvdf ) con(1:noseg)                     ! now dummy values must be added to
         endif                                                                   ! complete te last record
      endif

!        write the tau at the bottom

      if ( mode .eq. 0 ) then
         write ( luntau ) itim
      else
         sag(:,1) = 0.0
         con = 0.0
         kfmin = kmax
         do j = 1,mmax
            do i = 1,nmax
               if (kcs(i,j) .eq. 1) then
                  if ( zmodel ) kfmin = kfsmin(i,j)
                  iseg        = isaggr(i,j,kfmin)
                  con(  iseg) = con(  iseg) + gsqs(i,j)*tau(i,j)
                  sag(iseg,1) = sag(iseg,1) + gsqs(i,j)
               endif
            enddo
         enddo
         do i = 1,noseg
            if ( sag(i,1) .gt. 1.0E-25 ) con(i) = con(i)/sag(i,1)
         enddo
         write ( luntau ) con(1:noseg), itim
         if ( mode .eq. 2 ) write ( luntau ) con(1:noseg)
      endif

!        write sediment files

      if ( lsed .gt. 0 ) then
         sag = 0.0
         ml = max(lsal, ltem)
         do k = 1,kmax
            do j = 1,mmax
               do i = 1,nmax
                  if (kcs(i,j) .eq. 1) then
                     if ( zmodel .and. k .lt. kfsmin(i,j) ) cycle  ! not all layers may be active
                     iseg = isaggr(i,j,k)
                     if ( vol1(i,j,k) .gt. 1.0E-25 ) then
                        do l = 1,lsed
                           sag(iseg,l) = sag(iseg,l) + conc(i,j,k,l+ml)*vol1(i,j,k)
                        enddo
                     endif
                  endif
               enddo
            enddo
         enddo
         do i = 1,noseg
            if ( vol(i) .gt. 1.0E-25 ) then
               do l = 1,lsed
                  sag(i,l) = sag(i,l) / vol(i)
               enddo
            endif
         enddo
         do l = 1,lsed
            write ( lunsed(l) ) itim, sag(1:noseg,l)
         enddo
         !
         flx2day = 86400.0/naccum
         !
         ! sedimentation in kg/day
         !
         sag = 0.0
         con = 0.0 ! used here to (re)compute horsurf
         kfmin = kmax
         do j = 1,mmax
            do i = 1,nmax
               if (kcs(i,j) .eq. 1) then
                  if ( zmodel ) kfmin = kfsmin(i,j)
                  iseg = isaggr(i,j,kfmin)
                  if ( gsqs(i,j) .gt. 1.0E-25 ) then
                     do l = 1,lsed
                        sag(iseg,l) = sag(iseg,l) + cmsedf(i,j,l)*gsqs(i,j)
                     enddo
                     con(iseg) = con(iseg) + gsqs(i,j)
                  endif
               endif
            enddo
         enddo
         do i = 1,noseg
            if ( con(i) .gt. 1.0E-25 ) then
               do l = 1,lsed
                  sag(i,l) = flx2day * sag(i,l)  / con(i)
               enddo
            endif
         enddo
         do l = 1,lsed
            write ( lunsdf(l,1) ) itim, sag(1:noseg,l)
         enddo
         cmsedf    = 0.0_fp
         !
         ! resuspension in kg/day
         !
         sag = 0.0
         kfmin = kmax
         do j = 1,mmax
            do i = 1,nmax
               if (kcs(i,j) .eq. 1) then
                  if ( zmodel ) kfmin = kfsmin(i,j)
                  iseg = isaggr(i,j,kfmin)
                  if ( gsqs(i,j) .gt. 1.0E-25 ) then
                     do l = 1,lsed
                        sag(iseg,l) = sag(iseg,l) + cmresf(i,j,l)*gsqs(i,j)
                     enddo
                  endif
               endif
            enddo
         enddo
         do i = 1,noseg
            if ( con(i) .gt. 1.0E-25 ) then
               do l = 1,lsed
                  sag(i,l) = flx2day * sag(i,l) / con(i)
               enddo
            endif
         enddo
         do l = 1,lsed
            write ( lunsdf(l,2) ) itim, sag(1:noseg,l)
         enddo
         cmresf    = 0.0_fp
      endif

!

      end subroutine wrwaqvol
