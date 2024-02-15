!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! 
! 

subroutine readprofilesdef(ja)    ! in afwachting van een module die profieldefinities leest
 USE UNSTRUC_MODEL
 use m_flowgeom
 use m_profiles
 use m_physcoef, only: ifrctypuni, frcuni1D
 use m_missing
 use messagehandling
 use m_alloc
 implicit none
 integer          :: ja

 integer              :: minp, n, nr, ierr, k, L, Lp, nyz, npfx, myzprofs, mxprof
 logical              :: jawel
 character (len=256)  :: rec
 integer, allocatable :: npr2(:)
 double precision     :: yp, zp, base, talud, width, height, zmin

 integer, parameter   :: mx = 2000
 double precision     :: yyh(mx), zzh(mx)

 myzprofs = 0

 rec = trim (md_profdeffile)
 inquire(file = rec , exist=jawel)

 ja = 0
 if (jawel) then

     call oldfil(minp, md_profdeffile )

     mxprof    = nproflocs

     if (allocated (profiles1D) ) deallocate (profiles1D)
     allocate ( profiles1D(mxprof) , stat=ierr)
     call aerr('profiles1D(mxprof)', ierr, 40*nprofdefs)

     allocate ( npr2(mxprof) ) ; npr2 = 0


     profiles1D(1:mxprof)%ityp   = 0
     profiles1D(1:mxprof)%width  = 0
     profiles1D(1:mxprof)%height = 3d3
     profiles1D(1:mxprof)%zmin   = -999


20   read(minp, '(A)', end = 888)  rec
     Lp = index(rec,'PROFNR=')

     if (Lp > 0) then

       read(rec(Lp+7:),*)  nr                        ! profile NR

       n = 0
       do k = 1, nproflocs
          if (npr(k) == nr) then
             if (n == 0) then
                 n = k
             endif
             npr2(k) = n
             npr(k)  = 0
          endif
       enddo

       if (n > 0) then

        L = index(rec,'TYPE=')                       ! profile type
        if (L > 0) then
           read(rec(L+5:),*)  profiles1D(n)%ityp
        endif

        L = index(rec,'WIDTH=')                      ! profile width
        if (L > 0) then
           read(rec(L+6:),*) width
           if (width < 1d-3) then
              call qnerror('profile width too small', rec, ' ')
           endif
           profiles1D(n)%width = width
        endif

        L = index(rec,'HEIGHT=')                     ! profile height
        if (L > 0) then
           read(rec(L+7:),*) height
           if (height < 1d-3) then
              call qnerror('profile height too small', rec, ' ')
           endif
           profiles1D(n)%height = height
        endif

        L = index(rec,'ZMIN=')                      ! profile level
        if (L > 0) then
           read(rec(L+7:),*) zmin
           profiles1D(n)%zmin = zmin
        endif

        L = index(rec,'BASE=')                       ! trapezoid base
        base = 0d0
        if (L > 0) then
           read(rec(L+5:),*) base
        endif

        L = index(rec,'TALUD=')                       ! trapezoid base
        talud = 0d0
        if (L > 0) then
           read(rec(L+6:),*) talud
        endif

        L = index(rec,'FRCTP=')                      ! friction type
        if (L > 0) then
           read(rec(L+6:),*)  profiles1D(n)%frctp
        else
           profiles1D(n)%frctp = -999
        endif

        L = index(rec,'FRCCF=')                      ! friction coefficient, only if type specified
        if (L > 0 .and. profiles1D(n)%frctp .ne. -999) then
           read(rec(L+6:),*)  profiles1D(n)%frccf
        else
           profiles1D(n)%frccf = dmiss
        endif

        if ( profiles1D(n)%ityp == 200 .or. profiles1D(n)%ityp == 201) then  ! todo read true y,z or xyz profile

            if (myzprofs == 0 .and. len( trim(md_profdefxyzfile) ) > 1) then
               call oldfil(myzprofs, md_profdefxyzfile )
            endif

            if (myzprofs == 0) then
               call qnerror('xyzprofile (TYPE= 200 or 201) is referenced, but profdefxyzfile not specified in mdu',' ',' ')
            endif

            nyz = 0
            call readyzprofnr(myzprofs, nr, nyz, yyh, zzh, mx, width, height, zmin)

            if (nyz == 0) then
               call qnerror(' xyzprofile not found ',' ',' ')
            else
               allocate ( profiles1D(n)%y(nyz ), stat = ierr ) ; profiles1D(n)%y = 0d0
               allocate ( profiles1D(n)%z(nyz ), stat = ierr ) ; profiles1D(n)%z = 0d0
               do k  = 1,nyz
                  profiles1D(n)%y(k) = yyh(k)
                  profiles1D(n)%z(k) = zzh(k)
               enddo
               profiles1D(n)%width   = width
               profiles1D(n)%height  = height
               profiles1D(n)%zmin    = zmin
               profiles1D(n)%ityp    = profiles1D(n)%ityp - 100 ! internally only distinguish 100 and 101
            endif
        endif

        if ( profiles1D(n)%ityp == 4 .or. profiles1D(n)%ityp == 5) then  ! V-shape comes as a yz type
            nyz = 3
            allocate ( profiles1D(n)%y(nyz ), stat = ierr )
            allocate ( profiles1D(n)%z(nyz ), stat = ierr )
            do k  = 1,nyz
               profiles1D(n)%y(k) = profiles1D(n)%width * ( dble(k-1)/dble(nyz-1) - 0.5d0 )
               profiles1D(n)%z(k) = profiles1D(n)%height
            enddo
            profiles1D(n)%z(2) = 0d0
            profiles1D(n)%ityp = profiles1D(n)%ityp + 96
        endif

        if ( profiles1D(n)%ityp == 6 .or. profiles1D(n)%ityp == 7) then  ! Trapezoid comes as a yz type
            if (base == 0d0 .and. talud .ne. 0.d0) then
               base = max(0d0, profiles1D(n)%width - 2d0*profiles1D(n)%height*talud)
            endif

            nyz = 4
            allocate ( profiles1D(n)%y(nyz ), stat = ierr )
            allocate ( profiles1D(n)%z(nyz ), stat = ierr )
            profiles1D(n)%y(1) =  -profiles1D(n)%width / 2d0
            profiles1D(n)%y(2) =   profiles1D(n)%y(1) + (profiles1D(n)%width-base) / 2d0
            profiles1D(n)%y(3) =   profiles1D(n)%y(2) + base
            profiles1D(n)%y(4) =   profiles1D(n)%width / 2d0


            profiles1D(n)%z(1) = profiles1D(n)%height
            profiles1D(n)%z(2) = 0d0
            profiles1D(n)%z(3) = 0d0
            profiles1D(n)%z(4) = profiles1D(n)%height
            profiles1D(n)%ityp = profiles1D(n)%ityp + 94
        endif

     endif
     endif

     goto 20


     888 call doclose(minp)
     ja = 1
     do k = 1,nproflocs
        if (npr2(k) == 0) then
            ja = 0
            call mess(LEVEL_info, 'Profloc nr. not found in profdef : ' , npr(k)  )
            npr2(k) = 0
        endif
     enddo
     if (ja == 0) then
         call mess(LEVEL_error, 'Errors in 1D profile references' )
     endif

     npr = npr2
     deallocate (npr2)
     ja  = 1
     if (myzprofs < 0) then
        call doclose(myzprofs)
     endif

 endif

 end subroutine readprofilesdef
