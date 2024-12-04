!
! Lezen lozingen/onttrekkingen van uitwisselingsbestand
!
Subroutine MOZREADQ (qlat, nqlat, qlatid, qltpar, juer)

   integer      nqlat, juer
   real         qltpar(9,*), qlat(nqlat,*), qstat
   character*40 qlatid(*)
   integer      mozsobnodid

   character path*127
   CHARACTER FullName*250
   CHARACTER EntityName*20

   CHARACTER cNodeId*40, cNodeql*40(8)
   CHARACTER cDwCode*40, cDwCodeOld*40
   CHARACTER cPriority*4
   CHARACTER cUserCode*3
   CHARACTER cType*3
   CHARACTER cTimeBegin*15
   CHARACTER cTimeEnd*15
   DOUBLE precision dDwAlloc
   INTEGER i, nfm, nqln, nEr, istat, ii
   real    othdis(8), othext(8), dfldis(8), dflext(8)
   real    d
   INTEGER nE, n, nDwCode, nPriority
   logical r

   nfm = 0
   write(juer,'('' start mozreadq'')')

   open (120, file = 'Sbmzkop.fnm')
10 read (120,'(a)') path
   if(path(1:1) .eq. '*') then
      goto 10
   else
      nfm = nfm + 1
      if (nfm .lt. 8) goto 10
      read (path,*) fullname
   endif
   close (120)
!     write(juer,'(1x,a40)') fullname

   EntityName = 'NwDwEx'
   n = 0

!      write (cTB,'(i4,a,i4.4,a)') TB/10000, '.',
!     &                         TB - (TB/10000 * 10000),
!     &                        '000000'
!      write (cTE,'(i4,a,i4.4,a)') TE/10000, '.',
!     &                         TE - (TE/10000 * 10000),
!     &                        '000000'

!      WRITE(FullName,'(3a)') path(1:strlen(path)),
!    &                       EntityName(1:strlen(EntityName)),'D'
!      fullname = 'NwDwEx'

   nE = 0
   call OPNUWB (nE,FullName,'READ')
   if (nE .ne. 0) then
      call ErrOut('OPNUWB',nE)
   end if

   cDwCodeOld = ' '
   r = .true.
   nqln = 0
   do i = 1, 8
      othdis(i) = 0.
      othext(i) = 0.
      dfldis(i) = 0.
      dflext(i) = 0.
   end do

   do while (r)
      if (n .eq. 0) then
         CALL RDREC (nEr, FullName, EntityName, 'FIRST', ' ')
      else
         CALL RDREC (nEr, FullName, EntityName, 'NEXT', ' ')
      endif
      r = (r .and. nEr .eq. 0)
      call RDVLD_SC (nE, FullName, EntityName, 'flbo_id',cNodeId)
      r = (r .and. nE .eq. 0)
      call RDVLD_SC (nE, FullName, EntityName, 'DwCode', cDwCode)
      r = (r .and. nE .eq. 0)

      if ((cDwCode .ne. cDwCodeOld .and. n .gt.0) .or.&
      &nEr .eq. 5004) then
         do i = 1, nqln

            istat = mozsobnodid (cNodeql(i), nqlat, qltpar, qlatid)

            if (istat .gt. 0) then

               qstat = othdis(i) - othext(i) + dfldis(i) - dflext(i)
               write(juer,'('' Temp. alleen '',1x,a10,f10.2)')&
               &cNodeql(i),qstat
! JC 12/4/2000
!
!    Ingelezen waarde vasthouden in qlat(istat,3)

               qlat(istat,3) = qstat

            endif
         end do
         do i = 1, nqln
            othdis(i) = 0.
            othext(i) = 0.
            dfldis(i) = 0.
            dflext(i) = 0.
         end do
         nqln = 0
      endif
      cDwCodeOld = cDwCode

      call RDVLD_SC (nE, FullName, EntityName, 'Prio', cPriority)
      r = (r .and. nE .eq. 0)
      call RDVLD_SC(nE, FullName, EntityName,'UserCode',cUserCode)
      r = (r .and. nE .eq. 0)
      call RDVLD_SC (nE, FullName, EntityName, 'Type', cType)
      r = (r .and. nE .eq. 0)
      call RDVLD_SC (nE, FullName, EntityName,'TBegin',cTimeBegin)
      r = (r .and. nE .eq. 0)
      call RDVLD_SC (nE, FullName, EntityName, 'TEnd', cTimeEnd)
      r = (r .and. nE .eq. 0)
! JC 12/4/2000: Demand gewijzigd in Alloc (op meerdere plaatsen
      call RDVLD_D (nE, FullName, EntityName, 'DwAlloc', dDwAlloc)
      r = (r .and. nE .eq. 0)

      if (r) n=n+1
!        write (*,*) n
!        if (r .and. (cTimeBegin .eq. cTB .and. cTimeEnd .eq. cTE)) then

      if (r) then

         read (cDwCode,'(i4)')  nDwCode
         read (cPriority,'(i4)') nPriority

         d = sngl(dDwAlloc)
         ii = 0
         do i = 1, nqln
            ii =i
            if(cNodeId .eq. cNodeql(i)) goto 20
         end do
         nqln = nqln + 1
         cNodeql(nqln) = cNodeId
         ii  = nqln
!   Lozingen en onttrekkingen
20       if (cUserCode .eq. 'oth') then

            if (cType .eq. 'DIS') othdis(ii) = d

            if (cType .eq. 'EXT') othext(ii) = d

         else if(cUserCode .eq. 'dfl')then

            if (cType .eq. 'DIS') dfldis(ii) = d

            if (cType .eq. 'EXT') dflext(ii) = d

         END if

      end if

   END do
!
   call CLSUWB(nE,FullName)

end

subroutine ErrOut(cMsg, nErrCode)

   character *(*)cMsg
   integer nErrCode
   write (*,*) cMsg
   if (nErrCode .ne. -1) then
      write (*,'(a,i6,a)') '(code :',nErrCode,')'
   else
      WRITE(*,*) '(code unknown)'
   end if
   STOP 'Program aborted ...'
end
