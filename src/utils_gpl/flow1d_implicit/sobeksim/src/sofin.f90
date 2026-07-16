subroutine sofin(frobuf, strcpu, lfrou, lwstat, juer, inocon,&
&lustat, lurtn, itstat, ker)
   implicit none

   include '..\include\filsim.i'

   real endcpu, frobuf(8), xc, strcpu, cdum
   logical lfrou, lwstat
   integer ker1, juer, ibr, lbrnam, inocon, ker, nstru, ngrid,lustat,&
   &lurtn, strpar, strtyp, hpack, h2, itstat(4)
   character     ftxt*5, ttxt*6, itxt*4, xtxt*10, txtbuf*100
   character*40  branam
   character*18  txt
!
!     External functions
!
   integer, external       ::  gtipnt, gtrpnt, gtdpnt

   include '..\include\errcod.i'
   include '..\include\mempool.i'

#if defined (USE_MSWINDOWS)

   call CPU_TIME ( endcpu )

#endif
!
!        Write reference to froude-file
!
   if (lfrou) then
      ker1 = warnng
      call error(juer,'High Froude numbers (See froude file)',&
      &eflhfn,ker1)
!
      ker1 = info
      write(ftxt,'(f5.2)') frobuf(1)
      call error(juer,'Maximum Froude number in cell =@'//ftxt//'@',&
      &efroce,ker1)
      call getloc (nint(frobuf(4)),ibr,xc)
      write(ttxt,'(i6)') nint(frobuf(2))
      write(itxt,'(i4)') nint(frobuf(3))
      call getbrn (ibr,branam,lbrnam)
      write(xtxt,'(f10.2)') xc
      txtbuf ='(Time step@'//ttxt//'@ Iter. step@'//itxt//&
      &'@ Branch@'//branam(:lbrnam)//'@ X=@'//xtxt//'@ )'
      call error(juer,txtbuf,efrotl,ker1)
!
      write(ftxt,'(f5.2)') frobuf(5)
      call error(juer,'Maximum Froude number in grid point =@'//&
      &ftxt//'@',efrogr,ker1)
      call getloc (nint(frobuf(8)),ibr,xc)
      write(ttxt,'(i6)') nint(frobuf(6))
      write(itxt,'(i4)') nint(frobuf(7))
      call getbrn (ibr,branam,lbrnam)
      write(xtxt,'(f10.2)') xc
      txtbuf ='(Time step@'//ttxt//'@ Iter. step@'//itxt//&
      &'@ Branch@'//branam(:lbrnam)//'@ X=@'//xtxt//'@ )'
      call error(juer,txtbuf,efrotl,ker1)
   endif
!
!        Write number of timesteps continued without convergence
!
   if ( inocon .gt. 0 .and. ker .ne. fatal) then
      ker1 = warnng
      write(txt,'(2(1x,i8))') inocon
      call error(juer,&
      &'Number of timesteps without conv.@'//txt//'@ (See residu file)'&
      &,efltwc,ker1)
   endif
!
!       Generate data base structure warnings, if any
!
   if ( ker .ne. fatal) then
      nstru  = ip (gtipnt ( 'NSTRU' ))
      ngrid  = ip (gtipnt ( 'NGRID' ))
      strpar =     gtrpnt ( 'STRPAR')
      strtyp =     gtipnt ( 'STRTYP')
      hpack  =     gtdpnt ( 'HPACK')
      h2 = hpack + ngrid * 2
      call fltser (1      ,nstru  ,ngrid  ,rp(strpar) ,ip(strtyp),&
      &dp(h2) ,ker    ,juer   )
   endif
!
!     Rougness checking. Generate message if necessary
!
   if ( ker .ne. fatal)&
   &call flroulim (-4 ,cdum ,juer,ker)
!
!     Write statistical information on iteration steps to log file
!
   if (itstat(4) .gt. 0) then
      ker1 = info
      write(txt,'(2(1x,i8))') itstat(1)
      call error(juer,&
      &'Minumum number of iterations @'//txt//'@',&
      &eflmii,ker1)
      write(txt,'(2(1x,i8))') itstat(2)
      call error(juer,&
      &'Maximum number of iterations @'//txt//'@',&
      &eflmxi,ker1)
      write(txt,'(2(1x,i8))') int(real(itstat(3))/&
      &real(itstat(4))+.5)
      call error(juer,&
      &'Mean number of iterations @'//txt//'@',&
      &eflmni,ker1)
   endif
!
!     Close error log file and status file
!
   close ( unit = juer )
   if (lwstat) then
      if (ker .eq. fatal) then
         write ( lustat, '(A)' ) 'ERROR'
      else
         write ( lustat, '(A)' ) 'END'

#if defined (USE_MSWINDOWS)

         write ( lustat, '(A,F10.2)' ) 'CPU-time in sec: ',&
         &endcpu-strcpu
#endif
      endif
      if (itstat(4) .gt. 0) then
         open ( unit = lurtn, file = rtncod )
         write (lurtn,'(a)') '0'
         close (lurtn)
      endif
      close ( unit = lustat )
   endif
end
