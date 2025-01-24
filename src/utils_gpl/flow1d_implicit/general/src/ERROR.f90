subroutine error (juer ,recsim  ,nr   ,ker)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          General routines Module
!
! Programmer:         J.Brouwer / J.Kuipers
!
! Module:             ERROR (write an ERROR to the error file.)
!
! Module description: Write code and number of the error to the error
!                     file. Also a text containing the name of the sub-
!                     routine name where the error did occur and parame-
!                     ters for the error message to be issued are writ-
!                     ten. Mentioned parameters must be enclosed by @'s.
!
!
! Precondition:       The error file is opened and positioned correctly.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 juer              I  Unit number of error file.
!  4 ker               I  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  3 nr                I  Error number.
!  2 recsim            I  Text containing information about the error
!                         (i.e. Calling subroutine name and parameters
!                         for to be substituted in the actual message).
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: error.pf,v $
! Revision 1.5  1999/06/01  13:42:24  kuipe_j
! names in messages substituted + message template
!
! Revision 1.4  1999/03/15  15:51:17  kuipe_j
! tabs removed
!
! Revision 1.3  1995/09/22  10:02:59  kuipe_j
! variable dimensions, new headers
!
! Revision 1.2  1995/05/30  07:02:24  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:08:28  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/12/02  13:31:55  kuipe_j
! Check on overflow of message file.
!
! Revision 1.2  1993/11/26  15:31:59  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:59  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer    juer  ,nr   ,ker
   character  recsim*(*)

   integer    recmax
   parameter (recmax=50)
!
!     Include sobek error code file
!
   include '..\include\erradm.i'
   include '..\include\filsim.i'
!
!     Local variables
!
   character  rectem*100, rec*120, recdis*250
   integer    modnr, errnr, irec, ios, lendis, lutemp, fnr, i
   logical    templt

   include '..\include\errcod.i'

   if (nr .eq. 0 ) then
!
!        Initial call
!
      nmes   = 0
      nmesmx = 1000
      nmestm = 0
      do i=1,alerperr
         nrerperr(i) = 0
      enddo
      lutemp = 31
      jumes  = 32
      open (lutemp,file=errtem,status='OLD',iostat=ios)
      if (ios.eq.0) then
         open (jumes,status='SCRATCH',access='DIRECT',&
         &form='FORMATTED',recl=100,iostat=ios)
         if (ios.eq.0) then
!             Fill with dummy info
            rec (1:100) = '-DUMMY-'
            do irec=1,8*recmax
               write (jumes,'(a100)',rec=irec,iostat=ios) rec(1:100)
            enddo
            if (ios.eq.0) then
               do while (.true.)
                  rec = ' '
                  read (lutemp,'(a)',iostat=ios) rec
                  if (ios.ne.0) exit
                  if (rec(1:1) .ne. '#') then
                     read (rec(1:5),'(i5)',iostat=ios) fnr
                     if (ios.ne.0) cycle
                     errnr = mod( fnr , 1000)
                     modnr = fnr / 1000
                     irec  = modnr * recmax + errnr
                     write (jumes,'(a100)',rec=irec,iostat=ios)&
                     &rec(18:117)
                     if (ios.ne.0) cycle
                     nmestm = nmestm + 1
                  endif
               enddo
            endif
         endif
         close (lutemp,iostat=ios)
      endif
   else

!        Process error message

      nmes = nmes + 1
      if (nmes .lt. nmesmx .or. ker .eq. fatal) then
         templt = nmestm.gt.0
         if (templt) then
            errnr = mod( nr , 1000)
            modnr = nr / 1000
            irec  = modnr * recmax + errnr
            read (jumes,'(a100)',rec=irec,iostat=ios) rectem
            if (ios.ne.0) templt = .false.
            if (templt.and.rectem(:7).eq.'-DUMMY-') templt = .false.
         endif
         if (templt) then
            call mererr ( rectem, recsim, recdis, lendis)
         else
            lendis = len (recsim)
            recdis(:lendis) = recsim(:lendis)
         endif
         write (juer,'(i6.6,2x,a)') nr*10+ker,recdis(1:lendis)
      else if (nmes .eq. nmesmx) then
         write (juer,'(i6.6,2x,a)') errovf*10+info,&
         &'Too many messages'
      endif
   endif
!
end
