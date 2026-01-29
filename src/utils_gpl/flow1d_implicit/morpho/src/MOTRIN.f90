subroutine MOTRIN (filnam)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:         S.L. van der Woude
!
! Module:             MOTRIN (MOrphology TRaject definition INput file)
!
! Module description: Reading of file with input for the traject
!                     definition table.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 filnam            I  File name without extension.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! socnam  SObek Create NAMe
!=======================================================================
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: motrin.pf,v $
! Revision 1.5  1999/03/15  15:53:08  kuipe_j
! tabs removed
!
! Revision 1.4  1998/02/13  12:06:43  kuipe_j
! Adapt to CMT ; absolute file name
!
! Revision 1.3  1997/06/17  11:27:01  kuipe_j
! output in history format
!
! Revision 1.2  1997/06/04  11:13:49  kuipe_j
! Protection for short file name
!
! Revision 1.1  1995/10/18  09:00:08  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
!
!***********************************************************************
!
   include '..\include\filsim.i'
!
!     Declaration of parameters
!
   character*(*) filnam
!
!     Declaration of local variables
!
   character name*256, crefil*256
   character ext*4
   character nr*2
   character line*80
   integer   i, ios, lnam, lusc, lutd, lutr
   logical   loop
   integer   lt  , lg , idir
!
   parameter (lutd=70)
!
   integer dmtrjt
   parameter (dmtrjt=20)
   integer ntrjct , versie
   integer trjct(dmtrjt), brnch(dmtrjt)
   real    xb(dmtrjt), xe(dmtrjt), nw(dmtrjt), nd(dmtrjt)
   common /normtb/ ntrjct, trjct, brnch, xb, xe, nw, nd , versie
!
!     Open traject definition file
!
   lnam = index (filnam, ' ')
   if (lnam.gt.4) then
      lusc = index (filnam(lnam-4:lnam), '_')
   else
      lusc = 0
   endif
   if ( lusc .ne. 0 ) then
      lnam = lnam-4 + lusc-1
   endif
!
   if (trainp .eq. ' ') then
      ext = '.tra'
      call socnam ( filnam(1:lnam-1), crefil, ext )
      trainp = crefil
   endif
   open ( unit = lutd, file = trainp ,status='old', iostat = ios )
!
   if ( ios .ne. 0 ) then
      ntrjct = 0
   else
!
!        There are two versions for HIS-files. In SOBEK the old version
!        will be used. So, versie = 0
!
      versie = 0
!
!        Skip 8 header lines (fixed)
!
      do 10 i=1,8
         read(lutd,'(a)') line
10    continue
!
      loop = .true.
      i    = 0
20    continue
      if (loop) then
         i = i + 1
         read (lutd,*,iostat=ios) trjct(i), brnch(i), xb(i), xe(i),&
         &nw(i), nd(i)
         if ( ios .lt. 0 ) then
            ntrjct = i-1
            loop   = .false.
         endif
         goto 20
!        =======
      endif
!
      ext     = '.sto'
      lnam    = index (filnam, ' ')
!
      do 30 i = 1, ntrjct
         write (nr(1:2),'(i2.2)') i
         name = filnam(1:lnam-1)//nr
         call socnam ( name, crefil, ext )
         lutr = 70+i
         open (unit=lutr,file=crefil,status='unknown',iostat=ios)
30    continue

!        Open traject HIS file ( <file>_t.his )

      lutr = 70+ntrjct+1
      if (traout .eq. ' ') then
         ext  = '.his'
         name = filnam(1:lnam-1)
         idir = 0
         do 35 i=1,lnam
            if (name(i:i).eq.'\' .or. name(i:i).eq.'/') then
               idir = i
            endif
35       continue
         lt   = min (lnam,idir+7)
         name(lt:lt+1)='_t'
         call socnam ( name, crefil, ext )
         traout = crefil
      endif

!
#if defined (USE_MSWINDOWS)
      open ( lutr, file=traout, form='binary')
#else
#if defined (USE_HPUX)
      open ( lutr, file=traout, form='unformatted')
#else
      open ( lutr, file=traout, form='unformatted')
#endif
#endif
!
!        Open grid HIS file ( <file>_g.his )
!        (only along trajects)

      lutr = 70+ntrjct+2
      if (griout .eq. ' ') then
         ext          = '.his'
         name         = filnam(1:lnam-1)
         lg           = lt
         name(lg:lg+1)='_g'
         call socnam ( name, crefil, ext )
         griout = crefil
      endif
#if defined (USE_MSWINDOWS)
      open ( lutr, file=griout, form='binary')
#else
#if defined (USE_HPUX)
      open ( lutr, file=griout, form='unformatted')
#else
      open ( lutr, file=griout, form='unformatted')
#endif
#endif
   endif
   return
end
