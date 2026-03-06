subroutine wqfrto ( nposeg ,slen  ,npntr ,pntr ,tofrom ,&
&exdef  ,nexdef)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Water Quality Interface Module
!
! Programmer:         S.L. van der Woude
!
! Module:             WQFRTO (Water Quality FRom and TO lengths)
!
! Module description: This routine calculates the "from" and "to"
!                     lengths for each segment exchange.
!
!                     The routine uses the pointer table. Each exchange
!                     is defined by a "from" segment number and a "to"
!                     segment number. A "from" length is half the seg-
!                     ment length of a "from" segment number and a "to"
!                     length is half the segment length of a "to" seg-
!                     ment number. A special case is formed by a negati-
!                     ve segment number (boundary). In that case the
!                     "from" and "to" lengths are each half the length
!                     of the positive segment length. The segment
!                     lengths have already been calculated by routine
!                     WQSLEN.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  3 npntr             I  Number of entries in pntr table.
!  1 nposeg            I  Number of positive segment numbers.
!  4 pntr(4,npntr)     I  Definition of the pointer table. From here it
!                         is possible to find the exchanges between the
!                         segments and the starting location in the
!                         exdef array.
!                         (1,j) = From segment number.
!                         (2,j) = To segment number.
!                         (3,j) = Pointer to exchange table exdef.
!                         (4,j) = Number of exchange definitions in ex-
!                                 def.
!  2 slen(nposeg)      I  Lengths for each segment.
!  5 tofrom(2,npntrs)  O  To & From lengths for exchange.
!                         1 = To length
!                         2 = From length
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
! $Log: wqfrto.pf,v $
! Revision 1.4  1999/03/12  12:42:21  kuipe_j
! parallel segments added
!
! Revision 1.3  1996/11/12  15:08:58  kuipe_j
! Interchange to and from
!
! Revision 1.2  1995/05/30  07:08:32  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:54  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:35:42  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:28  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Parameters
!
   integer   npntr, nposeg ,nexdef

   integer   pntr  (4,npntr)

   real      tofrom (2,npntr),&
   &slen   (nposeg) ,&
   &exdef  (6,nexdef)
!
!     Variables
!
   integer   ifrom, ito,   iexch
   real      lenfr, lento
!
   include '..\include\sobcon.i'

!     Loop over segment exchanges
!
   do 100 iexch = 1, npntr
!
!       Read "from" and "to" segment numbers
!
      ifrom = pntr (1,iexch)
      ito   = pntr (2,iexch)

      if (int(exdef(1,pntr(3,iexch))).eq.cexbsc) then

!         Lengths between seperated segments are set to .5

         tofrom (1,iexch) = 0.5
         tofrom (2,iexch) = 0.5
      else
!
!         Read "from" and "to" segment numbers
!
         ifrom = pntr (1,iexch)
         ito   = pntr (2,iexch)
!
!         Read segment lengths for both segments
!
         if (ifrom .gt. 0) then
            lenfr = slen (ifrom)
         else
            lenfr = 0.
         endif
         if (ito   .gt. 0) then
            lento = slen (ito)
         else
            lento = 0.
         endif
!
!         Calculate from and to lengths
!
         if (ito .gt. 0 .and. ifrom .gt. 0 ) then
            tofrom (1,iexch) = lenfr / 2.
            tofrom (2,iexch) = lento / 2.
         elseif (ito .lt. 0 .and. ifrom .gt. 0 ) then
            tofrom (1,iexch) = lenfr / 2.
            tofrom (2,iexch) = lenfr / 2.
         elseif (ito .gt. 0 .and. ifrom .lt. 0 ) then
            tofrom (1,iexch) = lento / 2.
            tofrom (2,iexch) = lento / 2.
         else
            tofrom (1,iexch) = 1.0
            tofrom (2,iexch) = 1.0
         endif
      endif

100 continue
!
   return
end
