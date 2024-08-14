      subroutine wqfrto ( nposeg ,slen  ,npntr ,pntr ,tofrom ,
     +                    exdef  ,nexdef)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Water Quality Interface Module
c
c Programmer:         S.L. van der Woude
c
c Module:             WQFRTO (Water Quality FRom and TO lengths)
c
c Module description: This routine calculates the "from" and "to"
c                     lengths for each segment exchange.
c
c                     The routine uses the pointer table. Each exchange
c                     is defined by a "from" segment number and a "to"
c                     segment number. A "from" length is half the seg-
c                     ment length of a "from" segment number and a "to"
c                     length is half the segment length of a "to" seg-
c                     ment number. A special case is formed by a negati-
c                     ve segment number (boundary). In that case the
c                     "from" and "to" lengths are each half the length
c                     of the positive segment length. The segment
c                     lengths have already been calculated by routine
c                     WQSLEN.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  3 npntr             I  Number of entries in pntr table.
c  1 nposeg            I  Number of positive segment numbers.
c  4 pntr(4,npntr)     I  Definition of the pointer table. From here it
c                         is possible to find the exchanges between the
c                         segments and the starting location in the
c                         exdef array.
c                         (1,j) = From segment number.
c                         (2,j) = To segment number.
c                         (3,j) = Pointer to exchange table exdef.
c                         (4,j) = Number of exchange definitions in ex-
c                                 def.
c  2 slen(nposeg)      I  Lengths for each segment.
c  5 tofrom(2,npntrs)  O  To & From lengths for exchange.
c                         1 = To length
c                         2 = From length
c=======================================================================
c
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: wqfrto.pf,v $
c Revision 1.4  1999/03/12  12:42:21  kuipe_j
c parallel segments added
c
c Revision 1.3  1996/11/12  15:08:58  kuipe_j
c Interchange to and from
c
c Revision 1.2  1995/05/30  07:08:32  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:54  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:35:42  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:28  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Parameters
c
      integer   npntr, nposeg ,nexdef

      integer   pntr  (4,npntr)

      real      tofrom (2,npntr),
     +          slen   (nposeg) ,
     +          exdef  (6,nexdef)
c
c     Variables
c
      integer   ifrom, ito,   iexch
      real      lenfr, lento
c
      include '..\include\sobcon.i'

c     Loop over segment exchanges
c
      do 100 iexch = 1, npntr
c
c       Read "from" and "to" segment numbers
c
        ifrom = pntr (1,iexch)
        ito   = pntr (2,iexch)

        if (int(exdef(1,pntr(3,iexch))).eq.cexbsc) then

c         Lengths between seperated segments are set to .5

          tofrom (1,iexch) = 0.5
          tofrom (2,iexch) = 0.5
        else
c
c         Read "from" and "to" segment numbers
c
          ifrom = pntr (1,iexch)
          ito   = pntr (2,iexch)
c
c         Read segment lengths for both segments
c
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
c
c         Calculate from and to lengths
c
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

 100  continue
c
      return
      end
