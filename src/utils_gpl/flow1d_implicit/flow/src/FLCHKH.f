      subroutine FLCHKH (ngrid  ,nbran  ,branch ,typcr  ,maxlev ,hlev,
     +                   h      ,juer   ,ker    ,prslot ,psltvr )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLCHKH (FLow CHeck H (Water levels > bottom))
c
c Module description: Determine if all water levels are above the
c                     bottom levels.
c
c                     If the new water levels have been calculated
c                     this routine checks if all water levels are
c                     above the bottom. If not an error message will
c                     generated.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  3 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c  7 h(ngrid)          I  Water level in every grid point at the latest
c                         iteration.
c  6 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
c       maxlev)           - For a circle cross section:
c                         (i,1) = Reference level.
c                         - For a sedredge cross section:
c                         (i,1) = Bed level of main section (i.e. left
c                                 channel).
c                         (i,2) = Bed level of sub section 1 (i.e. right
c                                 channel).
c  8 juer              P  -
c  9 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c  5 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c  2 nbran             I  Number of branches.
c  1 ngrid             I  Number of grid points in network.
c 10 prslot(3,nbran)   I  -
c 11 psltvr(7,ngrid)   I  Preissmann slot variables for every grid point
c                         i (assuring positive water depths):
c                         (1,i) = Value for C**2*R for positive flow.
c                         (2,i) = Value for C**2*R for negative flow.
c                         (3,i) = Bottom of slot (funnel)
c                         (4,i) = Division level between trapezium and
c                                 rectangle of slot (top of rectangle
c                                 and bottom of trapezium)
c                         (5,i) = Top of slot
c                         (6,i) = Bottom width of slot (width of
c                                 rectangle)
c                         (7,i) = Top width of slot (top of trapezium)
c  4 typcr(nbran)      I  Type of cross section used in every branch:
c                         ccrtab (1) : Tabulated cross sections
c                         ccrcir (2) : Circle as cross section
c                         ccrsed (3) : Sedredge cross sections
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c getloc  GET LOCation of gridpoint
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flchkh.pf,v $
c Revision 1.14  1999/06/01  13:42:13  kuipe_j
c names in messages substituted + message template
c
c Revision 1.13  1999/03/15  15:49:38  kuipe_j
c tabs removed
c
c Revision 1.12  1997/06/04  11:18:10  kuipe_j
c Initialize arrays
c
c Revision 1.11  1997/02/17  10:20:48  kuipe_j
c Lateral Q in m3/s in cont equation now
c
c Revision 1.10  1997/01/23  08:29:01  kuipe_j
c Make flow module robust
c
c Revision 1.9  1996/11/05  13:47:44  kuipe_j
c Error in declaration hlev fixed
c
c Revision 1.8  1996/11/04  11:07:16  kuipe_j
c format changed
c
c Revision 1.7  1996/01/17  14:38:14  kuipe_j
c header update
c
c Revision 1.6  1996/01/16  15:01:16  kuipe_j
c Restart improvements
c
c Revision 1.5  1995/11/21  11:07:47  kuipe_j
c Added features are: Special morphology output for IVR; Improvement of
c     auxilliary output; Automatic speudo time stepping; general structure
c     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
c     relation); removal of grid points in messages; etc.
c
c Revision 1.4  1995/09/22  10:01:04  kuipe_j
c variable dimensions, new headers
c
c Revision 1.3  1995/05/30  09:54:52  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:58:48  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:34  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:30:38  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:47  kuipe_j
c Initial version
c
c
c***********************************************************************
c
      use flow_in_datools
c
c     Parameters
c
      integer     ngrid, nbran, maxlev, juer, ker
      integer     typcr(nbran), branch(4,nbran)
      real        FLBOTT, prslot(3,nbran), psltvr(7,ngrid)
      double precision hlev(ngrid,maxlev), h(ngrid)
c
c     Local variables
c
      integer         ibr, igr ,ibrd, lbrnam
      real            eps, xc
      double precision bottom
      character*11    btxt, htxt,xtxt
      character*40    branam
      logical         lslot
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
      include '..\include\sobcon.i'
c
      eps = .0001D0
      do 200 ibr = 1, nbran
         lslot = int(prslot(1,ibr)) .eq. cslena
         do 100 igr = branch(3,ibr), branch(4,ibr)
c
c           Determine bottom
c
            if ( lslot ) then
c
c              Use Preismann slot bottom for check
c
               bottom = dble( psltvr(3,igr) )
            else
               if (typcr(ibr) .eq. ccrsed) then
                  bottom = dble( FLBOTT (igr, ngrid, maxlev, hlev) )
               else
                  bottom = hlev(igr,1)
               endif
            endif
c
c           Check h < bottom
c
            if (h(igr) .lt. bottom  + eps) then
c              write(11,*) 'FLCHKH',igr,psltvr(3,igr),h(igr)
               ker = fatal
               call getloc (igr,ibrd,xc)
               write (htxt,'(f10.3)') h(igr)
               write (btxt,'(f10.3)') bottom
               call getbrn (ibr,branam,lbrnam)
               write (xtxt,'(f10.2)') xc
               if ( .not. da_running_in_da_tools() ) then
                  call ERROR (juer,'FLCHKH @'//branam(:lbrnam)//'@ @'
     +                                       //xtxt//'@ @'
     +                                       //htxt//'@ @'
     +                                       //btxt//'@',
     +                             eflhbo , ker )
               else
c
c                 Water level below bottom may be a result of the
c                 disturbance imposed by DATools.
c                 Report warning, and adjust it.
c
                  ker = warnng
                  call ERROR (juer,'ADJUST @'//branam(:lbrnam)//'@ @'
     +                                       //xtxt//'@ @'
     +                                       //htxt//'@ @'
     +                                       //btxt//'@',
     +                             eflhbo , ker )
                  h(igr) = bottom + 0.0001D0
               endif
            endif
 100     continue
 200  continue
c
      end
