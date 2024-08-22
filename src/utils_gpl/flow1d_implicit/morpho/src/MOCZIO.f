      subroutine MOCZIO ( nbran  ,ngrid  ,maxlev ,branch ,nlev   ,
     +                    hlev   ,typcr  ,h      ,
     +                    juer   ,ker    ,prslot )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Morphology module
c
c Programmer:         S.L. van der Woude
c
c Module:             MOCZIO (MOrphology Check Z Increasing Order)
c
c Module description: Check if cross section levels are still in increa-
c                     sing order and check if water levels are still >
c                     bottoms for branches without a preissmann slot.
c
c                     It is possible that for ill defined cross sections
c                     a level becomes higher than a level defined above
c                     this level. This condition is checked because
c                     routines which calculate areas and hydraulic radia
c                     can not process these cross sections.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  4 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c  8 h(ngrid)          I  Water level in every grid point at the latest
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
c  9 ker               IO Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c  3 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c  1 nbran             I  Number of branches.
c  2 ngrid             I  Number of grid points in network.
c  5 nlev(ngrid)       I  Number of h-levels for every cross section.
c                         (For a circle cross section   : 1 ;
c                          For a sedredge cross section : 2 )
c 10 prslot(3,nbran)   I  -
c  7 typcr(nbran)      I  Type of cross section used in every branch:
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
c $Log: moczio.pf,v $
c Revision 1.9  1999/06/01  13:42:31  kuipe_j
c names in messages substituted + message template
c
c Revision 1.8  1999/03/15  15:52:48  kuipe_j
c tabs removed
c
c Revision 1.7  1997/01/23  08:29:47  kuipe_j
c Make flow module robust
c
c Revision 1.6  1996/01/17  13:18:20  kuipe_j
c header update
c
c Revision 1.5  1996/01/16  15:01:36  kuipe_j
c Restart improvements
c
c Revision 1.4  1995/11/21  11:09:00  kuipe_j
c Added features are: Special morphology output for IVR; Improvement of
c     auxilliary output; Automatic speudo time stepping; general structure
c     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
c     relation); removal of grid points in messages; etc.
c
c Revision 1.3  1995/05/30  09:55:48  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:04:35  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:04  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:32:28  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:06  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Parameters
c
      integer     nbran,
     +            ngrid,
     +            maxlev,
     +            juer,
     +            ker

      integer     branch (4,nbran),
     +            nlev   (ngrid),
     +            typcr  (nbran)

      real        prslot (3,nbran)
      double precision hlev   (ngrid,maxlev), h(ngrid)
c
c     Local variables
c
      integer     ibr, igp, ilev, lbrnam
      real        bottom, level, xc
c  double precision bottom
      logical     fault
      character*9       btxt, htxt
      character*11      xtxt
      character*4       ttxt
      character*40      branam
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
      include '..\include\sobcon.i'
c
c     Check if adapted levels are in increasing order
c
      do 300 ibr = 1, nbran
c
c        Check for tabulated cross sections
c
         if (typcr(ibr) .eq. ccrtab) then
c
c           Do for each grid point
c
            do 200 igp = branch(3,ibr), branch(4,ibr)
c
c              Read lowest level and set fault flag
c
               level = hlev(igp,1)
               fault = .false.
c
c              Do for each level
c
               do 100 ilev = 2, nlev(igp)
                  if (level .lt. hlev(igp,ilev)) then
                     level = hlev(igp,ilev)
                  else
                     fault = .true.
                  endif
 100           continue
c
c              If fault write error message to log file
c
               if (fault) then
                  ker  = fatal
                  call getloc (igp,ibr,xc)
                  call getbrn (ibr,branam,lbrnam)
                  write(xtxt,'(f10.2)') xc
                  call ERROR (juer,'MOCZIO @' //branam(:lbrnam)//'@ @'
     +                                        // xtxt //'@',
     +                             emolev, ker )
c
c                 Info: Write current levels to log file
c
                  do 150 ilev = 1, nlev(igp)
                     write (ttxt,'(i4)'  ) ilev
                     write (htxt,'(f8.3)') hlev(igp,ilev)
                     call ERROR (juer,'MOCZIO @'//ttxt//'@ @'
     +                                          //htxt//'@',
     +                                emocrl , info )
 150              continue
               endif
 200        continue
         endif
 300  continue
c
c     If no errors found check if h < bottom
c
      if (ker .ne. fatal) then
c
         do 500 ibr = 1, nbran
c
c           Only check branches without a slot
c
            if (int(prslot(1,ibr)) .eq. csldis) then
c
               do 400 igp = branch(3,ibr), branch(4,ibr)
c
c                 Determine bottom (sedredge left or right)
c
                  if (typcr(ibr) .eq. ccrsed) then
                     bottom = min( hlev(igp,1), hlev(igp,2) )
                  else
                     bottom = hlev(igp,1)
                  endif
c
c                 Check h < bottom
c
                  if (h(igp) .lt. bottom) then
                     ker = fatal
                     call getloc (igp,ibr,xc)
                     write (htxt,'(f8.3)') h(igp)
                     write (btxt,'(f8.3)') sngl( bottom )
                     call getbrn (ibr,branam,lbrnam)
                     write (xtxt,'(f10.2)') xc
                     call ERROR (juer,'MOCZIO @'//branam(:lbrnam)//'@ @'
     +                                          //xtxt//'@ @'
     +                                          //htxt//'@ @'
     +                                          //btxt//'@',
     +                                emohbo , ker )
                  endif
 400           continue
            endif
 500     continue
      endif
c
      end
