subroutine MOCZIO ( nbran  ,ngrid  ,maxlev ,branch ,nlev   ,&
&hlev   ,typcr  ,h      ,&
&juer   ,ker    ,prslot )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:         S.L. van der Woude
!
! Module:             MOCZIO (MOrphology Check Z Increasing Order)
!
! Module description: Check if cross section levels are still in increa-
!                     sing order and check if water levels are still >
!                     bottoms for branches without a preissmann slot.
!
!                     It is possible that for ill defined cross sections
!                     a level becomes higher than a level defined above
!                     this level. This condition is checked because
!                     routines which calculate areas and hydraulic radia
!                     can not process these cross sections.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  4 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  8 h(ngrid)          I  Water level in every grid point at the latest
!                         iteration.
!  6 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
!       maxlev)           - For a circle cross section:
!                         (i,1) = Reference level.
!                         - For a sedredge cross section:
!                         (i,1) = Bed level of main section (i.e. left
!                                 channel).
!                         (i,2) = Bed level of sub section 1 (i.e. right
!                                 channel).
!  8 juer              P  -
!  9 ker               IO Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  3 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
!  1 nbran             I  Number of branches.
!  2 ngrid             I  Number of grid points in network.
!  5 nlev(ngrid)       I  Number of h-levels for every cross section.
!                         (For a circle cross section   : 1 ;
!                          For a sedredge cross section : 2 )
! 10 prslot(3,nbran)   I  -
!  7 typcr(nbran)      I  Type of cross section used in every branch:
!                         ccrtab (1) : Tabulated cross sections
!                         ccrcir (2) : Circle as cross section
!                         ccrsed (3) : Sedredge cross sections
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! getloc  GET LOCation of gridpoint
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: moczio.pf,v $
! Revision 1.9  1999/06/01  13:42:31  kuipe_j
! names in messages substituted + message template
!
! Revision 1.8  1999/03/15  15:52:48  kuipe_j
! tabs removed
!
! Revision 1.7  1997/01/23  08:29:47  kuipe_j
! Make flow module robust
!
! Revision 1.6  1996/01/17  13:18:20  kuipe_j
! header update
!
! Revision 1.5  1996/01/16  15:01:36  kuipe_j
! Restart improvements
!
! Revision 1.4  1995/11/21  11:09:00  kuipe_j
! Added features are: Special morphology output for IVR; Improvement of
!     auxilliary output; Automatic speudo time stepping; general structure
!     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
!     relation); removal of grid points in messages; etc.
!
! Revision 1.3  1995/05/30  09:55:48  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:04:35  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:04  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:32:28  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:06  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Parameters
!
   integer     nbran,&
   &ngrid,&
   &maxlev,&
   &juer,&
   &ker

   integer     branch (4,nbran),&
   &nlev   (ngrid),&
   &typcr  (nbran)

   real        prslot (3,nbran)
   double precision hlev   (ngrid,maxlev), h(ngrid)
!
!     Local variables
!
   integer     ibr, igp, ilev, lbrnam
   real        bottom, level, xc
!  double precision bottom
   logical     fault
   character*9       btxt, htxt
   character*11      xtxt
   character*4       ttxt
   character*40      branam
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
   include '..\include\sobcon.i'
!
!     Check if adapted levels are in increasing order
!
   do 300 ibr = 1, nbran
!
!        Check for tabulated cross sections
!
      if (typcr(ibr) .eq. ccrtab) then
!
!           Do for each grid point
!
         do 200 igp = branch(3,ibr), branch(4,ibr)
!
!              Read lowest level and set fault flag
!
            level = hlev(igp,1)
            fault = .false.
!
!              Do for each level
!
            do 100 ilev = 2, nlev(igp)
               if (level .lt. hlev(igp,ilev)) then
                  level = hlev(igp,ilev)
               else
                  fault = .true.
               endif
100         continue
!
!              If fault write error message to log file
!
            if (fault) then
               ker  = fatal
               call getloc (igp,ibr,xc)
               call getbrn (ibr,branam,lbrnam)
               write(xtxt,'(f10.2)') xc
               call ERROR (juer,'MOCZIO @' //branam(:lbrnam)//'@ @'&
               &// xtxt //'@',&
               &emolev, ker )
!
!                 Info: Write current levels to log file
!
               do 150 ilev = 1, nlev(igp)
                  write (ttxt,'(i4)'  ) ilev
                  write (htxt,'(f8.3)') hlev(igp,ilev)
                  call ERROR (juer,'MOCZIO @'//ttxt//'@ @'&
                  &//htxt//'@',&
                  &emocrl , info )
150            continue
            endif
200      continue
      endif
300 continue
!
!     If no errors found check if h < bottom
!
   if (ker .ne. fatal) then
!
      do 500 ibr = 1, nbran
!
!           Only check branches without a slot
!
         if (int(prslot(1,ibr)) .eq. csldis) then
!
            do 400 igp = branch(3,ibr), branch(4,ibr)
!
!                 Determine bottom (sedredge left or right)
!
               if (typcr(ibr) .eq. ccrsed) then
                  bottom = min( hlev(igp,1), hlev(igp,2) )
               else
                  bottom = hlev(igp,1)
               endif
!
!                 Check h < bottom
!
               if (h(igp) .lt. bottom) then
                  ker = fatal
                  call getloc (igp,ibr,xc)
                  write (htxt,'(f8.3)') h(igp)
                  write (btxt,'(f8.3)') sngl( bottom )
                  call getbrn (ibr,branam,lbrnam)
                  write (xtxt,'(f10.2)') xc
                  call ERROR (juer,'MOCZIO @'//branam(:lbrnam)//'@ @'&
                  &//xtxt//'@ @'&
                  &//htxt//'@ @'&
                  &//btxt//'@',&
                  &emohbo , ker )
               endif
400         continue
         endif
500   continue
   endif
!
end
