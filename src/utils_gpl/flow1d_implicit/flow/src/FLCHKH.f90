subroutine FLCHKH (ngrid  ,nbran  ,branch ,typcr  ,maxlev ,hlev,&
&h      ,juer   ,ker    ,prslot ,psltvr )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLCHKH (FLow CHeck H (Water levels > bottom))
!
! Module description: Determine if all water levels are above the
!                     bottom levels.
!
!                     If the new water levels have been calculated
!                     this routine checks if all water levels are
!                     above the bottom. If not an error message will
!                     generated.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  3 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  7 h(ngrid)          I  Water level in every grid point at the latest
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
!  9 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  5 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
!  2 nbran             I  Number of branches.
!  1 ngrid             I  Number of grid points in network.
! 10 prslot(3,nbran)   I  -
! 11 psltvr(7,ngrid)   I  Preissmann slot variables for every grid point
!                         i (assuring positive water depths):
!                         (1,i) = Value for C**2*R for positive flow.
!                         (2,i) = Value for C**2*R for negative flow.
!                         (3,i) = Bottom of slot (funnel)
!                         (4,i) = Division level between trapezium and
!                                 rectangle of slot (top of rectangle
!                                 and bottom of trapezium)
!                         (5,i) = Top of slot
!                         (6,i) = Bottom width of slot (width of
!                                 rectangle)
!                         (7,i) = Top width of slot (top of trapezium)
!  4 typcr(nbran)      I  Type of cross section used in every branch:
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
! $Log: flchkh.pf,v $
! Revision 1.14  1999/06/01  13:42:13  kuipe_j
! names in messages substituted + message template
!
! Revision 1.13  1999/03/15  15:49:38  kuipe_j
! tabs removed
!
! Revision 1.12  1997/06/04  11:18:10  kuipe_j
! Initialize arrays
!
! Revision 1.11  1997/02/17  10:20:48  kuipe_j
! Lateral Q in m3/s in cont equation now
!
! Revision 1.10  1997/01/23  08:29:01  kuipe_j
! Make flow module robust
!
! Revision 1.9  1996/11/05  13:47:44  kuipe_j
! Error in declaration hlev fixed
!
! Revision 1.8  1996/11/04  11:07:16  kuipe_j
! format changed
!
! Revision 1.7  1996/01/17  14:38:14  kuipe_j
! header update
!
! Revision 1.6  1996/01/16  15:01:16  kuipe_j
! Restart improvements
!
! Revision 1.5  1995/11/21  11:07:47  kuipe_j
! Added features are: Special morphology output for IVR; Improvement of
!     auxilliary output; Automatic speudo time stepping; general structure
!     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
!     relation); removal of grid points in messages; etc.
!
! Revision 1.4  1995/09/22  10:01:04  kuipe_j
! variable dimensions, new headers
!
! Revision 1.3  1995/05/30  09:54:52  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:58:48  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:34  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:30:38  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:47  kuipe_j
! Initial version
!
!
!***********************************************************************
!
   use flow_in_datools
!
!     Parameters
!
   integer     ngrid, nbran, maxlev, juer, ker
   integer     typcr(nbran), branch(4,nbran)
   real        FLBOTT, prslot(3,nbran), psltvr(7,ngrid)
   double precision hlev(ngrid,maxlev), h(ngrid)
!
!     Local variables
!
   integer         ibr, igr ,ibrd, lbrnam
   real            eps, xc
   double precision bottom
   character*11    btxt, htxt,xtxt
   character*40    branam
   logical         lslot
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
   include '..\include\sobcon.i'
!
   eps = .0001D0
   do 200 ibr = 1, nbran
      lslot = int(prslot(1,ibr)) .eq. cslena
      do 100 igr = branch(3,ibr), branch(4,ibr)
!
!           Determine bottom
!
         if ( lslot ) then
!
!              Use Preismann slot bottom for check
!
            bottom = dble( psltvr(3,igr) )
         else
            if (typcr(ibr) .eq. ccrsed) then
               bottom = dble( FLBOTT (igr, ngrid, maxlev, hlev) )
            else
               bottom = hlev(igr,1)
            endif
         endif
!
!           Check h < bottom
!
         if (h(igr) .lt. bottom  + eps) then
!              write(11,*) 'FLCHKH',igr,psltvr(3,igr),h(igr)
            ker = fatal
            call getloc (igr,ibrd,xc)
            write (htxt,'(f10.3)') h(igr)
            write (btxt,'(f10.3)') bottom
            call getbrn (ibr,branam,lbrnam)
            write (xtxt,'(f10.2)') xc
            if ( .not. da_running_in_da_tools() ) then
               call ERROR (juer,'FLCHKH @'//branam(:lbrnam)//'@ @'&
               &//xtxt//'@ @'&
               &//htxt//'@ @'&
               &//btxt//'@',&
               &eflhbo , ker )
            else
!
!                 Water level below bottom may be a result of the
!                 disturbance imposed by DATools.
!                 Report warning, and adjust it.
!
               ker = warnng
               call ERROR (juer,'ADJUST @'//branam(:lbrnam)//'@ @'&
               &//xtxt//'@ @'&
               &//htxt//'@ @'&
               &//btxt//'@',&
               &eflhbo , ker )
               h(igr) = bottom + 0.0001D0
            endif
         endif
100   continue
200 continue
!
end
