subroutine CSSECT(ngrid  ,nbran  ,branch ,typcr  ,&
&maxlev ,nlev   ,hlev   ,&
&wft    ,aft    ,of     ,&
&subsec ,secth0 ,secth1 ,&
&wfh0   ,afh0   ,oh0    ,&
&wfh1   ,afh1   ,oh1    ,&
&psltvr ,secths ,ws     ,lsedt  )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Cross Sectional Table Module
!
! Programmer:         J.Brouwer
!
! Module:             CSSECT (Cross Section SECTional parameters)
!
! Module description: In this subroutine the time-independent parameters
!                     Af(h0) = flow area in the main section for h=h0,
!                     and
!                     Af(h1) = flow area in sub section 1 for h=h1, will
!                     be computed.
!
!                     In subroutine CSSECT for each cross section the
!                     constant flow area Af(h0) and the wetted perimeter
!                     O0 are computed for the main section. The parame-
!                     ters Af(h1) and O1 are computed for sub section 1.
!                     Parameters computed in this subroutine are applied
!                     in the computation of Boussinesq's constant and
!                     the Chezy friction (See subroutine FLBOCH in flow
!                     module).
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 15 afh0              P  -
! 18 afh1              P  -
!  9 aft               P  -
!  3 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  7 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
!       maxlev)           - For a circle cross section:
!                         (i,1) = Reference level.
!                         - For a sedredge cross section:
!                         (i,1) = Bed level of main section (i.e. left
!                                 channel).
!                         (i,2) = Bed level of sub section 1 (i.e. right
!                                 channel).
!  5 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
!  2 nbran             I  Number of branches.
!  1 ngrid             I  Number of grid points in network.
!  6 nlev(ngrid)       I  Number of h-levels for every cross section.
!                         (For a circle cross section   : 1 ;
!                          For a sedredge cross section : 2 )
! 10 of                P  -
! 16 oh0(ngrid)        I  Wetted perimeter Ot at water level h=h0 for
!                         every grid point.
! 19 oh1(ngrid)        IO Wetted perimeter Ot at water level h=h1 for
!                         every grid point.
! 20 psltvr            P  -
! 12 secth0(ngrid)     O  H0-value (for 1 or 2 sub sections) for every
!                         grid point.
! 13 secth1            P  -
! 11 subsec(ngrid)     I  Defines the number of sub sections for every
!                         cross section:
!                         c1sec (0) : Main section only (0 sub sections)
!                         c2sec (1) : 1 sub section
!                         c3sec (2) : 2 sub sections
!                         (For a circle cross section   : 0 ;
!                          For a sedredge cross section : 1 )
!  4 typcr(nbran)      I  Type of cross section used in every branch:
!                         ccrtab (1) : Tabulated cross sections
!                         ccrcir (2) : Circle as cross section
!                         ccrsed (3) : Sedredge cross sections
! 14 wfh0              P  -
! 17 wfh1              P  -
!  8 wft               P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! csseci  Cross Section SECtion I
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: cssect.pf,v $
! Revision 1.5  1997/01/23  08:28:47  kuipe_j
! Make flow module robust
!
! Revision 1.4  1996/05/30  09:59:54  kuipe_j
! comment char
!
! Revision 1.3  1995/05/30  09:54:25  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:55:31  hoeks_a
! files changed from dos-file to unix-files
!
! Revision 1.1  1995/04/13  06:58:34  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:29:54  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:41  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of Parameters:
!
   integer   ngrid, maxlev, nbran, branch(4,nbran), typcr(nbran)
   integer   nlev(ngrid)
   real      subsec(ngrid)
   real      wft (ngrid,maxlev) ,aft(ngrid,maxlev)
   real      of  (ngrid,maxlev)
   real      secth0(ngrid), secth1(ngrid)
   real      wfh0(ngrid), afh0(ngrid), oh0(ngrid)
   real      wfh1(ngrid), afh1(ngrid), oh1(ngrid)
   real      psltvr(7,ngrid)
   real      secths(ngrid), ws(ngrid)
   double precision hlev(ngrid,maxlev)
   logical   lsedt
!
!     Local variables
!
   integer   i, i1, i2 ,ibr
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
!     Computation for all grid points
!
   do i=1,ngrid
      secths(i) = 0.
   enddo
!
   do 20 ibr = 1, nbran
!
!        i1 = global grid point number at node n1
!        i2 = global grid point number at node n2
!
      i1 = branch (3,ibr)
      i2 = branch (4,ibr)
!
      if (typcr(ibr) .eq. ccrtab) then
!
         do 10 i = i1, i2
!
!              For rivers with only main section secth0 = hlev(maxlev)
!
            if ( int( subsec(i) ) .eq. 0 ) then
               secth0(i) = hlev(i,nlev(i))
               afh0  (i) = aft (i,nlev(i))
            endif
!
            if ( int( subsec(i) ) .ge. 1 ) then
!
!                 Calculate water level, flow area and wetted perimeter
!
               call CSSECI (ngrid   ,i        ,maxlev  ,nlev   ,&
               &wft     ,hlev     ,aft     ,of     ,&
               &wfh0(i) ,secth0(i),afh0(i) ,oh0(i) ,&
               &psltvr )
            endif
!
            if ( int( subsec(i) ) .eq. 2 ) then
!
!                 Calculate water level, flow area and wetted perimeter
!
               call CSSECI (ngrid   ,i        ,maxlev  ,nlev   ,&
               &wft     ,hlev     ,aft     ,of     ,&
               &wfh1(i) ,secth1(i),afh1(i) ,oh1(i) ,&
               &psltvr )
!
!                 Calculate O for section 1
!
               oh1 (i) = oh1 (i) - oh0 (i)
            endif
            if (lsedt) then
               call cswsed ( ngrid  ,i    ,maxlev ,nlev     ,&
               &wft    ,hlev ,ws(i)  ,secths(i))
            endif
10       continue
      endif
20 continue
!
end
