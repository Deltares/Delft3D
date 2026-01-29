subroutine mowrst (fd_nefis_rst, grnams, nentri,&
&ngrid , maxlev, hlev  , ncelst, itim  ,&
&nameel, neferr&
&)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:         S.L. van der Woude
!
! Module:             MOWRST (MOrphology WRiting of reSTart information)
!
! Module description: Write restart information from memory to restart
!                     file
!
!                     Write cross sections to the restart file.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  9 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  2 dafdst            P  -
!  1 defdst            P  -
!  3 grnams            P  -
! 10 hlev(ngrid,       IO (i,j) = H at level j in cross section i.
!       maxlev)           - For a circle cross section:
!                         (i,1) = Reference level.
!                         - For a sedredge cross section:
!                         (i,1) = Bed level of main section (i.e. left
!                                 channel).
!                         (i,2) = Bed level of sub section 1 (i.e. right
!                                 channel).
! 14 itim              P  -
!  8 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
! 15 nameel            P  -
!  6 nbran             I  Number of branches.
! 13 ncelst            I  Actual cell number of a restart block of the
!                         restart file.
! 16 neferr            O  Nefis error code:
!                         0   = No error
!                         <0  = Error:  odd  = fatal
!                                       even = non fatal
!  4 nentri            I  Number of entries (possible report paramaters)
!                         in the tables of a block (i.e.nameel).
!  5 ngrid             I  Number of grid points in network.
! 11 nlev(ngrid)       I  Number of h-levels for every cross section.
!                         (For a circle cross section   : 1 ;
!                          For a sedredge cross section : 2 )
!  7 ntmpgr            I  Number of scratch arrays with length ngrid
!                         that are packed in tmpgr.
! 12 tmpgr(ngrid,      IO Scratch array for a module:
!                         (i,1) = source(i)
!                         (i,2) = qltgim(i)
!                         (i,3) = distmp(i),dcdx(i),filc(i) or buf(i)
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! flsdat  FLuSh buffers of DATa file
! putiel  PUT Integer ELement to nefis file
! putrel  PUT Real ELement to a nefis file
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: mowrst.pf,v $
! Revision 1.8  1997/02/17  10:23:20  kuipe_j
! Lateral Q in m3/s in cont. equation now
!
! Revision 1.7  1997/01/23  08:29:53  kuipe_j
! Make flow module robust
!
! Revision 1.6  1996/11/05  13:48:21  kuipe_j
! Error in declaration hlev fixed
!
! Revision 1.5  1996/01/17  13:18:24  kuipe_j
! header update
!
! Revision 1.4  1995/10/18  09:00:10  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.3  1995/05/30  09:55:59  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:05:03  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:28  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:33:05  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:10  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Parameters
!
   integer       nentri, ngrid, maxlev, itim(2),&
   &ncelst, neferr

   integer       fd_nefis_rst

   double precision hlev(ngrid,maxlev)

   character*(*) grnams
   character*(*) nameel(nentri)
!
!     Local variables
!
   integer       error
   integer       uindex(3), usrord(1)
!
!     Include sobek constants
!
!     Declaration of external functions
!
   integer       flsdat, putrel, putiel
   external      flsdat, putrel, putiel
!
   data          usrord /1/
!
   uindex(1) = ncelst
   uindex(2) = ncelst
   uindex(3) = 1
!
   error = putiel (fd_nefis_rst, grnams, nameel(1),&
   &uindex, usrord, itim  )
   if (error .ne. 0) goto 1000
!
!     Write h-levels
!
   error = putrel (fd_nefis_rst, grnams, nameel(2),&
   &uindex, usrord, hlev  )
!
!     Check for error
!
   if (error .ne. 0) goto 1000
!
   error = flsdat (fd_nefis_rst)
!
1000 continue
   neferr = error

   return
end
