subroutine morest (fd_nefis_rst, grnams, nentri,&
&ngrid , maxlev, hlev  ,&
&ncelst, nameel,&
&neferr&
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
! Module:             MOREST (MOrphology REading of reSTart information)
!
! Module description: Read restart information into memory
!
!                     Depending on the dispersion formulation used se-
!                     veral variables must be read from the restart
!                     file.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 dafdst            P  -
!  1 defdst            P  -
!  3 grnams            P  -
!  7 hlev              P  -
!  6 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
!  9 nameel            P  -
!  8 ncelst            I  Actual cell number of a restart block of the
!                         restart file.
! 10 neferr            O  Nefis error code:
!                         0   = No error
!                         <0  = Error:  odd  = fatal
!                                       even = non fatal
!  4 nentri            I  Number of entries (possible report paramaters)
!                         in the tables of a block (i.e.nameel).
!  5 ngrid             I  Number of grid points in network.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! getrel  GET Real ELement from a nefis file
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: morest.pf,v $
! Revision 1.3  1996/11/05  13:48:16  kuipe_j
! Error in declaration hlev fixed
!
! Revision 1.2  1995/05/30  07:04:58  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:24  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:32:56  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:09  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Parameters
!
   integer       nentri, ngrid, maxlev, ncelst, neferr
   integer       fd_nefis_rst
   double precision hlev(ngrid,maxlev)
   character*(*) grnams
   character*(*) nameel(nentri)
!
!     Local variables
!
   integer       error, buflen
   integer       uindex(3), usrord(1)
!
!     Declaration of external functions
!
   integer       getrel
   external      getrel
!
   data          usrord /1/
!
   uindex(1) = ncelst
   uindex(2) = ncelst
   uindex(3) = 1
!
   buflen = ngrid * maxlev * 4
!
   error = getrel (fd_nefis_rst, grnams, nameel(2),&
   &uindex, usrord, buflen, hlev     )
!
   neferr = error

   return
end

