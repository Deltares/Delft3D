      subroutine morest (fd_nefis_rst, grnams, nentri,
     +                    ngrid , maxlev, hlev  ,
     +                    ncelst, nameel,
     +                    neferr
     +                  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Morphology module
c
c Programmer:         S.L. van der Woude
c
c Module:             MOREST (MOrphology REading of reSTart information)
c
c Module description: Read restart information into memory
c
c                     Depending on the dispersion formulation used se-
c                     veral variables must be read from the restart
c                     file.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 dafdst            P  -
c  1 defdst            P  -
c  3 grnams            P  -
c  7 hlev              P  -
c  6 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c  9 nameel            P  -
c  8 ncelst            I  Actual cell number of a restart block of the
c                         restart file.
c 10 neferr            O  Nefis error code:
c                         0   = No error
c                         <0  = Error:  odd  = fatal
c                                       even = non fatal
c  4 nentri            I  Number of entries (possible report paramaters)
c                         in the tables of a block (i.e.nameel).
c  5 ngrid             I  Number of grid points in network.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c getrel  GET Real ELement from a nefis file
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: morest.pf,v $
c Revision 1.3  1996/11/05  13:48:16  kuipe_j
c Error in declaration hlev fixed
c
c Revision 1.2  1995/05/30  07:04:58  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:24  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:32:56  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:09  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Parameters
c
      integer       nentri, ngrid, maxlev, ncelst, neferr
      integer       fd_nefis_rst
      double precision hlev(ngrid,maxlev)
      character*(*) grnams
      character*(*) nameel(nentri)
c
c     Local variables
c
      integer       error, buflen
      integer       uindex(3), usrord(1)
c
c     Declaration of external functions
c
      integer       getrel
      external      getrel
c
      data          usrord /1/
c
      uindex(1) = ncelst
      uindex(2) = ncelst
      uindex(3) = 1
c
      buflen = ngrid * maxlev * 4
c
      error = getrel (fd_nefis_rst, grnams, nameel(2),
     +                 uindex, usrord, buflen, hlev     )
c
      neferr = error

      return
      end

