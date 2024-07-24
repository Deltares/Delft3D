      subroutine modfst (fd_nefis_rst, grnams, nentri,
     +                    ngrid , maxlev,
     +                    ndim  , nameel, quanel, unitel,
     +                    descel, define, inires, nameac, neferr )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Morphology module
c
c Programmer:         S.L. van der Woude
c
c Module:             MODFST (MOrphology DeFine group for reSTart information)
c
c Module description: Define a group for restart information.
c
c                     Depending on the used dispersion formulation se-
c                     veral variables must be saved on the restart file.
c                     This is determined by this routine.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 dafdst            P  -
c    define            I  If true and no data group exist define the
c                         data group
c  1 defdst            P  -
c 11 descel            P  -
c  3 grnams            I  Name of data group for Salt restart block.
c 12 inires            O  True when no restart info of this module has
c                         been written before.
c  6 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c 13 nameac(nentri)    O  All element names of a data group.
c  8 nameel(nentri)    I  All possible element names of a block.
c  7 ndim              P  -
c 14 neferr            O  Nefis error code:
c                         0   = No error
c                         <0  = Error:  odd  = fatal
c                                       even = non fatal
c  4 nentri            I  Number of entries (possible report paramaters)
c                         in the tables of a block (i.e.nameel).
c  5 ngrid             I  Number of grid points in network.
c  9 quanel            P  -
c 10 unitel            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c credat  CREation of a data group in the DATa file
c defcel  DEFinition of a CELl
c defelm  DEFinition of an ELeMent
c defgrp  DEFinition of a GRouP
c flsdef  FLuSh buffers of DEFinition file
c inqdat  INQuire for info of DATa group on data file
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: modfst.pf,v $
c Revision 1.5  1999/07/23  15:10:28  kuipe_j
c improve restart
c
c Revision 1.4  1999/03/15  15:52:50  kuipe_j
c tabs removed
c
c Revision 1.3  1996/12/03  08:29:24  kuipe_j
c dimension of element names improved
c
c Revision 1.2  1995/05/30  07:04:41  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:10  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:32:35  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:07  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Parameters
c
      integer       nentri, ngrid, maxlev, neferr
      integer       fd_nefis_rst, ndim(nentri)
      logical       inires, define
      character*(*) grnams
      character*(*) nameel(nentri), quanel(nentri), unitel(nentri),
     +              nameac(nentri+1),
     +              descel(nentri)
c
c     Local variables
c
      integer       error, i, ie, l, nelems
      integer       dims(2,2), dimn(1), ord(1)
      character*16  celnam
c
c     Declaration of external functions
c
      integer       defelm, defcel, defgrp, credat, inqdat, flsdef
      external      defelm, defcel, defgrp, credat, inqdat, flsdef
c
      data          dimn, ord /0,1/
c
c
      error = inqdat (fd_nefis_rst, grnams, grnams )
c
      if (error .eq. 0) then
c
c        Data group for restart of morphology module does exist.
c        It is apparently a restart run for the morphology module.
c
         inires = .false.
c
      else if (define) then
c
c        Data group for restart of morphology module doesn't exist
c        and restart info must be written to this file, so
c        define elements, cel and group def. It is apparently an
c        initial run for the morphology module.
c
         inires = .true.
c
c        Define dimensions first
c
         do 10 i = 1, nentri
            dims(2,i) = 0
 10      continue
c
c        Time step number
c
         dims(1,1) = 2
c
c        Array hlev
c
         dims(1,2) = ngrid
         dims(2,2) = maxlev
c
c        Define time step number
c
         ie = 1
         error = defelm (fd_nefis_rst, nameel(ie) , 'INTEGER' , 4,
     +                   quanel(ie), unitel(ie) , descel(ie) ,
     +                   ndim(ie)  , dims(1,ie) )
         if (error .ne. 0 .and. error .ne. 5007) goto 1000
c
         nelems = 1
         nameac(1) = nameel(1)
c
c        Define cross sections
c
         ie = 2
         error = defelm (fd_nefis_rst, nameel(ie) , 'REAL'    , 4,
     +                   quanel(ie), unitel(ie) , descel(ie) ,
     +                   ndim(ie)  , dims(1,ie) )
         if (error .ne. 0) goto 1000

         nelems = nelems + 1
         nameac(nelems) = nameel(2)
c
         l      = index ( grnams, 'GROUP') - 1
         celnam = grnams (1:l) // 'CEL'
         error  = defcel (fd_nefis_rst, celnam, nelems, nameac )
         if (error .ne. 0) goto 1000
c
         error  = defgrp (fd_nefis_rst, grnams, celnam, 1, dimn, ord)
         if (error .ne. 0) goto 1000
c
c        Create data group on data file
c
         error = credat (fd_nefis_rst, grnams, grnams )
         if (error .ne. 0) goto 1000
c
         error = flsdef (fd_nefis_rst)
         if (error .ne. 0) goto 1000
c
      else
c
c        Data group for restart of morphology module does not exist.
c        It is apparently an initial run for the morphology module.
c
         inires = .true.

      endif
c

      goto 1010
c
 1000 continue
      neferr = error
 1010 continue
      end
