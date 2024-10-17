subroutine modfst (fd_nefis_rst, grnams, nentri,&
&ngrid , maxlev,&
&ndim  , nameel, quanel, unitel,&
&descel, define, inires, nameac, neferr )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:         S.L. van der Woude
!
! Module:             MODFST (MOrphology DeFine group for reSTart information)
!
! Module description: Define a group for restart information.
!
!                     Depending on the used dispersion formulation se-
!                     veral variables must be saved on the restart file.
!                     This is determined by this routine.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 dafdst            P  -
!    define            I  If true and no data group exist define the
!                         data group
!  1 defdst            P  -
! 11 descel            P  -
!  3 grnams            I  Name of data group for Salt restart block.
! 12 inires            O  True when no restart info of this module has
!                         been written before.
!  6 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
! 13 nameac(nentri)    O  All element names of a data group.
!  8 nameel(nentri)    I  All possible element names of a block.
!  7 ndim              P  -
! 14 neferr            O  Nefis error code:
!                         0   = No error
!                         <0  = Error:  odd  = fatal
!                                       even = non fatal
!  4 nentri            I  Number of entries (possible report paramaters)
!                         in the tables of a block (i.e.nameel).
!  5 ngrid             I  Number of grid points in network.
!  9 quanel            P  -
! 10 unitel            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! credat  CREation of a data group in the DATa file
! defcel  DEFinition of a CELl
! defelm  DEFinition of an ELeMent
! defgrp  DEFinition of a GRouP
! flsdef  FLuSh buffers of DEFinition file
! inqdat  INQuire for info of DATa group on data file
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: modfst.pf,v $
! Revision 1.5  1999/07/23  15:10:28  kuipe_j
! improve restart
!
! Revision 1.4  1999/03/15  15:52:50  kuipe_j
! tabs removed
!
! Revision 1.3  1996/12/03  08:29:24  kuipe_j
! dimension of element names improved
!
! Revision 1.2  1995/05/30  07:04:41  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:10  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:32:35  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:07  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Parameters
!
   integer       nentri, ngrid, maxlev, neferr
   integer       fd_nefis_rst, ndim(nentri)
   logical       inires, define
   character*(*) grnams
   character*(*) nameel(nentri), quanel(nentri), unitel(nentri),&
   &nameac(nentri+1),&
   &descel(nentri)
!
!     Local variables
!
   integer       error, i, ie, l, nelems
   integer       dims(2,2), dimn(1), ord(1)
   character*16  celnam
!
!     Declaration of external functions
!
   integer       defelm, defcel, defgrp, credat, inqdat, flsdef
   external      defelm, defcel, defgrp, credat, inqdat, flsdef
!
   data          dimn, ord /0,1/
!
!
   error = inqdat (fd_nefis_rst, grnams, grnams )
!
   if (error .eq. 0) then
!
!        Data group for restart of morphology module does exist.
!        It is apparently a restart run for the morphology module.
!
      inires = .false.
!
   else if (define) then
!
!        Data group for restart of morphology module doesn't exist
!        and restart info must be written to this file, so
!        define elements, cel and group def. It is apparently an
!        initial run for the morphology module.
!
      inires = .true.
!
!        Define dimensions first
!
      do 10 i = 1, nentri
         dims(2,i) = 0
10    continue
!
!        Time step number
!
      dims(1,1) = 2
!
!        Array hlev
!
      dims(1,2) = ngrid
      dims(2,2) = maxlev
!
!        Define time step number
!
      ie = 1
      error = defelm (fd_nefis_rst, nameel(ie) , 'INTEGER' , 4,&
      &quanel(ie), unitel(ie) , descel(ie) ,&
      &ndim(ie)  , dims(1,ie) )
      if (error .ne. 0 .and. error .ne. 5007) goto 1000
!
      nelems = 1
      nameac(1) = nameel(1)
!
!        Define cross sections
!
      ie = 2
      error = defelm (fd_nefis_rst, nameel(ie) , 'REAL'    , 4,&
      &quanel(ie), unitel(ie) , descel(ie) ,&
      &ndim(ie)  , dims(1,ie) )
      if (error .ne. 0) goto 1000

      nelems = nelems + 1
      nameac(nelems) = nameel(2)
!
      l      = index ( grnams, 'GROUP') - 1
      celnam = grnams (1:l) // 'CEL'
      error  = defcel (fd_nefis_rst, celnam, nelems, nameac )
      if (error .ne. 0) goto 1000
!
      error  = defgrp (fd_nefis_rst, grnams, celnam, 1, dimn, ord)
      if (error .ne. 0) goto 1000
!
!        Create data group on data file
!
      error = credat (fd_nefis_rst, grnams, grnams )
      if (error .ne. 0) goto 1000
!
      error = flsdef (fd_nefis_rst)
      if (error .ne. 0) goto 1000
!
   else
!
!        Data group for restart of morphology module does not exist.
!        It is apparently an initial run for the morphology module.
!
      inires = .true.

   endif
!

   goto 1010
!
1000 continue
   neferr = error
1010 continue
end
