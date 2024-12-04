subroutine gsdfst (fd_nefis_rst, grnams, nentri,&
&ngrid , nfrac , nunlay, nlayer,&
&ndim  , nameel, quanel, unitel,&
&descel, define, inires, nameac,&
&neferr)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Graded Sediment module
!
! Programmer:         J.Kuipers
!
! Module:             GSDFST (Graded Sediment DeFine group for reSTart
!                             information)
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
!  6
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
!     Parameters
!
   integer       nentri, ngrid, nfrac , nunlay, nlayer,neferr
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
   integer       dims(3,13), dimn(1), ord(1)
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
      dims(1,1) = 2
      do i=2,nentri
         dims(1,i)=ngrid
      enddo
      do i = 1, nentri
         dims(2,i) = 0
         dims(3,i) = 0
      enddo
      dims(2,2) = nfrac
      dims(2,3) = nfrac
      dims(2,4) = nfrac
      dims(3,4) = nunlay
!
!        Define time step number
!
      ie = 1
      error = defelm (fd_nefis_rst, nameel(ie) , 'INTEGER' , 4,&
      &quanel(ie), unitel(ie) , descel(ie) ,&
      &ndim(ie)  , dims(1,ie) )
      if (error .ne. 0 .and. error .ne. 5007) goto 1000
      nelems = 1
      nameac(1) = nameel(1)
!
      do ie=2,10
!
!          Define Ptrla,Pexla,P0la,Dzr,Level,Zbave,Zbfl,Dmed0
!
         if (.not.(ie.eq.3.and.nlayer.eq.1)) then
            error = defelm (fd_nefis_rst, nameel(ie) , 'REAL'     , 4,&
            &quanel(ie), unitel(ie) , descel(ie) ,&
            &ndim(ie)  , dims(1,ie) )
            nelems = nelems + 1
            nameac(nelems) = nameel(ie)
            if (error .ne. 0) goto 1000
         endif
      enddo
!
      do ie=11,12
!
!          Define Nrdzdl,Lanrinbt
!
         error = defelm (fd_nefis_rst, nameel(ie) , 'INTEGER'  , 4,&
         &quanel(ie), unitel(ie) , descel(ie) ,&
         &ndim(ie)  , dims(1,ie) )
         nelems = nelems + 1
         nameac(nelems) = nameel(ie)
         if (error .ne. 0) goto 1000
      enddo
!
!        Define Depos
!
      ie = 13
      error = defelm (fd_nefis_rst, nameel(ie) , 'LOGICAL'  , 4,&
      &quanel(ie), unitel(ie) , descel(ie) ,&
      &ndim(ie)  , dims(1,ie) )
      nelems = nelems + 1
      nameac(nelems) = nameel(ie)
      if (error .ne. 0) goto 1000
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
