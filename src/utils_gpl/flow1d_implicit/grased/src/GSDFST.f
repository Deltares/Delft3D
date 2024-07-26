      subroutine gsdfst (fd_nefis_rst, grnams, nentri,
     +                    ngrid , nfrac , nunlay, nlayer,
     +                    ndim  , nameel, quanel, unitel,
     +                    descel, define, inires, nameac,
     +                    neferr)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Graded Sediment module
c
c Programmer:         J.Kuipers
c
c Module:             GSDFST (Graded Sediment DeFine group for reSTart 
c                             information)
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
c  6 
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
c     Parameters
c
      integer       nentri, ngrid, nfrac , nunlay, nlayer,neferr
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
      integer       dims(3,13), dimn(1), ord(1)
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
c
c        Define time step number
c
         ie = 1
         error = defelm (fd_nefis_rst, nameel(ie) , 'INTEGER' , 4,
     +                   quanel(ie), unitel(ie) , descel(ie) ,
     +                   ndim(ie)  , dims(1,ie) )
         if (error .ne. 0 .and. error .ne. 5007) goto 1000
         nelems = 1
         nameac(1) = nameel(1)
c
         do ie=2,10
c
c          Define Ptrla,Pexla,P0la,Dzr,Level,Zbave,Zbfl,Dmed0
c
           if (.not.(ie.eq.3.and.nlayer.eq.1)) then
              error = defelm (fd_nefis_rst, nameel(ie) , 'REAL'     , 4,
     +                        quanel(ie), unitel(ie) , descel(ie) ,
     +                        ndim(ie)  , dims(1,ie) )
              nelems = nelems + 1
              nameac(nelems) = nameel(ie)     
              if (error .ne. 0) goto 1000
           endif   
         enddo
c         
         do ie=11,12
c
c          Define Nrdzdl,Lanrinbt
c
           error = defelm (fd_nefis_rst, nameel(ie) , 'INTEGER'  , 4,
     +                     quanel(ie), unitel(ie) , descel(ie) ,
     +                     ndim(ie)  , dims(1,ie) )
           nelems = nelems + 1
           nameac(nelems) = nameel(ie)     
           if (error .ne. 0) goto 1000
         enddo
c
c        Define Depos
c
         ie = 13
         error = defelm (fd_nefis_rst, nameel(ie) , 'LOGICAL'  , 4,
     +                   quanel(ie), unitel(ie) , descel(ie) ,
     +                   ndim(ie)  , dims(1,ie) )
         nelems = nelems + 1
         nameac(nelems) = nameel(ie)
         if (error .ne. 0) goto 1000
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
