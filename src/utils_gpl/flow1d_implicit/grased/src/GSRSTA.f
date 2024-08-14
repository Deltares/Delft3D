      subroutine gsrsta (ngrid  ,nfrac  ,nunlay ,nlayer ,itim  ,ptrla2 ,
     &                   pexla2 ,p0la   ,nrdzdl ,lanrinbt      ,zbave  ,
     &                   zbfl   ,levunl ,dzr    ,deff2  ,dmed0 ,depos  ,
     &                   newres ,fd_nefis_rst, fd_nefis_new,juer   ,
     &                   first  ,ncelst ,inires ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Graded Sediment module
c
c Programmer:         J.Kuipers
c
c Module:             GSRSTA (Graded Sediment read or write ReSTArt 
c                             information)
c
c Module description: Read or write the restart information from or to
c                     the restart file.
c
c                     When the user restarts a simulation run from a
c                     specific point of time this routine will read the
c                     saved restart information from the previous run.
c                     The restart information contains a.o. the actual
c                     mixture of the various layers and the under layer
c                     description.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  8 branch            P  -
c 14 dafdrn            P  -
c 12 dafdst            P  -
c 13 defdrn            P  -
c 11 defdst            P  -
c 16 first             I  True in case of first call.
c  6 hlev              P  -
c 18 inires            I  True when no restart info of this module has
c                         been written before.
c  5 itim              P  -
c 15 juer              P  -
c 19 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c  2 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c  3 nbran             I  Number of branches.
c 17 ncelst            IO Actual cell number of a restart block of the
c                         restart file.
c 10 newres            I  true, if a new restart file will be made
c  1 ngrid             I  Number of grid points in network.
c  7 nlev              P  -
c  4 ntmpgr            I  Number of scratch arrays with length ngrid
c                         that are packed in tmpgr.
c  9 tmpgr             P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c gsdfst  Graded Sediment DeFine group for reSTart information
c gsrest  Graded Sediment REading of reSTart information
c gswrst  Graded Sediment WRiting of reSTart information
c statim  Search resTArt TIMe point
c=======================================================================
c
c     Parameters
c
      integer       ngrid, nfrac,nunlay, nlayer,itim(2),
     +              juer, ncelst, ker

      integer       fd_nefis_rst, fd_nefis_new,
     &              nrdzdl(ngrid)             ,lanrinbt(ngrid)
      real          ptrla2 (ngrid,nfrac)      ,pexla2 (ngrid,nfrac)  ,
     &              p0la   (ngrid,nfrac,nunlay)                      ,
     &              zbave  (ngrid)            ,zbfl   (ngrid)        ,
     &              levunl (ngrid)            ,dzr    (ngrid)        ,
     &              deff2  (ngrid)            ,dmed0  (ngrid)        
      logical       depos  (ngrid)            
c
      logical       first, inires ,newres 
c
c     Local variables
c
      integer       nentri
      parameter    (nentri=13)
      integer       errr, nrerr, i
      integer       ndim (nentri)
      character*16  grnams
      character*16  nameel(nentri), quanel(nentri), unitel(nentri),
     +              nameac(nentri+1)
      character*64  descel(nentri)
      character*8   txt
      logical       inidum, lees
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
c
c     Definition of elements
c
      data (ndim(i),   descel(i),  nameel(i),  quanel(i),
     +      unitel(i), i=1,nentri) /
c
     + 1, 'Restart time step'   ,'ISTEP'   ,'t'   ,'-' ,
     + 2, 'P-transport layer'   ,'PTRLA'   ,'P'   ,'-' ,
     + 2, 'P-exchange layer '   ,'PEXLA'   ,'P'   ,'-' ,
     + 3, 'P-under layers   '   ,'P0LA'    ,'P'   ,'-' ,
     + 1, 'Thick. Top und la'   ,'DZR'     ,'H'   ,'m' ,
     + 1, 'Thick. Effect lay'   ,'DEFF'    ,'H'   ,'m' ,
     + 1, 'Level low und lay'   ,'LEVUNL'  ,'H'   ,'m' ,
     + 1, 'Mean bed level   '   ,'ZBAVE'   ,'H'   ,'m' ,
     + 1, 'Level Fixed layer'   ,'ZBFL'    ,'H'   ,'m' ,
     + 1, 'Grnsz Dmed n-1   '   ,'DMED0'   ,'D'   ,'m' ,
     + 1, 'Numbr Top und lay'   ,'NRDZDL'  ,'#'   ,'-' ,
     + 1, 'Numbr Ref und lay'   ,'LANRINBT','#'   ,'-' ,
     + 1, 'Deposition       '   ,'DEPOS'   ,'?'   ,'-' /
c
      data grnams /'GSED-RES-GROUP' /
c
      errr = 0
c
      if (first) then
c
c        Determine if it is an initial run of a restart run for this
c        module. In the latter case restart information must exist
c        already.
c
         nrerr = egsbor
         call gsdfst (fd_nefis_rst, grnams, nentri, ngrid , nfrac ,
     +                 nunlay, nlayer, ndim  , nameel, quanel, unitel,
     +                 descel, .not.newres   , inires, nameac, errr  )
         if (errr .ne. 0) goto 1000

         if (inires) then
            if (newres) then
c
c              Initialize a new restart file
c
               nrerr = egsbor
               call gsdfst (fd_nefis_new, grnams, nentri, ngrid ,
     +                       nfrac , nunlay, nlayer, ndim  , nameel,
     +                       quanel, unitel, descel, .true., inidum,
     +                       nameac, errr  )
               if (errr .ne. 0) goto 1000
            endif
            ncelst = 1
         else
c
c           Read from restart file. Select proper time step first
c
           
            nrerr = egsrrd
            call statim (fd_nefis_rst, grnams, nameel(1), itim,
     +                    ncelst, errr  )
            lees = .true.
            if (errr .ne. 0) goto 1000
c
            if (lees)
     &         call gsrest (fd_nefis_rst ,grnams ,nentri ,ngrid  ,
     &                      nfrac  ,nunlay ,nlayer ,ptrla2 ,pexla2 ,
     &                      p0la   ,nrdzdl ,lanrinbt       ,zbave  ,
     &                      zbfl   ,levunl ,dzr    ,deff2  ,dmed0  ,
     &                      depos  ,ncelst ,nameel ,errr   )
c
            if (errr .ne. 0) goto 1000
c
            if (newres) then
c
c              Initialize a new restart file
c
               nrerr = egsbor
               call gsdfst (fd_nefis_new, grnams, nentri, ngrid ,
     +                       nfrac , nunlay, nlayer, ndim  , nameel,
     +                       quanel, unitel, descel, .true., inidum,
     +                       nameac, errr  )
               if (errr .ne. 0) goto 1000
               ncelst = 1
            else
c
c              Continue with existing restart file
c
               ncelst = ncelst + 1
            endif
         endif
      else
c
c        Write to restart file
c
         nrerr = egswrd
         call gswrst (fd_nefis_rst ,grnams ,nentri ,ngrid ,nfrac ,
     &                nunlay ,nlayer ,ptrla2 ,pexla2 ,p0la  ,nrdzdl,
     &                lanrinbt       ,zbave  ,zbfl   ,levunl,dzr   ,
     &                deff2  ,dmed0  ,depos  ,ncelst ,itim  ,nameel,
     &                errr )
         if (errr .ne. 0) goto 1000
         ncelst = ncelst + 1
      endif

      return

 1000 continue
c
      if (errr .gt. 0) then
c
c        Could not find restart time step
c
         ker = fatal
         call error (juer, 'GSRSTA', egsrtt, ker )
      else
c
c        Nefis error (<0)
c
         ker = fatal
         write(txt,'(i8)') errr
         call error (juer, 'GSRSTA @'//txt//'@', nrerr, ker)
      endif

      return
      end
