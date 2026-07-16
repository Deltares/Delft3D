subroutine gsrsta (ngrid  ,nfrac  ,nunlay ,nlayer ,itim  ,ptrla2 ,&
&pexla2 ,p0la   ,nrdzdl ,lanrinbt      ,zbave  ,&
&zbfl   ,levunl ,dzr    ,deff2  ,dmed0 ,depos  ,&
&newres ,fd_nefis_rst, fd_nefis_new,juer   ,&
&first  ,ncelst ,inires ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Graded Sediment module
!
! Programmer:         J.Kuipers
!
! Module:             GSRSTA (Graded Sediment read or write ReSTArt
!                             information)
!
! Module description: Read or write the restart information from or to
!                     the restart file.
!
!                     When the user restarts a simulation run from a
!                     specific point of time this routine will read the
!                     saved restart information from the previous run.
!                     The restart information contains a.o. the actual
!                     mixture of the various layers and the under layer
!                     description.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  8 branch            P  -
! 14 dafdrn            P  -
! 12 dafdst            P  -
! 13 defdrn            P  -
! 11 defdst            P  -
! 16 first             I  True in case of first call.
!  6 hlev              P  -
! 18 inires            I  True when no restart info of this module has
!                         been written before.
!  5 itim              P  -
! 15 juer              P  -
! 19 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  2 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
!  3 nbran             I  Number of branches.
! 17 ncelst            IO Actual cell number of a restart block of the
!                         restart file.
! 10 newres            I  true, if a new restart file will be made
!  1 ngrid             I  Number of grid points in network.
!  7 nlev              P  -
!  4 ntmpgr            I  Number of scratch arrays with length ngrid
!                         that are packed in tmpgr.
!  9 tmpgr             P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! gsdfst  Graded Sediment DeFine group for reSTart information
! gsrest  Graded Sediment REading of reSTart information
! gswrst  Graded Sediment WRiting of reSTart information
! statim  Search resTArt TIMe point
!=======================================================================
!
!     Parameters
!
   integer       ngrid, nfrac,nunlay, nlayer,itim(2),&
   &juer, ncelst, ker

   integer       fd_nefis_rst, fd_nefis_new,&
   &nrdzdl(ngrid)             ,lanrinbt(ngrid)
   real          ptrla2 (ngrid,nfrac)      ,pexla2 (ngrid,nfrac)  ,&
   &p0la   (ngrid,nfrac,nunlay)                      ,&
   &zbave  (ngrid)            ,zbfl   (ngrid)        ,&
   &levunl (ngrid)            ,dzr    (ngrid)        ,&
   &deff2  (ngrid)            ,dmed0  (ngrid)
   logical       depos  (ngrid)
!
   logical       first, inires ,newres
!
!     Local variables
!
   integer       nentri
   parameter    (nentri=13)
   integer       errr, nrerr, i
   integer       ndim (nentri)
   character*16  grnams
   character*16  nameel(nentri), quanel(nentri), unitel(nentri),&
   &nameac(nentri+1)
   character*64  descel(nentri)
   character*8   txt
   logical       inidum, lees
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
!
!     Definition of elements
!
   data (ndim(i),   descel(i),  nameel(i),  quanel(i),&
   &unitel(i), i=1,nentri) /&
!
   &1, 'Restart time step'   ,'ISTEP'   ,'t'   ,'-' ,&
   &2, 'P-transport layer'   ,'PTRLA'   ,'P'   ,'-' ,&
   &2, 'P-exchange layer '   ,'PEXLA'   ,'P'   ,'-' ,&
   &3, 'P-under layers   '   ,'P0LA'    ,'P'   ,'-' ,&
   &1, 'Thick. Top und la'   ,'DZR'     ,'H'   ,'m' ,&
   &1, 'Thick. Effect lay'   ,'DEFF'    ,'H'   ,'m' ,&
   &1, 'Level low und lay'   ,'LEVUNL'  ,'H'   ,'m' ,&
   &1, 'Mean bed level   '   ,'ZBAVE'   ,'H'   ,'m' ,&
   &1, 'Level Fixed layer'   ,'ZBFL'    ,'H'   ,'m' ,&
   &1, 'Grnsz Dmed n-1   '   ,'DMED0'   ,'D'   ,'m' ,&
   &1, 'Numbr Top und lay'   ,'NRDZDL'  ,'#'   ,'-' ,&
   &1, 'Numbr Ref und lay'   ,'LANRINBT','#'   ,'-' ,&
   &1, 'Deposition       '   ,'DEPOS'   ,'?'   ,'-' /
!
   data grnams /'GSED-RES-GROUP' /
!
   errr = 0
!
   if (first) then
!
!        Determine if it is an initial run of a restart run for this
!        module. In the latter case restart information must exist
!        already.
!
      nrerr = egsbor
      call gsdfst (fd_nefis_rst, grnams, nentri, ngrid , nfrac ,&
      &nunlay, nlayer, ndim  , nameel, quanel, unitel,&
      &descel, .not.newres   , inires, nameac, errr  )
      if (errr .ne. 0) goto 1000

      if (inires) then
         if (newres) then
!
!              Initialize a new restart file
!
            nrerr = egsbor
            call gsdfst (fd_nefis_new, grnams, nentri, ngrid ,&
            &nfrac , nunlay, nlayer, ndim  , nameel,&
            &quanel, unitel, descel, .true., inidum,&
            &nameac, errr  )
            if (errr .ne. 0) goto 1000
         endif
         ncelst = 1
      else
!
!           Read from restart file. Select proper time step first
!

         nrerr = egsrrd
         call statim (fd_nefis_rst, grnams, nameel(1), itim,&
         &ncelst, errr  )
         lees = .true.
         if (errr .ne. 0) goto 1000
!
         if (lees)&
         &call gsrest (fd_nefis_rst ,grnams ,nentri ,ngrid  ,&
         &nfrac  ,nunlay ,nlayer ,ptrla2 ,pexla2 ,&
         &p0la   ,nrdzdl ,lanrinbt       ,zbave  ,&
         &zbfl   ,levunl ,dzr    ,deff2  ,dmed0  ,&
         &depos  ,ncelst ,nameel ,errr   )
!
         if (errr .ne. 0) goto 1000
!
         if (newres) then
!
!              Initialize a new restart file
!
            nrerr = egsbor
            call gsdfst (fd_nefis_new, grnams, nentri, ngrid ,&
            &nfrac , nunlay, nlayer, ndim  , nameel,&
            &quanel, unitel, descel, .true., inidum,&
            &nameac, errr  )
            if (errr .ne. 0) goto 1000
            ncelst = 1
         else
!
!              Continue with existing restart file
!
            ncelst = ncelst + 1
         endif
      endif
   else
!
!        Write to restart file
!
      nrerr = egswrd
      call gswrst (fd_nefis_rst ,grnams ,nentri ,ngrid ,nfrac ,&
      &nunlay ,nlayer ,ptrla2 ,pexla2 ,p0la  ,nrdzdl,&
      &lanrinbt       ,zbave  ,zbfl   ,levunl,dzr   ,&
      &deff2  ,dmed0  ,depos  ,ncelst ,itim  ,nameel,&
      &errr )
      if (errr .ne. 0) goto 1000
      ncelst = ncelst + 1
   endif

   return

1000 continue
!
   if (errr .gt. 0) then
!
!        Could not find restart time step
!
      ker = fatal
      call error (juer, 'GSRSTA', egsrtt, ker )
   else
!
!        Nefis error (<0)
!
      ker = fatal
      write(txt,'(i8)') errr
      call error (juer, 'GSRSTA @'//txt//'@', nrerr, ker)
   endif

   return
end
