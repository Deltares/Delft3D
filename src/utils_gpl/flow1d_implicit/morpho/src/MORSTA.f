      subroutine morsta ( ngrid , maxlev, itim  ,
     +                    hlev  , newres, fd_nefis_rst, fd_nefis_new,
     +                    juer   , first , ncelst, inires,
     +                    ker   ,lgrad  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Morphology module
c
c Programmer:         S.L. van der Woude
c
c Module:             MORSTA (MOrphology read or write ReSTart information)
c
c Module description: Read or write the restart information from or to
c                     the restart file.
c
c                     When the user restarts a simulation run from a
c                     specific point of time this routine will read the
c                     saved restart information from the previous run.
c                     The restart information contains the actual adap-
c                     ted cross sections on the time level the informa-
c                     tion was saved.
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
c modfst  MOrphology DeFine group for reSTart information
c morest  MOrphology REading of reSTart information
c mowrst  MOrphology WRiting of reSTart information
c statim  Search resTArt TIMe point
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: morsta.pf,v $
c Revision 1.11  1999/07/23  15:10:29  kuipe_j
c improve restart
c
c Revision 1.10  1999/03/15  15:53:05  kuipe_j
c tabs removed
c
c Revision 1.9  1997/02/17  10:23:18  kuipe_j
c Lateral Q in m3/s in cont. equation now
c
c Revision 1.8  1997/01/23  08:29:51  kuipe_j
c Make flow module robust
c
c Revision 1.7  1996/12/03  08:29:25  kuipe_j
c dimension of element names improved
c
c Revision 1.6  1996/01/17  13:18:23  kuipe_j
c header update
c
c Revision 1.5  1996/01/16  15:01:38  kuipe_j
c Restart improvements
c
c Revision 1.4  1995/10/18  09:00:05  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.3  1995/05/30  09:55:57  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:05:00  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:26  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:33:00  kuipe_j
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
      integer       ngrid, maxlev, itim(2),
     +              juer, ncelst, ker

      integer       fd_nefis_rst, fd_nefis_new

      double precision hlev (ngrid,maxlev)

      logical       first, inires ,newres ,lgrad
c
c     Local variables
c
      integer       nentri
      parameter    (nentri=2)
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
     + 2, 'H-levels'            ,'HLEV'    ,'H'   ,'m' /
c
      data grnams /
     +     'MORP-RES-GROUP' /
c
      errr = 0
c
      if (first) then
c
c        Determine if is an initial run of a restart run for this
c        module. In the latter case restart information must exist
c        already.
c
         nrerr = emobor
         call modfst (fd_nefis_rst, grnams, nentri, ngrid , maxlev,
     +                 ndim  , nameel, quanel, unitel, descel,
     +                 .not.newres   , inires, nameac, errr  )
         if (errr .ne. 0) goto 1000

         if (inires) then
            if (newres) then
c
c              Initialize a new restart file
c
               nrerr = emobor
               call modfst (fd_nefis_new, grnams, nentri, ngrid ,
     +                       maxlev, ndim  , nameel, quanel, unitel,
     +                       descel, .true., inidum, nameac, errr  )
               if (errr .ne. 0) goto 1000
            endif
            ncelst = 1
         else
c
c           Read from restart file. Select proper time step first
c
           
            nrerr = emorrd
            call statim (fd_nefis_rst, grnams, nameel(1), itim,
     +                    ncelst, errr  )
            lees = .true.
            if (errr .ne. 0) then
               if (lgrad) then 
                  call error (juer, 'MORSTA', egrest, info )
                  errr = 0
                  lees = .false.
               else
                  goto 1000
               endif
            endif
c
            if (lees)
     +         call morest (fd_nefis_rst, grnams, nentri,
     +                       ngrid , maxlev, hlev  ,
     +                       ncelst, nameel,
     +                       errr
     +                     )
c
            if (errr .ne. 0) goto 1000
c
            if (newres) then
c
c              Initialize a new restart file
c
               nrerr = emobor
               call modfst (fd_nefis_rst, grnams, nentri, ngrid ,
     +                       maxlev, ndim  , nameel, quanel, unitel,
     +                       descel, .true., inidum, nameac, errr  )
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
         nrerr = emowrd
         call mowrst (fd_nefis_rst, grnams, nentri,
     +                 ngrid , maxlev, hlev  , ncelst, itim  ,
     +                 nameel, errr
     +               )
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
         call error (juer, 'MORSTA', emortt, ker )
      else
c
c        Nefis error (<0)
c
         ker = fatal
         write(txt,'(i8)') errr
         call error (juer, 'MORSTA @'//txt//'@', nrerr, ker)
      endif

      return
      end
