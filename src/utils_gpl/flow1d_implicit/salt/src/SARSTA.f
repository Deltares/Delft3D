      subroutine sarsta(dsopt  ,nmouth ,nboun  ,nbran  ,ngrid  ,itim   ,
     &                  curtim ,juer   ,first  ,newres ,fd_nefis_rst,
     &                  fd_nefis_new,   csa2   ,csd2   ,cdcdx0 ,cdcdx1 ,
     &                  cdcdx2 ,thasca ,mouqpu ,thcsum ,timout ,sbdscr ,
     &                  sbdpar ,ncelst ,inires ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SARSTA (SAlt read or write of ReSTArt information)
c
c Module description: Reading or writing of restart information.
c
c                     A parameter determines if reading or writing takes
c                     place. For an initial run no reading will be done
c                     as there is no information on the file. In that
c                     case only group definitions will be made. When the
c                     user restarts a simulation run from a specific
c                     point of time the saved restart information from a
c                     previous run will be read. Writing can be done at
c                     some interval in time. Previously in the same run
c                     written information will be lost.
c
c Precondition:       The NEFIS data and definition files are opened.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 16 cdcdx0            P  -
c 17 cdcdx1            P  -
c 18 cdcdx2            P  -
c 14 csa2              P  -
c 15 csd2              P  -
c 13 dafdrn            P  -
c 11 dafdst            P  -
c 12 defdrn            P  -
c 10 defdst            P  -
c  1 dsopt             P  -
c  8 first             I  True in case of first call.
c 25 inires            I  True when no restart info of this module has
c                         been written before.
c  6 itim              P  -
c  7 curtim            P  -
c  7 juer              P  -
c 26 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 20 mouqpu            P  -
c  3 nboun             I  Number of boundary nodes.
c  4 nbran             I  Number of branches.
c 24 ncelst            IO Actual cell number of a restart block of the
c                         restart file.
c  9 newres            I  true, if a new restart file will be made
c  5 ngrid             I  Number of grid points in network.
c  2 nmouth            P  -
c 23 sbdscr            P  -
c 23 sbdpar            P  -
c 19 thasca            P  -
c 21 thcsum            P  -
c 22 timout            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c sadfst  SAlt DeFine group for reSTart information
c sarest  SAlt REading of reSTart information
c sawrst  SAlt WRiting of reSTart information
c statim  Search resTArt TIMe point
c=======================================================================
c
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: sarsta.pf,v $
c Revision 1.9  1999/07/23  15:10:33  kuipe_j
c improve restart
c
c Revision 1.8  1998/06/18  13:32:21  kuipe_j
c Bug in resart flag solved
c
c Revision 1.7  1996/12/02  15:31:42  kuipe_j
c Salt restart improved
c
c Revision 1.6  1996/01/17  13:59:24  kuipe_j
c header update
c
c Revision 1.5  1996/01/16  15:01:45  kuipe_j
c Restart improvements
c
c Revision 1.4  1995/10/18  09:00:28  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.3  1995/05/30  09:56:14  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:06:13  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:55  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:33:59  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:15  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer      dsopt   ,nmouth ,nboun ,nbran  ,ngrid  ,ncelst ,
     &             juer    ,ker
      integer      fd_nefis_rst, fd_nefis_new, itim(2)
      real         csa2  (ngrid)   ,csd2  (ngrid) ,
     &             cdcdx0(ngrid)   ,cdcdx1(ngrid) ,cdcdx2(ngrid)  ,
     &             thasca(3    )   ,mouqpu(3,0:2,*),curtim        ,
     &             thcsum(2,nbran) ,timout(2,*)    ,sbdscr(3,nboun),
     &             sbdpar(5,nboun)
      logical      first           ,inires         ,newres
c
c     Declaration of local variables
c
      integer      nentri
      parameter   (nentri=11)
      integer      errr  ,nrerr ,i
      integer      ndim  (nentri)
      character*16 grnams
      character*16 nameel(nentri)  ,quanel(nentri)  ,unitel(nentri) ,
     &             nameac(nentri+1)
      character*64 descel(nentri)
      character*8  txt
      logical      inidum
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
c
c     Definition of elements.
c
      data (ndim(i)  ,descel(i)   ,nameel(i) ,quanel(i) ,
     &      unitel(i),i=1,nentri) /
c
     & 1,'Restart time step'              ,'ISTEP'  ,'t'  ,'-'    ,
     & 1,'Salt concentration (csa2)'      ,'Cs'     ,'C'  ,'kg/m3',
     & 1,'A*D*dc/dx (csd2)'               ,'C''s'   ,'C'  ,'kg/s' ,
     & 1,'c/c0*dc/dx (current sum)'       ,'CDCDX0' ,'<>' ,'kg/m4',
     & 1,'<c/c0*dc/dx> last tide'         ,'CDCDX1' ,'<>' ,'kg/m4',
     & 1,'<c/c0*dc/dx> previous tide'     ,'CDCDX2' ,'<>' ,'kg/m4',
     & 2,'Thatcher Harleman sum'          ,'THCSUM' ,'-'  ,'?'    ,
     & 3,'Dispersion at mouths'           ,'MOUQPU' ,'-'  ,'?'    ,
     & 2,'Tidal information at mouths'    ,'TIMOUT' ,'-'  ,'?'    ,
     & 1,'Administration for <c/c0*dc/dx>','THASCA' ,'-'  ,'?'    ,
     & 2,'Administration at boundaries'   ,'SBDSCR' ,'-'  ,'?'    /
c
      data  grnams   /
     &      'SALT-RES-GROUP' /
c
      errr = 0
c
      if (first) then
c
c        Determine if it is an initial run or a restart run for this
c        module. In the latter case restart information must exist
c        already.
c
         nrerr = esabor
         call sadfst (fd_nefis_rst, grnams ,nentri ,nbran  ,nboun ,
     &                nmouth ,ngrid  ,dsopt  ,ndim   ,nameel ,quanel,
     &                unitel ,descel ,.not.newres    ,inires ,nameac,
     &                errr   )
         if (errr.ne.0) goto 1000
c
         if (inires) then
            if (newres) then
c
c              Initialize a new restart file
c
               nrerr = esabor
               call sadfst (fd_nefis_new, grnams ,nentri ,nbran  ,
     &                      nboun  ,nmouth ,ngrid  ,dsopt  ,ndim   ,
     &                      nameel ,quanel ,unitel ,descel ,.true. ,
     &                      inidum ,nameac ,errr   )
               if (errr.ne.0) goto 1000
            endif
            ncelst = 1
         else
c
c           Read from restart file. Select proper time step first.
c
            nrerr = esarrd
            call statim (fd_nefis_rst, grnams ,nameel(1),itim ,ncelst,
     &                   errr   )
            if (errr.ne.0) goto 1000
c
            call sarest (fd_nefis_rst, grnams ,nentri ,nbran  ,nboun ,
     &                   nmouth ,ngrid  ,dsopt  ,ncelst ,nameel ,csa2  ,
     &                   csd2   ,cdcdx0 ,cdcdx1 ,cdcdx2 ,thasca ,mouqpu,
     &                   thcsum ,timout ,sbdscr ,errr   )
            if (errr.ne.0) goto 1000
c
            if (newres) then
c
c              Initialize a new restart file
c
               nrerr = esabor
               call sadfst (fd_nefis_new, grnams ,nentri ,nbran  ,
     &                      nboun  ,nmouth ,ngrid  ,dsopt  ,ndim   ,
     &                      nameel ,quanel ,unitel ,descel ,.true. ,
     &                      inidum ,nameac ,errr   )
               if (errr.ne.0) goto 1000
               ncelst = 1
            else
c
c              Continue with existing restart file
c
               ncelst = ncelst+1
            endif
         endif
c
      else
c
c        Write to restart file.
c
         nrerr = esawrd
         call sawrst (fd_nefis_rst, grnams ,nentri ,nbran  ,nboun ,
     &                ngrid  ,dsopt  ,ncelst ,itim   ,curtim ,nameel,
     &                csa2   ,csd2   ,cdcdx0 ,cdcdx1 ,cdcdx2 ,thasca,
     &                mouqpu ,thcsum ,nmouth ,timout ,sbdscr ,sbdpar,
     &                errr   )
         if (errr.ne.0) goto 1000
         ncelst = ncelst+1
c
      endif
      return
c
 1000 continue
      if (errr.gt.0) then
c
c        Could not find restart time step.
         ker = fatal
         call error (juer ,'SARSTA' ,esartt ,ker)
      else
c
c        NEFIS error ( <0 )
         ker = fatal
         write (txt,'(i8)') errr
         call error (juer ,'SARSTA @'//txt//'@' ,nrerr ,ker)
      endif
c
      end
