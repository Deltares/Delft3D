subroutine sarsta(dsopt  ,nmouth ,nboun  ,nbran  ,ngrid  ,itim   ,&
&curtim ,juer   ,first  ,newres ,fd_nefis_rst,&
&fd_nefis_new,   csa2   ,csd2   ,cdcdx0 ,cdcdx1 ,&
&cdcdx2 ,thasca ,mouqpu ,thcsum ,timout ,sbdscr ,&
&sbdpar ,ncelst ,inires ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SARSTA (SAlt read or write of ReSTArt information)
!
! Module description: Reading or writing of restart information.
!
!                     A parameter determines if reading or writing takes
!                     place. For an initial run no reading will be done
!                     as there is no information on the file. In that
!                     case only group definitions will be made. When the
!                     user restarts a simulation run from a specific
!                     point of time the saved restart information from a
!                     previous run will be read. Writing can be done at
!                     some interval in time. Previously in the same run
!                     written information will be lost.
!
! Precondition:       The NEFIS data and definition files are opened.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 16 cdcdx0            P  -
! 17 cdcdx1            P  -
! 18 cdcdx2            P  -
! 14 csa2              P  -
! 15 csd2              P  -
! 13 dafdrn            P  -
! 11 dafdst            P  -
! 12 defdrn            P  -
! 10 defdst            P  -
!  1 dsopt             P  -
!  8 first             I  True in case of first call.
! 25 inires            I  True when no restart info of this module has
!                         been written before.
!  6 itim              P  -
!  7 curtim            P  -
!  7 juer              P  -
! 26 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 20 mouqpu            P  -
!  3 nboun             I  Number of boundary nodes.
!  4 nbran             I  Number of branches.
! 24 ncelst            IO Actual cell number of a restart block of the
!                         restart file.
!  9 newres            I  true, if a new restart file will be made
!  5 ngrid             I  Number of grid points in network.
!  2 nmouth            P  -
! 23 sbdscr            P  -
! 23 sbdpar            P  -
! 19 thasca            P  -
! 21 thcsum            P  -
! 22 timout            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! sadfst  SAlt DeFine group for reSTart information
! sarest  SAlt REading of reSTart information
! sawrst  SAlt WRiting of reSTart information
! statim  Search resTArt TIMe point
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: sarsta.pf,v $
! Revision 1.9  1999/07/23  15:10:33  kuipe_j
! improve restart
!
! Revision 1.8  1998/06/18  13:32:21  kuipe_j
! Bug in resart flag solved
!
! Revision 1.7  1996/12/02  15:31:42  kuipe_j
! Salt restart improved
!
! Revision 1.6  1996/01/17  13:59:24  kuipe_j
! header update
!
! Revision 1.5  1996/01/16  15:01:45  kuipe_j
! Restart improvements
!
! Revision 1.4  1995/10/18  09:00:28  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.3  1995/05/30  09:56:14  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:06:13  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:55  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:33:59  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:15  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer      dsopt   ,nmouth ,nboun ,nbran  ,ngrid  ,ncelst ,&
   &juer    ,ker
   integer      fd_nefis_rst, fd_nefis_new, itim(2)
   real         csa2  (ngrid)   ,csd2  (ngrid) ,&
   &cdcdx0(ngrid)   ,cdcdx1(ngrid) ,cdcdx2(ngrid)  ,&
   &thasca(3    )   ,mouqpu(3,0:2,*),curtim        ,&
   &thcsum(2,nbran) ,timout(2,*)    ,sbdscr(3,nboun),&
   &sbdpar(5,nboun)
   logical      first           ,inires         ,newres
!
!     Declaration of local variables
!
   integer      nentri
   parameter   (nentri=11)
   integer      errr  ,nrerr ,i
   integer      ndim  (nentri)
   character*16 grnams
   character*16 nameel(nentri)  ,quanel(nentri)  ,unitel(nentri) ,&
   &nameac(nentri+1)
   character*64 descel(nentri)
   character*8  txt
   logical      inidum
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
!
!     Definition of elements.
!
   data (ndim(i)  ,descel(i)   ,nameel(i) ,quanel(i) ,&
   &unitel(i),i=1,nentri) /&
!
   &1,'Restart time step'              ,'ISTEP'  ,'t'  ,'-'    ,&
   &1,'Salt concentration (csa2)'      ,'Cs'     ,'C'  ,'kg/m3',&
   &1,'A*D*dc/dx (csd2)'               ,'C''s'   ,'C'  ,'kg/s' ,&
   &1,'c/c0*dc/dx (current sum)'       ,'CDCDX0' ,'<>' ,'kg/m4',&
   &1,'<c/c0*dc/dx> last tide'         ,'CDCDX1' ,'<>' ,'kg/m4',&
   &1,'<c/c0*dc/dx> previous tide'     ,'CDCDX2' ,'<>' ,'kg/m4',&
   &2,'Thatcher Harleman sum'          ,'THCSUM' ,'-'  ,'?'    ,&
   &3,'Dispersion at mouths'           ,'MOUQPU' ,'-'  ,'?'    ,&
   &2,'Tidal information at mouths'    ,'TIMOUT' ,'-'  ,'?'    ,&
   &1,'Administration for <c/c0*dc/dx>','THASCA' ,'-'  ,'?'    ,&
   &2,'Administration at boundaries'   ,'SBDSCR' ,'-'  ,'?'    /
!
   data  grnams   /&
   &'SALT-RES-GROUP' /
!
   errr = 0
!
   if (first) then
!
!        Determine if it is an initial run or a restart run for this
!        module. In the latter case restart information must exist
!        already.
!
      nrerr = esabor
      call sadfst (fd_nefis_rst, grnams ,nentri ,nbran  ,nboun ,&
      &nmouth ,ngrid  ,dsopt  ,ndim   ,nameel ,quanel,&
      &unitel ,descel ,.not.newres    ,inires ,nameac,&
      &errr   )
      if (errr.ne.0) goto 1000
!
      if (inires) then
         if (newres) then
!
!              Initialize a new restart file
!
            nrerr = esabor
            call sadfst (fd_nefis_new, grnams ,nentri ,nbran  ,&
            &nboun  ,nmouth ,ngrid  ,dsopt  ,ndim   ,&
            &nameel ,quanel ,unitel ,descel ,.true. ,&
            &inidum ,nameac ,errr   )
            if (errr.ne.0) goto 1000
         endif
         ncelst = 1
      else
!
!           Read from restart file. Select proper time step first.
!
         nrerr = esarrd
         call statim (fd_nefis_rst, grnams ,nameel(1),itim ,ncelst,&
         &errr   )
         if (errr.ne.0) goto 1000
!
         call sarest (fd_nefis_rst, grnams ,nentri ,nbran  ,nboun ,&
         &nmouth ,ngrid  ,dsopt  ,ncelst ,nameel ,csa2  ,&
         &csd2   ,cdcdx0 ,cdcdx1 ,cdcdx2 ,thasca ,mouqpu,&
         &thcsum ,timout ,sbdscr ,errr   )
         if (errr.ne.0) goto 1000
!
         if (newres) then
!
!              Initialize a new restart file
!
            nrerr = esabor
            call sadfst (fd_nefis_new, grnams ,nentri ,nbran  ,&
            &nboun  ,nmouth ,ngrid  ,dsopt  ,ndim   ,&
            &nameel ,quanel ,unitel ,descel ,.true. ,&
            &inidum ,nameac ,errr   )
            if (errr.ne.0) goto 1000
            ncelst = 1
         else
!
!              Continue with existing restart file
!
            ncelst = ncelst+1
         endif
      endif
!
   else
!
!        Write to restart file.
!
      nrerr = esawrd
      call sawrst (fd_nefis_rst, grnams ,nentri ,nbran  ,nboun ,&
      &ngrid  ,dsopt  ,ncelst ,itim   ,curtim ,nameel,&
      &csa2   ,csd2   ,cdcdx0 ,cdcdx1 ,cdcdx2 ,thasca,&
      &mouqpu ,thcsum ,nmouth ,timout ,sbdscr ,sbdpar,&
      &errr   )
      if (errr.ne.0) goto 1000
      ncelst = ncelst+1
!
   endif
   return
!
1000 continue
   if (errr.gt.0) then
!
!        Could not find restart time step.
      ker = fatal
      call error (juer ,'SARSTA' ,esartt ,ker)
   else
!
!        NEFIS error ( <0 )
      ker = fatal
      write (txt,'(i8)') errr
      call error (juer ,'SARSTA @'//txt//'@' ,nrerr ,ker)
   endif
!
end
