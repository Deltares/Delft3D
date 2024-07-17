      subroutine fltser (kode   ,nstru  ,ngrid  ,strpar ,strtyp ,h     ,
     &                   ker    ,juer   )
c
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Kuipers
c
c Module:             FLTSER (FLow Tabulated Structure ERror messages)
c
c Module description: Make warning if the domain of the data base of a
c                     structure is too small.
c
c-----------------------------------------------------------------------
c Parameters:
c NAME             IO DESCRIPTION
c kode             I  = 0 call at every completed flow time step
c                       1 call at end of simulation
c nstru               number of structures
c strpar(21,nstru) IO Parameters for data base structure: 
c                     (1,i) interpolation:
c                           1 = linear
c                           2 = spline
c                     (2,i) sequence number of data base structure 
c                           (1,2,…), i.e. data base structure number
c                     (3,i) number of dimensions of data base (2 or 3)
c                     (4,i) number of H1-values nh1
c                     (5,i) number of H2-values nh2
c                     (6,i) number of values of gate height, crest level 
c                           or width
c                           (extension in future; 1 in Sobek 2.51)
c                     (7,i) dummy
c                     (8,i) dummy
c                     (9,i) index of first element in array with 
c                           discharges stdbq for this structure
c                     (10,i) type of values of first row in stdbq for 
c                           this structure:
c                           1 : H2-values
c                           2 : dH-values (dH=H2-H1)
c                     (11,i) number of structure where the data base 
c                           of this structure is defined. This is
c                           the structure with the first occurrence 
c                           of a link to this data base table.
c                     (12,i) indicator that identifies if this data 
c                           base table is initialised:
c                           0 : no
c                           1 : yes
c                     (13,i) level of sill of structure or reference
c                           level
c                     (14,i) indicator that defines if discharge is zero
c                           if sill is dry (0), otherwise 1.
c                     (15,i) minimum H1 (wrt sill) in case of underflow 
c                           (otherwise 1E20)  
c                     (16,i) minimum H2/Dh in case of underflow 
c                           (otherwise 1E20)  
c                     (17,i) maximum H1 (wrt sill) in case of overflow 
c                           (otherwise -1E20)  
c                     (18,i) maximum H2/DH in case of overflow
c                           (otherwise -1E20)
c                     (19,i) 4-digit indicater for under or overflow in
c                           an iteration step.
c                           From right to left a digit corresponds for:
c                           H1-underflow, H2/DH-underflow,
c                           H1-overflow, H2/DH-overflow.
c                           A digit can be 0 (inside) or 1 (outside 
c                           data base).
c                           
c-----------------------------------------------------------------------
c
c     Include constants for array dimensions
c
      include '../include/sobdim.i'
c
c     Declaration of parameters:
c
      integer    kode  ,nstru   ,ngrid ,juer   ,ker
      integer    strtyp(10,nstru)
      real       strpar(dmstrpar,nstru)
      double precision h(ngrid)
c      
c     Declaration of local variables:
c      
      integer    il    ,ir     ,limit   ,lstnam, istru
      real       h1    ,h2     ,zs
      logical    deltah
      character  strnam*40
      character  h2txt*3
      character  txt*13 
c
c     Include sobek constants and error codes
c
      include '../include/errcod.i'
      include '../include/sobcon.i'

      do istru=1,nstru
         if ( strtyp(1,istru) .eq. cdtbst ) then
            if (kode.eq.0) then 
c              Time step completed
c              Update arguments that exceed data base
               il      = strtyp(3,istru)
               ir      = strtyp(4,istru)
               deltah  = nint(strpar(10,istru)).eq.2 
               zs      =      strpar(13,istru)
               limit   = nint(strpar(19,istru))
c
               if (limit.gt.0) then
                  h1 = sngl( h(il) ) - zs
                  if (deltah) then
                     h2 = sngl( h(il) - h(ir) )
                   else
                     h2 = sngl( h(ir) ) - zs
                  endif
                  if (mod(limit,10).eq.1) 
     +                strpar(15,istru) = min(strpar(15,istru),h1) 
                  if (mod(limit/10,10).eq.1)  
     +                strpar(16,istru) = min(strpar(16,istru),h2)             
                  if (mod(limit/100,10).eq.1) 
     +                strpar(17,istru) = max(strpar(17,istru),h1)             
                  if (mod(limit/1000,10).eq.1) 
     +                strpar(18,istru) = max(strpar(18,istru),h2)             
               endif
            else
c              Simulation completed
c              Make messages if any 
               call getstr(istru,strnam,lstnam)
               if (deltah) then
                  h2txt = 'DH='
               else
                  h2txt = 'H2='
               endif
               if (strpar(15,istru) .lt. 1.e19) then
                  call getstr(istru,strnam,lstnam)
                  ker = warnng
                  write (txt,'(a,f7.2,a)') ' (H1=',strpar(15,istru),')'
                  call sre_error (juer,
     +                        'FLTSER @Underflow@ in db structure @'
     +                        //strnam(:lstnam)//txt//'@', efllim, ker)               
               endif
               if (strpar(16,istru) .lt. 1.e19) then
                  call getstr(istru,strnam,lstnam)
                  ker = warnng           
                  write (txt,'(2a,f7.2,a)') 
     +                       ' (',h2txt,strpar(16,istru),')'
                  call sre_error (juer,
     +                        'FLTSER @Underflow@ in db structure @'
     +                        //strnam(:lstnam)//txt//'@', efllim, ker)               
               endif               
               if (strpar(17,istru) .gt. -1.e19) then
                  call getstr(istru,strnam,lstnam)
                  ker = warnng
                  write (txt,'(a,f7.2,a)') ' (H1=',strpar(17,istru),')'
                  call sre_error (juer,
     +                        'FLTSER @Overflow@ in db structure @'
     +                        //strnam(:lstnam)//txt//'@', efllim, ker)               
               endif
               if (strpar(18,istru) .gt. -1.e19) then
                  call getstr(istru,strnam,lstnam)
                  ker = warnng
                  write (txt,'(2a,f7.2,a)')
     +                       ' (',h2txt,strpar(18,istru),')'
                  call sre_error (juer,
     +                        'FLTSER @Overflow@ in db structure @'
     +                        //strnam(:lstnam)//txt//'@', efllim, ker)               
               endif            
            endif
         endif
      enddo
c      
      end
