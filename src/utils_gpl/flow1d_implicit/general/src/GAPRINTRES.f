      subroutine GAPrintResults (luanpri ,dattimsim ,request)   
c
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Getij Analyse Module
c
c Programmer:         J.Kuipers
c
c Module:             GAPrintResults
c
c Module description: The results of the tidal analyses of one 
c                     variable type are written to an Ascii file which
c                     can be used as print report.                     
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 1  lunapri           I  unit number print file tidal analyses  
c 2  dattimsim(2)      I  start date and time of simulation 
c                         (Sobeksim format) 
c 3  request           I  type of variable to be printed 
c-----------------------------------------------------------------------
c Subprogram calls:
c gapridattim       Date and time for print
c=======================================================================
c
c     Declaration of Parameters:
c
      use         gadata
c      
      integer     luanpri   ,request 
      integer     dattimsim(2)     
c
c     Declaration of local variables:
c      
      integer     vartype,ivar  ,iloc  ,ntides ,nr     ,iduur  ,date1  ,
     +            time1  ,date2 ,time2 ,datemin,timemin,datemax,timemax,
     +            index  ,i
      real        tbegin ,tend  ,tduur ,gem    ,tslag   ,tmin   ,tmax  ,
     +            minimum,maximum      ,vloedv ,ebv     
      character*3 head(3),txt 
c      
      data       head/'Con','SAL','CHL'/
c
      index = 0
      vartype = garesult(index+type)
      do while (vartype.gt.0)
         ntides   = garesult(index+ngetij)
         if (vartype.eq.request) then
            ivar     = garesult(index+grooth)
            iloc     = garesult(index+locat)
            index = index + 4
c           print header 
            write (luanpri,100) parnam(ivar)
            write (luanpri,101) locnam(iloc)
            if (vartype.eq.hreeks) then
               write (luanpri,200)
            else if (vartype.eq.qreeks) then
               write (luanpri,201)
            else if (vartype.eq.vreeks) then
               write (luanpri,202)
            else
               txt = head(request-vreeks)
               write (luanpri,203) txt,txt,txt,txt
            endif
            nr = 0
            do i=1,ntides
               nr      = garesult(index+igetij)
               tbegin  = garesult(index+begin)
               tend    = garesult(index+eind)
               tduur   = garesult(index+duur)
               gem     = garesult(index+gemidd)
               tslag   = garesult(index+slag)
               Tmin    = garesult(index+mintijd)
               Tmax    = garesult(index+maxtijd)
               minimum = garesult(index+minw)   
               maximum = garesult(index+maxw)  
               vloedv  = garesult(index+vloed)
               ebv     = garesult(index+eb)
               call gapridattim (dattimsim,tbegin,date1,time1)
               call gapridattim (dattimsim,tend  ,date2,time2)
               call gapridattim (dattimsim,tmin  ,datemin,timemin)
               call gapridattim (dattimsim,tmax  ,datemax,timemax)
               iduur = nint(tduur/60.)
c                      
               if (vartype.eq.hreeks) then
c                 water level               
                  write (luanpri,300) nr,date1,time1,date2,time2,iduur,
     +                                gem,tslag,datemin,timemin,
     +                                minimum,datemax,timemax,maximum
               else if (vartype.eq.qreeks) then
c                 discharge               
                  write (luanpri,301) nr,date1,time1,date2,time2,iduur,
     +                                gem,datemin,timemin,
     +                                minimum,datemax,timemax,maximum,
     +                                vloedv,ebv
               else if (vartype.eq.vreeks) then
c                 velocity               
                  write (luanpri,302) nr,date1,time1,date2,time2,iduur,
     +                                gem,datemin,timemin,
     +                                minimum,datemax,timemax,maximum,
     +                                ebv,vloedv
               else
c                 concentration
                  write (luanpri,303) nr,date1,time1,date2,time2,iduur,
     +                                gem,tslag,datemin,timemin,
     +                                minimum,datemax,timemax,maximum
               endif
               index = index + 12
            enddo   
            if (vartype.eq.hreeks) then
c              water level               
               write (luanpri,400) 
            else if (vartype.eq.qreeks) then
c              discharge               
               write (luanpri,401) 
            else if (vartype.eq.vreeks) then
c              velocity               
               write (luanpri,402) 
            else
c              concentration
               write (luanpri,403) 
            endif
         else
            index = index + 4 +ntides * 12
         endif
         vartype = garesult(index+type)
      enddo
c
  100 format (' Parameter : ',a20)
  101 format (' Location  : ',a20)
  200 format (1x,90('-'),
     +      /' Tide    Start           End      Tidal  Mean Tidal',
     +        '     Low Water          High Water',
     +      /'                                 Period Sea L Range',
     +      /'          date time     date time [min]   [m]   [m]',
     +        '     date time   [m]     date time   [m]',
     +      /1x,90('-'))
  201 format (1x,116('-'),
     +      /' Tide    Start           End      Tidal Residual   ',
     +        '    Minimum                 Maximum               ',
     +        ' Flood       Ebb',
     +      /'                                 Period     Flow   ',50x,
     +        'Volume    Volume',
     +      /'          date time     date time [min]   [m3/s]   ',
     +        '  date time    [m3/s]     date time    [m3/s]     ',
     +        '  [m3]      [m3]',
     +      /1x,116('-'))
  202 format (1x,105('-'),
     +      /' Tide    Start           End      Tidal    Av.     ',
     +        '  Minimum               Maximum             Av.   ',
     +        '  Av.',
     +      /'                                 Period           ',41x,
     +        ' pos. V  neg. V',
     +      /'          date time     date time [min]  [m/s]     ',
     +        'date time   [m/s]     date time   [m/s]   [m/s]   ',
     +        '[m/s]',
     +      /1x,105('-'))     
  203 format (1x,97('-'),
     +      /' Tide    Start           End      Tidal    Av.   De',
     +        'lta       Minimum               Maximum',
     +      /33x,'Period    ',a3,5x,a3,19x,a3,19x,a3,
     +      /'          date time     date time [min][kg/m3] [kg/',
     +        'm3]     date time [kg/m3]     date time [kg/m3]',
     +      /1x,97('-'))  
  300 format (1x,i4,2(1x,i8,1x,i4),i5,f7.2,f6.2,2(1x,i8,1x,i4,f6.2))
  301 format (1x,i4,2(1x,i8,1x,i4),i5,f10.2,2(1x,i8,1x,i4,f10.2),
     +        e11.3,e10.3)
  302 format (1x,i4,2(1x,i8,1x,i4),i5,f8.2,2(1x,i8,1x,i4,f8.2),
     +        2f8.2)
  303 format (1x,i4,2(1x,i8,1x,i4),i5,2f8.3,2(1x,i8,1x,i4,f8.2))
  400 format (1x,90('-')/)
  401 format (1x,116('-')/)
  402 format (1x,105('-')/)
  403 format (1x,97('-')/)
c
      end
c
      subroutine gapridattim (dattimsim,dt,date,time)
c     
c     Convert time in seconds to printable date and time 
c     The time is in minutes
c
      real                  dt
      integer               date,time,dattimsim(2)
c
      integer               yy,mm,dd,hh,min,ss  
      integer               dattimcur(2)        
      real                  dtr
      double precision      dtd
c
      dtr   = nint(dt/60.)
      dtd   = nint(dtr*60.) 
      dattimcur = dattimsim
      call sotime (dattimcur,dtd)
      call parsdt (dattimcur,yy,mm,dd,hh,min,ss)
      date = yy*10000+mm*100+dd
      time = hh*100+min
      end
