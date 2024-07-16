      SUBROUTINE PUTPER (StreamName,DataSetName)
C
C     DELFT HYDRAULICS
C
C     CREATED             : jan  2001 by J.v.Gils
C
C     FUNCTION            : Gives permission to DIO to proceed
C                           one step (synchronised mode)
C
C
      use dio_plt_rw
      include 'dio_function77.inc'

      character*(*) StreamName,DataSetName

      integer Nr_Variables, Nr_Locations, Nr_Times

      character(dioMaxParLen) vars
      character(dioMaxLocLen) locs
      character(dioMaxTimLen) tims

      dimension vars(1)
      dimension locs(1)
      dimension tims(1)

      real      values
      dimension values(1,1)

      integer dioOutStream
      integer dioOutSet

      logical first
      save first
      data first /.true./

      if ( first ) then

      first = .false.

C     Open data stream
      call dumpTimeStep('SOBEK PUTPER Start Init')
      dioOutStream = DioCreateStreamSynched(dio_Binary_stream,
     +                    StreamName, 'w')

C     Create data set
      Nr_Locations = 1
      Nr_Variables = 1
      locs(1) = 'dioLocNam'
      vars(1) = 'dioVarNam'
      tims(1) = 'dioDate'
      dioOutSet = DioDefinePltDataSet (
     j              dioOutStream,
     j              DataSetName,
     +              Dio_Plt_Real,
     +              Nr_Variables,vars,
     +              Nr_Locations,locs)
      values(1,1) = 0.0
      call dumpTimeStep('SOBEK PUTPER End   Init')

      endif

C     Put dataset values

      call dumpTimeStep('SOBEK PUTPER Start')
      call DioPutPltDataSetReals (dioOutSet,tims(1),1,1,values)
      call dumpTimeStep('SOBEK PUTPER End')

      RETURN
      END
