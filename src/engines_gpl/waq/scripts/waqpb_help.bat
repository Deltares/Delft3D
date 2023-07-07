@echo off
echo.
echo About the DELWAQ proces library tools
echo.
echo Please build the waq proces library tools first. You can build
echo them using build_all\all.sln.
echo
echo To modify/add to the proces library:
echo   - first run export_procdef_csvfiles.bat. This will 'export' the data in
echo     the csv-tables to a proc_def, a procesm.asc and latex files
echo     for the manuals.
echo   - copy procesm.asc to proces.asc, modify the latter to include
echo     the modifications you want.
echo   - run import_procesasc_changes.bat to import changes into the csv-files.
echo   - modify version information in export_procdef_csvfiles.bat.
echo   - run export_procdef_csvfiles.bat again to create a new proc_def, new 
echo     tables, and a new procesm.asc to see if proces.asc was
echo     imported correctly.
echo
echo   - note: by default export_procdef_csvfiles.bat and import_procesasc_changes.bat 
echo     export/import the database in the new proces.asc
echo     format. This allows longer descriptions because the unit has a
echo     seperate field. Using the old format is still possible
echo     by adding the argument -oldfrm. This functionality will be depreciated
echo     in the furture.
echo
echo   - note 2: It is also possible to make (usually small) changes in
echo     the csv-files directly. E.g. existing items will not always because
echo     updated when you make changes in the proces.asc. Then you just have
echo     to use export_procdef_csvfiles.bat to generate a new proc_def.
echo
pause
