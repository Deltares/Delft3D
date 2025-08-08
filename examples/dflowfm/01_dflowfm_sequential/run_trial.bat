@ echo off

rem The following command will result in error message:
rem Invoke-DIMRsetActivator : Could not determine script directory
rem At C:\Users\mourits\Downloads\tmp\x64\bin\activate.ps1:138 char:1
rem + Invoke-DIMRsetActivator @args
rem + ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rem     + CategoryInfo          : NotSpecified: (:) [Write-Error], WriteErrorException
rem     + FullyQualifiedErrorId : Microsoft.PowerShell.Commands.WriteErrorException,Invoke-DIMRsetActivator

Powershell.exe -executionpolicy remotesigned . c:\Users\mourits\Downloads\tmp\x64\bin\activate.ps1

