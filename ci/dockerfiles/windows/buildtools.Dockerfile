# Use the Windows Server Core 2022 image form ICT.
FROM containers.deltares.nl/base_windows_containers/server:ltsc2022

# Restore the default Windows shell for correct batch processing.
SHELL ["cmd", "/S", "/C"]

# Download the VS19 bootstrapper from https://aka.ms/vs/17/release/vs_community.exe
# This one is cached on a fileshare because the curl commando is not stable
ADD vs_Community.exe C:/

# Install visual studio with workloads, excluding workloads and components with known issues.
RUN (start /w vs_Community.exe --passive --wait --noUpdateInstaller --norestart --nocache \
    --installPath "C:\\Program Files (x86)\\Microsoft Visual Studio\\2022\\Community" \
    --add Microsoft.VisualStudio.Workload.NativeDesktop;includeRecommended \
    --add Microsoft.VisualStudio.Component.Windows10SDK.18362 \
    --add Microsoft.VisualStudio.Component.VC.ATLMFC \
    --add Microsoft.VisualStudio.Component.VC.CLI.Support \
    --remove Microsoft.VisualStudio.Component.Windows10SDK.10240 \
    --remove Microsoft.VisualStudio.Component.Windows10SDK.10586 \
    --remove Microsoft.VisualStudio.Component.Windows10SDK.14393 \
    --remove Microsoft.VisualStudio.Component.Windows81SDK \
    || IF "%ERRORLEVEL%"=="3010" EXIT 0)

# Set powershell as shell
SHELL ["powershell", "-Command", "$ErrorActionPreference = 'Stop'; $ProgressPreference = 'Continue'; $verbosePreference='Continue';"]

# Enable long pathnames
RUN reg add "HKLM\SYSTEM\CurrentControlSet\Control\FileSystem" /v "LongPathsEnabled" /t REG_DWORD /d 1 /f

# Install the Intel toolkits (combine ADD and RUN for better layer caching)
ADD intel-fortran-compiler-2025.2.0.535_offline.exe w_onemkl_p_2024.2.2.16_offline.exe intel-mpi-2021.16.0.441_offline.exe C:/
RUN Start-Process -FilePath "C:\intel-fortran-compiler-2025.2.0.535_offline.exe" -ArgumentList "--silent", "-a", "--action", "install", "--eula=accept", "-p=NEED_VS2022_INTEGRATION=1", "-s" -Wait ; \
    Start-Process -FilePath "C:\w_onemkl_p_2024.2.2.16_offline.exe" -ArgumentList "--silent", "-a", "--action", "install", "--eula=accept", "-p=NEED_VS2022_INTEGRATION=1", "-s" -Wait ; \
    Start-Process -FilePath "C:\intel-mpi-2021.16.0.441_offline.exe" -ArgumentList "--silent", "-a", "--action", "install", "--eula=accept", "-s" -Wait ; \
    Remove-Item C:\*.exe

# Install CMake and create junction points in one layer
ADD cmake-3.31.0-rc1-windows-x86_64.msi C:\\cmake-3.31.0-rc1-windows-x86_64.msi
RUN Start-Process msiexec -Wait -ArgumentList '/i C:\\cmake-3.31.0-rc1-windows-x86_64.msi /qn' ; \
    setx /M PATH $($Env:PATH + ';C:\\Program Files\\CMake\\bin') ; \
    Remove-Item C:\\cmake-3.31.0-rc1-windows-x86_64.msi ; \
    New-Item -ItemType Junction -Path 'C:\\VS2022' -Target 'C:\\Program Files (x86)\\Microsoft Visual Studio\\2022\\Community' -Force ; \
    New-Item -ItemType Junction -Path 'C:\\Intel' -Target 'C:\\Program Files (x86)\\Intel' -Force

# Set environment variables after junction points are created
ENV ONEAPI_ROOT="C:\\Intel\\oneAPI\\"
ENV VS2022INSTALLDIR="C:\\VS2022\\"

# Download and install Python 3.12.7
ADD python-3.12.7-amd64.exe C:\\python-3.12.7-amd64.exe
RUN Start-Process python-3.12.7-amd64.exe -ArgumentList '/quiet InstallAllUsers=1 PrependPath=1' -Wait
RUN Remove-Item C:\\python-3.12.7-amd64.exe
RUN New-Item -ItemType SymbolicLink -Path 'C:\\Program Files\\Python312\\python3.exe' -Target 'C:\\Program Files\\Python312\\python.exe'

# Download and install git
ADD Git-2.47.0-64-bit.exe C:\\Git-2.47.0-64-bit.exe
RUN Start-Process -FilePath "C:\\Git-2.47.0-64-bit.exe" -ArgumentList "/SILENT" -Wait
RUN Remove-Item C:\\Git-2.47.0-64-bit.exe
RUN setx /M PATH $($Env:PATH + ';C:\\Program Files\\Git\\usr\\bin')

# Copy the setup script into the container
COPY set-env-vs2022.cmd C:\\set-env-vs2022.cmd

# Set the default command to run the setup script
CMD ["cmd", "/S", "/C", "C:\\set-env-vs2022.cmd"]