# Set up C++ Formatting
We use clang-format for automatically formatting C++ source files.
Clang-format will automatically use the src/tools_gpl/csumo_precice/.clang-format file for its settings.
Your editor should be set-up to use clang-format when saving the document.

## VSCode
Ensure that the Microsoft C/C++ extension is installed. This ships with clang-format as the default formatter.
To enable format on save, go to settings (Click on File -> Preferences -> Settings or use the Ctrl+, keyboard shortcut).
Choose whether you would like to set it for the User (global) or for the Workspace (this project) by clicking on the proper tab.
Then, go to Text Editor -> Formatting, and tick 'Format On Save.'

## Visual Studio 2022
In the Visual Studio Installer, click 'Modify' and check that 'Desktop development with C++' is checked and installed.
(The optional 'C++ Clang tools for Windows' does not need to be installed).
Open Visual Studio, and under Tools -> Options -> Text Editor -> Code Cleanup, check 'Run Code Cleanup profile on Save.'
Then click on 'Configure Code Cleanup', select the profile that was listed earlier, and add 'Format Document (C++)' to it
by clicking it and using the up arrow. The other options may be removed by clicking the down arrow.
Then, clang-format will be called upon save.
