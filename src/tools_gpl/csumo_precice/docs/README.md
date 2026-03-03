# Set up clang-format C++ Formatting
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

# Set up clang-tidy static analysis (optional)
We use clang-tidy to get live feedback while you code in your IDE.
Clang-tidy will automatically use the src/tools_gpl/csumo_precice/.clang-tidy file for its settings.
Your editor should be set-up to use run clang-tidy in its language server for providing annotations.

## VSCode
You can either use the Microsoft C/C++ extension (slow) or the clangd extension (fast) as the language server for C++.
Clangd will use precompiled and cached files, and compile headers separate from source files,
so feedback is much faster.
### clangd extension (recommended)
Install the clangd extension. It should prompt you to install clangd, confirm that.
Turn off the intellisense option from the C/C++ extension:
Go to settings (Click on File -> Preferences -> Settings or use the Ctrl+, keyboard shortcut),
and in the User or Workspace settings, go to Extensions -> C/C++ -> IntelliSense -> C_Cpp: Intelli Sense Engine and set it to 'disabled.'
Then, got to Extensions -> clangd -> Arguments, click 'Add Item' and add '--clang-tidy' (without the quotes).
Further, under Extensions -> clangd -> Fallback Flags, add '-std=c++23' (without the quotes).
### C/C++ extension
Go to settings, Extensions -> C/C++ -> Code Analysis and find C_Cpp > Code Analysis > Clang Tidy and set it to 'enabled.'
Then, go to Extensions -> C/C++ -> IntelliSense and find C_Cpp > Default: Cpp Standard and choose 'c++23.'
This is much slower than clangd, because it does not cache any of its reading.

## Visual Studio 2022
In Visual Studio, clang-tidy can be turned on per project.
Go to the C++ project (csumo_precice) in the solution explorer, right click and go to Properties,
or click on the project and go to Project -> Properties.
Then, find Code Analysis -> General -> Enable Clang Tidy and set it to 'Yes.'
It will automatically use the settings from the Visual Studio solution, and also find the project headers.
