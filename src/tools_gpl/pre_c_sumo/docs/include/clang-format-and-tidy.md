\page clang_format_and_tidy_setup clang-format and clang-tidy setup

# clang-format and clang-tidy setup

This page contains the C++ formatting and static-analysis setup for preC-SUMO.

## Set up clang-format C++ formatting

We use clang-format for automatically formatting C++ source files.
Clang-format will automatically use the `src/tools_gpl/pre_c_sumo/.clang-format` file for its settings.
Your editor should be set up to use clang-format when saving the document.

### VSCode

Ensure that the Microsoft C/C++ extension is installed. This ships with clang-format as the default formatter.
To enable format on save, go to Settings (click File -> Preferences -> Settings or use the Ctrl+, keyboard shortcut).
Choose whether you would like to set it for the User (global) or for the Workspace (this project) by clicking the proper tab.
Then, go to Text Editor -> Formatting and tick `Format On Save`.

### Visual Studio 2022

In the Visual Studio Installer, click `Modify` and check that `Desktop development with C++` is checked and installed.
(The optional `C++ Clang tools for Windows` does not need to be installed.)
Open Visual Studio, and under Tools -> Options -> Text Editor -> Code Cleanup, check `Run Code Cleanup profile on Save.`
Then click `Configure Code Cleanup`, select the profile that was listed earlier, and add `Format Document (C++)` to it by clicking it and using the up arrow. The other options may be removed by clicking the down arrow.
Then, clang-format will be called upon save.

## Set up clang-tidy static analysis (optional)

We use clang-tidy to get live feedback while you code in your IDE.
Clang-tidy will automatically use the `src/tools_gpl/pre_c_sumo/.clang-tidy` file for its settings.
Your editor should be set up to run clang-tidy in its language server for providing annotations.

### VSCode

You can either use the Microsoft C/C++ extension (slow) or the clangd extension (fast) as the language server for C++.
Clangd will use precompiled and cached files, and compile headers separately from source files, so feedback is much faster.

#### clangd extension (recommended)

Install the clangd extension. It should prompt you to install clangd; confirm that.
Turn off the IntelliSense option from the C/C++ extension:
Go to Settings (click File -> Preferences -> Settings or use the Ctrl+, keyboard shortcut),
and in the User or Workspace settings, go to Extensions -> C/C++ -> IntelliSense -> C_Cpp: Intelli Sense Engine and set it to `disabled`.
Then, go to Extensions -> clangd -> Arguments, click `Add Item`, and add `--clang-tidy`.
Further, under Extensions -> clangd -> Fallback Flags, add `-std=c++23`.

#### C/C++ extension

Go to Settings, then Extensions -> C/C++ -> Code Analysis and find `C_Cpp > Code Analysis > Clang Tidy`, then set it to `enabled`.
Then, go to Extensions -> C/C++ -> IntelliSense and find `C_Cpp > Default: Cpp Standard`, and choose `c++23`.
This is much slower than clangd because it does not cache reads.

### Visual Studio 2022

In Visual Studio, clang-tidy can be turned on per project.
Go to the C++ project (preC-SUMO) in the Solution Explorer, right-click it and go to `Properties`,
or click the project and go to `Project -> Properties`.
Then, find `Code Analysis -> General -> Enable Clang Tidy` and set it to `Yes`.
It will automatically use the settings from the Visual Studio solution and also find the project headers.
