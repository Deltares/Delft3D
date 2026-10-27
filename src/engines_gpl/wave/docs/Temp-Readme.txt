Install the below software tools:
- Doxygen 1.13.2 (installed in C:\Program Files\doxygen, by default)
- DrawIO v26.2.15 (installed from Microsoft store)
- Graphviz 12.2.1 (installed in C:\Program Files\Graphviz, by default)


The structure for developer documentation per project:

.\docs
|-\source\<project>-mainpage.dox -> Contains main page information for the project.
|-<project>-doxyfile.dxg -> Doxygen settings for the project
|-\source\ -> Contains dox, Markdown and tex pages.
|-\include\ -> Contains drawio/png/jpg/dot and other media source files that can be included in the doxygen documentation
|-\header\ -> COntains project specific logos, header images, etc.


..\result_doc -> Doxygen output directory per project. Not part of git.



Starting with Doxygen -> https://www.doxygen.nl/manual/starting.html


Building the D-Waves developer guide:

- Run Doxygen from this docs directory so the configuration's relative paths
  resolve correctly:

    doxygen dwaves-doxyfile.dxg

- Open ..\result_doc\html\index.html.
- The main page links to the Markdown communication-model and pathway pages.

Markdown assets and GitHub:

The documentation is intended for Doxygen, which renders the DOT diagrams.
GitHub Markdown does not render DOT files; the corresponding SVG previews can
be browsed in the include\ folder.