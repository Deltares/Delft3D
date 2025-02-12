import os


class Executables:
    """A class to manage and validate the paths of required executables."""

    bibtex = "not set"
    initexmf = "not set"
    makeindex = "not set"
    miktexpm = "not set"
    pdflatex = "not set"
    svnexe = "not set"

    def are_executables_invalid(self) -> bool:
        """
        Check if all required executables are set.

        This method verifies that the following executables are not None:
        - bibtex
        - initexmf
        - makeindex
        - miktexpm
        - pdflatex
        - svnexe
        Returns:
            bool: False if all executables are set, True otherwise.
        """
        if (
            self.bibtex is None
            or self.initexmf is None
            or self.makeindex is None
            or self.miktexpm is None
            or self.pdflatex is None
            or self.svnexe is None
        ):
            return True

        return False

    def assign_installations(self) -> None:
        """Check the installation of required executables."""
        self.bibtex = self.which("bibtex.exe")
        self.initexmf = self.which("initexmf.exe")
        self.makeindex = self.which("makeindex.exe")
        self.miktexpm = self.which("mpm.exe")
        self.pdflatex = self.which("pdflatex.exe")
        self.svnexe = self.which("svn.exe")

        print("Using bibtex   : %s" % self.bibtex)
        print("Using initexmf : %s" % self.initexmf)
        print("Using makeindex: %s" % self.makeindex)
        print("Using miktexpm : %s" % self.miktexpm)
        print("Using pdflatex : %s" % self.pdflatex)
        print("Using svn      : %s" % self.svnexe)

    def is_exe(self, fpath: str) -> bool:
        """Check if the file at the given path is executable.

        Args:
            fpath (str): The file path.

        Returns
        -------
            bool: True if the file is executable, False otherwise.
        """
        return os.path.isfile(fpath) and os.access(fpath, os.X_OK)


    def which(self, program: str) -> str:
        """Locate a program file in the system's PATH.

        Args:
            program (str): The name of the program to locate.

        Returns
        -------
            str: The path to the program if found, None otherwise.
        """
        fpath, fname = os.path.split(program)
        if fpath:
            if self.is_exe(program):
                return program
        else:
            for path in os.environ["PATH"].split(os.pathsep):
                path = path.strip('"')
                exe_file = os.path.join(path, program)
                if self.is_exe(exe_file):
                    return exe_file

        return None

