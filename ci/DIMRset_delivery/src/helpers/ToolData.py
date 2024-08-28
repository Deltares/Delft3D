class ToolData(object):
    """
    Class to hold the various variations of names for a tool.
    """

    def __init__(self, name_for_extracting_version: str, name_for_svn_log: str):
        """
        Creates a new instance of ToolData.

        Args:
            name_for_extracting_version (str): The name of the tool used to extract the version number from
            the artifact.
            name_for_svn_log (str): The name of the tool used in the SVN log message.
        """
        self.name_for_extracting_version = name_for_extracting_version
        self.name_for_svn_log = name_for_svn_log
