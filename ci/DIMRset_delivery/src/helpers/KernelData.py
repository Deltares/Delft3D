class KernelData(object):
    """
    Class to hold the various variations of names for a kernel.
    """
    def __init__(self, name_for_extracting_revision: str, name_for_email: str):
        """
        Creates a new instance of KernelData.

        Args:
            name_for_extracting_revision (str): The name of the kernel used to extract the revision number from
            the build dependencies.
            name_for_email (str): The name to represent the kernel in the email.
        """
        self.name_for_extracting_revision = name_for_extracting_revision
        self.name_for_email = name_for_email
