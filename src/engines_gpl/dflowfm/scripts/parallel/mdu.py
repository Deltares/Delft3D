import collections
import ctypes
import logging
import os
import platform
import ConfigParser as configparser

class MduParserKeepComments(configparser.ConfigParser):
    """
    parse an mdu file, without splitting comments, without lowercasing
    """

    def optionxform(self, optionstr):
        return str(optionstr)

    def write(self, fp):
        """Write an .ini-format representation of the configuration state."""
        if self._defaults:
            fp.write("[%s]\n" % configparser.DEFAULTSECT)
            for (key, value) in self._defaults.items():
                fp.write("%s = %s\n" % (key, str(value).replace('\n', '\n\t')))
            fp.write("\n")

        # compute max length:
        maxkey = 0
        maxval = 0
        for section, options in self._sections.items():
            for key, value in options.items():
                maxkey = max(len(key), maxkey)
                # lookup length before comment
                maxval = max(len(value.split("#", 1)[0]), maxval)
        lineformat = ("{key:%d} = {val}" % (maxkey, ))
        for section, options in self._sections.items():
            fp.write("[%s]\n" % section)
            for (key, value) in options.items():
                if key == "__name__":
                    continue
                if (value is not None) or (self._optcre == self.OPTCRE):
                    val = str(value).replace('\n', '\n\t')
                    split = val.split("#", 1)  # split of first comment
                    # we have comments in the value, align....
                    if len(split) > 1:
                        valformat = "{val:%d} # {comment}" % (maxval,)
                        val = valformat.format(val=split[0],
                                               comment=split[1].strip())

                line = lineformat.format(key=key, val=val)
                fp.write("%s\n" % (line))
            fp.write("\n")
