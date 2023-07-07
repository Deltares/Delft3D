import os

def modelpath(slug):
    """return the model path"""
    top_srcdir = get_top_srcdir()
    data_dir = os.path.join(top_srcdir, 'test_data')
    return os.path.join(data_dir, slug)

def get_top_srcdir():
    """search directory of the configure.ac"""
    def dirgen(curdir):
        """generate directories from curdir and up"""
        yield curdir
        for path in dirgen(os.path.join(curdir, os.path.pardir)):
            if os.path.samefile(path, curdir):
                # if we can't go up we're done
                raise StopIteration("no more paths")
            yield path
    try:
        # go up from this file
        topdir = os.path.dirname(__file__)
    except NameError:
        topdir = "."
    for dirname in dirgen(topdir):
        if os.path.exists(os.path.join(dirname, "configure.ac")):
            return dirname
    else:
        raise ValueError("Could not find configure.ac searching from %s upwards" % (topdir))
