import sys
import os
from operator import itemgetter
from collections import defaultdict

class ConfigReaderException (Exception) : 
    pass

class Sobek2ConfigFile : 
    
    def __init__ (self, path) : 
        self.configLines = []
        data = None
        with open(path) as f : 
            data = f.read()
        i_start = data.find ("set elements {") + 15
        i_end = data.find ("{", i_start) -1
        lines = [ l for l in data[i_start:i_end].split("\n") if l.strip() != "" and l.strip() != "}" ]
        for line in lines : 
            try : 
                if not line.strip().lower().startswith("qwb.his") :   # Water balance not supported. 
                    self.configLines.append(self.line2elements(line))
            except ConfigReaderException as e : 
                print str(e)
    
    def line2elements (self, line) :
        # Return a list of 5-tuples, containing the lines of the sobek.cnf file. 
        # For instance: [ (UPFLOWDT.HIS "Groundw.outfl." "2" 0.00003 100), ... ]  
        chunks = [ l for l in line.split(" ") if l.strip() != "" ]
        elementLst = []
        iChunk = 0
        chunksToMerge = []
        while iChunk < len(chunks) : 
            chunk = chunks[iChunk]

            if chunksToMerge == [] : 
                # Not a pending quote
                if not chunk.startswith('"') and chunk.endswith('"') : 
                    raise ConfigReaderException ("Cannot close quote without opening quote: " + line)
                elif chunk.startswith('"') and not chunk.endswith('"') : 
                    # There is a starting quote
                    chunksToMerge.append(chunk[1:])
                elif chunk.startswith('"') and chunk.endswith('"') : 
                    # Just normal: one element == one chunk. Remove quotes, though. 
                    elementLst.append(chunk[1:-1])
                else : 
                    # Just normal: one element == one chunk. 
                    elementLst.append(chunk)
            else : 
                # Pending quote
                if chunk.startswith('"') : 
                    raise ConfigReaderException ("Accumulating opening quotes: " + line)
                elif chunk.endswith('"') : 
                    # End quote found
                    chunksToMerge.append(chunk[:-1])
                    elementLst.append (" ".join(chunksToMerge))
                    chunksToMerge = []
                else : 
                    # No end quote found
                    chunksToMerge.append(chunk)
            iChunk += 1
        
        if len(elementLst) != 5 : 
            raise ConfigReaderException (str(len(elementLst)) + " elements found in line: " + str(elementLst))
        if len(chunksToMerge) != 0 :
            raise ConfigReaderException ("Quote not properly closed: " + line)
        
        # Turn 4th and 5th elements into floats. 
        elementLst[3] = float(elementLst[3])
        elementLst[4] = float(elementLst[4])

        return elementLst
            
    
def readAllConfigFiles(rootPath) :
    
    sobekConfigFiles = []
    for root, dirs, filenames in os.walk (rootPath) :
        for filename in filenames :
            if filename.lower() == "sobek.cnf" : 
                sobekConfigFiles.append (os.path.join(root, filename))
    
    d = defaultdict(int)
    for sobekCnfFilename in sobekConfigFiles : 
        sobekConfig = Sobek2ConfigFile (sobekCnfFilename)
        
        for configLine in sobekConfig.configLines : 
            d[ (configLine[0].lower(), configLine[1].lower()) ] += 1
    
    # Show stats
    for (k, v) in sorted(d.items(), key=itemgetter(1)) : 
        print "%-15s & %-30s & %6i" % (k[0], k[1], v)
   
    
if __name__ == "__main__" : 
    readAllConfigFiles(sys.argv[1])