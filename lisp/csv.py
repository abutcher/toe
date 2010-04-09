#!/usr/bin/env python

import sys
import re

infile = open(sys.argv[1], 'r')
outfile = open(sys.argv[2], 'w')

writeline = ""

for line in infile:
    if not re.match("^,.*", line):
        if writeline != "":
            outfile.write(writeline + '\n')
        writeline = line
    else:
        writeline = writeline + line
        writeline = writeline.replace('\n','')

print "Done!"

infile.close()
outfile.close()
exit(0);
    
