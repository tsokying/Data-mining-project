# encoding: utf-8

import os
import io
import codecs

#loop every txt file in cwd
for file in os.listdir(os.getcwd()):
    filename = os.fsdecode(file)
    if filename.endswith(".txt"): 
        
        #newdir
        newpath = r'para'
        if not os.path.exists(newpath):
            os.makedirs(newpath)
        doc = os.path.splitext(filename)[0]

        #split by newline
        with io.open(file, "r", encoding="utf-8") as f:
            lines = f.read().split('\n')
            for (n, line)  in enumerate(lines):
                codecs.open( "para/{}_{}.txt".format( doc, n), "w", "utf-8").write(line)
