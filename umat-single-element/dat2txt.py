#!/usr/bin/env python
"""
    Extracts data from a DAT file and saves data into a TXT file.
"""
import re
import glob
import os

data={}
pH = re.compile(' (.+)for .*set\\s(\\S+) and time  (.+)')
skip = 0 # if empty lines are to be skipped
body = 0 # if data lines are expected
nev = 0 # number of eigenvalue file
stop_on_empty = 0 # empty line triggers end of body

def get_name(names, file_name):
    for name in names:
        if name in file_name:
            return name
    raise Exception("The name has not been found!")

dat_file = input("Please enter the name of the DAT file:")
models = ['linde', 'tsaiwu', 'maxstress', 'hashin', 'maxstrain','ortho']
output_vars = ['stress', 'strain', 'displacement', 'force', 'energy']

os.chdir(os.getcwd()+'\\data')
files = glob.glob('*.dat')

if dat_file+'.dat' in files: 
    print('Working on the ', dat_file, ' file' )
else:
    print('No DAT file with a given name has been found in the current dir...')
    print('Available DAT files:')
    for f in files:
        print('  ', f)
    quit()


f = open(dat_file+'.dat',"r")
model_name = get_name(models, dat_file)


for line in f:
    if skip: # if the line is known to be useless
        skip = skip -1
        continue
    line = line.replace("(","")
    line = line.replace(")","")
    line = line.replace("\n","")
    b = pH.match(line)
    if b: # a result header was found
        resname = b.group(1)
        group = b.group(2)
        res = resname+"_"+group
        res = res.replace(" _","_")   # remove spaces in front of _
        time = float(b.group(3))
        file_name = get_name(output_vars, resname) +'-'+ model_name
        if not (res in data.keys()): # new result type
            data[res] = open(file_name+".txt","w")
            print(file_name)
        data[res].write("\n"+str(time)+" ")
        line_end = " "
        body = 1
        stop_on_empty = 0
    elif line.startswith("     E I G E N V A L U E   O U T P U T"):
        # eigenvalue data
        nev+=1
        res = "Eigenvalues_"+str(nev)
        print(res)
        data[res] = open(res+".txt","w")
        data[res].write("# mode  EV  Re(f)_rad/time Re(f)_cycles/time Im(f)_rad/time\n")
        line_end = "\n"
        body = 1
        skip = 5 
        stop_on_empty = 1
    elif line.startswith("     P A R T I C I P A T I O N   F A C T O R S"):
        # eigenvalue data
        res = "Eigenvalues_PF_"+str(nev)
        print(res)
        data[res] = open(res+".txt","w")
        data[res].write("# mode  UX  UY UZ RX RY RZ\n")
        line_end = "\n"
        body = 1
        skip = 3        
        stop_on_empty=1
    elif line.startswith("     E F F E C T I V E   M O D A L   M A S S"):
        # eigenvalue data
        res = "Eigenvalues_MM_"+str(nev)
        print( res)
        data[res] = open(res+".txt","w")
        data[res].write("# mode  UX  UY UZ RX RY RZ\n")
        line_end = "\n"
        body = 1
        skip = 3        
        stop_on_empty=1
    else:
        if body==1:
            if not "force" in line and not "center" in line:
                data[res].write(line+line_end)
            if stop_on_empty and line =="":
                body = 0

for name in data.keys():
    data[name].close()