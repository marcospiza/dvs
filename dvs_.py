#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Mar 23 12:25:47 2022

@author: marcos
"""

# Assess WOFOST simulations

import matplotlib
#from matplotlib.ticker import FormatStrFormatter
import matplotlib.pyplot as plt
import numpy as np
from sys import exit
#import matplotlib.gridspec as gridspec
import time as tm

def locvar(varname,lines):
#  varname: name of the variable given in the file
#  lines  : list of lines returned by readlines() procedure    
   c1 = [] 
   for l in lines:
        l = l.split("=")
        c1.append(l[0].strip().lower())
   return c1.index(varname.lower())     




def RLR(varname,lines):
#  Purpose: reads a list type variable anywhere in a file and assigns as real
# Example of reading:
#    DTSMTB   =    0.00,    0.00,     ! daily increase in temp. sum
#                  35.00,   1.00,     ! as function of av. temp. [cel; cel d]
#                  45.00,   0.5
#    DVSI     =    0.    ! initial DVS
#  Argumments:
#  varname: name of the variable given in the file
#  lines  : list of lines returned by readlines() procedure    

    loc = locvar(varname, lines)
    
    lOut = []
    ldummy = lines[loc].split("=")
    ldummy = ldummy[1].split(",")
    lOut.append(float(ldummy[0]))
    lOut.append(float(ldummy[1]))
#    print(lOut)
    
    flag = True
    while(flag):
         loc = loc + 1
         ldummy = lines[loc].split(",")
         lOut.append(float(ldummy[0]))
         lOut.append(float(ldummy[1]))
         flag = not("=" in lines[loc+1]) and "," in lines[loc]
#         print(flag)
    return np.array(lOut)

def readchar(varname,lines):
#  Argumments:
#  varname: name of the variable given in the file
#  lines  : list of lines returned by readlines() procedure    
  loc = locvar(varname, lines)
  l = lines[loc].split("=")
  if ("!" in l[1]):
      ll = l[1].split("!")
      vout = ll[0].strip()
      if ((vout[0] == '"' and not(vout[-1] == '"')) or \
             (vout[0] == "'" and not(vout[-1]) == "'")):
          print("Error in char value format for " + varname)
          exit()
      elif(vout[0] == '"' or vout[0] == "'"):
          return vout[1:-1]
      else:
          return vout 
  else:
      vout = l[1].strip()
      if ((vout[0] == '"' and not(vout[-1] == '"')) or \
             (vout[0] == "'" and not(vout[-1]) == "'")):
          print("Error in char value format for " + varname)
          exit()
      elif(vout[0] == '"' or vout[0] == "'"):
          return vout[1:-1]
      else:
          return vout 
  
def readfloat(varname,lines):
#  Argumments:
#  varname: name of the variable given in the file
#  lines  : list of lines returned by readlines() procedure    
  loc = locvar(varname, lines)
  l = lines[loc].split("=")
  if ("!" in l[1]):
      ll = l[1].split("!")
      vout = ll[0].strip()
      return float(vout)
  else:
      vout = l[1].strip()
      return float(vout)
  
    
    

def afgen(ldummy,x):
# Linear interpolation fuction
    if (x < ldummy[0]):
        return ldummy[1]
    if (x > ldummy[-2]):
        return ldummy[-1]     
    for i in np.arange(2,len(ldummy)):
        if (x <= ldummy[i]):
            slope = (ldummy[i+1]-ldummy[i-1])/(ldummy[i]-ldummy[i-2])
            return ldummy[i-1] + slope * (x - ldummy[i-2])
       
#def ft1900(dd,mes,ano,hora,mm,ss):
def strtoFloat(strd1):
   if (strd1 == ""):
       return -999
   else:
       return float(strd1)       

def datefmt(stVar):
# stVar is a strutc_time var and returns in a string format
   return tm.strftime("%d-%m-%Y",stVar)
    


# ******************** Start program ***********************************
cropfile = "cassava.w41" 
meteofile = "dados_meteo.csv" 

print("Before starting, verify if the following files are in the same\
directoy of this program:")
print(cropfile)
print(meteofile)
input("Press Enter to continue")

f = open(cropfile,"r")
lines = f.readlines()
f.close()

# Reading some variables from wofost crop file
dtsmtb = RLR("dtsmtb",lines)
plantdateST = tm.strptime(readchar("dateplant",lines),"%d-%m-%Y")
tbemerg = readfloat("TBASEM", lines)
TsumEm  = readfloat("TSUMEM",lines)
tsum1   = readfloat("tsum1", lines)
tsum2   = readfloat("tsum2", lines)



# Read meteo file

f = open(meteofile,"r")
lnmeteo = f.readlines()
f.close()


# Variables in the meteofile
dateST,Tmed,Tmin,Tmax,Urmed,Urmin,Urmax,Vv2m,Hg,P,ETo,ETc = \
    [],[],[],[],[],[],[],[],[],[],[],[]


#print(lnmeteo[0])
for l in lnmeteo[2:]:
    l = l.split(",")
    idatestr = l[0].strip()+"-"+ l[1].strip()+"-" + l[2].strip()
    dateST.append(tm.strptime(idatestr,"%d-%m-%Y"))
    Tmed.append(strtoFloat(l[3]))
    Tmin.append(strtoFloat(l[4]))
    Tmax.append(strtoFloat(l[5]))
    Urmed.append(strtoFloat(l[6]))
    Urmin.append(strtoFloat(l[7]))
    Urmax.append(strtoFloat(l[8]))
    Vv2m.append(strtoFloat(l[9]))
    Hg.append(strtoFloat(l[10]))
    P.append(strtoFloat(l[11]))
    ETo.append(strtoFloat(l[12]))
    ETc.append(strtoFloat(l[13]))

if (Tmed.count(-999) > 0):
    print("Missing temperature values. \n It will be necessary a function for\
          for missing data")
    exit()



iPdate = 0 ; flag = True
while (flag):
    if dateST[iPdate] >= plantdateST:
        flag = False
        iPdate = iPdate - 1
    iPdate = iPdate + 1

# ipdate: position in general meteo file for the plant date

# **** Generating file with DVS calculated as function of DATE and DAP *****

#   openning file 

f1 = open("dvs.out","w")
f1.write("*DSV program by Marcos Alex\n")
f1.write("*Find below the values of dvs calculated with information taken from\
 the following files\n")
f1.write("*meteorological file name : " + meteofile + "\n")
f1.write("*crop file name           : " + cropfile  + "\n")
f1.write(30*"*"+"\n")
f1.write('{:15} {:6} {:8}\n'.format("*Date","DAP","DVS"))
f1.write(30*"*"+"\n")

pEm = 0; idvs = 0
iemtsum = 0.0; flag = True

i = iPdate ; dap = 0
while (flag):
    idate = dateST[i]
    itmed = Tmed[i]
# determine firts the day of emergency
    iemtsum = iemtsum + (itmed - tbemerg)

    if(iemtsum < TsumEm):
       idvs = 0
       pEm = pEm + 1
    else:
       tav = afgen(dtsmtb, itmed)
       if (idvs <= 1.0):
           dvr = tav/tsum1
       elif(idvs < 2.0):
           dvr = tav/tsum2
       else:
           flag = False
           
       idvs = idvs + dvr
    i = i + 1
    f1.write('{:12} {:6} {:6.2f}\n'.format(datefmt(idate),dap,idvs) )
    dap = dap + 1    
f1.close()               
     
print("")  
print("OK")
print("file dvs.out was created")
     

# PEm is the first position in vec when the plant emerges

    
    

# t1970 = tm.time()
#local_time = tm.ctime(t1970)
# localtimeTM = tm.localtime(t1970) # It store in strutc_time format
# This variable also returns the Julian day

#print(localtimeTM)
#print("\nYear :", localtimeTM.tm_year)
#print("tm_hour : ", localtimeTM.tm_hour)
#print("Julian Day :", localtimeTM.tm_yday)

# time.strftime takes an epoch time in strutc_time format and returns in str format
# according to your  formatting settings
#timeString = tm.strftime("%d/%m/\n%Y",localtimeTM)
#print(timeString)

# strptime() takes a string and corresponding time format to return strutc_time 
# since epoch
# Example:
    
# date1 = "24-Mar-2022"
# date2 = "25-Mar-2022"
# date1ST = tm.strptime(date1,"%d-%b-%Y")
# date2ST = tm.strptime(date2,"%d-%b-%Y")
# print(date1ST)
# print(date2ST > date1ST)



# Convert strutc_time in float epoch

    
    


