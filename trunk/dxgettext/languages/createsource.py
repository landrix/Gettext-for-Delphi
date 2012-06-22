#!/usr/bin/env python2.2

import string

def createpython (infilename,outfilename):
	infile=open (infilename,"r")
	outfile=open (outfilename,"w")
	outfile.write ("def getlanguagelist ():\n")
	outfile.write ("\tlist={")
	first=1
	while 1:
		line=infile.readline ()
		if line=="":
			break
		line=string.strip(line)
		if line!="":
			parts=string.split(line,"\t")
			if first==1:
				first=0
			else:
				outfile.write (", ")
			outfile.write ("\""+parts[0]+"\": \""+parts[1]+"\"")
	outfile.write (" }\n")
	outfile.write ("\treturn list\n")
	outfile.close()
	infile.close()

def createphp (infilename,outfilename):
	infile=open (infilename,"r")
	outfile=open (outfilename,"w")
	outfile.write ("<?php\n\n")
	outfile.write ("function getlanguagelist() {\n")
	while 1:
		line=infile.readline ()
		if line=="":
			break
		line=string.strip(line)
		if line!="":
			parts=string.split(line,"\t")
			outfile.write ("  $list[\""+parts[0]+"\"]=\""+parts[1]+"\";\n")
	outfile.write ("  return $list;\n")
	outfile.write ("}\n")
	outfile.write ("\n")
	outfile.write ("?>\n")
	outfile.close()
	infile.close()

def createpascal (infilename,outfilename):
	infile=open (infilename,"r")
	outfile=open (outfilename,"w")
	outfile.write ("unit languagecodes;\n")
	outfile.write ("// This unit is automatically generated from a language list.\n\n")
	outfile.write ("interface\n\n")
	outfile.write ("// Return the English name of the language specified as 2 or 5 char\n")
	outfile.write ("// language codes, e.g. 'en' or 'en_GB'.\n")
	outfile.write ("function getlanguagename(langcode:string):string;\n\n")
	outfile.write ("implementation\n\n")
	outfile.write ("function getlanguagename(langcode:string):string;\n")
	outfile.write ("begin\n")
	first=1
	while 1:
		line=infile.readline ()
		if line=="":
			break
		line=string.strip(line)
		if line!="":
			parts=string.split(line,"\t")
			if first==1:
				first=0
			outfile.write ("  if langcode='"+parts[0]+"' then Result:='"+parts[1]+"' else\n")
	outfile.write ("  Result:='';\n")
	outfile.write ("end;\n\n")
	outfile.write ("end.\n")
	outfile.close()
	infile.close()

createpython ("languagecodes.txt","languagecodes.py")
createpascal ("languagecodes.txt","languagecodes.pas")
createphp ("languagecodes.txt","languagecodes.php")
