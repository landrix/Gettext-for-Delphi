#!/usr/bin/env python2.2

# This tool can extract strings from a static html page and apply translations to it.
# Translations must be stored in MO file format in a locale/LL/LC_MESSAGES
# subdirectory, where LL is the language code.

import sys
import string
import HTMLParser
import gettext
import os


def sysout (s):
	sys.stdout.write (unicode(s,"utf-8").encode(syscharset))

def text2po(msg):
	res=""
	for ch in msg:
		if ch=="\"":
			res=res+"\\\""
		elif ch=="\n":
			res=res+"\\n"
		elif ch!="\r":
			res=res+ch
	return res

class BaseParser (HTMLParser.HTMLParser):
	def __init__(self):
		HTMLParser.__init__(self)

class ExtractParser (BaseParser):
	def __init__(self, destfilename):
		BaseParser.__init__(self)
		self.reset ()
		self.destfile=file(destfilename,"w")
		self.destfile.write ("# SOME DESCRIPTIVE TITLE.\n")
		self.destfile.write ("# Copyright (C) YEAR THE PACKAGE'S COPYRIGHT HOLDER\n")
		self.destfile.write ("# This file is distributed under the same license as the PACKAGE package.\n")
		self.destfile.write ("# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.\n")
		self.destfile.write ("#\n")
		self.destfile.write ("msgid \"\"\n")
		self.destfile.write ("msgstr \"\"\n")
		self.destfile.write ("\"Content-Type: text/plain; charset=CHARSET\\n\"\n")
		self.destfile.write ("\"Content-Transfer-Encoding: 8bit\\n\"\n")
		self.destfile.write ("\n")
		self.polist={}
	def handlemsg (self, msg):
		msgs=string.strip(msg)
		if msgs!="":
			haschar=0
			for ch in msgs:
				if string.find(string.letters, ch)!=-1:
					haschar=1
					break
			if haschar:
				self.polist[msg]=msg
	def handle_starttag(self, tag, attrs):
		for attr in attrs:
			key, value=attr
			if key=="alt":
				self.handlemsg (value)
	def handle_data(self, data):
		self.handlemsg (data)
	def close(self):
		BaseParser.close (self)
		for msg in self.polist:
			self.destfile.write ("msgid \""+text2po(msg)+"\"\n")
			self.destfile.write ("msgstr \"\"\n")
			self.destfile.write ("\n")

class ApplyParser (BaseParser):
	def __init__(self, destfilename, langcode):
		self.reset ()
		self.destfile=file(destfilename,"w")
		BaseParser.__init__(self)
		localedir=os.path.abspath(os.path.dirname(sys.argv[0]))+os.sep+"locale"
		mofile=gettext.find("default",localedir,[langcode])
		self.translator=gettext.GNUTranslations(open(mofile, 'rb'))
	def handle_starttag(self, tag, attrs):
		self.destfile.write ("<"+tag)
		for attr in attrs:
			key, value=attr
			if key=="alt":
				value=self.translator.gettext(value)
			self.destfile.write (" "+key+"=\""+value+"\"")
		self.destfile.write (">")
	def handle_startendtag(self, tag, attrs):
		self.destfile.write ("<"+tag)
		for attr in attrs:
			key, value=attr
			if key=="alt":
				value=self.translator.gettext(value)
			self.destfile.write (" "+key+"=\""+value+"\"")
		self.destfile.write (" />")
	def handle_endtag(self, tag):
		self.destfile.write ("</"+tag+">")
	def handle_comment(self, data):
		self.destfile.write ("<!-"+data+"->")
	def handle_decl(self, decl):
		self.destfile.write ("<!"+decl+">")
	def handle_data(self, data):
		self.destfile.write (self.translator.gettext(data))

def RunParser (sourcefilename, destfilename, parser):
	sourcefile=file(sourcefilename,"r")
	while 1:
		line=sourcefile.readline()
		if line=="":
			break
		parser.feed (line)
	parser.close()
	sourcefile.close()





# MAIN routine
action=0
currenttask={ "": "Running" }
error=[]
syscharset="us-ascii"
langcode="en"
argidx=1
argidxtop=len(sys.argv)-1
while argidx<=argidxtop:
	arg=sys.argv[argidx]
	if arg=="--help":
		action=1
	elif arg=="--extract":
		action=2
		argidx=argidx+1
		sourcefilename=sys.argv[argidx]
		destfilename="default.po"
	elif arg=="--apply":
		action=3
		argidx=argidx+1
		sourcefilename=sys.argv[argidx]
		argidx=argidx+1
		destfilename=sys.argv[argidx]
		argidx=argidx+1
		langcode=sys.argv[argidx]
	elif arg[0:2]=="--":
		error.append ("Option not implemented: %s"%(arg)+"\n")
	else:
		error.append ("Parameter not understood: %s"%(arg)+"\n")
	argidx=argidx+1
if len(sys.argv)<=1:
	error.append ("No parameters specified\n")

if error==[] and action>=2:
	try:
		if action==2:
			RunParser (sourcefilename, destfilename,ExtractParser(destfilename))
		elif action==3:
			RunParser (sourcefilename, destfilename,ApplyParser(destfilename, langcode))
		else:
			raise Exception("Invalid action number")
	except:
		sysout ("Weblayout (C) by Lars B. Dybdahl\n")
		sys.stderr.write (currenttask[""]+"\n")
		raise
else:
	sysout ("HTMLTranslator (C) by Lars B. Dybdahl\n")
	if error!=[]:
		sysout ("\nERROR:\n")
		for err in error:
			sysout (err)
	sysout ("\nUsage:\n")
	sysout ("  --extract sourcefile                  Extracts texts from sourcefile into sourcefile.po\n")
	sysout ("  --apply sourcefile destfile langcode  Applies translations using text domain 'default'.\n")

## EOF ##

