#!/usr/bin/python

import pycurl
from optparse import OptionParser
from xml.etree import ElementTree as ET



class Test:
    def __init__(self):
        self.contents = ''

    def body_callback(self, buf):
        self.contents = self.contents + buf
        elements = ET.XML(self.contents)
        subelements = elements.getchildren()

        print subelements[5]
        print subelements[6]



parser = OptionParser()
parser.add_option("-i","--image", type="string", dest="file",
                    help="the image you're uploading")

(options, args) = parser.parse_args()

if options.file:

    t = Test()

    c = pycurl.Curl()
    values = [
              ("key", "793579906fc2a799847e12db2b01cdcd"),
              ("image", (c.FORM_FILE, options.file))]

    c.setopt(c.URL, "http://imgur.com/api/upload.xml")
    c.setopt(c.HTTPPOST, values)
    c.setopt(c.WRITEFUNCTION, t.body_callback)

    c.perform()
    c.close()

else:
    print "You need to provide a file"

