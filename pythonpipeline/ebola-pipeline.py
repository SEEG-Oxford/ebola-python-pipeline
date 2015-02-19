__author__ = 'andrew.schofield@tessella.com'

import model.WHORequestObject
import services.WHODataExtractor
from subprocess import call
import sys
import getopt
import os.path
import urllib2
import re


class Pipeline(object):
    def __init__(self):
        self.whodataextractor = None

    def main(self, argv):
        rsourcefile = ''
        outputdir = ''
        try:
            opts, args = getopt.getopt(argv, "hr:o:", ["rfile=", "outputdir="])
        except getopt.GetoptError:
            print 'ebola-pipeline.py -r <R source filename> -o <output directory>'
            sys.exit(2)
        for opt, arg in opts:
            if opt == '-h':
                print 'ebola-pipeline.py -r <R source filename> -o <output directory>'
                sys.exit()
            elif opt in ("-r", "--rfile"):
                rsourcefile = arg
            elif opt in ("-o", "--outputdir"):
                outputdir = arg

        self.downloadforcountry("GIN", outputdir)
        self.downloadforcountry("LBR", outputdir)
        self.downloadforcountry("SLE", outputdir)

        # as long as R is on your path this should work
        output = call(["R", "--silent", "--slave", "--vanilla", "--file=" + os.path.abspath(rsourcefile)], cwd=outputdir)

    def downloadforcountry(self, countryname, outputdir):
        requestobject = model.WHORequestObject.WHORequestObject()
        requestobject.format = "data-verbose.csv"
        requestobject.target = "EBOLA_MEASURE"
        requestobject.targetcolumns = ["CASES"]
        requestobject.profile = "verbose"
        requestobject.countries = [countryname]
        requestobject.location = ["*"]
        requestobject.datapackageid = [self.findlatestavailabledateforcountry(countryname)]
        requestobject.indicatortype = ["SITREP_NEW"]
        requestobject.sex = "-"

        self.whodataextractor = services.WHODataExtractor.WHODataExtractor(requestobject)

        data = self.whodataextractor.downloadfromwhowebsite()

        with open(os.path.join(outputdir, countryname + ".csv"), "w") as file:
            file.write(data.read())
            file.close()


    def findlatestavailabledateforcountry(self, country):
        date = None
        data = urllib2.urlopen("http://apps.who.int/gho/data/node.ebola-sitrep.ebola-country-" + country + "-latest?lang=en")
        # regular expression to pull out the latest packageid from the page
        m = re.search("DATAPACKAGEID:(.*?);", data.read())
        date = m.group(1)
        print "Latest data found for " + country + ": " + date
        return date

if __name__ == '__main__':
    Pipeline = Pipeline()

    Pipeline.main(sys.argv[1:])