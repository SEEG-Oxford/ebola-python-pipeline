__author__ = 'andrew.schofield@tessella.com'

import model.WHORequestObject
import services.WHODataExtractor
from subprocess import call
import sys
import getopt
import os.path


class Pipeline(object):
    def __init__(self):
        self.whodataextractor = None

    def main(self, argv):
        date = None
        rsourcefile = ''
        outputdir = ''
        try:
            opts, args = getopt.getopt(argv, "hd:r:o:", ["date=", "rfile=", "outputdir="])
        except getopt.GetoptError:
            print 'main.py -d <date> -r <R source filename> -o <output directory>'
            sys.exit(2)
        for opt, arg in opts:
            if opt == '-h':
                print 'main.py -d <date> -r <R source filename> -o <output directory>'
                sys.exit()
            elif opt in ("-d", "--date"):
                date = arg
            elif opt in ("-r", "--rfile"):
                rsourcefile = arg
            elif opt in ("-o", "--outputdir"):
                outputdir = arg

        self.downloadforcountry("GIN", date, outputdir)
        self.downloadforcountry("LBR", date, outputdir)
        self.downloadforcountry("SLE", date, outputdir)
        self.downloadforcountry("SLE", date, outputdir)

        # windows specific at the moment
        # need to replace with a platform agnostic call to run R
        output = call(["C:\\Program Files\\R\\R-3.1.2\\bin\\R.exe", "--no-save", "--file=" + os.path.abspath(rsourcefile)], cwd=outputdir)

    def downloadforcountry(self, countryname, date, outputdir):
        requestobject = model.WHORequestObject.WHORequestObject()
        requestobject.format = "data-verbose.csv"
        requestobject.target = "EBOLA_MEASURE"
        requestobject.targetcolumns = ["CASES"]
        requestobject.profile = "verbose"
        requestobject.countries = [countryname]
        requestobject.location = ["*"]
        requestobject.datapackageid = [date]
        requestobject.indicatortype = ["SITREP_NEW"]
        requestobject.sex = "-"

        self.whodataextractor = services.WHODataExtractor.WHODataExtractor(requestobject)

        data = self.whodataextractor.downloadfromwhowebsite()

        with open(os.path.join(outputdir, countryname + ".csv"), "w") as file:
            file.write(data.read())
            file.close()

if __name__ == '__main__':
    Pipeline = Pipeline()

    Pipeline.main(sys.argv[1:])