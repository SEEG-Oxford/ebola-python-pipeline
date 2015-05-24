__author__ = 'andrew.schofield@tessella.com'

import model.WHORequestObject
import services.WHODataExtractor
import services.WHODataPostProcessor
from subprocess import call
import sys
import getopt
import os
import urllib2
import csv
import shutil
import glob
import datetime
import re


class Pipeline(object):
    def __init__(self):
        self.whodataextractor = None

    def main(self, argv):
        rdir = ''
        datadir = ''
        publishrepo = ''
        localfiles = False
        try:
            opts, args = getopt.getopt(argv, "hlr:d:p:", ["rdir=", "datadir=", "publishrepo="])
        except getopt.GetoptError:
            print 'ebola-pipeline.py -r <R directory> ' \
                  '-d <data directory>'
            sys.exit(2)
        for opt, arg in opts:
            if opt == '-h':
                print 'ebola-pipeline.py -r <R directory> ' \
                      '-d <data directory>'
                sys.exit()
            elif opt in ("-l"):
                localfiles = True
            elif opt in ("-r", "--rdir"):
                rdir = arg
            elif opt in ("-d", "--datadir"):
                datadir = arg
            elif opt in ("-p", "--publishrepo"):
                publishrepo = arg

        if not localfiles:
            self.downloadforcountry("GIN", datadir)
            self.downloadforcountry("LBR", datadir)
            self.downloadforcountry("SLE", datadir)

        rcodedir = os.path.abspath(rdir)

        # as long as R is on your path this should work
        output = call(["R", "--silent", "--slave", "--vanilla",
                       "--file=" + os.path.abspath(rdir + "/import_EVD_case_data.R")], cwd=datadir)

        # fix the files to make them compatible with our plotting code
        with open(datadir + "/EVD_conf_prob_.csv") as csvfile:
            reader = csv.reader(csvfile)
            with open(datadir + "/expected_output_headers.csv") as headerfile:
                headerreader = csv.reader(headerfile)
                headers = headerreader.next()
                postprocessor = services.WHODataPostProcessor\
                    .WHODataPostProcessor(reader, headers)
                correcteddata = postprocessor.FindAndInsertMissingRegions()

            rowcount = len(correcteddata)

            with open(datadir + "/EVD_conf_prob_.csv", 'w') as csvoutput:
                writer = csv.writer(csvoutput, lineterminator='\n')
                writer.writerows(correcteddata)

            with open(datadir + "/EVD_conf_prob_additional.csv") as additionalcsvfile:
                reader = csv.reader(additionalcsvfile)
                row_count = sum(1 for row in reader)
                if row_count < rowcount:
                    # there are fewer lines in the additional file than the
                    # original
                    linestoadd = rowcount - row_count
                    # start back at the beginning of the file
                    additionalcsvfile.seek(0)
                    reader.next()
                    datarow = reader.next()
                    newrow = [0 for x in range(len(datarow))]
            if row_count < rowcount:
                with open(datadir + "/EVD_conf_prob_additional.csv",
                          "a") as additionalcsvfile:
                    writer = csv.writer(additionalcsvfile, lineterminator='\n')
                    for x in range(linestoadd):
                        writer.writerow(newrow)

        output = call(["R", "--silent", "--slave", "--vanilla",
                       "--file=" + os.path.abspath(rdir + "/process.R")], cwd=rdir)

        historycount = 0

        if publishrepo != '':
            # update the repository
            # WARNING: This needs to be run from a shell that can access a valid private key for the repository
            call(["git", "pull", "-v"], cwd=os.path.abspath(publishrepo))
            repopath = os.path.abspath(publishrepo)
            os.chdir(os.path.abspath(rdir))
            for file in glob.glob("regional*.png"):
                filename = os.path.basename(file)
                shutil.copy(file, repopath + "/images/" + filename)
                call(["git", "add", repopath + "/images/" + filename], cwd=repopath)

            # basic markdown editing
            s = open(repopath + "/local-risk.md", "r")
            w = open(repopath + "/local-risk.md1", "w")
            for line in s.readlines():
                line = re.sub(r"(### Latest data as of )(.*)", r"\1 " + str(datetime.date.today()), line)
                w.write(line)
            s.close()
            w.close()
            os.remove(repopath + "/local-risk.md")
            os.rename(repopath + "/local-risk.md1", repopath + "/local-risk.md")

            call(["git", "add", repopath + "/local-risk.md"], cwd=repopath)

            shutil.copy(rcodedir + "/weightings.csv", repopath + "/weightings.csv")

            call(["git", "add", repopath + "/weightings.csv"], cwd=repopath)

            for file in glob.glob("global*.png"):
                filename = os.path.basename(file)
                shutil.copy(file, repopath + "/images/" + filename)
                call(["git", "add", repopath + "/images/" + filename], cwd=repopath)

            for file in glob.glob("*_cases_week.png"):
                historycount += 1
                filename = os.path.basename(file)
                shutil.copy(file, repopath + "/images/cases/" + filename)
                call(["git", "add", repopath + "/images/cases/" + filename], cwd=repopath)

            for file in glob.glob("*regional_prediction_weighted.png"):
                filename = os.path.basename(file)
                shutil.copy(file, repopath + "/images/predictions/" + filename)
                call(["git", "add", repopath + "/images/predictions/" + filename], cwd=repopath)

            # basic markdown editing
            s = open(repopath + "/global-risk.md", "r")
            w = open(repopath + "/global-risk.md1", "w")
            for line in s.readlines():
                line = re.sub(r"(### Latest data as of )(.*)", r"\1 " + str(datetime.date.today()), line)
                w.write(line)
            s.close()
            w.close()
            os.remove(repopath + "/global-risk.md")
            os.rename(repopath + "/global-risk.md1", repopath + "/global-risk.md")

            call(["git", "add", repopath + "/global-risk.md"], cwd=repopath)


            # basic markdown editing
            s = open(repopath + "/case-history.md", "r")
            w = open(repopath + "/case-history.md1", "w")
            for line in s.readlines():
                line = re.sub(r"(<input type=\"range\" min=\"1\" max=\")(\d*)(\" value=\"1\" step=\"1\" data-rangeslider>)", r"\g<1>" + str(historycount) + r"\g<3>", line)
                w.write(line)
            s.close()
            w.close()
            os.remove(repopath + "/case-history.md")
            os.rename(repopath + "/case-history.md1", repopath + "/case-history.md")

            call(["git", "add", repopath + "/case-history.md"], cwd=repopath)

            # basic markdown editing
            s = open(repopath + "/prediction-history.md", "r")
            w = open(repopath + "/prediction-history.md1", "w")
            for line in s.readlines():
                line = re.sub(r"(<input type=\"range\" min=\"4\" max=\")(\d*)(\" value=\"1\" step=\"1\" data-rangeslider>)", r"\g<1>" + str(historycount) + r"\g<3>", line)
                w.write(line)
            s.close()
            w.close()
            os.remove(repopath + "/prediction-history.md")
            os.rename(repopath + "/prediction-history.md1", repopath + "/prediction-history.md")

            call(["git", "add", repopath + "/prediction-history.md"], cwd=repopath)

            ## copy all geojson plots around
            shutil.copy(rcodedir + "/districts.geojson", repopath + "/geojson/Regional_Risk/districts.geojson")
            shutil.copy(rcodedir + "/weighted-districts.geojson", repopath + "/geojson/Regional_Risk_Source/districts.geojson")
            shutil.copy(rcodedir + "/Regional_Risk/Regional_Risk.html", repopath + "/geojson/Regional_Risk/Regional_Risk.html")
            shutil.copy(rcodedir + "/global_Adjacency_prediction/global_Adjacency_prediction.html", repopath + "/geojson/global_Adjacency_prediction.html")
            shutil.copy(rcodedir + "/global_Gravity_prediction/global_Gravity_prediction.html", repopath + "/geojson/global_Gravity_prediction.html")
            shutil.copy(rcodedir + "/global_Migration_prediction/global_Migration_prediction.html", repopath + "/geojson/global_Migration_prediction.html")
            shutil.copy(rcodedir + "/global_Overall_prediction/global_Overall_prediction.html", repopath + "/geojson/global_Overall_prediction.html")
            shutil.copy(rcodedir + "/countries.geojson", repopath + "/geojson/countries.geojson")
            call(["git", "add", repopath + "/geojson/Regional_Risk/Regional_Risk.html"], cwd=repopath)
            call(["git", "add", repopath + "/geojson/Regional_Risk/districts.geojson"], cwd=repopath)
            call(["git", "add", repopath + "/geojson/Regional_Risk_Source/districts.geojson"], cwd=repopath)
            call(["git", "add", repopath + "/geojson/countries.geojson"], cwd=repopath)
            call(["git", "add", repopath + "/geojson/global_Adjacency_prediction.html"], cwd=repopath)
            call(["git", "add", repopath + "/geojson/global_Gravity_prediction.html"], cwd=repopath)
            call(["git", "add", repopath + "/geojson/global_Migration_prediction.html"], cwd=repopath)
            call(["git", "add", repopath + "/geojson/global_Overall_prediction.html"], cwd=repopath)

            call(["git", "commit", "-m", "Updated WHO data and regional risk plots as of " + str(datetime.date.today())], cwd=repopath)

            call(["git", "push", "-v"], cwd=repopath)

    def downloadforcountry(self, countryname, outputdir):
        requestobject = model.WHORequestObject.WHORequestObject()
        requestobject.format = "data-verbose.csv"
        requestobject.target = "EBOLA_MEASURE"
        requestobject.targetcolumns = ["CASES"]
        requestobject.profile = "verbose"
        requestobject.countries = [countryname]
        requestobject.location = ["*"]
        requestobject.datapackageid = [
            self.findlatestavailabledateforcountry(countryname)]
        requestobject.indicatortype = ["SITREP_NEW"]
        requestobject.sex = "-"

        self.whodataextractor = services.WHODataExtractor.WHODataExtractor(
            requestobject)

        data = self.whodataextractor.downloadfromwhowebsite()

        with open(os.path.join(outputdir, countryname + ".csv"), "w") as file:
            file.write(data.read())
            file.close()

    def findlatestavailabledateforcountry(self, country):
        date = None
        data = urllib2.urlopen(
            "http://apps.who.int/gho/data/node.ebola-sitrep.ebola-country-" +
            country + "-latest?lang=en")
        # regular expression to pull out the latest packageid from the page
        m = re.search("DATAPACKAGEID:(.*?);", data.read())
        date = m.group(1)
        print "Latest data found for " + country + ": " + date
        return date


if __name__ == '__main__':
    Pipeline = Pipeline()

    Pipeline.main(sys.argv[1:])