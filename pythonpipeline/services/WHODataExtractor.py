__author__ = 'andrew.schofield@tessella.com'

# http://apps.who.int/gho/athena/xmart/data-verbose.csv?target=EBOLA_MEASURE/CASES&profile=verbose&filter=LOCATION:*;COUNTRY:GIN;INDICATOR_TYPE:SITREP_NEW;DATAPACKAGEID:2015-01-21;SEX:-
# http://apps.who.int/gho/data/node.resources.api
import urllib2
import csv

BASE_URL = 'http://apps.who.int/gho/athena/xmart/'


class WHODataExtractor(object):
    def __init__(self, requestobject):
        self.requestobject = requestobject

    def urlconstructor(self):
        targetcolumns = ','.join(self.requestobject.targetcolumns)
        filterlist = str.format("{0}{1}{2}{3}SEX:{4}",
                                self.formatlist("COUNTRY", self.requestobject.countries),
                                self.formatlist("LOCATION", self.requestobject.location),
                                self.formatlist("DATAPACKAGEID", self.requestobject.datapackageid),
                                self.formatlist("INDICATOR_TYPE", self.requestobject.indicatortype),
                                self.requestobject.sex)
        output = str.format("{0}?target={1}/{2}&profile={3}&filter={4}",
                            self.requestobject.format,
                            self.requestobject.target,
                            targetcolumns,
                            self.requestobject.profile,
                            filterlist)
        return BASE_URL + output

    def formatlist(self, typekey, list):
        output = ""
        for listitem in list:
            output += str.format("{0}:{1};", typekey, listitem)

        return output

    def downloadfromwhowebsite(self):
        return urllib2.urlopen(self.urlconstructor())

    def parsefile(self, csvfile):
        reader = csv.DictReader(csvfile)
        #TODO: Find unique countries, then parse the file as a set of WHOMeasure objects
