__author__ = 'andrew.schofield@tessella.com'

import model.WHORequestObject
import services.WHODataExtractor


class Pipeline(object):
    def __init__(self):
        self.whodataextractor = None

    def main(self):
        self.downloadforcountry("GIN")
        #self.downloadforcountry("LBR")
        #self.downloadforcountry("SLE")

    def downloadforcountry(self, countryname):
        requestobject = model.WHORequestObject.WHORequestObject()
        requestobject.format = "data-verbose.csv"
        requestobject.target = "EBOLA_MEASURE"
        requestobject.targetcolumns = ["CASES"]
        requestobject.profile = "verbose"
        requestobject.countries = [countryname]
        requestobject.location = ["*"]
        requestobject.datapackageid = ["2015-02-11"]
        requestobject.indicatortype = ["SITREP_NEW"]
        requestobject.sex = "-"

        self.whodataextractor = services.WHODataExtractor.WHODataExtractor(requestobject)

        data = self.whodataextractor.downloadfromwhowebsite()

        with open(countryname + ".csv", "w") as file:
            file.write(data.read())
            file.close()

if __name__ == '__main__':
    Pipeline = Pipeline()

    Pipeline.main()