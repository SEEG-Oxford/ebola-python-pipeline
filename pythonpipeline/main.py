__author__ = 'andrew.schofield@tessella.com'

import model.WHORequestObject
import services.WHODataExtractor


class Pipeline(object):
    def __init__(self):
        self.whodataextractor = None

    def main(self):
        requestobject = model.WHORequestObject.WHORequestObject()
        requestobject.format = "data-text.csv"
        requestobject.target = "EBOLA_MEASURE"
        requestobject.targetcolumns = ["CASES", "DEATHS"]
        requestobject.profile = "text"
        requestobject.countries = ["GIN", "UNSPECIFIED", "LBR", "UNSPECIFIED", "SLE", "UNSPECIFIED"]
        requestobject.location = ["-"]
        requestobject.datapackageid = ["2014-11-14"]
        requestobject.indicatortype = ["SITREP_CUMULATIVE", "SITREP_CUMULATIVE_21_DAYS"]
        requestobject.sex = "-"

        self.whodataextractor = services.WHODataExtractor.WHODataExtractor(requestobject)

        data = self.whodataextractor.downloadfromwhowebsite()

        with open("download.file", "w") as file:
            file.write(data.read())
            file.close()

if __name__ == '__main__':
    Pipeline = Pipeline()

    Pipeline.main()