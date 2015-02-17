__author__ = 'andrew.schofield@tessella.com'

# http://apps.who.int/gho/athena/xmart/data-text.csv?target=EBOLA_MEASURE/CASES,DEATHS&profile=text&filter=COUNTRY:GIN;COUNTRY:UNSPECIFIED;COUNTRY:LBR;COUNTRY:UNSPECIFIED;COUNTRY:SLE;COUNTRY:UNSPECIFIED;LOCATION:-;DATAPACKAGEID:2014-11-14;INDICATOR_TYPE:SITREP_CUMULATIVE;INDICATOR_TYPE:SITREP_CUMULATIVE_21_DAYS;SEX:-


BASE_URL = 'http://apps.who.int/gho/athena/xmart/'

class WHODataExtractor(object):
    def __init__(self, requestobject):
        self.requestobject = requestobject


    def urlconstructor(self):
        return BASE_URL

    def formatlist(self, typekey, list):
        output = ""
        for listitem in list:
            output += str.format("{0}:{1};", typekey, listitem)

        return output
