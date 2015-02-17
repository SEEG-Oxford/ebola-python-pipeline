__author__ = 'andrew.schofield@tessella.com'

# http://apps.who.int/gho/athena/xmart/data-text.csv?target=EBOLA_MEASURE/CASES,DEATHS&profile=text&filter=COUNTRY:GIN;COUNTRY:UNSPECIFIED;COUNTRY:LBR;COUNTRY:UNSPECIFIED;COUNTRY:SLE;COUNTRY:UNSPECIFIED;LOCATION:-;DATAPACKAGEID:2014-11-14;INDICATOR_TYPE:SITREP_CUMULATIVE;INDICATOR_TYPE:SITREP_CUMULATIVE_21_DAYS;SEX:-

BASE_URL = 'http://apps.who.int/gho/athena/xmart/'


def urlconstructor(baseurl, requestobject):
    return baseurl