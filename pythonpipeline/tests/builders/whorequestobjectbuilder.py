__author__ = 'andrew.schofield@tessella.com'

import model.WHORequestObject


class WHORequestObjectBuilder(object):
    requestobject = None
    def __init__(self):
        self.requestobject = model.WHORequestObject.WHORequestObject()

    def withformat(self, format):
        self.requestobject.format = format
        return self

    def withtarget(self, target):
        self.requestobject.target = target
        return self

    def withtargetcolumns(self, targetcolumns):
        self.requestobject.targetcolumns = targetcolumns
        return self

    def withprofile(self, profile):
        self.requestobject.profile = profile
        return self

    def withcountries(self, countries):
        self.requestobject.countries = countries
        return self

    def withlocations(self, locations):
        self.requestobject.location = locations
        return self

    def withdatapackgeid(self, datapackageid):
        self.requestobject.datapackageid = datapackageid
        return self

    def withindicatortypes(self, indicatortypes):
        self.requestobject.indicatortype = indicatortypes
        return self

    def withsex(self, sex):
        self.requestobject.sex = sex
        return self

    def build(self):
        return self.requestobject