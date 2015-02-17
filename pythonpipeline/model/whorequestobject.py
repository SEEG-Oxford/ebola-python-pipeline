__author__ = 'andrew.schofield@tessella.com'


class WHORequestObject(object):
    def __init__(self):
        self.format = None
        self.target = None
        self.targetcolumns = []
        self.profile = None
        self.countries = []
        self.location = []
        self.datapackageid = []
        self.indicatortype = []
        self.sex = None