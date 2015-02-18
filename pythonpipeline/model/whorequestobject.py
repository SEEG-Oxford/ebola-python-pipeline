__author__ = 'andrew.schofield@tessella.com'

# http://apps.who.int/gho/data/node.resources.api


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