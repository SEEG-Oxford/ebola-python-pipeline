__author__ = 'andrew.schofield@tessella.com'


class WHODataPackage(object):
    def __init__(self):
        self.id = None
        self.countries = []

    def __init__(self, id, countries):
        self.id = id
        self.countries = countries