__author__ = 'andrew.schofield@tessella.com'


class WHOCountry(object):
    def __init__(self):
        self.name = None
        self.measures = []

    def __init__(self, name, measures):
        self.name = name
        self.measures = measures
