__author__ = 'andrew.schofield@tessella.com'


class WHOMeasure(object):
    def __init__(self):
        self.definition = None
        self.epiweek = None
        self.indicatortype = None
        self.value = None
        self.comments = None

    def __init__(self, definition, epiweek, indicatortype, value, comments):
        self.definition = definition
        self.epiweek = epiweek
        self.indicatortype = indicatortype
        self.value = value
        self.comments = comments