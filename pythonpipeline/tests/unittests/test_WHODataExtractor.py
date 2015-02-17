__author__ = 'andrew.schofield@tessella.com'

import unittest
import services.WHODataExtractor
import tests.builders.whorequestobjectbuilder


class formatlisttests(unittest.TestCase):
    def test_formatlist_works_with_one_int(self):
        extractor = services.WHODataExtractor.WHODataExtractor(None)
        actual = extractor.formatlist('TEST', [1])
        expected = 'TEST:1;'
        self.assertEqual(expected, actual)

    def test_formatlist_works_with_two_ints(self):
        extractor = services.WHODataExtractor.WHODataExtractor(None)
        actual = extractor.formatlist('TEST', [1,2])
        expected = 'TEST:1;TEST:2;'
        self.assertEqual(actual, expected)

    def test_formatlist_works_with_one_string(self):
        extractor = services.WHODataExtractor.WHODataExtractor(None)
        actual = extractor.formatlist('TEST', ['1'])
        expected = 'TEST:1;'
        self.assertEqual(expected, actual)

    def test_formatlist_works_with_two_strings(self):
        extractor = services.WHODataExtractor.WHODataExtractor(None)
        actual = extractor.formatlist('TEST', ['1','2'])
        expected = 'TEST:1;TEST:2;'
        self.assertEqual(expected, actual)

    def test_urlconstructor_returns_unparameterised_url_if_parameters_are_empty(self):
        requestobject = tests.builders.whorequestobjectbuilder.whorequestobjectbuilder().build()
        extractor = services.WHODataExtractor.WHODataExtractor(requestobject)
        actual = extractor.urlconstructor()
        expected = "http://apps.who.int/gho/athena/xmart/None?target=None/&profile=None&filter=SEX:None"
        self.assertEqual(expected, actual)

    def test_urlconstructor_returns_url_with_format(self):
        requestobject = tests.builders.whorequestobjectbuilder.whorequestobjectbuilder().withformat("data-text.csv").build()
        extractor = services.WHODataExtractor.WHODataExtractor(requestobject)
        actual = extractor.urlconstructor()
        expected = "http://apps.who.int/gho/athena/xmart/data-text.csv?target=None/&profile=None&filter=SEX:None"
        self.assertEqual(expected, actual)

    def test_urlconstructor_returns_url_with_target(self):
        requestobject = tests.builders.whorequestobjectbuilder.whorequestobjectbuilder().withtarget("EBOLA_MEASURE").build()
        extractor = services.WHODataExtractor.WHODataExtractor(requestobject)
        actual = extractor.urlconstructor()
        expected = "http://apps.who.int/gho/athena/xmart/None?target=EBOLA_MEASURE/&profile=None&filter=SEX:None"
        self.assertEqual(expected, actual)

    def test_urlconstructor_returns_url_with_targetcolumns(self):
        requestobject = tests.builders.whorequestobjectbuilder.whorequestobjectbuilder().withtargetcolumns(["CASES", "DEATHS"]).build()
        extractor = services.WHODataExtractor.WHODataExtractor(requestobject)
        actual = extractor.urlconstructor()
        expected = "http://apps.who.int/gho/athena/xmart/None?target=None/CASES,DEATHS&profile=None&filter=SEX:None"
        self.assertEqual(expected, actual)

    def test_urlconstructor_returns_url_with_profile(self):
        requestobject = tests.builders.whorequestobjectbuilder.whorequestobjectbuilder().withprofile("text").build()
        extractor = services.WHODataExtractor.WHODataExtractor(requestobject)
        actual = extractor.urlconstructor()
        expected = "http://apps.who.int/gho/athena/xmart/None?target=None/&profile=text&filter=SEX:None"
        self.assertEqual(expected, actual)

    def test_urlconstructor_returns_url_with_countries(self):
        requestobject = tests.builders.whorequestobjectbuilder.whorequestobjectbuilder().withcountries(["GIN", "LIB", "SER"]).build()
        extractor = services.WHODataExtractor.WHODataExtractor(requestobject)
        actual = extractor.urlconstructor()
        expected = "http://apps.who.int/gho/athena/xmart/None?target=None/&profile=None&filter=COUNTRY:GIN;COUNTRY:LIB;COUNTRY:SER;SEX:None"
        self.assertEqual(expected, actual)

    def test_urlconstructor_returns_url_with_location(self):
        requestobject = tests.builders.whorequestobjectbuilder.whorequestobjectbuilder().withlocations(["-"]).build()
        extractor = services.WHODataExtractor.WHODataExtractor(requestobject)
        actual = extractor.urlconstructor()
        expected = "http://apps.who.int/gho/athena/xmart/None?target=None/&profile=None&filter=LOCATION:-;SEX:None"
        self.assertEqual(expected, actual)

    def test_urlconstructor_returns_url_with_datapackageid(self):
        requestobject = tests.builders.whorequestobjectbuilder.whorequestobjectbuilder().withdatapackgeid(["2014-11-14"]).build()
        extractor = services.WHODataExtractor.WHODataExtractor(requestobject)
        actual = extractor.urlconstructor()
        expected = "http://apps.who.int/gho/athena/xmart/None?target=None/&profile=None&filter=DATAPACKAGEID:2014-11-14;SEX:None"
        self.assertEqual(expected, actual)

    def test_urlconstructor_returns_url_with_indicatortypes(self):
        requestobject = tests.builders.whorequestobjectbuilder.whorequestobjectbuilder().withindicatortypes(["SITREP_CUMULATIVE", "SITREP_CUMULATIVE_21_DAYS"]).build()
        extractor = services.WHODataExtractor.WHODataExtractor(requestobject)
        actual = extractor.urlconstructor()
        expected = "http://apps.who.int/gho/athena/xmart/None?target=None/&profile=None&filter=INDICATOR_TYPE:SITREP_CUMULATIVE;INDICATOR_TYPE:SITREP_CUMULATIVE_21_DAYS;SEX:None"
        self.assertEqual(expected, actual)

    def test_urlconstructor_returns_url_with_sex(self):
        requestobject = tests.builders.whorequestobjectbuilder.whorequestobjectbuilder().withsex("-").build()
        extractor = services.WHODataExtractor.WHODataExtractor(requestobject)
        actual = extractor.urlconstructor()
        expected = "http://apps.who.int/gho/athena/xmart/None?target=None/&profile=None&filter=SEX:-"
        self.assertEqual(expected, actual)

    def test_urlconstructor_returns_url_with_complete_parameters(self):
        requestobject = tests.builders.whorequestobjectbuilder\
            .whorequestobjectbuilder()\
            .withformat("data-text.csv")\
            .withtarget("EBOLA_MEASURE")\
            .withtargetcolumns(["CASES", "DEATHS"])\
            .withprofile("text")\
            .withcountries(["GIN", "UNSPECIFIED", "LBR", "UNSPECIFIED", "SLE", "UNSPECIFIED"])\
            .withlocations(["-"])\
            .withdatapackgeid(["2014-11-14"])\
            .withindicatortypes(["SITREP_CUMULATIVE", "SITREP_CUMULATIVE_21_DAYS"])\
            .withsex("-").build()
        extractor = services.WHODataExtractor.WHODataExtractor(requestobject)
        actual = extractor.urlconstructor()
        expected = "http://apps.who.int/gho/athena/xmart/data-text.csv?target=EBOLA_MEASURE/CASES,DEATHS&profile=text&filter=COUNTRY:GIN;COUNTRY:UNSPECIFIED;COUNTRY:LBR;COUNTRY:UNSPECIFIED;COUNTRY:SLE;COUNTRY:UNSPECIFIED;LOCATION:-;DATAPACKAGEID:2014-11-14;INDICATOR_TYPE:SITREP_CUMULATIVE;INDICATOR_TYPE:SITREP_CUMULATIVE_21_DAYS;SEX:-"
        self.assertEqual(expected, actual)

if __name__ == '__main__':
    unittest.main()
