__author__ = 'andrew.schofield@tessella.com'

import unittest
import services.WHODataExtractor
import model.whorequestobject


class formatlisttests(unittest.TestCase):
    def test_formatlist_works_with_one_int(self):
        extractor = services.WHODataExtractor.WHODataExtractor(None)
        actual = extractor.formatlist('TEST', [1])
        expected = 'TEST:1;'
        self.assertEqual(actual, expected)

    def test_formatlist_works_with_two_ints(self):
        extractor = services.WHODataExtractor.WHODataExtractor(None)
        actual = extractor.formatlist('TEST', [1,2])
        expected = 'TEST:1;TEST:2;'
        self.assertEqual(actual, expected)

    def test_formatlist_works_with_one_string(self):
        extractor = services.WHODataExtractor.WHODataExtractor(None)
        actual = extractor.formatlist('TEST', ['1'])
        expected = 'TEST:1;'
        self.assertEqual(actual, expected)

    def test_formatlist_works_with_two_strings(self):
        extractor = services.WHODataExtractor.WHODataExtractor(None)
        actual = extractor.formatlist('TEST', ['1','2'])
        expected = 'TEST:1;TEST:2;'
        self.assertEqual(actual, expected)

    def test_urlconstructor_returns_unparameterised_url_if_parameters_are_empty(self):
        requestobject = model.whorequestobject.WHORequestObject()
        extractor = services.WHODataExtractor.WHODataExtractor(requestobject)
        actual = extractor.urlconstructor()
        expected = "http://apps.who.int/gho/athena/xmart/None?target=None/&profile=None&filter=None"
        self.assertEqual(actual, expected)

    def test_urlconstructor_returns_url_with_format(self):
        requestobject = model.whorequestobject.WHORequestObject()
        extractor = services.WHODataExtractor.WHODataExtractor(requestobject)
        actual = extractor.urlconstructor()
        expected = "http://apps.who.int/gho/athena/xmart/None?target=None/&profile=None&filter=None"
        self.assertEqual(actual, expected)

if __name__ == '__main__':
    unittest.main()
