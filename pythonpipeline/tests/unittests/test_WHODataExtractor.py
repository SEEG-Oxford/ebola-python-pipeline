__author__ = 'andrew.schofield@tessella.com'

import unittest
import services.WHODataExtractor


class MyTestCase(unittest.TestCase):
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


if __name__ == '__main__':
    unittest.main()
