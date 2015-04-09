__author__ = 'andrew.schofield@tessella.com'

import unittest
import services.WHODataPostProcessor
import csv


class processortests(unittest.TestCase):
    def test_formatlist_works_with_one_int(self):
        with open("../../EVD_conf_prob_.csv") as csvfile:
            reader = csv.reader(csvfile)
            with open("../../data/expected_output_headers.csv") as headerfile:
                headerreader = csv.reader(headerfile)
                headers = headerreader.next()
                postprocessor = services.WHODataPostProcessor.WHODataPostProcessor(reader, headers)
                test = postprocessor.FindAndInsertMissingRegions()

if __name__ == '__main__':
    unittest.main()
