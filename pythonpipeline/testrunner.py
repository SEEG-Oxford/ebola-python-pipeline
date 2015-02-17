"""Module for running tests with the teamcity test runner
while still maintaining compatibility with unittest"""
from teamcity.unittestpy import TeamcityTestRunner
import unittest

if __name__ == '__main__':
    unittest.main(testRunner=TeamcityTestRunner())
