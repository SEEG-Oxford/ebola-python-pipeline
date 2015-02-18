from distutils.core import setup

setup(
    name='EVDpythonpipeline',
    version='0.1',
    packages=['', '', 'builders', 'unittests'],
    package_dir={'': 'services'},
    url='https://github.com/SEEG-Oxford/ebola-python-pipeline',
    license='GPL v2.0',
    author='Andrew Schofield',
    author_email='andrew.schofield@tessella.com',
    description='Python package to automate downloading and processing of EVD case data from the WHO'
)
