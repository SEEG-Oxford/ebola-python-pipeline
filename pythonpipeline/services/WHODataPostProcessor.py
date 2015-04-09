__author__ = 'andrew.schofield@tessella.com'

class WHODataPostProcessor(object):
    def __init__(self, whodata, expectedheaders):
        self.WHOData = whodata
        self.ExpectedHeaders = expectedheaders

    def FindAndInsertMissingRegions(self):
        # get actual headers
        headers = self.WHOData.next()
        columnstoadd = []
        all = []
        # The index is 1 here because although the expected headers start at 0,
        # the actual headers start at 1
        index = 1
        for expectedHeader in self.ExpectedHeaders:
            if expectedHeader in headers:
                None
            else:
                columnstoadd.append(index)
                headers.insert(index, expectedHeader)
            index += 1
        all.append(headers)
        for row in self.WHOData:
            for column in columnstoadd:
                row.insert(column, 0)
                all.append(row)
        return all
