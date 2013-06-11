import nose
import beautiful_string


class TestBeautifulNumber(nose.TestCase):
    knownValues = (('ABbCcc', 152),
                   ('Good luck in the Facebook Hacker Cup this year!', 754),
                   ('Ignore punctuation, please :)', 491),
                   ('Sometimes test cases are hard to make up.', 729),
                   ('So I just go consult Professor Dalves', 646))

    def testKnownValues(self):
        for (x, fx) in self.knownValues:
            result = beautiful_string.beautiful_number(x)
            assert fx == result


if __name__ == '__main__':
    nose.main()
