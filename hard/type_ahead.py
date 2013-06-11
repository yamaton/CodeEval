"""
Your task is to build a type-ahead feature for an upcoming product.

When the user enters a word or set of words, we want to be able to "predict" what they're going to type next with some level of accuracy. We've chosen to implement this using the N-Gram algorithm defined here

http://en.wikipedia.org/wiki/N-gram.

Your program should return a tuple of predictions sorted high to low based on the prediction score (upto a maximum of three decimal places, or pad with zeroes upto three decimal places i.e. 0.2 should be shown as 0.200), (if predictions share the same score, they are sorted alphabetically.) Words should be split by whitespace with all non-alphanumeric characters stripped off the beginning and end.

Prediction scores are calculated like this:

Occurrences of a word after an N-gram / total number of words after an N-gram
e.g. for an N-gram of length 2:

ONE TWO ONE TWO THREE TWO THREE

"TWO" has the following predictions:

THREE:0.666,ONE:0.333

"THREE" occurred 2 times after a "TWO" and "ONE" occurred 1 time after a "TWO", for a total of 3 occurrences after a "TWO".

Your program will run against the following text i.e. Harcode it into your program:

Mary had a little lamb its fleece was white as snow;
And everywhere that Mary went, the lamb was sure to go. 
It followed her to school one day, which was against the rule;
It made the children laugh and play, to see a lamb at school.
And so the teacher turned it out, but still it lingered near,
And waited patiently about till Mary did appear.
"Why does the lamb love Mary so?" the eager children cry;
"Why, Mary loves the lamb, you know" the teacher did reply."


Input sample:

Your program should accept as its first argument a path to a filename.The input file contains several lines. Each line is one test case. Each line contains a number followed by a string, separated by a comma. eg

2,the

The first number is the n-gram length. The second string is the text printed by the user and whose prediction you have to print out.

Output sample:

For each set of input produce a single line of output which is the predictions for what the user is going to type next. eg.

lamb,0.375;teacher,0.250;children,0.125;eager,0.125;rule,0.125

Things that are good: Documentation, Readability, Simplicity. Things that are not so good: Pre-optimization

Bonus points for:
1. Making it able to dynamically add new corpus data. 
      ---> Done
2. Allow reverse ngram lookups to find what words commonly precede words/phrases
      ---> Done

Ver 0.12  2012-08-16  Fixed cleanup function following the instruction.
Ver 0.11  2012-08-16  collections.Counter is not available in CodeEval
                      So make my own.
Ver 0.10  2012-08-16  Created



[TODOs]
For some reason the program does not pass with 100 score.
I have some misunderstandings in the specification

"""
import sys
import re
import itertools

class NGram:
    def __init__(self):
        self.data = {}
        self.data_backward = {}
    
    def learn(self, text, n):
        """
        Update n-gram data with new text. Specify n-gram length n to learn.
        """
        self._learn(self.data, text, n)
        self._learn(self.data_backward, text, n, reverse=True)
    
    def _learn(self, data, text, n, reverse=False):
        chain = itertools.chain
        new_data = (gather_by_nwords(t, n, reverse) for t in cleanup(text))
        for (most, last) in chain(*new_data):
            if most in data:
                data[most].append(last)
            else:
                data[most] = [last]

    def get(self, words, reverse=False):
        """
        Get (next_word, probability) pairs from N-ram following the given words.
        Reverse ngram lookup is returned if reverse option is set True.
        """
        data = self.data_backward if reverse else self.data
        words = self._gate_transform(words)
        counter = Counter(data[words])
        total = float(sum(counter.itervalues()))
        words_and_probs = ((w, times/total) for (w, times) in counter.iteritems())
        return sorted(words_and_probs, key=lambda x: (-x[1], x[0]))

    def _gate_transform(self, words):
        """
        Transform the input words into tuple format appropriate for the Ngram data.
        """
        if isinstance(words, str):
            if " " in words:
                words = tuple(words.split())
            else:
                words = (words,)
        elif isinstance(words, list):
            words = tuple(words)
        return words
        
    def reset(self):
        """
        Erase n-gram data.
        """
        self.data = {}


def Counter(seq):
    d = {}
    for key in seq:
        if key in d:
            d[key] += 1
        else:
            d[key] = 1
    return d


def cleanup(text):
    """
    Remove separators and newlines from one chunk of text and return list of subtext.
    
    cleanup("Hello, there. I'm in NY.")
    ----> ["Hello there I'm in NY"]
    """
    text = text.replace('\n', ' ')
    text = re.sub('[,.;:?!\"]', '', text)
    return [text]


def gather_by_nwords(text, n, reverse=False):
    """
    Return (most, last) pairs in generator.
    """ 
    out = text.split()
    if reverse: out.reverse()
    size = len(out)
    return [(tuple(out[i:i+n-1]), out[i+n-1]) for i in xrange(size-n+1)]


def process(n, word):
    ngram = NGram()
    ngram.learn(THE_TEXT, n)
    return ngram.get(word)


def format(output):
    return ";".join("%s,%1.3f" % (word, prob) for (word, prob) in output)


def test():
    assert cleanup("Hello, there. I'm in NY.") == ["Hello there I'm in NY"]
    
    text = "ONE TWO ONE TWO THREE TWO THREE TWO THREE."
    ngram = NGram()
    ngram.learn(text, 2)
    assert ngram.get("TWO") == [("THREE", 0.75), ("ONE", 0.25)]
    assert ngram.get("TWO", reverse=True) == [("ONE", 0.5), ("THREE", 0.5)]
    ngram.learn(text, 3)
    assert ngram.get("ONE TWO") == [("ONE", 0.5), ("THREE", 0.5)]
    
    ngram = NGram()
    ngram.learn(THE_TEXT, 2)
    assert ngram.get("the") == [('lamb', 0.375), ('teacher', 0.25), ('children', 0.125), 
                                ('eager', 0.125), ('rule', 0.125)]
    ngram.learn(THE_TEXT, 3)
    assert ngram.get("the lamb") == [('love', 1./3), ('was',  1./3), ('you',  1./3)]
    
    assert format([('lamb', 0.375), ('teacher', 0.25), ('children', 0.125), 
                   ('eager', 0.125), ('rule', 0.125)]) \
          == "lamb,0.375;teacher,0.250;children,0.125;eager,0.125;rule,0.125"
    print "passed all tests!"




THE_TEXT = """
Mary had a little lamb its fleece was white as snow;
And everywhere that Mary went, the lamb was sure to go. 
It followed her to school one day, which was against the rule;
It made the children laugh and play, to see a lamb at school.
And so the teacher turned it out, but still it lingered near,
And waited patiently about till Mary did appear.
"Why does the lamb love Mary so?" the eager children cry;
"Why, Mary loves the lamb, you know" the teacher did reply."""
    


if __name__ == '__main__':
    # test()
    
    with open(sys.argv[1], "r") as f:
        data = (line.rstrip().split(',') for line in f if line.rstrip())
        data = [(int(n), s) for (n, s) in data]
    
    outputs = [process(n, word) for (n, word) in data]
    for out in outputs:
        print format(out)
