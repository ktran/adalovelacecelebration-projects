# from https://github.com/jsvine/markovify

import markovify
import nltk
import re
nltk.download('averaged_perceptron_tagger')

from nltk.tokenize import TweetTokenizer


class POSifiedText(markovify.Text):

    def set_user_words(self, user_words):
        self.user_words = user_words

    def test_sentence_output(self, words, max_overlap_ratio, max_overlap_total):
        """
            Given a generated list of words, accept or reject it.

            Matches with overlap (built-in) and by matching to a list of user-strings (extended).
        """
        l_words = [s.lower() for s in words]
        l_userwords = [s.lower() for s in self.user_words]

        if set(l_words).intersection(l_userwords):
            return markovify.Text.test_sentence_output(self, words, max_overlap_ratio, max_overlap_total)
        else:
            return False


class DonaldGen():

    def __init__(self, filename="donald.txt"):
        self.model = self.__build_model(filename)

    def __build_model(self, filename):

        # Get raw text as string.
        with open(filename) as f:
            text = f.read()

        # Build the model.
        text_model = POSifiedText(text, state_size=1)

        return text_model

    def __noun_finder(self, sentence):

        tknzr = TweetTokenizer()
        text = tknzr.tokenize(sentence)

        words = ["::".join(tag) for tag in nltk.pos_tag(text) ]

        pattern = r'(.*)\::NN'

        nouns = [w for w in words if 'NN' in w]

        just_words = []
        for w in words:
            match = re.match(pattern, w)
            if match:
                just_words.append(match.group(1))

        return just_words

    def print_examples(self):

        # Print five randomly-generated sentences
        for i in range(5):
            print(self.model.make_sentence())

        # Print three randomly-generated sentences of no more than 140 characters
        for i in range(3):
            print(self.model.make_short_sentence(140))

    def reply(self, user_says):
        user_words = self.__noun_finder(user_says)
        self.model.set_user_words(user_words)
        return self.model.make_short_sentence(140, tries=100)

if __name__ == '__main__':

    donald = DonaldGen()
    s = donald.reply("My name is adam and I like computers")
    print(s)
