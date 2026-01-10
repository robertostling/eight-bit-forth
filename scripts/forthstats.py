# Quick and dirty tool to generate statistics for Forth code.

import sys
from collections import Counter, defaultdict


def tokenize(s):
    word = ''
    in_comment = False
    in_string = False
    for c in s:
        if in_comment:
            if c == ')':
                in_comment = False
                word = ''
        elif in_string:
            if c == '"':
                in_string = False
                word = ''
        elif c.isspace():
            if word in ('(', '.('):
                in_comment = True
            elif word in ('"', '."'):
                in_string = True
            elif word:
                yield word
                word = ''
        else:
            word += c
    if word:
        yield word


def analyze(code):
    words = list(tokenize(code))
    print(words)
    defined = defaultdict(list)
    all_defined = set()
    uses = Counter()
    defining_words = ': VAR VARW VARIABLE VARIABLEW'.split()
    for i, word in enumerate(words):
        if i > 1 and words[i-1] in defining_words:
            defined[words[i-1]].append(word)
            all_defined.add(word)
        else:
            uses[word] += 1

    for word, n in uses.most_common():
        print(f"  {n:3d}  {word}")

    print("Created")
    for creator, created_words in defined.items():
        print(f"  {len(created_words)} words from {creator}")
    print()
    print("Usage counts")
    print(Counter(uses.values()))
    print()
    unused = all_defined - set(uses.keys())
    print("Unused: ", sorted(unused))


if __name__ == '__main__':
    code = ''
    for filename in sys.argv[1:]:
        with open(filename) as f:
            code += f.read() + '\n'
    analyze(code)
