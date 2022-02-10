from typing import List
from operator import itemgetter
def main():
    build_dzn_file()

def build_dzn_file():
    file_name = "words5Sol.dzn"
    all_allowed = sorted(get_all_solutions())

    s = f"int: numWords5 = {len(all_allowed)};\n"
    s += "array[1..numWords5, 1..5] of int: words5 = array2d(1..numWords5, 1..5,[\n"
    for w in all_allowed:
        for c in list(w):
            s += c + ","
        s+= "\n"
    s += "]);"
    with open(file_name, "w") as f:
        f.write(s)

def find_freq():
    all_allowed = get_all_allowed()

    freq_dict = {}
    tot = 0
    for word in all_allowed:
        for c in list(word):
            if c in freq_dict:
                freq_dict[c] += 1
            else:
                freq_dict[c] = 1
            tot += 1
    print("Total letters:" + str(tot))
    sort_list = sorted(list(freq_dict.items()), key=itemgetter(0))
    s = ""
    for el in sort_list:
        s += str(el[1]) + ","
    s = s[:-1]
    print("Letter frequencies:[" + s + "]")
    print("Most frequent letter:" + max(freq_dict, key= lambda x: freq_dict[x]))


def get_all_allowed()-> List[str]:
    allowed_path = "wordle-allowed-guesses.txt"
    answers_path = "wordle-answers-alphabetical.txt"
    with open(answers_path) as f:
        answers = f.read().splitlines()
    with open(allowed_path) as f:
        allowed = f.read().splitlines()

    return sorted(answers + allowed)

def get_all_solutions()-> List[str]:
    answers_path = "wordle-answers-alphabetical.txt"
    with open(answers_path) as f:
        answers = f.read().splitlines()

    return sorted(answers)

if __name__ == '__main__':
    main()