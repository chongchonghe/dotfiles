import os
import sys
import json

JSON_PATH = "~/.config/workon/projects.json"

def main():
    json_fn = os.path.expanduser(JSON_PATH)
    flag = sys.argv[1]
    with open(json_fn, 'r') as f:
        try:
            data = json.load(f)
        except json.decoder.JSONDecodeError:
            print("JSONDecodeError. Fix the json file:\n" + json_fn, file=sys.stderr)
            return "."
        if flag not in data:
            print("{} not defined in projects.".format(flag), file=sys.stderr)
            return "."
        else:
            return data[flag]["dir"]

if __name__ == "__main__":
    try:
        print(main())
    except BaseException as err:
        print(err, file=sys.stderr)
        print(".")
