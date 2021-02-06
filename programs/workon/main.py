import os
import sys
import json
from argparse import ArgumentParser, RawTextHelpFormatter
from getdir import JSON_PATH

def parse_arguments():
    parser = ArgumentParser(description="A simple utility to manage projects",
                            formatter_class=RawTextHelpFormatter)
    parser.add_argument("action", help="""The action. One of the following:
add: add a project
    add flag dir
update: update a project
    add flag dir
info: show info of a project
    info flag
rm: remove a project
    rm flag
""")
    parser.add_argument("flag", help="The flag.")
    parser.add_argument("opt", nargs='?', )
    return parser.parse_args()

def make_project(proj, is_root=True):
    """ Initiate a project """
    for _dir in ["src", "docs", "tests", "main", "plots"]:
        os.system(f"mkdir -p {proj}/{_dir}")
    if is_root:
        os.system(f"mkdir -p {proj}/writeup")
        os.system(f"touch {proj}/README.md")

def make_projects(proj):
    """ Initiate a project """
    for _dir in ["src", "docs", "tests", "projects", "writeup"]:
        os.system(f"mkdir -p {proj}/{_dir}")
    os.system(f"touch {proj}/README.md")

# def make_project(proj, is_projects=False):
#     """ Initiate a project """
#     dirs = ["src", "docs", "tests"]
#     dirs += "projects" if is_projects else "main"
#     for _dir in dirs:
#         os.system("mkdir -p {proj}/{_dir}")

def main(args, is_force=False):
    json_fn = os.path.expanduser(JSON_PATH)
    is_write = False
    if args.action == "add":
        is_write = True
        with open(json_fn, 'r') as f:
            data = json.load(f)
            if not is_force and args.flag in data:
                print("{} already defined in projects. Use 'update flag dir' to update a project".format(args.flag))
                return
            thisdir = os.path.abspath(args.opt)
            dir_dict = {
                "dir": thisdir
            }
            print("Added the following to project {}:".format(args.flag))
            print(dir_dict)
            data[args.flag] = dir_dict
    elif args.action == "update":
        args.action = "add"
        main(args, is_force=True)
    elif args.action == "info":
        with open(json_fn, 'r') as f:
            data = json.load(f)
            print(data[args.flag])
    elif args.action == "rm":
        is_write = True
        with open(json_fn, 'r') as f:
            data = json.load(f)
            del data[args.flag]
    elif args.action == "prj":
        # initiate a project
        make_project(args.flag)
    elif args.action == "prjs":
        # initiate a complex project
        make_projects(args.flag)
        os.chdir(f"{args.flag}/projects")
        make_project("project1")
        make_project("project2")
        
    if is_write:
        with open(json_fn, 'w') as f:
            json.dump(data, f, indent=2)

if __name__ == '__main__':
    sys.exit(main(parse_arguments()))
