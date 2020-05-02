#!/usr/bin/env nix-shell
#!nix-shell -i python -p "python3.withPackages (ps: with ps; [ruamel_yaml])"

from argparse import ArgumentParser
from ruamel.yaml import YAML
from collections.abc import Iterable

parser = ArgumentParser(description='Converts tag separator from dot to colon in sidecar files, easing migration after GH-164.')
parser.add_argument('file', type=str, nargs='+', help='YAML sidecar file(s) to migrate.')
args = parser.parse_args()

yaml = YAML(typ='rt')  # preserve order, style and comments
yaml.indent(mapping=2, sequence=2, offset=2)

for file_path in args.file:
  with open(file_path, 'r+') as file:
    sidecar = yaml.load(file)
    if not sidecar: continue

    if 'tags' in sidecar and isinstance(sidecar['tags'], Iterable):
      sidecar['tags'] = [tag.replace('.', ':') for tag in sidecar['tags']]

    file.seek(0)
    yaml.dump(sidecar, file)
    file.truncate()
