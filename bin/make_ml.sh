#!/bin/bash

echo generating ml files...

for name in example example_pdf anim logistique matrix surf3d gamma gslex
do
    echo $name
    grep -v "^#" ../top/$name.ml > $name.ml
    echo "quit ()" >> $name.ml
done
