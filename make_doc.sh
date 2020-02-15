#!/bin/bash -ve

dir="oplot"

cd /home/san/prog/ocaml/$dir/dune-version
dune build @doc
rsync -avz --delete _build/default/_doc/_html/ docs
for file in $(find docs/ -name index.html)          
do
    # UTF8 right arrow
    sed -i "s|<span>&#45;&gt;</span>|<span class=\"arrow\">→</span>|g" $file
done

echo "Done"
