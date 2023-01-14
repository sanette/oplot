#!/bin/bash -ve

dune build @doc
rsync -avz --delete _build/default/_doc/_html/ docs
for file in $(find docs/ -name index.html)
do
    # UTF8 right arrow
    sed -i 's|<span class="arrow">&#45;&gt;</span>|<span class="arrow">→</span>|g' $file
    sed -i 's|val</span>\([^:]*\) :|val</span><span class="val">\1</span> :|g' $file

done

chmod 644 docs/odoc.css
chmod 644 docs/oplot/Oplot/index.html
if [[ $(odoc --version) < 2.2.0 ]]
then
    echo "copy odoc-fix.css"
    cp odoc-fix.css docs/odoc.css
fi
echo "img.oplot {max-height: 10em;} " >>  docs/odoc.css
echo "span.keyword {color: #999;} span.val {font-weight: bold;}" >> docs/odoc.css
cp top/example.png docs/oplot/Oplot/
cp share/example2.png docs/oplot/Oplot/
cp top/gamma.png docs/oplot/Oplot/
cp top/surf3d.png docs/oplot/Oplot/

echo "Done"
