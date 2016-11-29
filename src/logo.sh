#!/bin/bash

cat > logo.ml << EOF
let program_name = 
"
EOF

toilet SAJA -f mono9 --metal >> logo.ml
sed -i 's/[ \t]*$//' logo.ml
echo "\"" >> logo.ml
