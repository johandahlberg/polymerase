#!/bin/bash
set -o nounset
set -o errexit

sbt stage


diff <(cat README.md | md5sum) \
     <(cat README.md | ./target/universal/stage/bin/polymerase-erlich-encode | \
    ./target/universal/stage/bin/polymerase-simulate-errors | \
    ./target/universal/stage/bin/polymerase-drop-reads | \
    ./target/universal/stage/bin/polymerase-erlich-decode | md5sum) 

