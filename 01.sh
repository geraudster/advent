#!/bin/bash

s=$1
prev=${s:0:1}
while read -n 1 c; do
    [[ $c -eq $prev ]] && ((a+=c))
    prev=$c
done <<< "${s:1}"

# circular
[[ ${s: -1} -eq ${s:0:1} ]] && ((a+=${s: -1}))

[[ ${s: -1} -eq ${s:0:1} ]] && echo ok
echo ${s: -1} ${s:0:1}
echo $a
