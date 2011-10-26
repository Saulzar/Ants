#! /bin/bash

rm package -rf
mkdir package
cp dist/build/Ants/Ants package
cp *.hs package

cd package
zip ../Ants.zip *
rm package -rf