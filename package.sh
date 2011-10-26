#! /bin/bash

rm package -rf
mkdir package

cabal install
cp dist/build/Ants/Ants package
cp MyBot.hs package/
cp Ant package/ -rf

cd package
zip ../Ants.zip * -r

cd ..
rm package -rf