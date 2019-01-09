#! /bin/bash -e

rm -rf ../lmcpgen_out/*
java -Xmx2048m -jar dist/LmcpGen.jar -mdm "../OpenUxAS/mdms/CMASI.xml" -ada -dir "../lmcpgen_out"
