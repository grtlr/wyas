#!/bin/bash
cabal configure --enable-tests
cabal build
dist/build/parser-tests/parser-tests
