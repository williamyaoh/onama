# Onama [![License](https://img.shields.io/badge/license-BSD--3-ff69b4.svg)](https://github.com/williamyaoh/onama/blob/master/LICENSE) [![Build Status](https://travis-ci.org/williamyaoh/onama.svg?branch=master)](https://travis-ci.org/williamyaoh/onama)

Defines some HTML primitive parsers to use with Parsec.

## Usage

You'll need to import Parsec into your project in order to use this library.

```haskell
testParser1 = do
  openTagAny
  openTag "div"
  data <- text
  closeTag "div"
  closeTagAny
  return data
```
