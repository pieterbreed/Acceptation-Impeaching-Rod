# impeaching-rod

Code appreciation homework.

This code was written as part of our Code Appreciation initiative at Allan Gray. The code creates an in-memory 
corpus of knowledge (a collection of facts represented as simple documents) and then provides a way to 'search' 
through the corpus with customizable matching and ranking criteria.

A search is represented as a tree of matching functions as well as a document that parameterizes the functions.
For every document in the corpus the matching function produces a value between 0 and 1 which provides the ranking
value for every document, which can then be used to sort the documents.

The code was written in two afternoons using Clojure and as basically only a proof of concept. I don't think there
is anything wrong with it, but that might just be because it doesn't really do a lot.

## Installation

Download the [standalone jar](https://github.com/downloads/pieterbreed/Acceptation-Impeaching-Rod/impeaching-rod-0.1.0-SNAPSHOT-standalone.jar)

## Source code

The main code is in [`rules.clj`](https://github.com/pieterbreed/Acceptation-Impeaching-Rod/blob/master/src/impeaching_rod/rules.clj), but the recruitment example is presented in the [`recruitment.clj`](https://github.com/pieterbreed/Acceptation-Impeaching-Rod/blob/master/src/impeaching_rod/recruitment.clj) file. The examples in the comments in that file can be executed in a repl by running the jar as below and going into the impeaching-rod.recruitment namespace like shown

## Usage

    $ java -jar impeaching-rod-0.1.0-standalone.jar 
    > (in-ns 'impeaching-rod.recruitment)

