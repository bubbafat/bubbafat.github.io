---
title: "Finding word frequencies using Erlang"
date: "2009-07-02"
categories: 
  - "Programming#Erlang"
---

[Here's a follow-up post that provides a better implementation.](http://www.roberthorvick.com/blog/word-frequency-redux-erlang-list-comprehension-regex-and-list-folding/)

I wanted to try and bite off something a little larger today hitting a few areas that seem generally useful:

1. Basic text [file](http://www.erlang.org/doc/man/file.html) operations
2. Basic string operations
3. Using the [gb\_trees](http://www.erlang.org/doc/man/gb_trees.html) module
4. Avoiding any usage of lists:foreach; instead using tail recursion (that whole "thinking in Erlang" thing).

The problem is to produce the word frequency of a specific text file and to print out the frequency information. For example if a file overdunn.txt contained the text:

_"Dunn was over Unger and I was over Dunn."_ ([Capt. Oveur](http://www.imdb.com/character/ch0006136/quotes), [Airplane II: The Sequel)](http://www.imdb.com/title/tt0083530/)

The output would be:

    Dunn: 2 I: 1 Unger: 1 and: 1 over: 2 was: 2

Word frequencies have some practical uses (fuzzy text matching, building tag clouds, etc). So while it's a bit contrived it is the basis for something useful.

The code doesn't need a lot of explanation - so here you go ...

```erlang
\-module(wordlist).

-export(\[print\_word\_counts/1\]).

%% matches/\* and words/1 From: http://www.trapexit.org/Matching\_Words
matches(H,{match,M}) -> matches(H,M,\[\]).
matches(\_,\[\],Acc) -> Acc;
matches(H,\[{I,L}|T\],Acc) ->
    matches(H,T,\[lists:sublist(H,I,L)|Acc\]).

words(String) -> matches(String,regexp:matches(String, "\[A-Za-z0-1\]+")).

%% builds a tree of word/count pairs.  If the word does not exist in 
%% the tree it is added with an initial value of 1. If the word does
%% exist the count is retrieved and incremented
build\_word\_tree(\[\], Tree) -> Tree;
build\_word\_tree(\[W|R\], Tree) ->
	case gb\_trees:is\_defined(W, Tree) of
		true ->
			Count = gb\_trees:get(W, Tree),
			NewTree = gb\_trees:update(W, Count + 1, Tree),
			build\_word\_tree(R, NewTree);
		false ->
			NewTree = gb\_trees:insert(W, 1, Tree),
			build\_word\_tree(R, NewTree)
	end.

%% reads the next line from the file.  If there is data then...
%% split the data into a list of words and add those to the word tree
process\_each\_line(IoDevice, Tree) ->
	case io:get\_line(IoDevice, "") of
		eof -> 
			file:close(IoDevice),
			Tree;
		{error, Reason} ->
			file:close(IoDevice),
			throw(Reason);
		Data ->
			NewTree = build\_word\_tree(words(Data), Tree),
			process\_each\_line(IoDevice, NewTree)
	end.

%% walks the gb\_tree and prints each word/count pair
print\_tree(Iter) ->
	case gb\_trees:next(Iter) of
		none -> ok;
		{Key, Val, NewIter} ->
			io:format("~s: ~w~n", \[Key,Val\]),
			print\_tree(NewIter)
	end.

%% opens the indicated file, processes the contents and prints
%% out the word/count pairs to stdout
print\_word\_counts(Filename) ->
	case file:open(Filename, read) of
		{ok, IoDevice} ->
			Tree = process\_each\_line(IoDevice, gb\_trees:empty()),
			print\_tree(gb\_trees:iterator(Tree));
		{error, Reason} ->
			io:format("~s~n", \[Reason\])
	end.
```

As usual - I'm just getting started with Erlang. What is the good, bad and ugly with this code?
