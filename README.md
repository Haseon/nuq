nuQ
======

nuQ or nuq (pronounced "nuke") is a quiz script interpreter. Its main use is assisting learning and memorization by making yourself repeatedly solve quizzes. It allows you to deal with more complex data(e.g. Periodic table, Latin verb conjugation) as quizzes, as well as to customize and change anything you want quickly by command line options and modifying scripts(which is written in a simple DSL for quiz making). It is mainly inspired by 'quiz' in bsdgames package in the basic way it interacts with solvers.

## Solving a quiz - Basic guide

1. To solve a quiz, feed a .nuq file to nuq as a command line argument.

```$ ./nuq periodic.nuq```

2. Then it will immediately give you a question by suggesting a key(or by quiz options, keys) from which you should be able to infer other information of that object. You type an answer in order listed above, seperated by comma(s)(if the answer have multiple values together.) If your answer were wrong, nuq will ask "What?" again, until you have the right answer or submit an empty one. nuq will show what was the correct answer when you did the latter.

```The number of this element is 1. What are the name, symbol, period and group of this one?
> booboolah!
What?
> Hydrogen, 1 1, 1
What?
> Hydrogen, H, 1, 1
Correct!
The symbol of this element is Li. What are the number, name, period and group of this one?
> Lithium, 3, 3, asdf
What?
>
3, Lithium, 2, 1
The name of this element is Helium. What are the number, symbol, period and group of this one?
>
```

## write a quiz script

What nuq does is, in fact, to assist memorizing 'tables'. So writing a quiz is no more than writing tables, modified by options and blocked by block signs if you want.

A .nuq file is composed of tables and options. A tables is composed of a category, attributes and objects.

A category stands for what this all is about. For intance, 'element' is the only category in Periodic table. A category is marked by ```<``` and ```>```. In this case, we should write ```<element>``` in the first line of the table to tell the interpreter the category of this table is 'element'.

Attributes defines what types of information you will answer about. In this example, number, symbol and group are attributes. There are two types of attributes: those which can be a key to infer other information, and those which cannot. the former are followed by ```:```, while the latter are followed by ```;```. In this case, we write ```number : name : symbol : period ; group ;``` to define the attributes.

Finally, you should describe objects by writing down the values for attributes in the order listed in the attributes description, seperated by ```:``` or ```;```. ```1 : Hydrogen : H : 1 ; 1 ;``` is a good example for representing 'Hydrgen' object in this system. Ah, you are curious if the correct matching for ```:``` and ```;``` is necessary? Now, the answer will be 'no' (the developer's laziness is the why). But correct colon matching on the matched attributes is heavily recommended.

Here is a simple example explaining what we've read so far:

```<element>

number : name : symbol : period ; group ;

1 : Hydrogen : H : 1 ; 1 ;
2 : Helium : He : 1 ; 18 ;
3 : Lithium : Li : 2 ; 1 ;
```

== Lock out something!

"Now, I want to practise only for this things, and except the other items from quiz for a while, but don't want to permenently remove them from quiz file. Because, later, I will practise for that things, and except what i'm now practising about." might be a kind of very common desire for quiz learners (or, at least, me and my friend.) With nuQ, you can do this by three types of means:

1. You can make nuQ to ignore certain parts of .nuq file by blocking with {{{ and }}}. nuQ reads nothing between {{{ and }}}.

2. To make nuQ ignore a certain line, add '##' at the head of the line.

3. nuQ will also ignore any attributes whose names are preceded by '{{'.

As you can see, it is possible to use the feature to left comments.

== Quiz Options

You can control various things by quiz options. There is a quiz option related with question order. '--fromstart 1' means it will ask you everything from start to end. In contrast, '--fromend 1' means it will ask you from end to start. '--truerandom 20' means it will ask you anything randomly for 20 times.

== Command line options

If you want a quiz to be executed not as the quiz options written in the .nuq file, and it is not worth your 10 seconds for editing, you are just able to change quiz option one-timely with command line options. It is no more than writing what you've been writing in .nuq files in command line.

```$ ./nuq --fromstart 1 periodic.nuq
the number of this element is 1. What are its name, symbol, period and group?
>Hydrogen, H, 1, 1
Correct!
the number of this element is 2. What are its name, symbol, period and group?
>Helium, He, 1, 18
Correct!
the number of this element is 3. What are its name, symbol, period and group?
>Lithium, Li, 2, 1
Correct!
Rights 3, wrongs 0, extra guesses 0, scores 100.0%
```
