# AdventCode

There is some Practice with `Haskell`.

## Getting Started

### Test

```bash
 runhaskell day1.hs test
```

### Get OutPut

```bash
 runhaskell day1.hs
```

## Thoughts

### Day 1

It uses the `rotate`, which is used in most algorithm problems.
I use a trick solution :blush:

### Day 2

It need to know how to solve the `Matrix` datastructure or **2D** structure.
:smiley:

### Day 3

It's too hard for me to implement spiral and i founded abstract to practice is a long way.
the second part make me hard. it use the _hashmap_ and more high order function to make it elegant:sob:.

## Day 4

:yum: It needs some knowledge about the `Set` and needs know something about the [**nub**](https://hoogle.haskell.org/?hoogle=nub) function. but it's have O(N^2) complexity.
then i found the other algorithm on [Stackoverflow](https://stackoverflow.com/questions/16108714/removing-duplicates-from-a-list-in-haskell-without-elem)

- The original solution is too Un-Haskell(aha step by step).
- The second solution comes from the **AI (ChatGPT)**,which use the classical _List Comprehension_

```haskell
part1 xs = length [x | x <- map (splitOn " ") (splitOn "\n" xs), length (nub x) == length x]

part2 xs = length [x | x <- map (splitOn " ") (splitOn "\n" xs), length (setdups x) == length x]
```

## Day 5

Use a abstact python code to get the answer.
I am not good at **Haskell**.

![day5](https://i.imgur.com/wJAqNjM.jpg)

if you use `runhaskell` to get the result. It will comes to a **stack overflow** error.

And you compiled it to run you will find it would got the answer.
I think this is about the _Loop_.:wink:

## Day 6

Oh, It seems more complicated than before:scream:
So I used **Python** to implement it and use **GPT-4** to translate it.
![day6](https://i.imgur.com/tNqJR0R.jpg)

## ScreenShots

![pic](Assets/example.jpg)

## Help

some code was helped by `OpenAI ChatGPT`. because i am not familiar with **Haskell** :blush:.

## License

MIT
