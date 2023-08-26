# Sculk
Sculk is a scripting language designed to compile into Minecraft datapacks. Using Sculk, you can easily create datapacks with complex functionalities and logic while remaining in the comfort of a simple and easy language.

# Motivation
Minecraft possesses a wide variety of commands in the game. Most of them are easy to learn and use, and often perform just one singular task. Originally, commands were only meant to be administrative actions that players can do - nothing more, nothing less.

However, players eventually noticed that with some clever tricks and techniques, those commands can be pushed to do far greater things: Gameplay mechanics, features, logic, and the list goes on - but this came at a cost: using them can be unintuitive or difficult, especially as projects grow more complex and demanding. Many things that are easily expressible in normal programming languages are either cumbersome or very difficult to represent using only commands.

It's clear that Minecraft's commands were never meant to be used this way, and although Minecraft has taken steps to make the lives of datapack developers easier, the system itself hasn't changed and it likely never will. 

To alleviate some of those pain points, many great tools have been created to make the process of creating datapacks easier. They are often useful and can be powerful, but they share the same design choice: they are all, fundamentally, preprocessors that follow an elaborate expansion scheme. You will still be thinking in terms of functions and commands, and these tools emphasize their power at compile-time, but fall short at runtime.

Sculk targets a different approach: it is a scripting language that is designed to feel as if it is being run by the game itself. It has runtime semantics, designed to conceal implementation details such as scoreboards, storage, functions, etc. and instead fixates on empowering you to focus on the logic of your datapack - not the gritty, boilerplate/verbose details of *how* to implement it.

Put simply, Sculk is one answer to the question *"What if Java Edition had a scripting language?"*. If other tools are like C's preprocessor, Sculk is like C itself.

# Usage

Sculk is in a *heavy* work-in-progress state. While there are a lot of features that have been implemented, the journey is just getting started.
Once Sculk is more ready for use, it will be available as a standalone CLI tool. If you insist on trying it out now, you can clone this repository and build it yourself - but things are still very much in flux.

If you want to know what Sculk will look like, read the [wiki](https://github.com/Trivaxy/sculk/wiki) page.

# Goals
- [x] Arithmetic
- [x] Variables
- [x] Control Flow
- [x] Functions
- [x] Inline Command Literals
- [x] Loops
- [ ] Recursion
- [ ] Fixed floating point numbers
- [ ] Arrays/Lists
- [ ] Structs
- [ ] Strings
- [ ] Events and event handlers
- [ ] Standard library

# Contributing
Contributions are always welcome:
- If you encounter bugs or find something unclear, please open an issue!
- Features and ideas are always welcome, don't hesitate to open an issue for discussion!
