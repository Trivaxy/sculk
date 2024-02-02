# Sculk
A *scripting language* designed to compile into Minecraft datapacks. Far more than just a preprocessor, Sculk is a language that abstracts away the notion of using commands, and instead empowers you to focus purely on your logic and your ideas instead of the implementation details. No more constantly worrying about implementing complicated expressions, scoreboard management, decimal emulation, or dreading the lack of control flow. [Sculk gives it all to you, and more.](https://github.com/Trivaxy/sculk/wiki)

Sculk has 3 goals, which are to make you...
- ... never touch a mcfunction file again
- ... never touch a json file again
- ... forget you were ever writing commands

# Motivation
Back in Minecraft's early days, commands were nothing more than simple administrative actions players can perform. The syntax was clear and there really wasn't much to it.

Fast forward to 1.4.2 and command blocks were introduced. Suddenly, commands could now be used in-world to create gameplay experiences, maps, logic and so on. The system was still rudimentary back then, but this would pave the way for commands to evolve in the future.

As more and more updates dropped, commands grew and became more powerful. Several overhauls, particularly the addition of `execute`, made commands a lot more versatile and capable. Players were not slow to take full advantage of every new feature that was added, and often pushed the limits of what was possible.

Fast forward yet again to 1.13, which marked the introduction of datapacks as a whole, and by extension, mcfunctions. Players finally had a semblance of some form of "scripting" in the game, instead of having to rely on command blocks and redstone, and most importantly in a way that was easy to distribute and use.

All of the additions that accumulated over the years were positive, but they also came with a cost: commands had experienced a big jump in complexity, and their usage was no longer as clear as it used to be. Players in the community often want to accomplish endeavors that, while sounding simple on paper, are either verbose or tedious to implement via commands. Furthermore, utilizing commands to their full potential requires a lot of intimate knowledge about the game's inner workings, its mechanics, quirks and limitations, and the list goes on.

It's not that commands had become bad by any means - we, as players in the community, are creative (and greedy for more features!) and the solutions we want are often impossible, unintuitive or just plain annoying to implement using commands. 

Many tools were made to try and address these issues ([beet](https://github.com/mcbeet/beet), [sandstone](https://github.com/sandstone-mc/sandstone), [trident](https://energyxxer.com/trident/), and a lot others), and they're all powerful in their own right.

One thing that unites those aforementioned tools is that they place heavy emphasis on compile-time features and reduction of verbosity. This is great, but they expose gritty details such as scoreboards, data storage, etc, and often feel a lot more like a preprocessor or just alternative syntax for commands. They perform well as *build tools* but poorly as *abstractions*. It's not a bad thing, it's simply a design choice, but it's  not what I'm looking for.

Sculk focuses on a different approach: it's a *runtime* language designed to make you *forget* that you're working with commands. It's meant to be and feel like it's executed by the game itself - an answer to the "What if Java Edition had proper scripting?" question. You are given all your familiar high-level language constructs that you know and love unconditionally, freeing up your mind to focus on your ideas and your logic, instead of the implementation details.

# Usage

Sculk is in a *heavy* work-in-progress state. While there are a lot of features that have been implemented, the journey is just getting started.
Once Sculk is more ready for use, it will be available as a standalone CLI tool. If you insist on trying it out now, you can clone this repository and build it yourself - but things are still very much in flux.

If you want to know what Sculk will look like and how to use it, read the [wiki](https://github.com/Trivaxy/sculk/wiki) page.

# Goals
- [x] Arithmetic
- [x] Variables
- [x] Control Flow
- [x] Functions
- [x] Inline Command Literals
- [x] Loops
- [ ] Recursion
- [ ] Fixed point numbers
- [ ] Arrays/Lists
- [ ] Structs
- [ ] Strings
- [ ] Events and event handlers
- [ ] Standard library

# Contributing
Contributions are always welcome:
- If you encounter bugs or find something unclear, please open an issue!
- Features and ideas are always welcome, don't hesitate to open an issue for discussion!
