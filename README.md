# Elmboy [![Build Status](https://travis-ci.org/Malax/elmboy.svg?branch=master)](https://travis-ci.org/Malax/elmboy)
A work in progress Game Boy emulator written in pure Elm. 

Try it out here: [https://malax.github.io/elmboy](https://malax.github.io/elmboy)

![The Legend of Zelda: Link's Awakening Gameplay](https://raw.githubusercontent.com/Malax/elmboy/master/readme-assets/zelda.png)
![Kirby's Dreamland Gameplay](https://raw.githubusercontent.com/Malax/elmboy/master/readme-assets/kirby.png)
![Pokemon Red Title Screen](https://raw.githubusercontent.com/Malax/elmboy/master/readme-assets/pokemon.png)

## Goals
This is mainly for my own entertainment, education and challenge. It's a non-trivial problem to solve, I love retro games and have a some sort of emotional 
connection  to the Game Boy. There are hundreds of Game Boy emulators out there, but none for Elm yet. And as I like Elm very much, this is the perfect 
side-project for me. Also, I think it's pretty cool!

However, there are some goals I set for myself when I started. I wanted to be able to run my favourite Game Boy games:

1. [The Legend of Zelda: Link's Awakening](https://en.wikipedia.org/wiki/The_Legend_of_Zelda:_Link%27s_Awakening)
2. [Pokémon Red and Blue](https://en.wikipedia.org/wiki/Pok%C3%A9mon_Red_and_Blue)
3. [Super Mario Land 2: 6 Golden Coins](https://en.wikipedia.org/wiki/Super_Mario_Land_2:_6_Golden_Coins)
4. [Super Mario Land](https://en.wikipedia.org/wiki/Super_Mario_Land)
5. [Tetris](https://en.wikipedia.org/wiki/Tetris)

Except for poor performance and Pokémon, all of them run to a certain degree.

Another goal was staying true to Elm. As little Javascript as possible and nice functional code all around. Sadly, I had to compromise on the **nice** 
functional code in some cases due to performance.

## Non-Goals
- Achieving a very high degree of emulation accuracy
- Becoming a "serious" emulator that is used by Homebrew developers and/or speed runners
- Debugging capabilities
- Game Boy Color support

## Performance
In its current state, the emulator runs at about 95% speed compared to an original Game Boy. 
This is on my MacBook Pro (Mid 2015, 2.5 GHz Intel Core i7, 16 GB RAM), running Chrome 69.0.3497.100. Performance in Firefox is worse,
Chrome seems to have a better performing JIT for this project.

I already refactored the codebase several times for performance, removing a lot of abstractions that made the code a lot nicer but performed worse than the 
current version. I want to reintroduce some of those abstractions and conciser types down the line, without sacrificing performance too much. It's hard to 
strike a perfect balance on that. If you see very un-Elm-y code, it's probably due to some performance impact the more idiomatic solution had.

Eventually, I plan to bring emulation speed up to 100% on a decently sized machine.

### So Elm is slow?
No, that is not what I am saying. Elm is made for web applications — not emulator development. This project does almost never change the DOM and has
requirements that you usual web app does not have. Especially the amount of state updates is by magnitudes larger than I can imagine even the fanciest
web app might have. If you look at emulators written in other statically typed functional languages, like Haskell, you will see that, even though they compile to
native machine code, do use *escape hatches* to implement state in a performant way like 
[IORef](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-IORef.html)s that are not available in Elm.

I'm actually surprised how fast it is, without spending too much time on optimisation. I imagined that performance might be the hardest hurdle to overcome
with this project before I even started.

I'm curious how much performance can be squeezed out of the code after optimisation! I bet there are a lot of things I could do more efficiently.

## Building
Run `npm install` to produce an optimised build in the `dist` folder. For development, run `npm start` to spawn a HMR development server.
