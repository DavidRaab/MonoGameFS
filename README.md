### About the Project: MonoGame + F#

I started game development some years ago and was unhappy about most engines.
I learned Unity first but the way how you structure your project is just horrible
in my opinion. Even if they say it would be an ECS system, it isn't. Unity
just provides Components with the lack of Entity and the Systems making
it a different kind of an architecture.

On top I was already used to functional programming before from Perl & JavaScript
and really disliked OO over the years more and more. So working with Unity
and C# became more and more pain to me to. Especially after learning F#. When I
started to learn F# I already used it with Unity but it doesn't worked well together.

So after I deleted all my projects I started again and choosed MonoGame as
a development platform.

Monogame has the advantage of full control. Basically just four methods to
overwrite. How you structure and design your game is your own choice. But
that's also its disadvantage as you have to write everything from the ground up
yourself. What also is an advantage as I learn myself a lot about game
development.

This project aims to become a general engine on top of Monogame. The goal is to
provide a true ECS architecture combined with the MVU architecture introduced
by Elm.

A game is really nothing else as a continous call to an `update` function,
preferrably running at least with 60 frames per second and then `draw` the
current state (Model) of the game.

But a game has some critical performance requirements. It doesn't make
sense if everything becomes immutable. That's why it is still the Elm architecture.
The performance critical things are written with mutability in most cases, but
the less critical things, what is still most of the game logic, can use immutability.

With this project I work towards this goal. I implement most of the features
that a basic 2D game needs. Providing abstraction of the most parts with an ECS
architecture that works well in a functional approach.

When the goal complets a user of the engine will only have to write `09_Assets.fs`
and `10_Program.fs` for his own game. Maybe adding other files for his/her own
components and systems.

# Features implemented

* **Entity**: Everything is an entity and you can add/remove components at runtime.
* Sprite Sheet & Sprite Sheet Animations
* **Transforms**: Every Entity can have a parent and is positioned, scaled and rotated to its parent
* **Camera**: It supports multiple cameras that you can move or Zoom
* **Input Handling**: Inputs are transformed to Actions
* **Timer System**: Running code periodically or after a specific time-frame that depends on the GameTime
* **RenderTarget**: You specify an internal resolution and the game scales with the display resolution
* **Fixed Update**: A fixed update loop that runs as often as needed per seconds independent from the frame-rate.

# Features to come

* UI
* Animation System
* Multiple GameTime's
* State Machine or Behaviour Trees
* Basic Collision System maybe Physics
* Particle System
* Sound / Music
* Separation into its own library (will probably be the last step)

# Stability

Currently nothing is stable. The whole API and data-structures will likely change
over time as I see fit to it. The engine will only target F# and no other .NET language.

### License

[![CC0 1.0 Universal](https://licensebuttons.net/p/zero/1.0/88x31.png)](http://creativecommons.org/publicdomain/zero/1.0/)

License: ([CC0 1.0 Universal](http://creativecommons.org/publicdomain/zero/1.0/)) You're free to use the code in any project, personal or commercial. There's no need to ask permission before using these. Giving attribution is not required, but is greatly appreciated!

### Asset Licenses

Some Assets are not my own see Resources.md for those Licenses!