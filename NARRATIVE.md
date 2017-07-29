# Project narrative.

## My basic conceit.

I want to actually do something at least semi-useful with [Haskell][haskell],
rather than just repeatedly running through tutorials, going away for
a few months, remembering I love Haskell, coming back, re-running the
tutorials, lather, rinse, repeat ...

Concidentally, I just bought the latest repackaged version of my old favorite
RPG from way back, [Paranoia][paranoia].  "Look," says I, "I can combine my
old favorite RPG with my new favorite programming language, and maybe do
something just barely complicated enough so that I have an excuse to get in
some coding in Haskell.

As I recall, one of the first computer programs I ever wrote was a
character generator for [Twilight 2000][t2k], written in [Turbo Pascal][tpasc].
My heavens, that was a long time ago.  And it states clearly in the new
*Paranoia* rules that GMs shouldn't worry about generating NPCs, just
make it up as you go along.  So of course, I'll write a character generator
for new-edition *Paranoia* NPCs.

All is well.

## Setting up my development environment.

I use a mid-2015 MacBook Pro with way too much RAM, running (at the time
of writing) macOS Sierra 10.12.5; I use the [Homebrew][brew] package manager
for things that don't come standard with Sierra.  So the following setup notes
will be great for other Mac users, probably okay for Unix-y users, and fairly
worthless for anyone else.

To start, I need a Haskell development toolchain, courtesy of `brew` and
the [Stack][stack] project:

    % brew update
    % brew install haskell-stack

        % which stack
        /usr/local/bin/stack

        % stack --version | head -1
        Version 1.4.0 x86_64

    % cd ~/Desktop/Projects
    % stack new s9-concilium-dementia
    % cd s9-concilium-dementia
    % stack setup

Some explanation is in order: I keep all of my endless unfinished projects
under `${HOME}/Desktop/Projects` (and hopefully remember to check them into
source code control once in a while), and on my laptop, I use a naming
convention of `s9-concilium-dementia` to mean "You did this project under
your `stendhal9@white-knight.org account`, and probably checked it into
Github under the `concilium` organization, so hopefully when your local hard
drive crashes, you'll still be able to find something useful at
`https://github.com/concilium/dementia`, if you're lucky."

The `stack` invocations template out a starting project hierarchy, and also
install the various Haskell toolchain components as a handy side-effect
(well, as a handy something, since Haskell doesn't allow side-effects --
that's a joke).

The project name "dementia" is *ipso facto* the perfect name for a *Paranoia*
character generator.

## Things I did immediately after the above.

A few cleanup tasks:

*   Replaced the template-installed LICENSE file with one I liked better;
*   Renamed and adjusted the `*.cabal` package definition file;
*   Cleaned up (i.e., removed a bunch of comments from) the `stack.yaml` file;
*   Renamed `app/Main.hs` to `apps/dp.hs`;
*   Renamed `src/Lib.hs` to `src/Dementia.hs`;
*   Renamed `test/Spec.hs` to `tests/test.hs`.

I have no real idea if the above renames follow any kind of Haskell best
practices, and right now I don't care -- I'll fix them up later if I got them
wrong.

The main goal is to have a command-line utility, `dp`, that does my character
generation, using code mostly from the `Dementia` module.  Oh, and `dp` stands
for "dementia praecox", you pervs.

Another thing I did was to resolve to myself (at this point) that I was going
to do proper TDD/BDD on this project -- road to hell and all that, we shall
see.  So I added [HSpec][hspec]-based testing to the `*.cabal` file.  That
turned into its own special adventure, trying to get `stack build --test` to
actually run to completion against a quick "canary" test.  I eventually got
it working, but don't ask me how: it's basically the magic combination of
configuration in the `*.cabal` file, plus the voodoo incantation in
`test/specs.hs`, plus the right syntax in `DementiaSpec.hs`, plus remembering
to run `stack clean` and `stack setup` (compiled language, right).

## Starting with a basic data model.

So I started with the most basic `Character` type, with a `name`, a `sector`
identifier, and a `clone` number.  I wrote the HSpec tests for it in
`DementionSpec.hs`, forgetting at first about using `let`-bindings because
I hadn't written functional-syntax code in months or maybe years.  The tests
failed, so I wrote a quick minimal type definition in `Dementia.hs`, using
Haskell's record syntax; hooray, tests pass.  As an aside, this is my
usual command-line invocation:

    % stack clean && stack setup && stack build --test

About five minutes after this, I realized that I needed (well, wanted) to
put constraints on the `Character` fields, e.g. `sector` is technically a
`String`, but it's a `String` that's exactly three alphabetical characters
long, all uppercase.  In Haskell, the convention (I discovered) is to hide
the real type constructor(s), and then do instantiation (okay, OO term, but
forgive me) via factory functions that do all the value checking and
constraining -- this is called using [smart constructors][smcons] in
Haskell-speak.

The most important thing to remember: when you take the value constructors
out of the module exports list, you have to add in the implicitly-created
field access functions that the record syntax automagically creates for you.
Look at the top of `Dementia.hs` and you'll see what I mean.

After a while, the pattern I seemed to be settling on was:

*   Create a complex data type (e.g., `Character`) using record syntax,
    and supporting it with type aliases (e.g., `Sector`) so that I wasn't
    just using `String`, `Integer`, etc. for the record fields.

*   For each of the type aliases (e.g., `Sector`), create a verification
    function (e.g., `verifySector`) that implemented the value constraints
    appropriate to that aliased type (e.g., must be three uppercase letters)
    as a set of guards.

*   Create a smart constructor (e.g., `designedCharacter`) for the complex
    data type, using guards on the smart constructor to invoke the verification
    functions on each incoming parameter.

I added HSpec tests for each of the verification functions, using
`shouldSatisfy` and `shouldNotSatisfy` expectations to tease out good and
bad inputs -- I had a suspicion at this point that I should be looking at
something like [QuickCheck][qcheck] to generate test input data, but the
coffee shop was closing at this point, so I gave it a pass for the moment.

This was also the point where I realized I was only doing positive checks
on `designedCharacter`, and not testing to see if bad parameters produced
errors -- which then became the point where I realized I didn't really know
anything about the [error handling idioms][errors] in Haskell.

So ... remedial reading, particularly an article on eight different ways to
to [report errors in Haskell][eightways], led me to choose method 4 from that
article: I refactored so that `designedCharacter` was returning a type of
`(Monad m) => Name -> Sector -> Clone -> m Character`. Then I spent about
four hours trying to figure out what the corresponding HSpec assertions
should look like, including yet another trip through various tutorials on
Haskell monads.  I got to a moderately unsatisfying point (the tests in
`DementiaSpec.hs` look for `anyException`, but I still can't figure out
how to properly test for a specific exception), and decided to not let the
perfect be the enemy of the good.

## A moment of utter exhaustion.

Concidentally, at this exact point, I came across a more recent article on
[Haskell error handling][eightrevs], which contradicted everything I just
spent hours figuring out how to do. I cursed the perversity of the universe,
along with my own compulsive desire to do things "right", then wrote this
paragraph to remind myself to come back and figure this out once blood stopped
squirting out my ears.

## Followed by a moment of intractable stubbornness.

And right after writing the above paragraph, I decided to ignore the squirting
blood, dove back into the code, and refactored everything to use the
`Control.Exception` style, per the recommendation in the conclusion to the
more-recently-written article on error handling.

Then I spent another hour trying to figure out how to make HSpec properly
look for my newly specific `DementiaException` values, got 85% of the way
there, and decided to again not let the perfect be the enemy of the good.
Cursing, I moved on.

## In all honesty.

In all honesty, at this point, I took a break (okay, got distracted) for a
couple of weeks, and when I got back, all of this looked kind of mildly
pointless. So I said to myself, "go build a wiki", because it's what I do
when I can't think of anything else to do ... so off to learn [Yesod][yesod].



[haskell]: https://www.haskell.org/
[paranoia]: https://www.kickstarter.com/projects/1990654819/paranoia-rpg
[t2k]: https://en.wikipedia.org/wiki/Twilight_2000
[tpasc]: https://en.wikipedia.org/wiki/Turbo_Pascal
[brew]: https://brew.sh/
[stack]: https://www.haskellstack.org/
[hspec]: https://hspec.github.io/
[smcons]: https://wiki.haskell.org/Smart_constructors
[qcheck]: https://hackage.haskell.org/package/QuickCheck
[errors]: http://learnyouahaskell.com/input-and-output#exceptions
[eightways]: http://www.randomhacks.net/2007/03/10/haskell-8-ways-to-report-errors/
[eightrevs]: http://blog.ezyang.com/2011/08/8-ways-to-report-errors-in-haskell-revisited/
[yesod]: http://www.yesodweb.com/
