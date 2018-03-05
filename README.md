## haskell-web-anki (flashcard for electron and web)

This repository contains my start with both: any web development, functional reactive programming.
It uses great Ryan Trinkle's library Reflex (and Reflex-DOM).
It's not very usable atm, but if you're Haskell geek and have some time for setting it up 
and flashcards to memoize (prepared as images), then go ahead and try.

The code is a mess, full of patches and experiments. But should compile (may require some additional tinkering)
to an electron app using scripts in top directory. At first it was meant to be a web application, that's why it's split into 
server (haskell servant) and frontend application. Both parts are written almost exclusively in Haskell.
Frontend is compiled to JS using amazing GHCJS.

## License

MIT License

Copyright (c) 2018 Michał Adamczyk

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
