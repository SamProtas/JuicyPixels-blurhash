# JuicyPixels-blurhash

![CI](https://github.com/SamProtas/JuicyPixels-blurhash/workflows/CI/badge.svg)
[![Hackage Version](https://img.shields.io/hackage/v/JuicyPixels-blurhash?color=blue)](https://hackage.haskell.org/package/JuicyPixels-blurhash)
![Hackage Deps](https://img.shields.io/hackage-deps/v/JuicyPixels-blurhash)


Blurhash is a very compact representation of a placeholder for an image.

This library provides a Blurhash encoding and decoding implementation based on the JuicyPixels representation of images.

For the full Blurhash sales pitch and algorithm explaination see either of:

  - The website: <https://blurha.sh/>

  - The central git repo: <https://github.com/woltapp/blurhash>

An image such as:

![example image](https://raw.githubusercontent.com/SamProtas/JuicyPixels-blurhash/master/docs/example.jpg)

Can be encoded as:

LGFFaWYk^6#M@-5c,1Ex\@\@or[j6o

Which your client can render as:

![example blurred image](https://raw.githubusercontent.com/SamProtas/JuicyPixels-blurhash/master/docs/blurred.png)


Full library documentation can be found in the Haddocks for `Codec.Picture.Blurhash`.
