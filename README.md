# JuicyPixels-blurhash

![CI](https://github.com/SamProtas/JuicyPixels-blurhash/workflows/CI/badge.svg)
![Hackage Version](https://img.shields.io/badge/hackage/JuicyPixels-blurhash?color=blue)
![Hackage Deps](https://img.shields.io/hackage-deps/v/JuicyPixels-blurhash)


Blurhash is a very compact represenation of a placeholder for an image.

This library provides a Blurhash encoding and decoding implementation based on the JuicyPixels represenation of images.

For the full Blurhash sales pitch and algorithm explaination see either of:

  - The website: <https://blurha.sh/>

  - The central git repo: <https://github.com/woltapp/blurhash>

An image such as:

![example image](docs/example.jpg)

Can be encoded as:

LGFFaWYk^6#M@-5c,1Ex\@\@or[j6o

Which your client can render as:

![example blurred image](docs/blurred.png)


Full library documentation can be found in the Haddocks for `Codec.Picture.Blurhash`.
