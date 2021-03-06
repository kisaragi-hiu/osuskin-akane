# Akane

New skin.

Name is maybe temporary.

Starting off the base of Retome because this is easier.

Information below may be outdated.

## TODO

We’ll have to trim the history at some point. Certainly the tags, but maybe also everything that’s shared with Retome.

## Building

```bash
racket mkosuskin.rkt -m ja -m external -r dev
```

to render the Japanese version with external assets as the "dev" release.

### Requirements

#### Programs

- inkscape
- lmms
- blender
- bash
- sed
- gm (graphicsmagick)
- convert (imagemagick)
- pngquant
- vips

#### Fonts

Download and move fonts to:

- [GenJyuu Gothic Light](http://jikasei.me/font/genjyuu/) → `_common/fonts/GenJyuuGothic-Light.ttf`
- [KikaiChokokuJIS](http://font.kim/) → `_common/fonts/KikaiChokokuJIS-Md.otf`
- [JK Gothic M](http://font.cutegirl.jp/jk-font-medium.html) → `_common/fonts/JKG-M_3.ttf`
- [JK Gothic L](http://font.cutegirl.jp/jk-font-light.html) → `_common/fonts/JKG-L_3.ttf`
- [Noto Sans CJK Light (OTC)](https://www.google.com/get/noto/help/cjk/) → `_common/fonts/NotoSansCJK-Light.ttc`
- [Noto Sans CJK Medium (OTC)](https://www.google.com/get/noto/help/cjk/) → `_common/fonts/NotoSansCJK-Medium.ttc`

## License

[![image](https://i.creativecommons.org/l/by-nc/4.0/88x31.png)](http://creativecommons.org/licenses/by-nc/4.0/)

This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/">Creative Commons Attribution-NonCommercial 4.0 International License</a>.

### Credits

- applause.ogg, failsound.ogg, and pause-loop.ogg are piano performances of Tree Shade Waltz, Gate of Steiner, and Flower Dance, respectively.
- Hitsounds are taken from [osu-resources](//github.com/ppy/osu-resources), which is under CC-BY-NC 4.0 as well.
