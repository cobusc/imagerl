# imagerl [![Build Status](https://travis-ci.org/cobusc/imagerl.png?branch=master)](https://travis-ci.org/cobusc/imagerl)

Image renderer and cacher in Erlang, with WURFL Cloud support. Image convertion is done using the `convert` utility provided by ImageMagick.

Quick usage guide
=================

Cache and serve an image
------------------------
If the image is located at "http://www.google.co.za/images/srpr/logo3w.png", make the following call:
```
http://localhost:8000/render?url=http%3A%2F%2Fwww.google.co.za%2Fimages%2Fsrpr%2Flogo3w.png
```
The image will be retrieved, cached and served the first time it is request. Thereafter it will simply be served from the cache.

Local files can be served using the "file://" scheme. Source image caching can be disabled in the config file if necessary.

Restrict the width or height
----------------------------
If you require the image to have a restricted width or height, it can be specified:
```
http://localhost:8000/render?url=http%3A%2F%2Fwww.google.co.za%2Fimages%2Fsrpr%2Flogo3w.png&width=100
```
or
```
http://localhost:8000/render?url=http%3A%2F%2Fwww.google.co.za%2Fimages%2Fsrpr%2Flogo3w.png&height=100
```
The aspect ratio will be preserved in this case.

The `width` or `height` values may be set to `wurfl`, in which case a WURFL lookup will be performed using the specified `ua` argument, or the detected device. Opera Mini's custom user agent header is supported.

Resize an image to specific dimensions
--------------------------------------
If you need to resize the image to specific dimensions, specify BOTH the width and the height:
```
http://localhost:8000/render?url=http%3A%2F%2Fwww.google.co.za%2Fimages%2Fsrpr%2Flogo3w.png&width=100&height=100
```

Annotate the image with text
----------------------------
Text can be annotated diagonally across the image by adding "annotation=<text>", e.g.
```
http://localhost:8000/render?url=http%3A%2F%2Fwww.google.co.za%2Fimages%2Fsrpr%2Flogo3w.png&annotation=SAMPLE
```

Debugging
---------
For debugging purposes the following additional arguments have been exposed:
* `nocache` - Forces the imager to retrieve the specified image from the source and repopulate the cached copy, rather than use the cached copy.
* `debug` - Enables detailed debug logging for the request.


User interface
==============

A graphical user interface is available at
```
http://localhost:8000/ui
```


Notes to self...
================

Check out
```
http://www.imagemagick.org/Usage/annotating/
```

Some notes from the Imagick website relating to geometry specifications...

<table>
    <tr><th>size</th><th>General description (actual behavior can vary for different options and settings)</th></tr>
    <tr><td>scale%</td><td>Height and width both scaled by specified percentage.</td></tr>
    <tr><td>scale-x%xscale-y%</td><td>Height and width individually scaled by specified percentages. (Only one % symbol needed.)</td></tr>
    <tr><td>width</td><td>Width given, height automagically selected to preserve aspect ratio.</td></tr>
    <tr><td>xheight</td><td>Height given, width automagically selected to preserve aspect ratio.</td></tr>
    <tr><td>widthxheight</td><td>Maximum values of height and width given, aspect ratio preserved.</td></tr>
    <tr><td>widthxheight^</td><td>Minimum values of width and height given, aspect ratio preserved.</td></tr>
    <tr><td>widthxheight!</td><td>Width and height emphatically given, original aspect ratio ignored.</td></tr>
    <tr><td>widthxheight&gt;</td><td>Shrinks an image with dimension(s) larger than the corresponding width and/or height argument(s).</td></tr>
    <tr><td>widthxheight&lt;</td><td>Enlarges an image with dimension(s) smaller than the corresponding width and/or height argument(s).</td></tr>
    <tr><td>area@</td><td>Resize image to have specified area in pixels. Aspect ratio is preserved.</td></tr>
</table>


