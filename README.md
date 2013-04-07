imagerl
=======

Image renderer and cacher in Erlang

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


The `convert` utility works fine with piped input and output, for example:

```bash
cat $some_img_file | convert - -thumbnail '200x600!' - | display
```

Implement Erlang port to do the heavy lifting (since Erlang lacks ImageMagick wrappers ;) )...

```erlang
P = open_port({spawn, "/usr/bin/convert - -thumbnail '200x600!' -"}, [stream, binary]).
true = port_command(P, ImageAsBinary).
% Read response... {data, ConvertedImageAsBinary}
true = port_close(P).
```


