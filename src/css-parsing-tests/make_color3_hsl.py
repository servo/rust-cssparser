import colorsys  # It turns out Python already does HSL -> RGB!
import math
import re


def fix_rounding_error(x):
    # We get rounding errors on these values.
    # 127.5 sometimes rounds to 128, sometimes rounds to 127,
    # so let's just brute force fix (haxX0r!1) them!
    x = round(x)
    if x == 127:
        return 128
    return x


def rgba_to_str(r, g, b, a=None):
    if a is None:
        a = 1.0

    r = fix_rounding_error(r * 255)
    g = fix_rounding_error(g * 255)
    b = fix_rounding_error(b * 255)

    if a == 1.0:
        return 'rgb({:g}, {:g}, {:g})'.format(r, g, b)
    else:
        return 'rgba({:g}, {:g}, {:g}, {:g})'.format(r, g, b, a)


def trim(s): return s if not s.endswith('.0') else s[:-2]


print('[')
print(',\n'.join(
    function_format % tuple(
        [
            function_name,
            hue,
            trim(str(saturation / 10.)),
            trim(str(lightness / 10.)),
            alpha_format % round(alpha, 2) if alpha is not None else '',
            rgba_to_str(*colorsys.hls_to_rgb(hue / 360.,
                        lightness / 1000., saturation / 1000.), alpha),
        ]
    )
    for function_format, alpha_format in [
        ('"%s(%s, %s%%, %s%%%s)", "%s"', ', %s'),
        ('"%s(%s %s%% %s%%%s)", "%s"', ' / %s')
    ]
    for function_name in ["hsl", "hsla"]
    for alpha in [None, 1.0, 0.25, 0.0]
    for lightness in range(0, 1001, 125)
    for saturation in range(0, 1001, 125)
    for hue in range(0, 360, 30)
))
print(']')
