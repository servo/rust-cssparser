import colorsys  # It turns out Python already does HSL -> RGB!
import math

# We sometimes get values of 127.499999 which
# may be 127.5000001 in the implementation. We'd like
# to exclude these values.
#
# 127.5 is the only case where the set of values used
# in the test causes rounding errors.
def has_rounding_error(hue, saturation, lightness):
    rgb = colorsys.hls_to_rgb(
                hue / 360.,
                lightness / 1000.,
                saturation / 1000.
    )
    return any(abs((x * 255) - 127.5) < 0.01 for x in rgb)

trim = lambda s: s if not s.endswith('.0') else s[:-2]
print('[')
print(',\n'.join(
    function_format % tuple(
        [
            function_name,
            hue,
            trim(str(saturation / 10.)),
            trim(str(lightness / 10.)),
            alpha_format % round(alpha, 2) if alpha is not None else ''
        ] + [
            trim(str(min(255, round(component * 255.))))
            for component in colorsys.hls_to_rgb(
                hue / 360.,
                lightness / 1000.,
                saturation / 1000.
            )
        ] + [
            alpha if alpha is not None else 1.0
        ]
    )
    for function_format, alpha_format in [
        ('"%s(%s, %s%%, %s%%%s)", [%s, %s, %s, %s]', ', %s'),
        ('"%s(%s %s%% %s%%%s)", [%s, %s, %s, %s]', ' / %s')
    ]
    for function_name in ["hsl", "hsla"]
    for alpha in [None, 1.0, 0.25, 0.0]
    for lightness in range(0, 1001, 125)
    for saturation in range(0, 1001, 125)
    for hue in range(0, 360, 30)
    if not has_rounding_error(hue, saturation, lightness)
))
print(']')
