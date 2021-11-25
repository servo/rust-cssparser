import colorsys
import math

def fix_rounding_error(val):
    return round(val * 100000) / 100000

# Based on https://github.com/web-platform-tests/wpt/blob/master/css/css-color/color-resolving-hwb.html
def hwb_to_rgb(hue, white, black):
    if white + black >= 1:
        gray = min(max(round(white / (white + black) * 255.0), 0.0), 255.0)
        return (gray, gray, gray)

    rgb = colorsys.hls_to_rgb(
        hue / 360.0,
        0.5,
        1.0
    )

    t = (1.0 - white - black)
    return tuple([min(max(round((fix_rounding_error(x) * t + white) * 255.0), 0.0), 255.0) for x in rgb])

items = []
for hue in [0, 30, 60, 90, 120, 180, 210, 240, 270, 300, 330, 360]:
    for white in [0.0, 0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875, 1.0]:
        for black in [0.0, 0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875, 1.0]:
            (r, g, b) = hwb_to_rgb(hue, white, black)
            items.append('"hwb({:g}deg {:g}% {:g}%)", [{:g}, {:g}, {:g}, 255]'.format(hue, white * 100, black * 100, r, g, b))
            items.append('"hwb({:g} {:g}% {:g}%)", [{:g}, {:g}, {:g}, 255]'.format(hue, white * 100, black * 100, r, g, b))
            for alpha in [0, 0.2, 1]:
                items.append('"hwb({:g}deg {:g}% {:g}% / {:g})", [{:g}, {:g}, {:g}, {:g}]'.format(hue, white * 100, black * 100, alpha, r, g, b, alpha * 255))
                items.append('"hwb({:g} {:g}% {:g}% / {:g})", [{:g}, {:g}, {:g}, {:g}]'.format(hue, white * 100, black * 100, alpha, r, g, b, alpha * 255))

print('[')
print(',\n'.join(items))
print(']')
